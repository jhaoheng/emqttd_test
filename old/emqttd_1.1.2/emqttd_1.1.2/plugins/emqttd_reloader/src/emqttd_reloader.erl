%%--------------------------------------------------------------------
%% Copyright (c) 2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% Notice that this file is copied from mochiweb project.
%%
%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(emqttd_reloader).

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([reload_module/1, reload_modules/1, all_changed/0, is_changed/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {last, tref, log, trace}).

%%--------------------------------------------------------------------
%% API.
%%--------------------------------------------------------------------

%% @doc Start the Reloader.
start_link(Opts) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%% @doc Stop the reloader.
-spec(stop() -> ok).
stop() -> gen_server:call(?MODULE, stop).

%% @doc Reload modules
-spec(reload_modules([atom()]) -> [{module, atom()} | {error, term()}]).
reload_modules(Modules) -> [reload_module(M) || M <- Modules].

%% @doc Reload a module
-spec(reload_module(atom()) -> {error, term()} | {module(), atom()}).
reload_module(Module) when is_atom(Module) ->
    code:purge(Module), code:load_file(Module).

%% @doc Return a list of beam modules that have been changed.
-spec(all_changed() -> [atom()]).
all_changed() -> [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
-spec(is_changed(atom()) -> boolean()).
is_changed(M) when is_atom(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
        false
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Opts]) ->
    Interval = proplists:get_value(interval, Opts, 0),
    LogFile  = proplists:get_value(logfile, Opts, "log/emqttd_reloader.log"),
    {ok, Trace} = lager:trace_file(LogFile, [{module, ?MODULE}], info),
    {ok, init_timer(Interval, #state{last = stamp(), log = LogFile, trace = Trace})}.

init_timer(Secs, State) when Secs =< 0 ->
    State;
init_timer(Secs, State) ->
    {ok, TRef} = timer:send_interval(timer:seconds(Secs), do),
    State#state{tref = TRef}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(do, State) ->
    Now = stamp(), do_(State#state.last, Now),
    {noreply, State#state{last = Now}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{tref = TRef, trace = Trace}) ->
    cancel_timer(TRef), lager:stop_trace(Trace).

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam), Vsn;

module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs), Vsn.

do_(From, To) ->
    [case file:read_file_info(Filename) of
         {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
            case reload_module(Module) of
                {module, Module} ->
                    lager:info([{module, ?MODULE}], "Reload module ~s successfully.", [Module]),
                    ok;
                {error, Reason} ->
                    lager:info([{module, ?MODULE}], "Failed to reload module ~s: ~p.", [Module, Reason]),
                    error
            end;
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             lager:error([{module, ?MODULE}], "Error reading ~s's file info: ~p~n",
                         [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

stamp() -> erlang:localtime().

cancel_timer(undefined) -> ok;
cancel_timer(TRef)      -> timer:cancel(TRef).

