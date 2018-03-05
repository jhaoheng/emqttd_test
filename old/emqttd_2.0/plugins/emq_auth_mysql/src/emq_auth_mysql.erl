%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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

-module(emq_auth_mysql).

-behaviour(emqttd_auth_mod).

-include_lib("emqttd/include/emqttd.hrl").

-import(emq_auth_mysql_cli, [is_superuser/2, query/3]).

-export([init/1, check/3, description/0]).

-record(state, {user_auth_query, device_auth_query, super_query, hash_type}).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

init({UserAuthQuery, DeviceAuthQuery, SuperQuery, HashType}) ->
    {ok, #state{
        user_auth_query = UserAuthQuery, 
        device_auth_query = DeviceAuthQuery,
        super_query = SuperQuery, 
        hash_type = HashType}
    }.

% debug 用
% check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
%         io:format("Auth Demo: clientId=~p, username=~p, password=~p~n",
%                   [ClientId, Username, Password]),
%         Role = checkPattern([Username]),
%         io:format("~p~n", [Role]),
%         ok.

check(#mqtt_client{username = Username}, Password, _State) when ?EMPTY(Username); ?EMPTY(Password) ->
    {error, username_or_password_undefined};

check(Client = #mqtt_client{username = Username}, Password, #state{user_auth_query  = {UserAuthSql, UserAuthParams},
                                                                   device_auth_query  = {DeviceAuthSql, DeviceAuthParams},
                                                                   super_query = SuperQuery,
                                                                   hash_type   = HashType}) ->
    Role = checkPattern([Username]),
    case Role of
        "email" ->  {ok, AuthSql=UserAuthSql, AuthParams=UserAuthParams};
        "device" ->  {ok, AuthSql=DeviceAuthSql, AuthParams=DeviceAuthParams};
        false -> {ok, AuthSql=UserAuthSql, AuthParams=UserAuthParams}
    end,

    Result = case query(AuthSql, AuthParams, Client) of
                 {ok, [<<"password">>], [[PassHash]]} ->
                     check_pass(PassHash, Password, HashType);
                 {ok, [<<"password">>, <<"salt">>], [[PassHash, Salt]]} ->
                     check_pass(PassHash, Salt, Password, HashType);
                 {ok, _Columns, []} ->
                     {error, notfound};
                 {error, Reason} ->
                     {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperQuery, Client)}; Error -> Error end.

check_pass(PassHash, Password, HashType) ->
    check_pass(PassHash, hash(HashType, Password)).
check_pass(PassHash, Salt, Password, {salt, HashType}) ->
    check_pass(PassHash, hash(HashType, <<Salt/binary, Password/binary>>));
check_pass(PassHash, Salt, Password, {HashType, salt}) ->
    check_pass(PassHash, hash(HashType, <<Password/binary, Salt/binary>>)).

check_pass(PassHash, PassHash) -> ok;
check_pass(_, _)               -> {error, password_error}.

description() -> "Authentication with MySQL".

hash(Type, Password) -> emqttd_auth_mod:passwd_hash(Type, Password).



%%--------------------------------------------------------------------
%% 檢查使用哪一個 query 進行判斷
%%--------------------------------------------------------------------

-define (Email_pattern, "\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*\\b").
-define (Mac_address_pattern, "^(?:[0-9a-zA-Z]{2}[:]?){5}(?:[0-9a-zA-Z]{2}?)$").

% 檢查 username 是否符合 regx
multire([],_) ->
    nomatch;
multire([RE|RegExps],String) ->
    case re:run(String,RE,[{capture,none}]) of
    match ->
        RE;
    nomatch ->
        multire(RegExps,String)
    end.

checkPattern(Foo) -> 
    getRole(multire([[?Email_pattern], [?Mac_address_pattern]],Foo)).

getRole([?Email_pattern]) ->
    "email";
    % {ok, #state{auth_query = UserAuthQuery, super_query = SuperQuery, hash_type = HashType}};
getRole([?Mac_address_pattern]) ->
    "device";
    % {ok, #state{auth_query = DeviceAuthQuery, super_query = SuperQuery, hash_type = HashType}};
getRole(nomatch) ->
    false.