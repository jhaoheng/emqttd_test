%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
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

%% @doc emqttd plugin template application.
-module(emqttd_plugin_template_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_plugin_template_sup:start_link(),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_demo, []),
    ok = emqttd_access_control:register_mod(acl, emqttd_acl_demo, []),
    emqttd_plugin_template:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    emqttd_plugin_template:unload().

