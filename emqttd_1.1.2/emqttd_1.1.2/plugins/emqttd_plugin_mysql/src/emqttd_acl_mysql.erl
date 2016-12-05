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

%% @doc ACL with MySQL Database
-module(emqttd_acl_mysql).

-behaviour(emqttd_acl_mod).

-include("../../../include/emqttd.hrl").

%% ACL Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {super_query, acl_query, acl_nomatch}).

init({SuperQuery, AclQuery, AclNomatch}) ->
    {ok, #state{super_query = SuperQuery, acl_query = AclQuery, acl_nomatch = AclNomatch}}.

% check_acl({Client = #mqtt_client{client_id = ClientId, username = Username}, PubSub, Topic}, _State) ->
%         io:format("=====ACL CHECK=====~n"),
%         io:format("Acl Info: clientId=~p, username=~p, PubSub=~p~n, Topic=~p~n", [ClientId, Username, PubSub, Topic]),
%         splitTopic(Topic),
%         allow.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, #state{super_query = SuperQuery,
                                          acl_query   = {AclSqlBeta, AclParams},
                                          acl_nomatch = Default}) ->
    io:format("~n~n=====ACL CHECK=====~n"),
    io:format("ACL SQL = ~p ; ACL Params = ~p ;~n?_PubSub = ~p ; _Topic = ~p ; ~n",[AclSqlBeta, AclParams, PubSub, Topic]),
    Company = splitTopic(Topic),
    AclSql = sqlCombineWithCompany(AclSqlBeta,Company),
    ReceiveData = [1,topicToInt(atom_to_list(PubSub)),Topic],
    io:format("ReceiveData : ~p~n",[ReceiveData]),
    % allow.
    case emqttd_plugin_mysql:query(AclSql, AclParams, Client) of
        {ok, _Columns, []} ->
            io:format("***No Find Any Topic in this company Name.***~n~n"),
            Default;
        {ok, _Columns, Rows} ->
            io:format("Topic Rows = ~p~n~n",[Rows]),
            % allow
            case lists:member(ReceiveData,Rows) of
                true  -> allow;
                false -> {error, bad_username}, Default
            end
    end.

    % case emqttd_plugin_mysql:is_superuser(SuperQuery, Client) of
    %     false -> case emqttd_plugin_mysql:query(AclSql, AclParams, Client) of
    %                 {ok, _Columns, []} ->
    %                     Default;
    %                 {ok, _Columns, Rows} ->
    %                     Rules = filter(PubSub, compile(Rows)),
    %                     case match(Client, Topic, Rules) of
    %                         {matched, allow} -> allow;
    %                         {matched, deny}  -> deny;
    %                         nomatch          -> Default
    %                     end
    %             end;
    %     true  ->
    %         allow
    % end.

topicToInt(N) ->
    case N of
        "subscribe" -> 1;
        "publish" -> 2;
        "pubsub" -> 3
    end.


reload_acl(_State) ->
    ok.

description() ->
    "ACL with Mysql".


%%--------------------------------------------------------------------
%% 切割 Topic
%%--------------------------------------------------------------------

splitTopic(Topic) -> 
    % Topic = "/zavio/ipcam/alert/sic001/",
    FilterTopic = re:replace(Topic, "[^A-Za-z0-9/_#+]", "", [global, {return, list}]), % 過濾掉 <<>> 符號 保留特殊字元
    Keys = string:tokens(FilterTopic, "/"),
    [Company | Others] = Keys,
    io:format("~n===Split Topic===~n"),
    io:format("- Company = ~p~n- Others = ~p~n~n", [Company, Others]),
    rsplit(Company).

rsplit(Company) -> 
    Company.

%%--------------------------------------------------------------------
%% 結合 str
%%--------------------------------------------------------------------
-spec concat(String1, String2) -> String3 when
      String1 :: string(),
      String2 :: string(),
      String3 :: string().

concat(S1, S2) -> S1 ++ S2.

%%--------------------------------------------------------------------
%% 結合 full-sql = acl_sql + company
%%--------------------------------------------------------------------
sqlCombineWithCompany(AclSql , Company) -> 
    % AclSql = "select allow, access, topic from mqtt_acl where company = '<topic_company>'",
    % Company = "orbweb",
    AclSql2 = re:replace(AclSql, "<topic_company>", Company, [global, {return, list}]),
    io:format("full sql = ~p~n",[AclSql2]),
    rCombine(AclSql2).
rCombine(AclSql2) -> 
    AclSql2.

