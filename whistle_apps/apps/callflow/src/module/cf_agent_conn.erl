%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(cf_agent_conn).

-export([handle/2]).

-include("../callflow.hrl").

-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> 'ok'.
handle(Data, #cf_call{call_kvs=KVs, to_user=ToUser, to_realm=ToRealm}=Call) ->
    ShouldRecord = wh_json:is_true(<<"record_call">>, Data, true),
    ?LOG("Will record: ~s", [ShouldRecord]),

    Skills = wh_json:from_list(orddict:to_list(KVs)),
    Req = [{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Control-Queue">>, cf_exe:control_queue_name(Call)}
           ,{<<"Skills-Needed">>, Skills}
           ,{<<"Record-Call">>, ShouldRecord}
           ,{<<"To-User">>, ToUser}
           ,{<<"To-Realm">>, ToRealm}
           | wh_api:default_headers(<<>>, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_acd:publish_agent_connect(Req),
    ?LOG("waiting for hangup"),
    cf_call_command:wait_for_hangup(),
    cf_exe:continue(Call).
