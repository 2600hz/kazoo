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

-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    ShouldRecord = wh_json:is_true(<<"record_call">>, Data, true),
    ShouldRecord andalso ?LOG("will record this call"),   
    Skills = whapps_call:kvs_fetch(cf_agent_skills, Call),
    Req = [{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Control-Queue">>, cf_exe:control_queue_name(Call)}
           ,{<<"Skills-Needed">>, Skills}
           ,{<<"Record-Call">>, ShouldRecord}
           ,{<<"To-User">>, whapps_call:to_user(Call)}
           ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
           | wh_api:default_headers(<<>>, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_acd:publish_agent_connect(Req),
    ?LOG("waiting for hangup"),
    whapps_call_command:wait_for_hangup(),
    cf_exe:continue(Call).
