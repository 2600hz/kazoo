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

-spec handle/2 :: (json_object(), #cf_call{}) -> 'ok'.
handle(Data, #cf_call{call_kvs=KVs}=Call) ->
    ShouldRecord = wh_json:is_true(<<"record_call">>, Data, true),

    Skills = wh_json:from_list(orddict:to_list(KVs)),
    Req = [{<<"Call-Id">>, cf_exe:callid(Call)}
           ,{<<"Control-Queue">>, cf_exe:control_queue_name(Call)}
           ,{<<"Skills-Needed">>, Skills}
           ,{<<"Record-Call">>, ShouldRecord}
           | wh_api:default_headers(<<>>, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_acd:agent_connect(Req),
    cf_call_command:wait_for_hangup(),
    cf_exe:continue(Call).
