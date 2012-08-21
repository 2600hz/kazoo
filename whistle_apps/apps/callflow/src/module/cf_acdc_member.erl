%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id"
%%% }
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_acdc_member).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:hold(Call),

    QueueId = wh_json:get_value(<<"id">>, Data),

    MemberCall = props:filter_undefined(
                   [{<<"Account-ID">>, whapps_call:account_id(Call)}
                    ,{<<"Queue-ID">>, QueueId}
                    ,{<<"Call">>, whapps_call:to_json(Call)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ]),

    {ok, QueueJObj} = couch_mgr:open_cache_doc(whapps_call:call_id(Call), QueueId),
    MaxWait = wh_json:get_integer_value(<<"max_wait_time">>, QueueJObj),

    case whapps_util:amqp_pool_request(MemberCall
                                       ,fun wapi_acdc_queue:publish_member_call/1
                                       ,fun wapi_acdc_queue:member_call_success_v/1
                                       ,MaxWait * 1000
                                      ) of
        {ok, _SuccessJObj} ->
            lager:debug("agent took the member_call: ~p", [_SuccessJObj]),
            cf_exe:control_usurped(Call);
        {error, timeout} ->
            lager:debug("member_call timed out waiting in the queue for ~p s", [MaxWait]),
            cf_exe:continue(Call);
        {error, _Fail} ->
            lager:debug("failed to process the member_call: ~p", [_Fail]),
            cf_exe:continue(Call)
    end.
