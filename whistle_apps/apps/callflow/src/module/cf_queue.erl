%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_queue).

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
    whapps_call_command:ring(Call),
    QID = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(whapps_call:account_db(Call), QID) of
        {error, _Reason} ->
            lager:info("failed to find queue ~s: ~p", [QID, _Reason]),
            cf_exe:continue(Call);
        {ok, Queue} ->
            ConnTimeout = wh_json:get_integer_value(<<"connection_timeout">>, Queue, 30),
            lager:info("transfering call control to ACD queue '~s' with timeout ~b", [QID, ConnTimeout]),
            publish_queue_join(Queue, Call, ConnTimeout),
            wait_for_queue(Call)
    end.

-spec publish_queue_join/3 :: (wh_json:json_object(), whapps_call:call(), non_neg_integer()) -> 'ok'.
publish_queue_join(Queue, Call, ConnTimeout) ->
    JObj = wh_json:from_list([{<<"Queue">>, Queue}
                              ,{<<"Queue-ID">>, wh_json:get_value(<<"_id">>, Queue)}
                              ,{<<"Call">>, whapps_call:to_json(Call)}
                              ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                              | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
                             ]),
    wapi_queue:publish_new_member(JObj, ConnTimeout).

-spec wait_for_queue/1 :: (whapps_call:call()) -> 'ok'.
wait_for_queue(Call) ->
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"queue">>, <<"result">>} ->
                    case wh_json:get_value(<<"Result">>, JObj) of
                        <<"ANSWERED">> -> cf_exe:stop(Call);
                        _ -> cf_exe:continue(Call)
                    end;
                _ -> wait_for_queue(Call)
            end;   
        _ -> wait_for_queue(Call)                    
    end.


