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
    QID = wh_json:get_value(<<"id">>, Data),
    {ok, Queue} = couch_mgr:open_doc(whapps_call:account_db(Call), QID),

    ExitKey = wh_json:get_value(<<"caller_exit_key">>, Queue, <<"#">>),
    ConnTimeout = wh_json:get_integer_value(<<"connection_timeout">>, Queue, 30),

    publish_queue_join(Queue, Call),

    log_queue_activity(Call, <<"enter">>, QID),

    wait_for_conn_or_exit(Call, ExitKey, ConnTimeout*1000, QID).

publish_queue_join(Queue, Call) ->
    JObj = wh_json:from_list([{<<"Queue">>, Queue}
                              ,{<<"Call">>, whapps_call:to_json(Call)}
                              ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    wapi_queue:publish_queue_member(JObj).

wait_for_conn_or_exit(Call, ExitKey, ConnTimeout, QID) ->
    Start = erlang:now(),

    case whapps_call_command:wait_for_application_or_dtmf(<<"bridge">>, ConnTimeout) of
        {ok, _JObj} ->
            lager:debug("bridge returned: ~p", [_JObj]),
            log_queue_activity(Call, <<"exit">>, QID),
            cf_exe:hangup(Call);
        {dtmf, ExitKey} ->
            lager:debug("exit key ~s pushed", [ExitKey]),
            log_queue_activity(Call, <<"dtmf_exit">>, QID),
            cf_exe:continue(Call);
        {dtmf, _Key} ->
            lager:debug("other key ~s pressed", [_Key]),
            wait_for_conn_or_exit(Call, ExitKey, ConnTimeout - (timer:now_diff(erlang:now(), Start) div 1000), QID);
        {error, timeout} ->
            lager:debug("caller timed out in the queue"),
            log_queue_activity(Call, <<"timeout">>, QID),
            cf_exe:continue(Call);
        {error, _E} ->
            lager:debug("error: ~p", [_E]),
            log_queue_activity(Call, <<"error">>, QID),
            cf_exe:continue(Call)
    end.

log_queue_activity(Call, Action, QID) ->
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"action">>, Action}
                             ,{<<"timestamp">>, wh_util:current_tstamp()}
                             ,{<<"queue_id">>, QID}
                             ,{<<"pvt_type">>, <<"queue_activity">>}
                            ]),
    couch_mgr:save_doc(whapps_call:account_db(Call), Doc).
