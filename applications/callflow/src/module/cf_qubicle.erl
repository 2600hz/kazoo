%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz
%%% @doc Callflow interface for Qubicle.  This module will route a call
%%% to Qubicle, first by creating a session for the call, then joining
%%% the session to the queue-id configured in the callflow doc data.
%%%
%%% @contributors
%%%     Mark Magnusson
%%%-------------------------------------------------------------------
-module(cf_qubicle).

-include("../callflow.hrl").
-include_lib("whistle/src/wh_json.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @doc
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId     = wh_json:get_value(<<"queue_id">>, Data),
    QueueConfig = couch_mgr:open_cache_doc(whapps_call:account_db(Call), QueueId),

    case QueueConfig of
        {'ok', Config} ->
            whapps_call_command:answer(Call),
            create_session(Call, Config);

        {'error', 'not_found'} ->
            lager:error("queue is not configured for callflow"),
            cf_exe:continue(Call)
    end.

-spec create_session(whapps_call:call(), wh_json:object()) -> 'ok'.
create_session(Call, Queue) ->
    Args = wapi_qubicle_session:create_session_args(Call),
    cf_exe:send_amqp(Call, Args, fun wapi_qubicle_session:create_session/1),
    wait_for_create_response(Call, Queue).

-spec wait_for_create_response(whapps_call:call(), wh_json:object()) -> 'ok'.
wait_for_create_response(Call, Queue) ->
    receive {'amqp_msg', JObj} ->
        case wapi_qubicle_session:create_response(JObj) of
            'invalid' ->
                wait_for_create_response(Call, Queue);
            {'ok', SessionId} ->
                join_session_to_queue(Call, SessionId, Queue);
            {'failed', Reason} ->
                lager:error("create session failed for ~s, reason: ~s", [whapps_call:call_id(Call), Reason]),
                cf_exe:continue(Call)
        end
    after 10000 ->
        lager:error("timeout waiting for create session response for ~s", [whapps_call:call_id(Call)]),
        cf_exe:continue(Call)
    end.

-spec join_session_to_queue(whapps_call:call(), api_binary(), wh_json:object()) -> 'ok'.
join_session_to_queue(Call, SessionId, Queue) ->
    QueueName = wh_json:get_value(<<"queue_name">>, Queue),
    lager:info("joining ~p (~p) to queue ~p", [whapps_call:call_id(Call), SessionId, QueueName]),

    Args = wapi_qubicle_session:join_queue_args(Call, SessionId, Queue),
    cf_exe:send_amqp(Call, Args, fun wapi_qubicle_session:join_queue/1),
    wait_for_join_response(Call, SessionId, Queue).

wait_for_join_response(Call, SessionId, Queue) ->
    receive {'amqp_msg', JObj} ->
        case wapi_qubicle_session:join_queue_response(JObj) of
            'invalid' ->
                wait_for_join_response(Call, SessionId, Queue);
            'ok' -> 
                lager:info("session successfully joined queue");
            {'failed', Reason} ->
                lager:error("session join failed for ~s, reason: ~s", [SessionId, Reason]),
                cf_exe:continue(Call)
        end
    after 10000 ->
        lager:error("timeout waiting for session join response for ~s", [SessionId]),
        cf_exe:continue(Call)
    end.

