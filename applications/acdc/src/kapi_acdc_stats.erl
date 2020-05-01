%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_acdc_stats).

%% Convert JObj or Prop to iolist json
-export([call_waiting/1, call_waiting_v/1
        ,call_missed/1, call_missed_v/1
        ,call_abandoned/1, call_abandoned_v/1
        ,call_handled/1, call_handled_v/1
        ,call_processed/1, call_processed_v/1

        ,call_flush/1, call_flush_v/1

        ,current_calls_req/1, current_calls_req_v/1
        ,current_calls_err/1, current_calls_err_v/1
        ,current_calls_resp/1, current_calls_resp_v/1

        ,average_wait_time_req/1, average_wait_time_req_v/1
        ,average_wait_time_err/1, average_wait_time_err_v/1
        ,average_wait_time_resp/1, average_wait_time_resp_v/1

        ,status_req/1, status_req_v/1
        ,status_err/1, status_err_v/1
        ,status_resp/1, status_resp_v/1

        ,status_ready/1, status_ready_v/1
        ,status_logged_in/1, status_logged_in_v/1
        ,status_logged_out/1, status_logged_out_v/1
        ,status_pending_logged_out/1, status_pending_logged_out_v/1
        ,status_connecting/1, status_connecting_v/1
        ,status_connected/1, status_connected_v/1
        ,status_wrapup/1, status_wrapup_v/1
        ,status_paused/1, status_paused_v/1
        ,status_outbound/1, status_outbound_v/1
        ,status_update/1, status_update_v/1
        ]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).

-export([publish_call_waiting/1, publish_call_waiting/2
        ,publish_call_missed/1, publish_call_missed/2
        ,publish_call_abandoned/1, publish_call_abandoned/2
        ,publish_call_handled/1, publish_call_handled/2
        ,publish_call_processed/1, publish_call_processed/2

        ,publish_call_flush/1, publish_call_flush/2

        ,publish_current_calls_req/1, publish_current_calls_req/2
        ,publish_current_calls_err/2, publish_current_calls_err/3
        ,publish_current_calls_resp/2, publish_current_calls_resp/3

        ,publish_average_wait_time_req/1, publish_average_wait_time_req/2
        ,publish_average_wait_time_err/2, publish_average_wait_time_err/3
        ,publish_average_wait_time_resp/2, publish_average_wait_time_resp/3

        ,publish_status_req/1, publish_status_req/2
        ,publish_status_err/2, publish_status_err/3
        ,publish_status_resp/2, publish_status_resp/3

        ,publish_status_ready/1, publish_status_ready/2
        ,publish_status_logged_in/1, publish_status_logged_in/2
        ,publish_status_logged_out/1, publish_status_logged_out/2
        ,publish_status_pending_logged_out/1, publish_status_pending_logged_out/2
        ,publish_status_connecting/1, publish_status_connecting/2
        ,publish_status_connected/1, publish_status_connected/2
        ,publish_status_wrapup/1, publish_status_wrapup/2
        ,publish_status_paused/1, publish_status_paused/2
        ,publish_status_outbound/1, publish_status_outbound/2
        ,publish_status_update/1, publish_status_update/2
        ]).

-include("acdc.hrl").

-define(CALL_REQ_HEADERS, [<<"Call-ID">>, <<"Account-ID">>, <<"Queue-ID">>]).
-define(CALL_REQ_VALUES(Name), [{<<"Event-Category">>, <<"acdc_call_stat">>}
                               ,{<<"Event-Name">>, Name}
                               ]).

-define(WAITING_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                         ,<<"Entered-Timestamp">>, <<"Caller-Priority">>
                         ]).
-define(WAITING_VALUES, ?CALL_REQ_VALUES(<<"waiting">>)).
-define(WAITING_TYPES, []).

-define(MISS_HEADERS, [<<"Agent-ID">>, <<"Miss-Reason">>, <<"Miss-Timestamp">>]).
-define(MISS_VALUES, ?CALL_REQ_VALUES(<<"missed">>)).
-define(MISS_TYPES, []).

-define(ABANDON_HEADERS, [<<"Abandon-Reason">>, <<"Abandon-Timestamp">>]).
-define(ABANDON_VALUES, ?CALL_REQ_VALUES(<<"abandoned">>)).
-define(ABANDON_TYPES, []).

-define(HANDLED_HEADERS, [<<"Agent-ID">>, <<"Handled-Timestamp">>]).
-define(HANDLED_VALUES, ?CALL_REQ_VALUES(<<"handled">>)).
-define(HANDLED_TYPES, []).

-define(PROCESS_HEADERS, [<<"Agent-ID">>, <<"Processed-Timestamp">>, <<"Hung-Up-By">>]).
-define(PROCESS_VALUES, ?CALL_REQ_VALUES(<<"processed">>)).
-define(PROCESS_TYPES, []).

-define(FLUSH_HEADERS, [<<"Call-ID">>]).
-define(FLUSH_VALUES, ?CALL_REQ_VALUES(<<"flush">>)).
-define(FLUSH_TYPES, []).

-spec call_waiting(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
call_waiting(Props) when is_list(Props) ->
    case call_waiting_v(Props) of
        'true' -> kz_api:build_message(Props, ?CALL_REQ_HEADERS, ?WAITING_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_waiting"}
    end;
call_waiting(JObj) ->
    call_waiting(kz_json:to_proplist(JObj)).

-spec call_waiting_v(kz_term:api_terms()) -> boolean().
call_waiting_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?WAITING_VALUES, ?WAITING_TYPES);
call_waiting_v(JObj) ->
    call_waiting_v(kz_json:to_proplist(JObj)).

-spec call_missed(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
call_missed(Props) when is_list(Props) ->
    case call_missed_v(Props) of
        'true' -> kz_api:build_message(Props, ?CALL_REQ_HEADERS, ?MISS_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_missed"}
    end;
call_missed(JObj) ->
    call_missed(kz_json:to_proplist(JObj)).

-spec call_missed_v(kz_term:api_terms()) -> boolean().
call_missed_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?MISS_VALUES, ?MISS_TYPES);
call_missed_v(JObj) ->
    call_missed_v(kz_json:to_proplist(JObj)).

-spec call_abandoned(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
call_abandoned(Props) when is_list(Props) ->
    case call_abandoned_v(Props) of
        'true' -> kz_api:build_message(Props, ?CALL_REQ_HEADERS, ?ABANDON_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_abandoned"}
    end;
call_abandoned(JObj) ->
    call_abandoned(kz_json:to_proplist(JObj)).

-spec call_abandoned_v(kz_term:api_terms()) -> boolean().
call_abandoned_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?ABANDON_VALUES, ?ABANDON_TYPES);
call_abandoned_v(JObj) ->
    call_abandoned_v(kz_json:to_proplist(JObj)).

-spec call_handled(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
call_handled(Props) when is_list(Props) ->
    case call_handled_v(Props) of
        'true' -> kz_api:build_message(Props, ?CALL_REQ_HEADERS, ?HANDLED_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_handled"}
    end;
call_handled(JObj) ->
    call_handled(kz_json:to_proplist(JObj)).

-spec call_handled_v(kz_term:api_terms()) -> boolean().
call_handled_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?HANDLED_VALUES, ?HANDLED_TYPES);
call_handled_v(JObj) ->
    call_handled_v(kz_json:to_proplist(JObj)).

-spec call_processed(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
call_processed(Props) when is_list(Props) ->
    case call_processed_v(Props) of
        'true' -> kz_api:build_message(Props, ?CALL_REQ_HEADERS, ?PROCESS_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_processed"}
    end;
call_processed(JObj) ->
    call_processed(kz_json:to_proplist(JObj)).

-spec call_processed_v(kz_term:api_terms()) -> boolean().
call_processed_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?PROCESS_VALUES, ?PROCESS_TYPES);
call_processed_v(JObj) ->
    call_processed_v(kz_json:to_proplist(JObj)).

-spec call_flush(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
call_flush(Props) when is_list(Props) ->
    case call_flush_v(Props) of
        'true' -> kz_api:build_message(Props, ?CALL_REQ_HEADERS, ?FLUSH_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_flush"}
    end;
call_flush(JObj) ->
    call_flush(kz_json:to_proplist(JObj)).

-spec call_flush_v(kz_term:api_terms()) -> boolean().
call_flush_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CALL_REQ_HEADERS, ?FLUSH_VALUES, ?FLUSH_TYPES);
call_flush_v(JObj) ->
    call_flush_v(kz_json:to_proplist(JObj)).

-define(CURRENT_CALLS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_CURRENT_CALLS_REQ_HEADERS, [<<"Queue-ID">>, <<"Agent-ID">>
                                            ,<<"Status">>
                                            ,<<"Start-Range">>, <<"End-Range">>
                                            ]).
-define(CURRENT_CALLS_REQ_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                                  ,{<<"Event-Name">>, <<"current_calls_req">>}
                                  ]).
-define(CURRENT_CALLS_REQ_TYPES, []).

-spec current_calls_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
current_calls_req(Props) when is_list(Props) ->
    case current_calls_req_v(Props) of
        'true' -> kz_api:build_message(Props, ?CURRENT_CALLS_REQ_HEADERS, ?OPTIONAL_CURRENT_CALLS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for current_calls_req"}
    end;
current_calls_req(JObj) ->
    current_calls_req(kz_json:to_proplist(JObj)).

-spec current_calls_req_v(kz_term:api_terms()) -> boolean().
current_calls_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CURRENT_CALLS_REQ_HEADERS, ?CURRENT_CALLS_REQ_VALUES, ?CURRENT_CALLS_REQ_TYPES);
current_calls_req_v(JObj) ->
    current_calls_req_v(kz_json:to_proplist(JObj)).

-define(CURRENT_CALLS_ERR_HEADERS, [<<"Error-Reason">>]).
-define(OPTIONAL_CURRENT_CALLS_ERR_HEADERS, []).
-define(CURRENT_CALLS_ERR_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                                  ,{<<"Event-Name">>, <<"current_calls_err">>}
                                  ]).
-define(CURRENT_CALLS_ERR_TYPES, []).

-spec current_calls_err(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
current_calls_err(Props) when is_list(Props) ->
    case current_calls_err_v(Props) of
        'true' -> kz_api:build_message(Props, ?CURRENT_CALLS_ERR_HEADERS, ?OPTIONAL_CURRENT_CALLS_ERR_HEADERS);
        'false' -> {'error', "Proplist failed validation for current_calls_err"}
    end;
current_calls_err(JObj) ->
    current_calls_err(kz_json:to_proplist(JObj)).

-spec current_calls_err_v(kz_term:api_terms()) -> boolean().
current_calls_err_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CURRENT_CALLS_ERR_HEADERS, ?CURRENT_CALLS_ERR_VALUES, ?CURRENT_CALLS_ERR_TYPES);
current_calls_err_v(JObj) ->
    current_calls_err_v(kz_json:to_proplist(JObj)).

-define(CURRENT_CALLS_RESP_HEADERS, [<<"Query-Time">>]).
-define(OPTIONAL_CURRENT_CALLS_RESP_HEADERS, [<<"Waiting">>, <<"Handled">>
                                             ,<<"Abandoned">>, <<"Processed">>
                                             ]).
-define(CURRENT_CALLS_RESP_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                                   ,{<<"Event-Name">>, <<"current_calls_resp">>}
                                   ]).
-define(CURRENT_CALLS_RESP_TYPES, []).

-spec current_calls_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
current_calls_resp(Props) when is_list(Props) ->
    case current_calls_resp_v(Props) of
        'true' -> kz_api:build_message(Props, ?CURRENT_CALLS_RESP_HEADERS, ?OPTIONAL_CURRENT_CALLS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for current_calls_resp"}
    end;
current_calls_resp(JObj) ->
    current_calls_resp(kz_json:to_proplist(JObj)).

-spec current_calls_resp_v(kz_term:api_terms()) -> boolean().
current_calls_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CURRENT_CALLS_RESP_HEADERS, ?CURRENT_CALLS_RESP_VALUES, ?CURRENT_CALLS_RESP_TYPES);
current_calls_resp_v(JObj) ->
    current_calls_resp_v(kz_json:to_proplist(JObj)).

-define(AVERAGE_WAIT_TIME_REQ_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>]).
-define(OPTIONAL_AVERAGE_WAIT_TIME_REQ_HEADERS, [<<"Window">>]).
-define(AVERAGE_WAIT_TIME_REQ_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                                      ,{<<"Event-Name">>, <<"average_wait_time_req">>}
                                      ]).
-define(AVERAGE_WAIT_TIME_REQ_TYPES, [{<<"Account-ID">>, fun kz_term:is_ne_binary/1}
                                     ,{<<"Queue-ID">>, fun kz_term:is_ne_binary/1}
                                     ,{<<"Window">>, fun is_integer/1}
                                     ]).

-spec average_wait_time_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
average_wait_time_req(Props) when is_list(Props) ->
    case average_wait_time_req_v(Props) of
        'true' -> kz_api:build_message(Props, ?AVERAGE_WAIT_TIME_REQ_HEADERS, ?OPTIONAL_AVERAGE_WAIT_TIME_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for average_wait_time_req"}
    end;
average_wait_time_req(JObj) ->
    average_wait_time_req(kz_json:to_proplist(JObj)).

-spec average_wait_time_req_v(kz_term:api_terms()) -> boolean().
average_wait_time_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AVERAGE_WAIT_TIME_REQ_HEADERS, ?AVERAGE_WAIT_TIME_REQ_VALUES, ?AVERAGE_WAIT_TIME_REQ_TYPES);
average_wait_time_req_v(JObj) ->
    average_wait_time_req_v(kz_json:to_proplist(JObj)).

-define(AVERAGE_WAIT_TIME_ERR_HEADERS, [<<"Error-Reason">>]).
-define(OPTIONAL_AVERAGE_WAIT_TIME_ERR_HEADERS, []).
-define(AVERAGE_WAIT_TIME_ERR_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                                      ,{<<"Event-Name">>, <<"average_wait_time_err">>}
                                      ]).
-define(AVERAGE_WAIT_TIME_ERR_TYPES, [{<<"Error-Reason">>, fun is_binary/1}]).

-spec average_wait_time_err(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
average_wait_time_err(Props) when is_list(Props) ->
    case average_wait_time_err_v(Props) of
        'true' -> kz_api:build_message(Props, ?AVERAGE_WAIT_TIME_ERR_HEADERS, ?OPTIONAL_AVERAGE_WAIT_TIME_ERR_HEADERS);
        'false' -> {'error', "Proplist failed validation for average_wait_time_err"}
    end;
average_wait_time_err(JObj) ->
    average_wait_time_err_v(kz_json:to_proplist(JObj)).

-spec average_wait_time_err_v(kz_term:api_terms()) -> boolean().
average_wait_time_err_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AVERAGE_WAIT_TIME_ERR_HEADERS, ?AVERAGE_WAIT_TIME_ERR_VALUES, ?AVERAGE_WAIT_TIME_ERR_TYPES);
average_wait_time_err_v(JObj) ->
    average_wait_time_err_v(kz_json:to_proplist(JObj)).

-define(AVERAGE_WAIT_TIME_RESP_HEADERS, [<<"Average-Wait-Time">>]).
-define(OPTIONAL_AVERAGE_WAIT_TIME_RESP_HEADERS, []).
-define(AVERAGE_WAIT_TIME_RESP_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                                       ,{<<"Event-Name">>, <<"average_wait_time_resp">>}
                                       ]).
-define(AVERAGE_WAIT_TIME_RESP_TYPES, [{<<"Average-Wait-Time">>, fun is_integer/1}]).

-spec average_wait_time_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
average_wait_time_resp(Props) when is_list(Props) ->
    case average_wait_time_resp_v(Props) of
        'true' -> kz_api:build_message(Props, ?AVERAGE_WAIT_TIME_RESP_HEADERS, ?OPTIONAL_AVERAGE_WAIT_TIME_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for average_wait_time_resp"}
    end;
average_wait_time_resp(JObj) ->
    average_wait_time_resp(kz_json:to_proplist(JObj)).

-spec average_wait_time_resp_v(kz_term:api_terms()) -> boolean().
average_wait_time_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AVERAGE_WAIT_TIME_RESP_HEADERS, ?AVERAGE_WAIT_TIME_RESP_VALUES, ?AVERAGE_WAIT_TIME_RESP_TYPES);
average_wait_time_resp_v(JObj) ->
    average_wait_time_resp_v(kz_json:to_proplist(JObj)).

-define(STATUS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_STATUS_REQ_HEADERS, [<<"Agent-ID">>, <<"Start-Range">>, <<"End-Range">>
                                     ,<<"Status">>, <<"Limit">>
                                     ]).
-define(STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                           ,{<<"Event-Name">>, <<"status_req">>}
                           ]).
-define(STATUS_REQ_TYPES, []).

-spec status_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_req(Props) when is_list(Props) ->
    case status_req_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_REQ_HEADERS, ?OPTIONAL_STATUS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_req"}
    end;
status_req(JObj) ->
    status_req(kz_json:to_proplist(JObj)).

-spec status_req_v(kz_term:api_terms()) -> boolean().
status_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_REQ_HEADERS, ?STATUS_REQ_VALUES, ?STATUS_REQ_TYPES);
status_req_v(JObj) ->
    status_req_v(kz_json:to_proplist(JObj)).

-define(STATUS_ERR_HEADERS, [<<"Error-Reason">>]).
-define(OPTIONAL_STATUS_ERR_HEADERS, []).
-define(STATUS_ERR_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                           ,{<<"Event-Name">>, <<"status_err">>}
                           ]).
-define(STATUS_ERR_TYPES, []).

-spec status_err(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_err(Props) when is_list(Props) ->
    case status_err_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_ERR_HEADERS, ?OPTIONAL_STATUS_ERR_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_err"}
    end;
status_err(JObj) ->
    status_err(kz_json:to_proplist(JObj)).

-spec status_err_v(kz_term:api_terms()) -> boolean().
status_err_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_ERR_HEADERS, ?STATUS_ERR_VALUES, ?STATUS_ERR_TYPES);
status_err_v(JObj) ->
    status_err_v(kz_json:to_proplist(JObj)).

-define(STATUS_RESP_HEADERS, [<<"Agents">>]).
-define(OPTIONAL_STATUS_RESP_HEADERS, []).
-define(STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"acdc_stat">>}
                            ,{<<"Event-Name">>, <<"status_resp">>}
                            ]).
-define(STATUS_RESP_TYPES, []).

-spec status_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_resp(Props) when is_list(Props) ->
    case status_resp_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_RESP_HEADERS, ?OPTIONAL_STATUS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_resp"}
    end;
status_resp(JObj) ->
    status_resp(kz_json:to_proplist(JObj)).

-spec status_resp_v(kz_term:api_terms()) -> boolean().
status_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_RESP_HEADERS, ?STATUS_RESP_VALUES, ?STATUS_RESP_TYPES);
status_resp_v(JObj) ->
    status_resp_v(kz_json:to_proplist(JObj)).

-define(STATUS_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"Timestamp">>]).
-define(STATUS_OPTIONAL_HEADERS, [<<"Wait-Time">>, <<"Pause-Time">>, <<"Call-ID">>
                                 ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                 ,<<"Queue-ID">>
                                 ]).
-define(STATUS_VALUES(Name), [{<<"Event-Category">>, <<"acdc_status_stat">>}
                             ,{<<"Event-Name">>, Name}
                             ]).
-define(STATUS_TYPES, []).

-spec status_update(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_update(Props) when is_list(Props) ->
    case status_update_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_update"}
    end;
status_update(JObj) ->
    status_update(kz_json:to_proplist(JObj)).

-spec status_update_v(kz_term:api_terms()) -> boolean().
status_update_v(Prop) when is_list(Prop) ->
    EvtName = props:get_value(<<"Status">>, Prop),
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(EvtName), ?STATUS_TYPES);
status_update_v(JObj) ->
    status_update_v(kz_json:to_proplist(JObj)).

-spec status_ready(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_ready(Props) when is_list(Props) ->
    case status_ready_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_ready"}
    end;
status_ready(JObj) ->
    status_ready(kz_json:to_proplist(JObj)).

-spec status_ready_v(kz_term:api_terms()) -> boolean().
status_ready_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"ready">>), ?STATUS_TYPES);
status_ready_v(JObj) ->
    status_ready_v(kz_json:to_proplist(JObj)).

-spec status_logged_in(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_logged_in(Props) when is_list(Props) ->
    case status_logged_in_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_logged_in"}
    end;
status_logged_in(JObj) ->
    status_logged_in(kz_json:to_proplist(JObj)).

-spec status_logged_in_v(kz_term:api_terms()) -> boolean().
status_logged_in_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"logged_in">>), ?STATUS_TYPES);
status_logged_in_v(JObj) ->
    status_logged_in_v(kz_json:to_proplist(JObj)).

-spec status_logged_out(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_logged_out(Props) when is_list(Props) ->
    case status_logged_out_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_logged_out"}
    end;
status_logged_out(JObj) ->
    status_logged_out(kz_json:to_proplist(JObj)).

-spec status_logged_out_v(kz_term:api_terms()) -> boolean().
status_logged_out_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"logged_out">>), ?STATUS_TYPES);
status_logged_out_v(JObj) ->
    status_logged_out_v(kz_json:to_proplist(JObj)).

-spec status_pending_logged_out(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_pending_logged_out(Props) when is_list(Props) ->
    case status_pending_logged_out_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_logged_out"}
    end;
status_pending_logged_out(JObj) ->
    status_pending_logged_out(kz_json:to_proplist(JObj)).

-spec status_pending_logged_out_v(kz_term:api_terms()) -> boolean().
status_pending_logged_out_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"pending_logged_out">>), ?STATUS_TYPES);
status_pending_logged_out_v(JObj) ->
    status_pending_logged_out_v(kz_json:to_proplist(JObj)).

-spec status_connecting(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_connecting(Props) when is_list(Props) ->
    case status_connecting_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_connecting"}
    end;
status_connecting(JObj) ->
    status_connecting(kz_json:to_proplist(JObj)).

-spec status_connecting_v(kz_term:api_terms()) -> boolean().
status_connecting_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"connecting">>), ?STATUS_TYPES);
status_connecting_v(JObj) ->
    status_connecting_v(kz_json:to_proplist(JObj)).

-spec status_connected(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_connected(Props) when is_list(Props) ->
    case status_connected_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_connected"}
    end;
status_connected(JObj) ->
    status_connected(kz_json:to_proplist(JObj)).

-spec status_connected_v(kz_term:api_terms()) -> boolean().
status_connected_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"connected">>), ?STATUS_TYPES);
status_connected_v(JObj) ->
    status_connected_v(kz_json:to_proplist(JObj)).

-spec status_wrapup(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_wrapup(Props) when is_list(Props) ->
    case status_wrapup_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_wrapup"}
    end;
status_wrapup(JObj) ->
    status_wrapup(kz_json:to_proplist(JObj)).

-spec status_wrapup_v(kz_term:api_terms()) -> boolean().
status_wrapup_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"wrapup">>), ?STATUS_TYPES);
status_wrapup_v(JObj) ->
    status_wrapup_v(kz_json:to_proplist(JObj)).

-spec status_paused(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_paused(Props) when is_list(Props) ->
    case status_paused_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_paused"}
    end;
status_paused(JObj) ->
    status_paused(kz_json:to_proplist(JObj)).

-spec status_paused_v(kz_term:api_terms()) -> boolean().
status_paused_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"paused">>), ?STATUS_TYPES);
status_paused_v(JObj) ->
    status_paused_v(kz_json:to_proplist(JObj)).

-spec status_outbound(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
status_outbound(Props) when is_list(Props) ->
    case status_outbound_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATUS_HEADERS, ?STATUS_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for status_outbound"}
    end;
status_outbound(JObj) ->
    status_outbound(kz_json:to_proplist(JObj)).

-spec status_outbound_v(kz_term:api_terms()) -> boolean().
status_outbound_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATUS_HEADERS, ?STATUS_VALUES(<<"outbound">>), ?STATUS_TYPES);
status_outbound_v(JObj) ->
    status_outbound_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(AMQPQueue, Props) ->
    QueueId = props:get_value('queue_id', Props, <<"*">>),
    AgentId = props:get_value('agent_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    bind_q(AMQPQueue, AccountId, QueueId, AgentId, props:get_value('restrict_to', Props)).

bind_q(AMQPQueue, AccountId, QueueId, AgentId, 'undefined') ->
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, call_stat_routing_key(AccountId, QueueId)),
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, status_stat_routing_key(AccountId, AgentId)),
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, query_call_stat_routing_key(AccountId, QueueId)),
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, query_status_stat_routing_key(AccountId, AgentId));
bind_q(AMQPQueue, AccountId, QueueId, AgentId, ['call_stat'|L]) ->
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, call_stat_routing_key(AccountId, QueueId)),
    bind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
bind_q(AMQPQueue, AccountId, QueueId, AgentId, ['status_stat'|L]) ->
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, status_stat_routing_key(AccountId, AgentId)),
    bind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
bind_q(AMQPQueue, AccountId, QueueId, AgentId, ['query_call_stat'|L]) ->
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, query_call_stat_routing_key(AccountId, QueueId)),
    bind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
bind_q(AMQPQueue, AccountId, QueueId, AgentId, ['query_status_stat'|L]) ->
    kz_amqp_util:bind_q_to_kapps(AMQPQueue, query_status_stat_routing_key(AccountId, AgentId)),
    bind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
bind_q(AMQPQueue, AccountId, QueueId, AgentId, [_|L]) ->
    bind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
bind_q(_AMQPQueue, _AccountId, _QueueId, _AgentId, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(AMQPQueue, Props) ->
    QueueId = props:get_value('queue_id', Props, <<"*">>),
    AgentId = props:get_value('agent_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),

    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, props:get_value('restrict_to', Props)).

unbind_q(AMQPQueue, AccountId, QueueId, AgentId, 'undefined') ->
    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, ['call_stat', 'status_stat', 'query_call_stat', 'query_status_stat']);
unbind_q(AMQPQueue, AccountId, QueueId, AgentId, ['call_stat'|L]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(AMQPQueue, call_stat_routing_key(AccountId, QueueId)),
    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
unbind_q(AMQPQueue, AccountId, QueueId, AgentId, ['status_stat'|L]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(AMQPQueue, status_stat_routing_key(AccountId, AgentId)),
    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
unbind_q(AMQPQueue, AccountId, QueueId, AgentId, ['query_call_stat'|L]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(AMQPQueue, query_call_stat_routing_key(AccountId, QueueId)),
    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
unbind_q(AMQPQueue, AccountId, QueueId, AgentId, ['query_status_stat'|L]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(AMQPQueue, query_status_stat_routing_key(AccountId, AgentId)),
    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
unbind_q(AMQPQueue, AccountId, QueueId, AgentId, [_|L]) ->
    unbind_q(AMQPQueue, AccountId, QueueId, AgentId, L);
unbind_q(_AMQPQueue, _AccountId, _QueueId, _AgentId, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

-spec publish_call_waiting(kz_term:api_terms()) -> 'ok'.
publish_call_waiting(JObj) ->
    publish_call_waiting(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call_waiting(kz_term:api_terms(), binary()) -> 'ok'.
publish_call_waiting(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?WAITING_VALUES, fun call_waiting/1),
    kz_amqp_util:kapps_publish(call_stat_routing_key(API), Payload, ContentType).

-spec publish_call_missed(kz_term:api_terms()) -> 'ok'.
publish_call_missed(JObj) ->
    publish_call_missed(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call_missed(kz_term:api_terms(), binary()) -> 'ok'.
publish_call_missed(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?MISS_VALUES, fun call_missed/1),
    kz_amqp_util:kapps_publish(call_stat_routing_key(API), Payload, ContentType).

-spec publish_call_abandoned(kz_term:api_terms()) -> 'ok'.
publish_call_abandoned(JObj) ->
    publish_call_abandoned(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call_abandoned(kz_term:api_terms(), binary()) -> 'ok'.
publish_call_abandoned(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?ABANDON_VALUES, fun call_abandoned/1),
    kz_amqp_util:kapps_publish(call_stat_routing_key(API), Payload, ContentType).

-spec publish_call_handled(kz_term:api_terms()) -> 'ok'.
publish_call_handled(JObj) ->
    publish_call_handled(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call_handled(kz_term:api_terms(), binary()) -> 'ok'.
publish_call_handled(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?HANDLED_VALUES, fun call_handled/1),
    kz_amqp_util:kapps_publish(call_stat_routing_key(API), Payload, ContentType).

-spec publish_call_processed(kz_term:api_terms()) -> 'ok'.
publish_call_processed(JObj) ->
    publish_call_processed(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call_processed(kz_term:api_terms(), binary()) -> 'ok'.
publish_call_processed(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PROCESS_VALUES, fun call_processed/1),
    kz_amqp_util:kapps_publish(call_stat_routing_key(API), Payload, ContentType).

-spec publish_call_flush(kz_term:api_terms()) -> 'ok'.
publish_call_flush(JObj) ->
    publish_call_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call_flush(kz_term:api_terms(), binary()) -> 'ok'.
publish_call_flush(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FLUSH_VALUES, fun call_flush/1),
    lager:debug("flush payload ~s: ~s", [call_stat_routing_key(API), Payload]),
    kz_amqp_util:kapps_publish(call_stat_routing_key(API), Payload, ContentType).

-spec publish_status_update(kz_term:api_terms()) -> 'ok'.
publish_status_update(JObj) ->
    publish_status_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_update(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_update(API, ContentType) ->
    EvtName = status_value(API),
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(EvtName), fun status_update/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_ready(kz_term:api_terms()) -> 'ok'.
publish_status_ready(JObj) ->
    publish_status_ready(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_ready(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_ready(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"ready">>), fun status_ready/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_logged_in(kz_term:api_terms()) -> 'ok'.
publish_status_logged_in(JObj) ->
    publish_status_logged_in(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_logged_in(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_logged_in(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"logged_in">>), fun status_logged_in/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_logged_out(kz_term:api_terms()) -> 'ok'.
publish_status_logged_out(JObj) ->
    publish_status_logged_out(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_logged_out(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_logged_out(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"logged_out">>), fun status_logged_out/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_pending_logged_out(kz_term:api_terms()) -> 'ok'.
publish_status_pending_logged_out(JObj) ->
    publish_status_pending_logged_out(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_pending_logged_out(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_status_pending_logged_out(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"pending_logged_out">>), fun status_pending_logged_out/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_connecting(kz_term:api_terms()) -> 'ok'.
publish_status_connecting(JObj) ->
    publish_status_connecting(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_connecting(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_connecting(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"connecting">>), fun status_connecting/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_connected(kz_term:api_terms()) -> 'ok'.
publish_status_connected(JObj) ->
    publish_status_connected(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_connected(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_connected(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"connected">>), fun status_connected/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_wrapup(kz_term:api_terms()) -> 'ok'.
publish_status_wrapup(JObj) ->
    publish_status_wrapup(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_wrapup(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_wrapup(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"wrapup">>), fun status_wrapup/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_paused(kz_term:api_terms()) -> 'ok'.
publish_status_paused(JObj) ->
    publish_status_paused(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_paused(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_paused(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"paused">>), fun status_paused/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_outbound(kz_term:api_terms()) -> 'ok'.
publish_status_outbound(JObj) ->
    publish_status_outbound(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_outbound(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_outbound(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_VALUES(<<"outbound">>), fun status_outbound/1),
    kz_amqp_util:kapps_publish(status_stat_routing_key(API), Payload, ContentType).

-spec publish_current_calls_req(kz_term:api_terms()) -> 'ok'.
publish_current_calls_req(JObj) ->
    publish_current_calls_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_current_calls_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_current_calls_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?CURRENT_CALLS_REQ_VALUES, fun current_calls_req/1),
    kz_amqp_util:kapps_publish(query_call_stat_routing_key(API), Payload, ContentType).

-spec publish_current_calls_err(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_current_calls_err(RespAMQPQueue, JObj) ->
    publish_current_calls_err(RespAMQPQueue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_current_calls_err(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_current_calls_err(RespAMQPQueue, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?CURRENT_CALLS_ERR_VALUES, fun current_calls_err/1),
    kz_amqp_util:targeted_publish(RespAMQPQueue, Payload, ContentType).

-spec publish_current_calls_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_current_calls_resp(RespAMQPQueue, JObj) ->
    publish_current_calls_resp(RespAMQPQueue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_current_calls_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_current_calls_resp(RespAMQPQueue, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?CURRENT_CALLS_RESP_VALUES, fun current_calls_resp/1),
    kz_amqp_util:targeted_publish(RespAMQPQueue, Payload, ContentType).

-spec publish_average_wait_time_req(kz_term:api_terms()) -> 'ok'.
publish_average_wait_time_req(JObj) ->
    publish_average_wait_time_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_average_wait_time_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_average_wait_time_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?AVERAGE_WAIT_TIME_REQ_VALUES, fun average_wait_time_req/1),
    kz_amqp_util:kapps_publish(query_call_stat_routing_key(API), Payload, ContentType).

-spec publish_average_wait_time_err(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_average_wait_time_err(RespAMQPQueue, JObj) ->
    publish_average_wait_time_err(RespAMQPQueue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_average_wait_time_err(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_average_wait_time_err(RespAMQPQueue, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?AVERAGE_WAIT_TIME_ERR_VALUES, fun average_wait_time_err/1),
    kz_amqp_util:targeted_publish(RespAMQPQueue, Payload, ContentType).

-spec publish_average_wait_time_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_average_wait_time_resp(RespAMQPQueue, JObj) ->
    publish_average_wait_time_resp(RespAMQPQueue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_average_wait_time_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_average_wait_time_resp(RespAMQPQueue, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?AVERAGE_WAIT_TIME_RESP_VALUES, fun average_wait_time_resp/1),
    kz_amqp_util:targeted_publish(RespAMQPQueue, Payload, ContentType).

-spec publish_status_req(kz_term:api_terms()) -> 'ok'.
publish_status_req(JObj) ->
    publish_status_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_status_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_REQ_VALUES, fun status_req/1),
    kz_amqp_util:kapps_publish(query_status_stat_routing_key(API), Payload, ContentType).

-spec publish_status_err(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_status_err(RespAMQPQueue, JObj) ->
    publish_status_err(RespAMQPQueue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_err(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_status_err(RespAMQPQueue, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_ERR_VALUES, fun status_err/1),
    kz_amqp_util:targeted_publish(RespAMQPQueue, Payload, ContentType).

-spec publish_status_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_status_resp(RespAMQPQueue, JObj) ->
    publish_status_resp(RespAMQPQueue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_status_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_status_resp(RespAMQPQueue, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATUS_RESP_VALUES, fun status_resp/1),
    kz_amqp_util:targeted_publish(RespAMQPQueue, Payload, ContentType).

call_stat_routing_key(Prop) when is_list(Prop) ->
    call_stat_routing_key(props:get_value(<<"Account-ID">>, Prop)
                         ,props:get_value(<<"Queue-ID">>, Prop)
                         );
call_stat_routing_key(JObj) ->
    call_stat_routing_key(kz_json:get_value(<<"Account-ID">>, JObj)
                         ,kz_json:get_value(<<"Queue-ID">>, JObj)
                         ).
call_stat_routing_key(AccountId, QueueId) ->
    <<"acdc_stats.call.", AccountId/binary, ".", QueueId/binary>>.

status_stat_routing_key(Prop) when is_list(Prop) ->
    status_stat_routing_key(props:get_value(<<"Account-ID">>, Prop)
                           ,props:get_value(<<"Agent-ID">>, Prop)
                           );
status_stat_routing_key(JObj) ->
    status_stat_routing_key(kz_json:get_value(<<"Account-ID">>, JObj)
                           ,kz_json:get_value(<<"Agent-ID">>, JObj)
                           ).
status_stat_routing_key(AccountId, AgentId) ->
    <<"acdc_stats.status.", AccountId/binary, ".", AgentId/binary>>.

query_call_stat_routing_key(Prop) when is_list(Prop) ->
    query_call_stat_routing_key(props:get_value(<<"Account-ID">>, Prop)
                               ,props:get_value(<<"Queue-ID">>, Prop)
                               );
query_call_stat_routing_key(JObj) ->
    query_call_stat_routing_key(kz_json:get_value(<<"Account-ID">>, JObj)
                               ,kz_json:get_value(<<"Queue-ID">>, JObj)
                               ).

query_call_stat_routing_key(AccountId, 'undefined') ->
    <<"acdc_stats.query_call.", AccountId/binary, ".all">>;
query_call_stat_routing_key(AccountId, QueueId) ->
    <<"acdc_stats.query_call.", AccountId/binary, ".", QueueId/binary>>.

query_status_stat_routing_key(Prop) when is_list(Prop) ->
    query_status_stat_routing_key(props:get_value(<<"Account-ID">>, Prop)
                                 ,props:get_value(<<"Agent-ID">>, Prop)
                                 );
query_status_stat_routing_key(JObj) ->
    query_status_stat_routing_key(kz_json:get_value(<<"Account-ID">>, JObj)
                                 ,kz_json:get_value(<<"AgentId-ID">>, JObj)
                                 ).

query_status_stat_routing_key(AccountId, 'undefined') ->
    <<"acdc_stats.query_status.", AccountId/binary, ".all">>;
query_status_stat_routing_key(AccountId, QueueId) ->
    <<"acdc_stats.query_status.", AccountId/binary, ".", QueueId/binary>>.


status_value(API) when is_list(API) -> props:get_value(<<"Status">>, API);
status_value(API) -> kz_json:get_value(<<"Status">>, API).
