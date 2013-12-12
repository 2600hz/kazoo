%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_port_request).

-export([init/0
         ,current_state/1
         ,public_fields/1
         ,normalize_attachments/1
         ,normalize_numbers/1
         ,transition_to_ready/1
         ,transition_to_progress/1
         ,transition_to_complete/1
         ,transition_to_rejected/1
         ,maybe_transition/2
         ,charge_for_port/1, charge_for_port/2
        ]).

-include("wnm.hrl").
-include_lib("whistle_number_manager/include/wh_port_request.hrl").

-type transition_response() :: {'ok', wh_json:object()} |
                               {'error', 'invalid_state_transition'}.

-spec init() -> any().
init() ->
    _ = couch_mgr:db_create(?KZ_PORT_REQUESTS_DB),
    couch_mgr:revise_doc_from_file(?KZ_PORT_REQUESTS_DB, 'crossbar', <<"views/port_requests.json">>).

-spec current_state(wh_json:object()) -> api_binary().
current_state(JObj) ->
    lager:debug("current state: ~p", [JObj]),
    wh_json:get_first_defined([?PORT_PVT_STATE, ?PORT_STATE], JObj, ?PORT_WAITING).

-spec public_fields(wh_json:object()) -> wh_json:object().
public_fields(JObj) ->
    As = wh_json:get_value(<<"_attachments">>, JObj, wh_json:new()),
    NormalizedAs = normalize_attachments(As),

    wh_json:set_values([{<<"id">>, wh_json:get_value(<<"_id">>, JObj)}
                        ,{<<"created">>, wh_json:get_value(<<"pvt_created">>, JObj)}
                        ,{<<"updated">>, wh_json:get_value(<<"pvt_modified">>, JObj)}
                        ,{<<"uploads">>, NormalizedAs}
                        ,{?PORT_STATE, wh_json:get_value(?PORT_PVT_STATE, JObj, ?PORT_WAITING)}
                       ], wh_doc:public_fields(JObj)).

-spec normalize_attachments(wh_json:object()) -> wh_json:object().
normalize_attachments(Attachments) ->
    wh_json:map(fun normalize_attachments_map/2, Attachments).

-spec normalize_attachments_map(wh_json:key(), wh_json:json_term()) ->
                                       {wh_json:key(), wh_json:json_term()}.
normalize_attachments_map(K, V) ->
    {K, wh_json:delete_keys([<<"digest">>, <<"revpos">>, <<"stub">>], V)}.

-spec normalize_numbers(wh_json:object()) -> wh_json:object().
normalize_numbers(JObj) ->
    Numbers = wh_json:get_value(<<"numbers">>, JObj, wh_json:new()),
    wh_json:set_value(<<"numbers">>
                      ,wh_json:map(fun(N, Meta) ->
                                           {wnm_util:to_e164(N), Meta}
                                   end, Numbers)
                      ,JObj
                     ).

-spec transition_to_ready(wh_json:object()) -> transition_response().
-spec transition_to_progress(wh_json:object()) -> transition_response().
-spec transition_to_complete(wh_json:object()) -> transition_response().
-spec transition_to_rejected(wh_json:object()) -> transition_response().

transition_to_ready(JObj) ->
    transition(JObj, [?PORT_WAITING, ?PORT_REJECT], ?PORT_READY).
transition_to_progress(JObj) ->
    transition(JObj, [?PORT_READY], ?PORT_PROGRESS).
transition_to_complete(JObj) ->
    case transition(JObj, [?PORT_READY, ?PORT_PROGRESS, ?PORT_REJECT], ?PORT_COMPLETE) of
        {'error', _}=E -> E;
        {'ok', Transitioned} -> completed_port(Transitioned)
    end.
transition_to_rejected(JObj) ->
    transition(JObj, [?PORT_READY, ?PORT_PROGRESS], ?PORT_REJECT).

-spec maybe_transition(wh_json:object(), ne_binary()) -> transition_response().
maybe_transition(PortReq, ?PORT_READY) ->
    transition_to_ready(PortReq);
maybe_transition(PortReq, ?PORT_PROGRESS) ->
    transition_to_progress(PortReq);
maybe_transition(PortReq, ?PORT_COMPLETE) ->
    transition_to_complete(PortReq);
maybe_transition(PortReq, ?PORT_REJECT) ->
    transition_to_rejected(PortReq).

-spec transition(wh_json:object(), ne_binaries(), ne_binary()) ->
                        transition_response().
-spec transition(wh_json:object(), ne_binaries(), ne_binary(), ne_binary()) ->
                        transition_response().
transition(JObj, FromStates, ToState) ->
    transition(JObj, FromStates, ToState, current_state(JObj)).
transition(_JObj, [], _ToState, _CurrentState) ->
    lager:debug("cant go from ~s to ~s", [_CurrentState, _ToState]),
    {'error', 'invalid_state_transition'};
transition(JObj, [CurrentState | _], ToState, CurrentState) ->
    lager:debug("going from ~s to ~s", [CurrentState, ToState]),
    {'ok', wh_json:set_values([{?PORT_PVT_STATE, ToState}
                               ,{?PORT_STATE, ToState}
                              ]
                              ,JObj)};
transition(JObj, [_FromState | FromStates], ToState, CurrentState) ->
    lager:debug("skipping from ~s to ~s c ~p", [_FromState, ToState, CurrentState]),
    transition(JObj, FromStates, ToState, CurrentState).

%% charge for port
-spec charge_for_port(wh_json:object()) -> 'ok' | 'error'.
-spec charge_for_port(wh_json:object(), ne_binary()) -> 'ok' | 'error'.
charge_for_port(JObj) ->
    charge_for_port(JObj, wh_json:get_value(<<"pvt_account_id">>, JObj)).
charge_for_port(_JObj, AccountId) ->
    Services = wh_services:fetch(AccountId),
    Cost = wh_services:activation_charges(<<"number_services">>, <<"port">>, Services),
    Transaction = wh_transaction:debit(AccountId, Cost),
    wh_services:commit_transactions(Services, [Transaction]).

-spec completed_port(wh_json:object()) ->
                            transition_response().
completed_port(PortReq) ->
    case charge_for_port(PortReq) of
        'ok' ->
            lager:debug("successfully charged for port, transtitioning numbers to active"),
            transition_numbers(PortReq);
        'error' ->
            throw({'error', 'failed_to_charge'})
    end.

-spec transition_numbers(wh_json:object()) ->
                                transition_response().
transition_numbers(PortReq) ->
    Numbers = wh_json:get_keys(<<"numbers">>, PortReq),
    PortOps = [enable_number(N) || N <- Numbers],
    case lists:all(fun(X) -> wh_util:is_true(X) end, PortOps) of
        'true' ->
            lager:debug("all numbers ported, removing from port req"),
            ClearedPortRequest = clear_numbers_from_port(PortReq),
            {'ok', ClearedPortRequest};
        'false' ->
            lager:debug("failed to transition numbers: ~p", [PortOps]),
            {'error', PortReq}
    end.

-spec clear_numbers_from_port(wh_json:object()) -> wh_json:object().
clear_numbers_from_port(PortReq) ->
    case couch_mgr:save_doc(?KZ_PORT_REQUESTS_DB
                            ,wh_json:set_value(<<"numbers">>, wh_json:new(), PortReq)
                           )
    of
        {'ok', PortReq1} -> lager:debug("port numbers cleared"), PortReq1;
        {'error', 'conflict'} ->
            lager:debug("port request doc was updated before we could re-save"),
            PortReq;
        {'error', _E} ->
            lager:debug("failed to clear numbers: ~p", [_E]),
            PortReq
    end.

-spec enable_number(ne_binary()) -> boolean().
enable_number(N) ->
    try wh_number_manager:ported(N) of
        {'ok', _NumberDoc} ->
            lager:debug("ported ~s: ~p", [N, _NumberDoc]),
            'true';
        _E ->
            lager:debug("unexpected result to ported(~s): ~p", [N, _E]),
            {'false', N}
    catch
        'throw':_E ->
            lager:debug("throw from ported(~s): ~p", [N, _E]),
            {'false', N}
    end.
