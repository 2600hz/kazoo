%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number).

-export([
    get/1, get/2
    ,create/2
    ,move/2 ,move/3
    ,update/2 ,update/3
    ,delete/1 ,delete/2
    ,change_state/2 ,change_state/3
    ,assigned_to_app/2 ,assigned_to_app/3
    ,lookup_account/1
]).

-include("knm.hrl").
-type lookup_account_return() ::  {'ok', ne_binary(), wh_proplist()} | {'error', _}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> number_return().
-spec get(ne_binary(), ne_binary()) -> number_return().
get(Num) ->
    get(Num, <<"system">>).

get(Num, AuthBy) ->
    case knm_converters:is_reconcilable(Num) of
        'false' -> {'error', 'not_reconcilable'};
        'true' -> knm_phone_number:fetch(Num, AuthBy)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), wh_proplist()) -> number_return().
create(Num, Props) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates =
        props:filter_undefined([
            {fun knm_phone_number:set_number/2, NormalizedNum}
            ,{fun knm_phone_number:set_number_db/2, NumberDb}
            ,{fun knm_phone_number:set_state/2, props:get_binary_value(<<"state">>, Props, ?NUMBER_STATE_DISCOVERY)}
            ,{fun knm_phone_number:set_ported_in/2, props:get_is_true(<<"ported_in">>, Props, 'false')}
            ,{fun knm_phone_number:set_assigned_to/2, props:get_binary_value(<<"assigned_to">>, Props)}
            ,{fun knm_phone_number:set_auth_by/2, props:get_binary_value(<<"auth_by">>, Props, <<"system">>)}
            ,{fun knm_phone_number:set_dry_run/2, props:get_is_true(<<"dry_run">>, Props, 'false')}
        ]),
    Number = knm_phone_number:setters(knm_phone_number:new(), Updates),
    knm_phone_number:save(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binary(), ne_binary()) -> number_return().
-spec move(ne_binary(), ne_binary(), ne_binary()) -> number_return().
move(Num, MoveTo) ->
    move(Num, MoveTo, <<"system">>).

move(Num, MoveTo, AuthBy) ->
    lager:debug("trying to move ~s to ~s", [Num, MoveTo]),
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            AccountId = wh_util:format_account_id(MoveTo, 'raw'),
            AssignedTo = knm_phone_number:assigned_to(Number),
            Props = [
                {fun knm_phone_number:set_assigned_to/2, AccountId}
                ,{fun knm_phone_number:set_prev_assigned_to/2, AssignedTo}
            ],
            UpdatedNumber = knm_phone_number:setters(Number, Props),
            knm_phone_number:save(UpdatedNumber)
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), wh_proplist()) -> number_return().
-spec update(ne_binary(), wh_proplist(), ne_binary()) -> number_return().
update(Num, Props) ->
    update(Num, Props, <<"system">>).

update(Num, Props, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            UpdatedNumber = knm_phone_number:setters(Number, Props),
            knm_phone_number:save(UpdatedNumber)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) -> number_return().
-spec delete(ne_binary(), ne_binary()) -> number_return().
delete(Num) ->
    delete(Num, <<"system">>).

delete(Num, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            knm_phone_number:delete(Number)
    end.



%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_state(ne_binary(), ne_binary()) -> number_return().
-spec change_state(ne_binary(), ne_binary(), ne_binary()) -> number_return().
change_state(Num, State) ->
    change_state(Num, State, <<"system">>).

change_state(Num, State, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            maybe_change_state(Number, State)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to_app(ne_binary(), ne_binary()) -> number_return().
-spec assigned_to_app(ne_binary(), ne_binary(), ne_binary()) -> number_return().
assigned_to_app(Num, App) ->
    assigned_to_app(Num, App, <<"system">>).

assigned_to_app(Num, App, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            UpdatedNumber = knm_phone_number:set_used_by(Number, App),
            knm_phone_number:save(UpdatedNumber)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup_account(binary()) -> lookup_account_return().
lookup_account('undefined') -> {'error', 'not_reconcilable'};
lookup_account(Num) ->
    NormalizedNum = knm_converters:normalize(Num),
    Key = {'account_lookup', NormalizedNum},
    case wh_cache:peek_local(?KNM_CACHE, Key) of
        {'ok', Ok} -> Ok;
        {'error', 'not_found'} ->
            case fetch_account_from_number(NormalizedNum) of
                {'ok', _, _}=Ok ->
                    NumberDb = knm_converters:to_db(NormalizedNum),
                    CacheProps = [{'origin', [{'db', NumberDb, NormalizedNum}]}],
                    wh_cache:store_local(?KNM_CACHE, Key, Ok, CacheProps),
                    Ok;
                Else -> Else
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_change_state(number(), ne_binary()) -> number_return().
maybe_change_state(Number, ToState) ->
    FromState = knm_phone_number:state(Number),
    case is_change_allowed(FromState, ToState) of
        {'error', _R}=E -> E;
        'ok' ->
            UpdatedNumber = knm_phone_number:set_state(Number, ToState),
            knm_phone_number:save(UpdatedNumber)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_change_allowed(ne_binary(), ne_binary() | ne_binaries()) -> 'ok' | {'error', _}.
is_change_allowed(_FromState, ?NUMBER_STATE_PORT_IN) ->
    'ok';
is_change_allowed(FromState, ?NUMBER_STATE_PORT_OUT) ->
    is_change_allowed(FromState, [?NUMBER_STATE_PORT_IN]);
is_change_allowed(_FromState, ?NUMBER_STATE_DISCOVERY) ->
    'ok';
is_change_allowed(FromState, ?NUMBER_STATE_IN_SERVICE) ->
    is_change_allowed(FromState, [?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_DISCOVERY]);
is_change_allowed(FromState, ?NUMBER_STATE_RELEASED) ->
    is_change_allowed(FromState, [?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED]);
is_change_allowed(FromState, ?NUMBER_STATE_RESERVED) ->
    is_change_allowed(FromState, [?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_AVAILABLE]);
is_change_allowed(FromState, ?NUMBER_STATE_AVAILABLE) ->
    is_change_allowed(FromState, [?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_RELEASED]);
is_change_allowed(FromState, ?NUMBER_STATE_DISCONNECTED) ->
    is_change_allowed(FromState, []);
is_change_allowed(FromState, ?NUMBER_STATE_DELETED) ->
    is_change_allowed(FromState, []);
is_change_allowed(FromState, Allowed) when is_list(Allowed) ->
    case lists:member(FromState, Allowed) of
        'true' -> 'ok';
        'false' -> {'error', 'wrong_state'}
    end;
is_change_allowed(_FromState, _ToState) ->
    lager:error("unknown state ~s", [_ToState]),
    {'error', 'unknown_state'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_account_from_number(ne_binary()) -> lookup_account_return().
fetch_account_from_number(NormalizedNum) ->
    case knm_phone_number:fetch(NormalizedNum, <<"system">>) of
        {'error', _}=Error -> fetch_account_from_ports(NormalizedNum, Error);
        {'ok', Number} -> check_number(Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_number(number()) -> lookup_account_return().
check_number(Number) ->
    AssignedTo = knm_phone_number:assigned_to(Number),
    case wh_util:is_empty(AssignedTo) of
        'true' -> {'error', 'unassigned'};
        'false' ->
            States = [?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_PORT_OUT],
            State = knm_phone_number:state(Number),
            case lists:member(State, States) of
                'false' -> {'error', {'not_in_service', AssignedTo}};
                'true' -> check_account(Number)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_account(number()) -> lookup_account_return().
check_account(Number) ->
    AssignedTo = knm_phone_number:assigned_to(Number),
    case wh_util:is_account_enabled(AssignedTo) of
        'false' -> {'error', {'account_disabled', AssignedTo}};
        'true' ->
            Module = knm_phone_number:module_name(Number),
            State = knm_phone_number:state(Number),
            Num = knm_phone_number:state(Number),
            Props = [
                {'pending_port', State =:= ?NUMBER_STATE_PORT_IN}
                ,{'local', Module =:= 'wnm_local'}
                ,{'number', Num}
                ,{'account_id', AssignedTo}
                ,{'prepend', feature_prepend(Number)}
                ,{'inbound_cnam', feature_inbound_cname(Number)}
                ,{'ringback_media', find_early_ringback(Number)}
                ,{'transfer_media', find_transfer_ringback(Number)}
                ,{'force_outbound', should_force_outbound(Number)}
            ],
            {'ok', AssignedTo, Props}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_prepend(number()) -> ne_binary().
feature_prepend(Number) ->
    Prepend = knm_phone_number:feature(Number, <<"prepend">>),
    case wh_json:is_true(<<"enabled">>, Prepend) of
        'false' -> 'undefined';
        'true' -> wh_json:get_ne_value(<<"name">>, Prepend)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_inbound_cname(number()) -> boolean().
feature_inbound_cname(Number) ->
    case knm_phone_number:feature(Number, <<"inbound_cnam">>) of
        'undefined' -> 'false';
        Module ->
            try Module:should_lookup_cnam() of
                Boolean -> wh_util:is_true(Boolean)
            catch
                _E:_R -> 'true'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_early_ringback(number()) -> api_binary().
find_early_ringback(Number) ->
    RingBack = knm_phone_number:feature(Number, <<"ringback">>),
    wh_json:get_ne_value(<<"early">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_transfer_ringback(number()) -> api_binary().
find_transfer_ringback(Number) ->
    RingBack = knm_phone_number:feature(Number, <<"ringback">>),
    wh_json:get_ne_value(<<"transfer">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_force_outbound(number()) -> boolean().
-spec should_force_outbound(ne_binary(), ne_binary(), boolean()) -> boolean().
should_force_outbound(Number) ->
    Module = knm_phone_number:module_name(Number),
    State = knm_phone_number:state(Number),
    ForceOutbound = knm_phone_number:feature(Number, <<"force_outbound">>),
    should_force_outbound(State, Module, ForceOutbound).

should_force_outbound(?NUMBER_STATE_PORT_IN, _Module, ForceOutbound) ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_in_outbound">>, 'true')
    orelse force_module_outbound(ForceOutbound);
should_force_outbound(?NUMBER_STATE_PORT_OUT, _Module, ForceOutbound) ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_out_outbound">>, 'true')
    orelse force_module_outbound(ForceOutbound);
should_force_outbound(_State, <<"wnm_local">>, _ForceOutbound) ->
    force_local_outbound();
should_force_outbound(_State, _Module, 'undefined') ->
    default_force_outbound();
should_force_outbound(_State, _Module, ForceOutbound) ->
    wh_util:is_true(ForceOutbound).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec force_module_outbound(ne_binary()) -> boolean().
force_module_outbound(<<"wnm_local">>) -> force_local_outbound();
force_module_outbound(_Mod) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec force_local_outbound() -> boolean().
force_local_outbound() ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_local_outbound">>, 'true').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_force_outbound() -> boolean().
default_force_outbound() ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"default_force_outbound">>, 'false').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_account_from_ports(ne_binary(), {'error', _}) -> lookup_account_return().
fetch_account_from_ports(NormalizedNum, Error) ->
    case
        couch_mgr:get_results(
            ?KZ_PORT_REQUESTS_DB
            ,<<"port_requests/port_in_numbers">>
            ,[{'key', NormalizedNum}]
        )
    of
        {'ok', []} ->
            lager:debug("no port for ~s: ~p", [NormalizedNum, Error]),
            Error;
        {'ok', [Port]} ->
            AccountId = wh_json:get_value(<<"value">>, Port),
            Props = [
                {'force_outbound', 'true'}
                ,{'pending_port', 'true'}
                ,{'local', 'true'}
                ,{'inbound_cnam', 'false'}
                ,{'number', NormalizedNum}
                ,{'account_id', AccountId}
            ],
            {'ok', AccountId, Props};
        {'error', 'not_found'}=E ->
            lager:debug("port number ~s not found", [NormalizedNum]),
            E;
        {'error', _E} ->
            lager:debug("failed to query for port number '~s': ~p", [NormalizedNum, _E]),
            Error
    end.