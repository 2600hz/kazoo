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
-spec get(ne_binary(), wh_proplist()) -> number_return().
get(Num) ->
    get(Num, knm_phone_number:default_options()).

get(Num, Options) ->
    case knm_converters:is_reconcilable(Num) of
        'false' -> {'error', 'not_reconcilable'};
        'true' -> knm_phone_number:fetch(Num, Options)
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
            ,fun knm_phone_number:save/1
        ]),
    knm_phone_number:setters(knm_phone_number:new(), Updates).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binary(), ne_binary()) -> number_return().
-spec move(ne_binary(), ne_binary(), wh_proplist()) -> number_return().
move(Num, MoveTo) ->
    move(Num, MoveTo, knm_phone_number:default_options()).

move(Num, MoveTo, Options) ->
    lager:debug("trying to move ~s to ~s", [Num, MoveTo]),
    case ?MODULE:get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            AccountId = wh_util:format_account_id(MoveTo, 'raw'),
            AssignedTo = knm_phone_number:assigned_to(Number),
            Props = [
                {fun knm_phone_number:set_assigned_to/2, AccountId}
                ,{fun knm_phone_number:set_prev_assigned_to/2, AssignedTo}
                ,fun knm_phone_number:save/1
            ],
            knm_phone_number:setters(Number, Props)
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), wh_proplist()) -> number_return().
-spec update(ne_binary(), wh_proplist(), wh_proplist()) -> number_return().
update(Num, Props) ->
    update(Num, Props, knm_phone_number:default_options()).

update(Num, Props, Options) ->
    case ?MODULE:get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            case knm_phone_number:setters(Number, Props) of
                {'error', _R}=Error -> Error;
                {'ok', UpdatedNumber} ->
                    knm_phone_number:save(UpdatedNumber);
                UpdatedNumber ->
                    knm_phone_number:save(UpdatedNumber)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) -> number_return().
-spec delete(ne_binary(), wh_proplist()) -> number_return().
delete(Num) ->
    delete(Num, knm_phone_number:default_options()).

delete(Num, Options) ->
    case ?MODULE:get(Num, Options) of
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
-spec change_state(ne_binary(), ne_binary(), wh_proplist()) -> number_return().
change_state(Num, State) ->
    change_state(Num, State, knm_phone_number:default_options()).

change_state(Num, State, Options) ->
    case ?MODULE:get(Num, Options) of
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
-spec assigned_to_app(ne_binary(), ne_binary(), wh_proplist()) -> number_return().
assigned_to_app(Num, App) ->
    assigned_to_app(Num, App, knm_phone_number:default_options()).

assigned_to_app(Num, App, Options) ->
    case ?MODULE:get(Num, Options) of
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
-spec maybe_change_state(number(), ne_binary(), ne_binary()) -> number_return().
maybe_change_state(Number, ToState) ->
    FromState = knm_phone_number:state(Number),
    maybe_change_state(Number, FromState, ToState).

% TO IN_SERVICE
maybe_change_state(Number, ?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_IN_SERVICE) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
        ,fun knm_services:activate_phone_number/1
        ,fun knm_carriers:maybe_acquire/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(Number, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_IN_SERVICE) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
        ,fun knm_services:activate_phone_number/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(Number, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_IN_SERVICE) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(_Number, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_IN_SERVICE) ->
    {'error', 'not_change_required'};

% TO AVAILABLE
maybe_change_state(Number, ?NUMBER_STATE_RELEASED, ?NUMBER_STATE_AVAILABLE) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_AVAILABLE}
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(_Number, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_AVAILABLE) ->
    {'error', 'not_change_required'};

% TO RESERVED
maybe_change_state(Number, ?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_RESERVED) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
        ,fun update_reserved_history/1
        ,fun knm_services:activate_phone_number/1
        ,fun knm_carriers:maybe_acquire/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(Number, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_RESERVED) ->
    Props = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
        ,fun update_reserved_history/1
        ,fun knm_services:activate_phone_number/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(_Number, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_RESERVED) ->
    {'error', 'not_change_required'};

% TO RELEASED
maybe_change_state(Number, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_RELEASED) ->
    NewState = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE),
    Props = [
        {fun knm_phone_number:set_state/2, NewState}
        ,fun maybe_disconnect/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(Number, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RELEASED) ->
    NewState = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE),
    Props = [
        {fun knm_phone_number:set_state/2, NewState}
        ,fun maybe_disconnect/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_RELEASED) ->
    NewState = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE),
    Props = [
        {fun knm_phone_number:set_state/2, NewState}
        ,fun maybe_disconnect/1
        ,fun knm_phone_number:save/1
    ],
    knm_phone_number:setters(Number, Props);
maybe_change_state(_Number, ?NUMBER_STATE_RELEASED, ?NUMBER_STATE_RELEASED) ->
    {'error', 'not_change_required'};

% UNKNOWN CHANGE
maybe_change_state(_Number, _FromState, _ToState) ->
    lager:error("invalid state transition from ~s to ~s", [_FromState, _ToState]),
    {'error', 'invalid_state_transition'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_disconnect(number()) -> number_return().
maybe_disconnect(Number) ->
    case knm_phone_number:reserve_history(Number) of
        [] -> disconnect_or_delete(Number);
        [PrevReservation|History] ->
            lager:debug(
                "unwinding reservation history, reserving on account ~s"
                ,[PrevReservation]
            ),
            Props = [
                {fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
                ,{fun knm_phone_number:set_reserve_history/2, History}
            ],
            {'ok', knm_phone_number:setters(Number, Props)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disconnect_or_delete(number()) -> number_return().
-spec disconnect_or_delete(number(), boolean()) -> number_return().
disconnect_or_delete(Number) ->
    Bool = whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_permanently_delete">>, 'false'),
    disconnect_or_delete(Number, Bool).

disconnect_or_delete(Number, 'false') ->
    attempt_disconnect(Number);
disconnect_or_delete(Number, 'true') ->
    case attempt_disconnect(Number) of
        {'ok', Number1} ->
            {'ok', knm_phone_number:set_state(Number1, ?NUMBER_STATE_DELETED)};
        {'error', _R} ->
            {'ok', knm_phone_number:set_state(Number, ?NUMBER_STATE_DELETED)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec attempt_disconnect(number()) -> number_return().
attempt_disconnect(Number) ->
    case knm_carriers:disconnect(Number) of
        {'error', _R}=Error -> Error;
        {'ok', Number1} ->
            {'ok', knm_phone_number:set_reserve_history(Number1, [])}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_reserved_history(number()) -> number().
update_reserved_history(Number) ->
    History = knm_phone_number:reserve_history(Number),
    AssignedTo = knm_phone_number:assigned_to(Number),
    knm_phone_number:set_reserve_history(Number, [AssignedTo|History]).

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
        _ ->
            Mod = knm_phone_number:module_name(Number),
            Module = wh_util:to_atom(Mod, 'true'),
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