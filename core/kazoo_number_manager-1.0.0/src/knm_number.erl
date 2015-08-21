%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number).

-export([new/0
         ,get/1, get/2
         ,create/2
         ,move/2, move/3
         ,update/2, update/3
         ,delete/1, delete/2
         ,change_state/2, change_state/3
         ,assign_to_app/2, assign_to_app/3
         ,lookup_account/1
         ,buy/2, buy/3
         ,save/1
         ,reconcile/3
        ]).

-export([phone_number/1, set_phone_number/2
         ,services/1, set_services/2
         ,transactions/1
         ,add_transaction/2
         ,errors/1
         ,charges/2, set_charges/3
        ]).

-include("knm.hrl").

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_number()
                     ,services :: wh_services:services()
                     ,transactions = [] :: wh_transaction:transactions()
                     ,errors = [] :: list()
                     ,charges = [] :: [{ne_binary(), integer()}]
                    }).
-opaque knm_number() :: #knm_number{}.
-type knm_numbers() :: [knm_number()].

-export_type([knm_number/0
              ,knm_numbers/0
              ,number_options/0
              ,knm_number_return/0
             ]).

-type number_option() :: {'pending_port', boolean()} |
                         {'local', boolean()} |
                         {'number', ne_binary()} |
                         {'account_id', ne_binary()} |
                         {'prepend', api_binary()} |
                         {'inbound_cnam', boolean()} |
                         {'ringback_media', api_binary()} |
                         {'transfer_media', api_binary()} |
                         {'force_outbound', boolean()}.

-type number_options() :: [number_option()].

-type lookup_error() :: 'not_reconcilable' |
                        'not_found' |
                        'unassigned' |
                        {'not_in_service', ne_binary()} |
                        {'account_disabled', ne_binary()}.

-type lookup_account_return() :: {'ok', ne_binary(), number_options()} |
                                 {'error', lookup_error()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> knm_number().
new() -> #knm_number{}.

-spec get(ne_binary()) ->
                 knm_number_return().
-spec get(ne_binary(), wh_proplist()) ->
                 knm_number_return().
get(Num) ->
    get(Num, knm_phone_number:default_options()).

get(Num, Options) ->
    case knm_converters:is_reconcilable(Num) of
        'false' -> {'error', 'not_reconcilable'};
        'true' -> get_number(Num, Options)
    end.

-spec get_number(ne_binary(), wh_proplist()) ->
                        knm_number_return().
get_number(Num, Options) ->
    wrap_phone_number_return(
      knm_phone_number:fetch(Num, Options)
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), wh_proplist()) -> knm_number_return().
create(Num, Props) ->
    Updates = create_updaters(Num, Props),
    wrap_phone_number_return(
      knm_phone_number:setters(knm_phone_number:new(), Updates)
     ).

-spec create_updaters(ne_binary(), wh_proplist()) ->
                             knm_phone_number:set_functions().
create_updaters(<<_/binary>> = Num, Props) when is_list(Props) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),

    props:filter_undefined(
      [{fun knm_phone_number:set_number/2, NormalizedNum}
       ,{fun knm_phone_number:set_number_db/2, NumberDb}
       ,{fun knm_phone_number:set_state/2, props:get_binary_value(<<"state">>, Props, ?NUMBER_STATE_IN_SERVICE)}
       ,{fun knm_phone_number:set_ported_in/2, props:is_true(<<"ported_in">>, Props, 'false')}
       ,{fun knm_phone_number:set_assigned_to/2, props:get_binary_value(<<"assigned_to">>, Props)}
       ,{fun knm_phone_number:set_auth_by/2, props:get_binary_value(<<"auth_by">>, Props, ?DEFAULT_AUTH_BY)}
       ,{fun knm_phone_number:set_dry_run/2, props:is_true(<<"dry_run">>, Props, 'false')}
       ,fun knm_phone_number:save/1
      ]).

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

            AssignedTo = knm_phone_number:assigned_to(phone_number(Number)),

            Routines = [{fun knm_phone_number:set_assigned_to/2, AccountId}
                        ,{fun knm_phone_number:set_prev_assigned_to/2, AssignedTo}
                        ,fun knm_phone_number:save/1
                       ],
            apply_routines(Number, Routines)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), knm_phone_number:set_functions()) ->
                    knm_number_return().
-spec update(ne_binary(), knm_phone_number:set_functions(), wh_proplist()) ->
                    knm_number_return().
update(Num, Routines) ->
    update(Num, Routines, knm_phone_number:default_options()).

update(Num, Routines, Options) ->
    case ?MODULE:get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            update_phone_number(Number, Routines)
    end.

-spec update_phone_number(knm_number(), knm_phone_number:set_functions()) ->
                                 knm_number_return().
update_phone_number(Number, Routines) ->
    PhoneNumber = phone_number(Number),
    case knm_phone_number:setters(PhoneNumber, Routines) of
        {'error', _R}=Error -> Error;
        {'ok', UpdatedPhoneNumber} ->
            wrap_phone_number_return(
              knm_phone_number:save(UpdatedPhoneNumber)
              ,Number
             );
        UpdatedPhoneNumber ->
            wrap_phone_number_return(
              knm_phone_number:save(UpdatedPhoneNumber)
              ,Number
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number()) -> knm_number_return().
save(Number) ->
    _ = knm_services:maybe_update_services(Number),
    wrap_phone_number_return(
      knm_phone_number:save(phone_number(Number))
      ,Number
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reconcile(ne_binary(), ne_binary(), ne_binary()) ->
                       knm_number_return().
reconcile(DID, AssignTo, AuthBy) ->
    case ?MODULE:get(DID) of
        {'ok', Number} ->
            reconcile_number(Number, AssignTo, AuthBy);
        {'error', 'not_found'} ->
            create(DID, [{<<"assigned_to">>, AssignTo}
                         ,{<<"auth_by">>, AuthBy}
                         ,{<<"state">>, ?NUMBER_STATE_IN_SERVICE}
                        ]);
        {'error', _}=E -> E
    end.

reconcile_number(Number, AssignTo, AuthBy) ->
    PhoneNumber = ?MODULE:phone_number(Number),
    Updaters = [{AssignTo
                 ,knm_phone_number:assigned_to(PhoneNumber)
                 ,fun knm_phone_number:set_assigned_to/2
                }
                ,{AuthBy
                  ,knm_phone_number:auth_by(PhoneNumber)
                  ,fun knm_phone_number:set_auth_by/2
                 }
                ,{?NUMBER_STATE_IN_SERVICE
                  ,knm_phone_number:state(PhoneNumber)
                  ,fun knm_phone_number:set_state/2
                 }
               ],
    case updates_require_save(PhoneNumber, Updaters) of
        {'true', UpdatedPhoneNumber} ->
            wrap_phone_number_return(
              knm_phone_number:save(UpdatedPhoneNumber)
              ,Number
             );
        {'false', _PhoneNumber} ->
            {'ok', Number}
    end.

-spec updates_require_save(knm_phone_number:knm_number(), up_req_els()) ->
                                  up_req_acc().
updates_require_save(PhoneNumber, Updaters) ->
    lists:foldl(fun update_requires_save/2
                ,{'false', PhoneNumber}
                ,Updaters
               ).

-type set_fun() :: fun((knm_phone_number:knm_number(), term()) -> knm_phone_number:knm_number()).

-type up_req_el() :: {ne_binary(), api_binary(), set_fun()}.
-type up_req_els() :: [up_req_el()].
-type up_req_acc() :: {boolean(), knm_phone_number:knm_number()}.

-spec update_requires_save(up_req_el(), up_req_acc()) -> up_req_acc().
update_requires_save({V, V, _Fun}, Acc) ->
    Acc;
update_requires_save({NewV, _OldV, SetFun}, {_, PhoneNumber}) ->
    {'true', SetFun(PhoneNumber, NewV)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) ->
                    knm_number_return().
-spec delete(ne_binary(), wh_proplist()) ->
                    knm_number_return().
delete(Num) ->
    delete(Num, knm_phone_number:default_options()).

delete(Num, Options) ->
    case ?MODULE:get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            PhoneNumber = phone_number(Number),
            wrap_phone_number_return(
              knm_phone_number:delete(PhoneNumber)
              ,Number
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_state(ne_binary() | knm_number(), ne_binary()) ->
                          knm_number_return().
-spec change_state(ne_binary() | knm_number(), ne_binary(), wh_proplist()) ->
                          knm_number_return().
change_state(Num, State) ->
    change_state(Num, State, knm_phone_number:default_options()).

change_state(<<_/binary>> = Num, State, Options) ->
    case ?MODULE:get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            maybe_change_state(Number, State)
    end;
change_state(Number, State, _Options) ->
    maybe_change_state(Number, State).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binary(), api_binary()) ->
                           knm_number_return().
-spec assign_to_app(ne_binary(), api_binary(), wh_proplist()) ->
                           knm_number_return().
assign_to_app(Num, App) ->
    assign_to_app(Num, App, knm_phone_number:default_options()).

assign_to_app(Num, App, Options) ->
    case ?MODULE:get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            maybe_update_assignment(Number, App)
    end.

-spec maybe_update_assignment(knm_number:knm_number(), api_binary()) ->
                                     knm_number_return().
maybe_update_assignment(Number, NewApp) ->
    PhoneNumber = knm_number:phone_number(Number),
    case knm_phone_number:used_by(PhoneNumber) of
        NewApp -> {'ok', Number};
        _OldApp ->
            UpdatedPhoneNumber = knm_phone_number:set_used_by(PhoneNumber, NewApp),
            wrap_phone_number_return(
              knm_phone_number:save(UpdatedPhoneNumber)
              ,Number
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup_account(api_binary()) -> lookup_account_return().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec buy(ne_binary(), ne_binary()) -> number_return().
-spec buy(ne_binary(), ne_binary(), wh_proplist()) -> number_return().
buy(Num, Account) ->
    buy(Num, Account, []).

buy(Num, Account, Options) ->
    Updates = [{fun knm_phone_number:set_assigned_to/2, wh_util:format_account_id(Account, 'raw')}
               ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
               ,fun knm_carriers:maybe_acquire/1
              ],
    update(Num, Updates, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_change_state(knm_number(), ne_binary()) ->
                                knm_number_return().
-spec maybe_change_state(knm_number(), ne_binary(), ne_binary()) ->
                                knm_number_return().
maybe_change_state(Number, ToState) ->
    FromState = knm_phone_number:state(phone_number(Number)),
    maybe_change_state(Number, FromState, ToState).

%% TO IN_SERVICE
maybe_change_state(Number, ?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_IN_SERVICE) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
                ,fun knm_services:activate_phone_number/1
                ,fun knm_carriers:maybe_acquire/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);

maybe_change_state(Number, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_IN_SERVICE) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
                ,fun knm_services:activate_phone_number/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(Number, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_IN_SERVICE) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(_Number, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_IN_SERVICE) ->
    {'error', 'no_change_required'};

%% TO AVAILABLE
maybe_change_state(Number, ?NUMBER_STATE_RELEASED, ?NUMBER_STATE_AVAILABLE) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_AVAILABLE}
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(_Number, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_AVAILABLE) ->
    {'error', 'no_change_required'};

%% TO RESERVED
maybe_change_state(Number, ?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_RESERVED) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
                ,fun update_reserved_history/1
                ,fun knm_services:activate_phone_number/1
                ,fun knm_carriers:maybe_acquire/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(Number, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_RESERVED) ->
    Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
                ,fun update_reserved_history/1
                ,fun knm_services:activate_phone_number/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(_Number, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_RESERVED) ->
    {'error', 'no_change_required'};

%% TO RELEASED
maybe_change_state(Number, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_RELEASED) ->
    NewState = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE),
    Routines = [{fun knm_phone_number:set_state/2, NewState}
                ,fun maybe_disconnect/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(Number, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RELEASED) ->
    NewState = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE),
    Routines = [{fun knm_phone_number:set_state/2, NewState}
                ,fun maybe_disconnect/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_RELEASED) ->
    NewState = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE),
    Routines = [{fun knm_phone_number:set_state/2, NewState}
                ,fun maybe_disconnect/1
                ,fun knm_phone_number:save/1
               ],
    apply_routines(Number, Routines);
maybe_change_state(_Number, ?NUMBER_STATE_RELEASED, ?NUMBER_STATE_RELEASED) ->
    {'error', 'no_change_required'};

%% UNKNOWN CHANGE
maybe_change_state(_Number, _FromState, _ToState) ->
    lager:error("invalid state transition from ~s to ~s", [_FromState, _ToState]),
    {'error', 'invalid_state_transition'}.

-spec apply_routines(knm_number(), knm_phone_number:set_functions()) ->
                            knm_number_return().
apply_routines(Number, Routines) ->
    wrap_phone_number_return(
      knm_phone_number:setters(phone_number(Number), Routines)
      ,Number
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_disconnect(knm_phone_number:knm_number()) ->
                              number_return().
maybe_disconnect(PhoneNumber) ->
    case knm_phone_number:reserve_history(PhoneNumber) of
        [] -> disconnect_or_delete(PhoneNumber);
        [PrevReservation|History] ->
            lager:debug(
              "unwinding reservation history, reserving on account ~s"
              ,[PrevReservation]
             ),
            Routines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
                        ,{fun knm_phone_number:set_reserve_history/2, History}
                       ],
            knm_phone_number:setters(PhoneNumber, Routines)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disconnect_or_delete(knm_phone_number:knm_number()) ->
                                  knm_phone_number:knm_number().
-spec disconnect_or_delete(knm_phone_number:knm_number(), boolean()) ->
                                  knm_phone_number:knm_number().
disconnect_or_delete(PhoneNumber) ->
    ShouldDelete = whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_permanently_delete">>, 'false'),
    disconnect_or_delete(PhoneNumber, ShouldDelete).

disconnect_or_delete(PhoneNumber, 'false') ->
    attempt_disconnect(PhoneNumber);
disconnect_or_delete(PhoneNumber, 'true') ->
    case attempt_disconnect(PhoneNumber) of
        {'ok', PhoneNumber1} ->
            knm_phone_number:set_state(PhoneNumber1, ?NUMBER_STATE_DELETED);
        {'error', _R} ->
            knm_phone_number:set_state(PhoneNumber, ?NUMBER_STATE_DELETED)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec attempt_disconnect(knm_phone_number:knm_number()) -> number_return().
attempt_disconnect(PhoneNumber) ->
    case knm_carriers:disconnect(PhoneNumber) of
        {'error', _R}=Error -> Error;
        {'ok', PhoneNumber1} ->
            {'ok', knm_phone_number:set_reserve_history(PhoneNumber1, [])}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_reserved_history(knm_phone_number:knm_number()) ->
                                     knm_phone_number:knm_number().
update_reserved_history(PhoneNumber) ->
    History = knm_phone_number:reserve_history(PhoneNumber),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    knm_phone_number:set_reserve_history(PhoneNumber, [AssignedTo|History]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_account_from_number(ne_binary()) -> lookup_account_return().
fetch_account_from_number(NormalizedNum) ->
    case knm_phone_number:fetch(NormalizedNum) of
        {'error', _}=Error -> fetch_account_from_ports(NormalizedNum, Error);
        {'ok', PhoneNumber} -> check_number(PhoneNumber)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_number(knm_phone_number:knm_number()) -> lookup_account_return().
check_number(PhoneNumber) ->
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    case wh_util:is_empty(AssignedTo) of
        'true' -> {'error', 'unassigned'};
        'false' ->
            States = [?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_PORT_OUT],
            State = knm_phone_number:state(PhoneNumber),
            case lists:member(State, States) of
                'false' -> {'error', {'not_in_service', AssignedTo}};
                'true' -> check_account(PhoneNumber)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_account(knm_phone_number:knm_number()) -> lookup_account_return().
check_account(PhoneNumber) ->
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    case wh_util:is_account_enabled(AssignedTo) of
        'false' -> {'error', {'account_disabled', AssignedTo}};
        'true' ->
            Module = knm_phone_number:module_name(PhoneNumber),
            State = knm_phone_number:state(PhoneNumber),
            Num = knm_phone_number:number(PhoneNumber),
            Props = [{'pending_port', State =:= ?NUMBER_STATE_PORT_IN}
                     ,{'local', Module =:= ?LOCAL_CARRIER}
                     ,{'number', Num}
                     ,{'account_id', AssignedTo}
                     ,{'prepend', feature_prepend(PhoneNumber)}
                     ,{'inbound_cnam', feature_inbound_cname(PhoneNumber)}
                     ,{'ringback_media', find_early_ringback(PhoneNumber)}
                     ,{'transfer_media', find_transfer_ringback(PhoneNumber)}
                     ,{'force_outbound', should_force_outbound(PhoneNumber)}
                    ],
            {'ok', AssignedTo, Props}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_prepend(knm_phone_number:knm_number()) -> api_binary().
feature_prepend(PhoneNumber) ->
    Prepend = knm_phone_number:feature(PhoneNumber, <<"prepend">>),
    case wh_json:is_true(<<"enabled">>, Prepend) of
        'false' -> 'undefined';
        'true' -> wh_json:get_ne_value(<<"name">>, Prepend)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_inbound_cname(knm_phone_number:knm_number()) -> boolean().
feature_inbound_cname(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, <<"inbound_cnam">>) of
        'undefined' -> 'false';
        _ ->
            Mod = knm_phone_number:module_name(PhoneNumber),
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
-spec find_early_ringback(knm_phone_number:knm_number()) -> api_binary().
find_early_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, <<"ringback">>),
    wh_json:get_ne_value(<<"early">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:knm_number()) -> api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, <<"ringback">>),
    wh_json:get_ne_value(<<"transfer">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_force_outbound(knm_phone_number:knm_number()) -> boolean().
-spec should_force_outbound(ne_binary(), ne_binary(), boolean()) -> boolean().
should_force_outbound(PhoneNumber) ->
    Module = knm_phone_number:module_name(PhoneNumber),
    State = knm_phone_number:state(PhoneNumber),
    ForceOutbound = force_outbound_feature(PhoneNumber),
    should_force_outbound(State, Module, wh_util:is_true(ForceOutbound)).

should_force_outbound(?NUMBER_STATE_PORT_IN, Module, _ForceOutbound) ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_in_outbound">>, 'true')
        orelse force_module_outbound(Module);
should_force_outbound(?NUMBER_STATE_PORT_OUT, Module, _ForceOutbound) ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_out_outbound">>, 'true')
        orelse force_module_outbound(Module);
should_force_outbound(_State, ?LOCAL_CARRIER, _ForceOutbound) ->
    force_local_outbound();
should_force_outbound(_State, _Module, ForceOutbound) ->
    ForceOutbound.

-spec force_outbound_feature(knm_phone_number:knm_number()) -> boolean().
force_outbound_feature(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, <<"force_outbound">>) of
        'undefined' -> default_force_outbound();
        FO -> wh_util:is_true(FO)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec force_module_outbound(ne_binary()) -> boolean().
force_module_outbound(?LOCAL_CARRIER) -> force_local_outbound();
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
-spec fetch_account_from_ports(ne_binary(), {'error', _}) ->
                                      lookup_account_return().
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
            Props = [{'force_outbound', 'true'}
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wrap_phone_number_return(number_return() | knm_phone_number:knm_number()) ->
                                      knm_number_return().
-spec wrap_phone_number_return(number_return() | knm_phone_number:knm_number(), knm_number()) ->
                                      knm_number_return().
wrap_phone_number_return(Result) ->
    wrap_phone_number_return(Result, new()).

wrap_phone_number_return({'error', _}=E, _Number) -> E;
wrap_phone_number_return({'ok', PhoneNumber}, Number) ->
    {'ok', set_phone_number(Number, PhoneNumber)};
wrap_phone_number_return(PhoneNumber, Number) ->
    {'ok', set_phone_number(Number, PhoneNumber)}.

-spec phone_number(knm_number()) -> knm_phone_number:knm_number().
-spec set_phone_number(knm_number(), knm_phone_number:knm_number()) -> knm_number().
phone_number(#knm_number{knm_phone_number=PhoneNumber}) -> PhoneNumber.
set_phone_number(Number, PhoneNumber) ->
    Number#knm_number{knm_phone_number=PhoneNumber}.

-spec services(knm_number()) -> wh_services:services() | 'undefined'.
services(#knm_number{services=Services}) -> Services.

-spec set_services(knm_number(), wh_services:services()) -> knm_number().
set_services(#knm_number{}=Number, Services) ->
    Number#knm_number{services=Services}.

-spec transactions(knm_number()) -> wh_transaction:transactions().
transactions(#knm_number{transactions=Transactions}) -> Transactions.

-spec add_transaction(knm_number(), wh_transaction:transaction()) -> knm_number().
add_transaction(#knm_number{transactions=Transactions}=Number, Transaction) ->
    Number#knm_number{transactions=[Transaction|Transactions]}.

-spec errors(knm_number()) -> list().
errors(#knm_number{errors=Errors}) -> Errors.

-spec charges(knm_number(), ne_binary()) -> number().
-spec set_charges(knm_number(), ne_binary(), number()) -> knm_number().
charges(#knm_number{charges=Charges}, Key) ->
    props:get_value(Key, Charges, 0).

set_charges(#knm_number{charges=Charges}=Number, Key, Amount) ->
    Number#knm_number{charges=props:set_value(Key, Amount, Charges)}.
