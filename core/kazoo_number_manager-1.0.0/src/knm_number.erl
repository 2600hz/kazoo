%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_number).

-export([new/0
         ,get/1, get/2
         ,create/2
         ,move/2, move/3
         ,update/2, update/3
         ,delete/1, delete/2
         ,assign_to_app/2, assign_to_app/3
         ,lookup_account/1
         ,buy/2, buy/3
         ,save/1
         ,reconcile/3
        ]).

-export([phone_number/1, set_phone_number/2
         ,services/1, set_services/2
         ,billing_id/1, set_billing_id/2
         ,transactions/1
         ,add_transaction/2
         ,errors/1
         ,charges/2, set_charges/3
        ]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("knm.hrl").

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_number()
                     ,services :: wh_services:services()
                     ,billing_id :: api_binary()
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
              ,dry_run_return/0
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
    attempt(fun create_or_load/2, [Num, Props]).

-spec create_or_load(ne_binary(), wh_proplist()) ->
                            knm_number() | dry_run_return().
-spec create_or_load(ne_binary(), wh_proplist(), number_return()) ->
                            knm_number() | dry_run_return().
create_or_load(Num, Props) ->
    create_or_load(Num, Props, knm_phone_number:fetch(Num)).

create_or_load(Num, Props, {'error', 'not_found'}) ->
    ensure_can_create(Num, Props),
    Updates = create_updaters(Num, Props),

    Number = set_phone_number(new()
                              ,knm_phone_number:setters(knm_phone_number:new(), Updates)
                             ),
    create_phone_number(Number);
create_or_load(Num, Props, {'ok', PhoneNumber}) ->
    ensure_can_load_to_create(PhoneNumber),
    Updates = create_updaters(Num, Props),
    create_phone_number(
      set_phone_number(new()
                       ,knm_phone_number:setters(PhoneNumber, Updates)
                      )
     ).

-spec ensure_can_load_to_create(knm_phone_number:knm_number()) -> 'true'.
ensure_can_load_to_create(PhoneNumber) ->
    ensure_state(PhoneNumber, ?NUMBER_STATE_AVAILABLE).

-spec ensure_state(knm_phone_number:knm_number(), ne_binary()) -> 'true'.
ensure_state(PhoneNumber, ExpectedState) ->
    case knm_phone_number:state(PhoneNumber) of
        ExpectedState -> 'true';
        _State ->
            knm_errors:number_exists(
              knm_phone_number:number(PhoneNumber)
             )
    end.

-spec create_phone_number(knm_number()) ->
                                 knm_number() |
                                 dry_run_return().
create_phone_number(Number) ->
    ensure_state(phone_number(Number), ?NUMBER_STATE_AVAILABLE),
    Routines = [fun knm_number_states:to_reserved/1
                ,fun save_number/1
                ,fun dry_run_or_number/1
               ],
    apply_number_routines(Number, Routines).

-spec save_number(knm_number()) -> knm_number().
save_number(Number) ->
    Routines = [fun knm_providers:save/1
                ,fun save_phone_number/1
                ,fun knm_services:update_services/1
               ],
    apply_number_routines(Number, Routines).

-spec save_phone_number(knm_number()) -> knm_number().
save_phone_number(Number) ->
    set_phone_number(
      Number
      ,knm_phone_number:save(phone_number(Number))
     ).

-spec dry_run_or_number(knm_number()) ->
                               dry_run_return() |
                               knm_number().
dry_run_or_number(Number) ->
    case knm_phone_number:dry_run(phone_number(Number)) of
        'true' -> dry_run_response(Number);
        'false' -> Number
    end.

-spec dry_run_response(knm_number()) -> dry_run_return().
dry_run_response(Number) ->
    {'dry_run'
     ,services(Number)
     ,knm_services:phone_number_activation_charges(Number)
    }.

-spec ensure_can_create(ne_binary(), wh_proplist()) -> 'true'.
ensure_can_create(Num, Props) ->
    ensure_account_can_create(Props)
        andalso ensure_number_is_not_porting(Num).

-spec ensure_account_can_create(wh_proplist()) -> 'true'.
ensure_account_can_create(Props) ->
    case props:get_value(<<"auth_by">>, Props) of
        'undefined' -> knm_errors:unauthorized();
        AccountId ->
            ensure_account_is_allowed_to_create(Props, AccountId)
    end.

-ifdef(TEST).
-define(LOAD_ACCOUNT(Props, _AccountId)
        ,{'ok', props:get_value(<<"auth_by_account">>, Props)}
       ).
-else.
-define(LOAD_ACCOUNT(_Props, AccountId)
        ,kz_account:fetch(AccountId)
       ).
-endif.

ensure_account_is_allowed_to_create(_Props, _AccountId) ->
    {'ok', JObj} = ?LOAD_ACCOUNT(_Props, _AccountId),
    case kz_account:allow_number_additions(JObj) of
        'true' -> 'true';
        'false' -> knm_errors:unauthorized()
    end.

-spec ensure_number_is_not_porting(ne_binary()) -> 'true'.
-ifdef(TEST).
ensure_number_is_not_porting(?TEST_CREATE_NUM) -> 'true';
ensure_number_is_not_porting(?TEST_EXISTING_NUM = Num) ->
    knm_errors:number_is_porting(Num).
-else.
ensure_number_is_not_porting(Num) ->
    case knm_port_request:get(Num) of
        {'ok', _Doc} -> knm_errors:number_is_porting(Num);
        {'error', 'not_found'} -> 'true'
    end.
-endif.

-spec create_updaters(ne_binary(), wh_proplist()) ->
                             knm_phone_number:set_functions().
create_updaters(<<_/binary>> = Num, Props) when is_list(Props) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),

    props:filter_undefined(
      [{fun knm_phone_number:set_number/2, NormalizedNum}
       ,{fun knm_phone_number:set_number_db/2, NumberDb}
       ,{fun knm_phone_number:set_state/2, props:get_binary_value(<<"state">>, Props, ?NUMBER_STATE_AVAILABLE)}
       ,{fun knm_phone_number:set_ported_in/2, props:is_true(<<"ported_in">>, Props, 'false')}
       ,{fun knm_phone_number:set_assign_to/2, props:get_binary_value(<<"assign_to">>, Props)}
       ,{fun knm_phone_number:set_auth_by/2, props:get_binary_value(<<"auth_by">>, Props)}
       ,{fun knm_phone_number:set_dry_run/2, props:is_true(<<"dry_run">>, Props, 'false')}
       ,{fun knm_phone_number:set_module_name/2, props:get_binary_value(<<"module_name">>, Props, knm_carriers:default_carrier())}
       ,{fun knm_phone_number:set_doc/2, props:get_value(<<"public_fields">>, Props)}
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
            wrap_phone_number_routines(Number, Routines)
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
    Number1 = knm_services:update_services(Number),
    wrap_phone_number_return(
      knm_phone_number:save(phone_number(Number1))
      ,Number1
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
            delete_number(Number)
    end.

-spec delete_number(knm_number()) ->
                           knm_number_return().
delete_number(Number) ->
    Routines = [fun knm_phone_number:release/1],
    unwind_or_disconnect(
      set_phone_number(
        Number
        ,apply_phone_number_routines(Number, Routines)
       )
     ).

-spec unwind_or_disconnect(knm_number()) ->
                                  knm_number_return().
-spec unwind_or_disconnect(knm_number(), knm_phone_number:knm_number()) ->
                                  knm_number_return().
unwind_or_disconnect(Number) ->
    PhoneNumber = phone_number(Number),
    unwind_or_disconnect(Number
                         ,knm_phone_number:unwind_reserve_history(PhoneNumber)
                        ).

unwind_or_disconnect(Number, PhoneNumber) ->
    case knm_phone_number:reserve_history(PhoneNumber) of
        [] -> disconnect(set_phone_number(Number, PhoneNumber));
        History -> unwind(Number, PhoneNumber, History)
    end.

-spec unwind(knm_number(), knm_phone_number:knm_number(), ne_binaries()) ->
                    knm_number_return().
unwind(Number, PhoneNumber, [NewAssignedTo|_]) ->
    Routines = [{fun knm_phone_number:set_assigned_to/2, NewAssignedTo}
                ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
               ],
    wrap_phone_number_routines(
      set_phone_number(Number, PhoneNumber)
      ,Routines
     ).

-spec disconnect(knm_number()) -> knm_number_return().
-spec disconnect(knm_number(), boolean()) -> knm_number_return().
disconnect(Number) ->
    disconnect(Number, knm_config:should_permanently_delete('false')).

disconnect(Number, ShouldDelete) ->
    try knm_carriers:disconnect(Number) of
        N1 -> maybe_delete_phone_number(N1, ShouldDelete)
    catch
        _E:_R when ShouldDelete ->
            lager:debug("failed to disconnect number: ~s: ~p", [_E, _R]),
            delete_phone_number(Number)
    end.

-spec maybe_delete_phone_number(knm_number(), boolean()) ->
                                       knm_number_return().
maybe_delete_phone_number(Number, 'false') -> {'ok', Number};
maybe_delete_phone_number(Number, 'true') -> delete_number(Number).

-spec delete_phone_number(knm_number()) -> {'ok', knm_number()}.
delete_phone_number(Number) ->
    {'ok', knm_number_states:to_deleted(Number)}.

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
-spec buy(ne_binary(), ne_binary()) ->
                 knm_number_return().
-spec buy(ne_binary(), ne_binary(), wh_proplist()) ->
                 knm_number_return().
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
-spec wrap_phone_number_routines(knm_number(), knm_phone_number:set_functions()) ->
                                         knm_number_return().
wrap_phone_number_routines(Number, Routines) ->
    wrap_phone_number_return(
      apply_phone_number_routines(Number, Routines)
      ,Number
     ).

-spec apply_phone_number_routines(knm_number(), knm_phone_number:set_functions()) ->
                                         knm_phone_number:knm_number().
apply_phone_number_routines(Number, Routines) ->
    knm_phone_number:setters(phone_number(Number), Routines).

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
                     ,{'local', Module =:= ?CARRIER_LOCAL}
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
should_force_outbound(_State, ?CARRIER_LOCAL, _ForceOutbound) ->
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
force_module_outbound(?CARRIER_LOCAL) -> force_local_outbound();
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
-spec set_phone_number(knm_number(), knm_phone_number:knm_number()) ->
                              knm_number().
phone_number(#knm_number{knm_phone_number=PhoneNumber}) -> PhoneNumber.
set_phone_number(Number, PhoneNumber) ->
    Number#knm_number{knm_phone_number=PhoneNumber}.

-spec services(knm_number()) -> wh_services:services() | 'undefined'.
services(#knm_number{services=Services}) -> Services.

-spec set_services(knm_number(), wh_services:services()) -> knm_number().
set_services(#knm_number{}=Number, Services) ->
    Number#knm_number{services=Services}.

-spec billing_id(knm_number()) -> api_binary().
billing_id(#knm_number{billing_id=BillingId}) ->
    BillingId.

-spec set_billing_id(knm_number(), ne_binary()) -> knm_number().
set_billing_id(#knm_number{}=Number, BillingId) ->
    Number#knm_number{billing_id=BillingId}.

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

-spec attempt(fun(), list()) -> knm_number_return().
attempt(Fun, Args) ->
    try apply(Fun, Args) of
        #knm_number{}=N -> {'ok', N};
        Resp -> Resp
    catch
        'throw':{'error', Reason} ->
            {'error', knm_errors:to_json(Reason)};
        'throw':{'error', Reason, Number} ->
            {'error', knm_errors:to_json(Reason, num_to_did(Number))};
        'throw':{'error', Reason, Number, Cause} ->
            {'error', knm_errors:to_json(Reason, num_to_did(Number), Cause)}
    end.

-spec num_to_did(ne_binary() | knm_number() | knm_phone_number:knm_number()) ->
                        ne_binary().
num_to_did(<<_/binary>> = DID) -> DID;
num_to_did(#knm_number{}=Number) ->
    num_to_did(phone_number(Number));
num_to_did(PhoneNumber) ->
    knm_phone_number:number(PhoneNumber).

-type number_routine() :: fun((knm_number()) ->
                                     knm_number() | dry_run_return()
                                         ).
-type number_routines() :: [number_routine()].
-spec apply_number_routines(knm_number(), number_routines()) ->
                                   knm_number() |
                                   dry_run_return().
apply_number_routines(Number, Routines) ->
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines).
