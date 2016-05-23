%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_number).

-export([new/0
         ,get/1, get/2
         ,create/2
         ,move/2, move/3
         ,update/2, update/3
         ,release/1, release/2
         %% TODO: delete/1,2 (calls knm_phone_number:delete/1
         ,assign_to_app/2, assign_to_app/3
         ,lookup_account/1
         ,save/1
         ,reconcile/2
         ,reserve/2
        ]).

-export([phone_number/1, set_phone_number/2
         ,services/1, set_services/2
         ,billing_id/1, set_billing_id/2
         ,transactions/1
         ,add_transaction/2
         ,errors/1
         ,charges/2, set_charges/3
         ,to_public_json/1
         ,is_number/1
        ]).


-ifdef(TEST).
-export([attempt/2]).
-export([ensure_can_load_to_create/1]).
-export([ensure_can_create/2]).
-export([create_or_load/3]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kazoo_number_manager/src/knm.hrl").

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_phone_number()
                     ,services :: kz_services:services()
                     ,billing_id :: api_binary()
                     ,transactions = [] :: kz_transaction:transactions()
                     ,errors = [] :: list()
                     ,charges = [] :: [{ne_binary(), integer()}]
                    }).
-opaque knm_number() :: #knm_number{}.
-type knm_numbers() :: [knm_number()].

-export_type([knm_number/0
              ,knm_numbers/0
              ,knm_number_return/0
              ,dry_run_return/0
             ]).

-type lookup_error() :: 'not_reconcilable' |
                        'not_found' |
                        'unassigned' |
                        {'not_in_service', ne_binary()} |
                        {'account_disabled', ne_binary()}.

-type lookup_account_return() :: {'ok', ne_binary(), knm_number_options:extra_options()} |
                                 {'error', lookup_error()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> knm_number().
new() -> #knm_number{}.

-spec is_number(any()) -> boolean().
is_number(#knm_number{}) -> 'true';
is_number(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc Attempts to get a number from DB.
%% Note: Number parameter has to be normalized.
%% Note: get/1,2 should not throw, instead returns: {ok,_} | {error,_} | ...
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> knm_number_return().
-spec get(ne_binary(), knm_number_options:options()) -> knm_number_return().
get(Num) ->
    get(Num, knm_number_options:default()).

get(Num, Options) ->
    case knm_converters:is_reconcilable(Num) of
        'false' -> {'error', 'not_reconcilable'};
        'true' -> get_number(Num, Options)
    end.

-spec get_number(ne_binary(), knm_number_options:options()) ->
                        knm_number_return().
get_number(Num, Options) ->
    wrap_phone_number_return(
      attempt(fun knm_phone_number:fetch/2, [Num, Options])
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), knm_number_options:options()) ->
                    knm_number_return().
create(Num, Options) ->
    attempt(fun create_or_load/2, [Num, Options]).

-spec create_or_load(ne_binary(), knm_number_options:options()) -> knm_number() |
                                                                   dry_run_return().
create_or_load(Num, Options) ->
    create_or_load(Num, Options, knm_phone_number:fetch(Num)).

-spec create_or_load(ne_binary(), knm_number_options:options(), knm_phone_number_return()) ->
                            knm_number() | dry_run_return().
create_or_load(Num, Options, {'ok', PhoneNumber}) ->
    ensure_can_load_to_create(PhoneNumber),
    Updates = create_updaters(Num, Options),
    {'ok', NewPhoneNumber} = knm_phone_number:setters(PhoneNumber, Updates),
    create_phone_number(set_phone_number(new(), NewPhoneNumber));
create_or_load(Num, Options, {'error', 'not_found'}) ->
    ensure_can_create(Num, Options),
    Updates = create_updaters(Num, Options),
    {'ok', PhoneNumber} = knm_phone_number:setters(knm_phone_number:new(), Updates),
    create_phone_number(set_phone_number(new(), PhoneNumber)).

-spec ensure_can_load_to_create(knm_phone_number:knm_phone_number()) -> 'true'.
ensure_can_load_to_create(PhoneNumber) ->
    ensure_state(PhoneNumber, ?NUMBER_STATE_AVAILABLE).

-spec ensure_state(knm_phone_number:knm_phone_number(), ne_binary()) -> 'true'.
ensure_state(PhoneNumber, ExpectedState) ->
    case knm_phone_number:state(PhoneNumber) of
        ExpectedState -> 'true';
        _State ->
            lager:debug("wrong state: expected ~s, got ~s", [ExpectedState, _State]),
            knm_errors:number_exists(
              knm_phone_number:number(PhoneNumber)
             )
    end.

-spec create_phone_number(knm_number()) -> knm_number() |
                                           dry_run_return().
create_phone_number(Number) ->
    ensure_state(phone_number(Number), ?NUMBER_STATE_AVAILABLE),
    Routines = [fun knm_number_states:to_reserved/1
                ,fun save_number/1
                ,fun dry_run_or_number/1
               ],
    apply_number_routines(Number, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches then transition an existing number to the reserved state.
%% @end
%%--------------------------------------------------------------------
-spec reserve(ne_binary(), knm_number_options:options()) -> knm_number_return().
reserve(Num, Options) ->
    case get(Num, Options) of
        {'ok', Number} ->
            attempt(fun do_reserve/1, [Number]);
        {'error', _R}=E -> E
    end.

-spec do_reserve(knm_number()) -> knm_number_return().
do_reserve(Number) ->
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

-spec dry_run_or_number(knm_number()) -> knm_number() |
                                         dry_run_return().
dry_run_or_number(Number) ->
    case knm_phone_number:dry_run(phone_number(Number)) of
        'false' -> Number;
        'true' ->
            {'dry_run'
            ,services(Number)
            ,knm_services:phone_number_activation_charges(Number)
            }
    end.

-spec ensure_can_create(ne_binary(), knm_number_options:options()) -> 'true'.
ensure_can_create(Num, Options) ->
    ensure_account_can_create(Options)
        andalso ensure_number_is_not_porting(Num, Options).

-spec ensure_account_can_create(knm_number_options:options()) -> 'true'.
ensure_account_can_create(Options) ->
    case knm_number_options:auth_by(Options) of
        'undefined' -> knm_errors:unauthorized();
        AccountId ->
            ensure_account_is_allowed_to_create(Options, AccountId)
    end.

-ifdef(TEST).
-define(LOAD_ACCOUNT(Props, _AccountId)
        ,{'ok', props:get_value(<<"auth_by_account">>, Props)}
       ).
-else.
-define(LOAD_ACCOUNT(_Options, AccountId)
        ,kz_account:fetch(AccountId)
       ).
-endif.

ensure_account_is_allowed_to_create(_Options, _AccountId) ->
    {'ok', JObj} = ?LOAD_ACCOUNT(_Options, _AccountId),
    case kz_account:allow_number_additions(JObj) of
        'true' -> 'true';
        'false' -> knm_errors:unauthorized()
    end.

-spec ensure_number_is_not_porting(ne_binary(), knm_number_options:options()) -> 'true'.
-ifdef(TEST).
ensure_number_is_not_porting(?TEST_CREATE_NUM, _Options) -> 'true';
ensure_number_is_not_porting(?TEST_AVAILABLE_NUM = Num, _Options) ->
    knm_errors:number_is_porting(Num).
-else.
ensure_number_is_not_porting(Num, Options) ->
    JustPorted = knm_number_options:ported_in(Options),
    case JustPorted orelse knm_port_request:get(Num) of
        'true' -> 'true';
        {'ok', _Doc} -> knm_errors:number_is_porting(Num);
        {'error', 'not_found'} -> 'true'
    end.
-endif.

-spec create_updaters(ne_binary(), knm_number_options:options()) ->
                             knm_phone_number:set_functions().
create_updaters(?NE_BINARY=Num, Options) when is_list(Options) ->
    NormalizedNum = knm_converters:normalize(Num),
    props:filter_undefined(
      [{fun knm_phone_number:set_number/2, NormalizedNum}
       ,{fun knm_phone_number:set_number_db/2, knm_converters:to_db(NormalizedNum)}
       ,{fun knm_phone_number:set_state/2
         ,knm_number_options:state(Options, ?NUMBER_STATE_AVAILABLE)
        }
       ,{fun knm_phone_number:set_ported_in/2
         ,knm_number_options:ported_in(Options)
        }
       ,{fun knm_phone_number:set_assign_to/2
         ,knm_number_options:assign_to(Options)
        }
       ,{fun knm_phone_number:set_auth_by/2
         ,knm_number_options:auth_by(Options)
        }
       ,{fun knm_phone_number:set_dry_run/2
         ,knm_number_options:dry_run(Options)
        }
       ,{fun knm_phone_number:set_module_name/2
         ,knm_number_options:module_name(Options)
        }
       ,{fun knm_phone_number:update_doc/2
         ,knm_number_options:public_fields(Options)
        }
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binary(), ne_binary()) -> knm_number_return().
-spec move(ne_binary(), ne_binary(), knm_number_options:options()) -> knm_number_return().
move(Num, MoveTo) ->
    move(Num, MoveTo, knm_number_options:default()).

move(Num, MoveTo, Options) ->
    case get(Num, Options) of
        {'ok', Number} ->
            attempt(fun move_to/2, [Number, MoveTo]);
        {'error', 'not_found'} ->
            PN = knm_phone_number:new(Num, Options),
            Number = set_phone_number(new(), PN),
            attempt(fun move_to/2, [Number, MoveTo]);
        {'error', _R}=E -> E
    end.

-spec move_to(knm_number(), ne_binary()) -> knm_number_return().
move_to(Number, MoveTo) ->
    AccountId = kz_util:format_account_id(MoveTo),
    PhoneNumber = phone_number(Number),
    MovedPhoneNumber = knm_phone_number:set_assign_to(PhoneNumber, AccountId),
    MovedNumber = set_phone_number(Number, MovedPhoneNumber),
    Routines = [fun knm_number_states:to_in_service/1
                ,fun save_number/1
                ,fun dry_run_or_number/1
               ],
    apply_number_routines(MovedNumber, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), knm_phone_number:set_functions()) ->
                    knm_number_return().
-spec update(ne_binary(), knm_phone_number:set_functions(), knm_number_options:options()) ->
                    knm_number_return().
update(Num, Routines) ->
    update(Num, Routines, knm_number_options:default()).

update(Num, Routines, Options) ->
    case get(Num, Options) of
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
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number()) -> knm_number_return().
save(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num =
        case 'undefined' == knm_phone_number:assigned_to(PhoneNumber)
            andalso ?NUMBER_STATE_DISCOVERY == knm_phone_number:state(PhoneNumber)
        of
            'true' ->
                %% Number was created as a result of carrier search
                %%  thus has no services associated with it
                Number;
            'false' ->
                knm_services:update_services(Number)
        end,
    wrap_phone_number_return(
      knm_phone_number:save(phone_number(Num))
      ,Num
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Note: option 'assign_to' needs to be set.
%% @end
%%--------------------------------------------------------------------
-spec reconcile(ne_binary(), knm_number_options:options()) ->
                       knm_number_return().
reconcile(DID, Options) ->
    knm_number_options:assign_to(Options) == 'undefined'
        andalso knm_errors:assign_failure(Options, 'field_undefined'),
    NewOptions = [ {'auth_by', ?KNM_DEFAULT_AUTH_BY}
                   | Options
                 ],
    case ?MODULE:get(DID) of
        {'ok', Number} ->
            reconcile_number(Number, NewOptions);
        {'error', 'not_found'} ->
            AssignTo = knm_number_options:assign_to(Options),
            %% Ensures state to be IN_SERVICE
            move(DID, AssignTo, NewOptions);
        {'error', _}=E -> E
    end.

-spec reconcile_number(knm_number(), knm_number_options:options()) ->
                              knm_number_return().
reconcile_number(Number, Options) ->
    PhoneNumber = phone_number(Number),
    Updaters = [{knm_number_options:assign_to(Options)
                 ,knm_phone_number:assigned_to(PhoneNumber)
                 ,fun knm_phone_number:set_assigned_to/2
                }
                ,{knm_number_options:auth_by(Options)
                  ,knm_phone_number:auth_by(PhoneNumber)
                  ,fun knm_phone_number:set_auth_by/2
                 }
                ,{knm_number_options:public_fields(Options)
                  ,knm_phone_number:doc(PhoneNumber)
                  ,fun knm_phone_number:update_doc/2
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

-spec updates_require_save(knm_phone_number:knm_phone_number(), up_req_els()) ->
                                  up_req_acc().
updates_require_save(PhoneNumber, Updaters) ->
    lists:foldl(fun update_requires_save/2
                ,{'false', PhoneNumber}
                ,Updaters
               ).

-type set_fun() :: fun((knm_phone_number:knm_phone_number(), any()) -> knm_phone_number:knm_phone_number()).

-type up_req_el() :: {ne_binary(), api_binary(), set_fun()}.
-type up_req_els() :: [up_req_el()].
-type up_req_acc() :: {boolean(), knm_phone_number:knm_phone_number()}.

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
-spec release(ne_binary()) ->
                     knm_number_return().
-spec release(ne_binary(), knm_number_options:options()) ->
                     knm_number_return().
release(Num) ->
    release(Num, knm_number_options:default()).

release(Num, Options) ->
    case get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            attempt(fun release_number/2, [Number, Options])
    end.

-spec release_number(knm_number(), knm_number_options:options()) -> knm_number_return().
release_number(Number, Options) ->
    Routines = [fun knm_phone_number:release/1
               ],
    {'ok', PhoneNumber} = knm_phone_number:setters(phone_number(Number), Routines),
    N1 = knm_providers:delete(set_phone_number(Number, PhoneNumber)),
    N = unwind_or_disconnect(N1, Options),
    wrap_phone_number_return(
      knm_phone_number:save(phone_number(N))
      ,N
     ).

-spec unwind_or_disconnect(knm_number(), knm_number_options:options()) -> knm_number().
unwind_or_disconnect(Number, Options) ->
    PhoneNumber = knm_phone_number:unwind_reserve_history(phone_number(Number)),
    N = set_phone_number(Number, PhoneNumber),
    case knm_phone_number:reserve_history(PhoneNumber) of
        [] -> disconnect(N, Options);
        History -> unwind(N, History)
    end.

-spec unwind(knm_phone_number:phone_number(), ne_binaries()) -> knm_number().
unwind(Number, [NewAssignedTo|_]) ->
    Routines = [{fun knm_phone_number:set_assigned_to/2, NewAssignedTo}
                ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
               ],
    {'ok', PhoneNumber} = knm_phone_number:setters(phone_number(Number), Routines),
    set_phone_number(Number, PhoneNumber).

-spec disconnect(knm_number(), knm_number_options:options()) -> knm_number().
disconnect(Number, Options) ->
    ShouldDelete = knm_config:should_permanently_delete(
                     knm_number_options:should_delete(Options)
                    ),
    lager:debug("will delete permanently: ~p", [ShouldDelete]),
    try knm_carriers:disconnect(Number) of
        N when not ShouldDelete -> N;
        N when     ShouldDelete -> delete_phone_number(N)
    catch
        _E:_R when ShouldDelete ->
            ?LOG_WARN("failed to disconnect number: ~s: ~p", [_E, _R]),
            delete_phone_number(Number)
    end.

-spec delete_phone_number(knm_number()) -> knm_number().
delete_phone_number(Number) ->
    knm_number_states:to_deleted(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binary(), api_binary()) ->
                           knm_number_return().
-spec assign_to_app(ne_binary(), api_binary(), knm_number_options:options()) ->
                           knm_number_return().
assign_to_app(Num, App) ->
    assign_to_app(Num, App, knm_number_options:default()).

assign_to_app(Num, App, Options) ->
    case get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            maybe_update_assignment(Number, App)
    end.

-spec maybe_update_assignment(knm_number(), api_binary()) -> knm_number_return().
maybe_update_assignment(Number, NewApp) ->
    PhoneNumber = phone_number(Number),
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
    case kz_cache:peek_local(?CACHE_NAME, Key) of
        {'ok', Ok} -> Ok;
        {'error', 'not_found'} ->
            case fetch_account_from_number(NormalizedNum) of
                {'ok', _, _}=Ok ->
                    NumberDb = knm_converters:to_db(NormalizedNum),
                    CacheProps = [{'origin', [{'db', NumberDb, NormalizedNum}]}],
                    kz_cache:store_local(?CACHE_NAME, Key, Ok, CacheProps),
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
-spec check_number(knm_phone_number:knm_phone_number()) -> lookup_account_return().
check_number(PhoneNumber) ->
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    case kz_util:is_empty(AssignedTo) of
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
-spec check_account(knm_phone_number:knm_phone_number()) -> lookup_account_return().
check_account(PhoneNumber) ->
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    case kz_util:is_account_enabled(AssignedTo) of
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
                     ,{'force_outbound', is_force_outbound(PhoneNumber)}
                    ],
            {'ok', AssignedTo, Props}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_prepend(knm_phone_number:knm_phone_number()) -> api_binary().
feature_prepend(PhoneNumber) ->
    Prepend = knm_phone_number:feature(PhoneNumber, <<"prepend">>),
    case kz_json:is_true(<<"enabled">>, Prepend) of
        'false' -> 'undefined';
        'true' -> kz_json:get_ne_value(<<"name">>, Prepend)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_inbound_cname(knm_phone_number:knm_phone_number()) -> boolean().
feature_inbound_cname(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, <<"inbound_cnam">>) of
        'undefined' -> 'false';
        _ ->
            Mod = knm_phone_number:module_name(PhoneNumber),
            Module = kz_util:to_atom(Mod, 'true'),
            try Module:should_lookup_cnam() of
                Boolean -> kz_util:is_true(Boolean)
            catch
                _E:_R -> 'true'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_early_ringback(knm_phone_number:knm_phone_number()) -> api_binary().
find_early_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, <<"ringback">>),
    kz_json:get_ne_value(<<"early">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:knm_phone_number()) -> api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, <<"ringback">>),
    kz_json:get_ne_value(<<"transfer">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_force_outbound(knm_phone_number:knm_phone_number()) -> boolean().
-spec is_force_outbound(ne_binary(), ne_binary(), boolean()) -> boolean().
is_force_outbound(PhoneNumber) ->
    Module = knm_phone_number:module_name(PhoneNumber),
    State = knm_phone_number:state(PhoneNumber),
    ForceOutbound = force_outbound_feature(PhoneNumber),
    is_force_outbound(State, Module, kz_util:is_true(ForceOutbound)).

is_force_outbound(?NUMBER_STATE_PORT_IN, Module, _ForceOutbound) ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_in_outbound">>, 'true')
        orelse force_module_outbound(Module);
is_force_outbound(?NUMBER_STATE_PORT_OUT, Module, _ForceOutbound) ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_out_outbound">>, 'true')
        orelse force_module_outbound(Module);
is_force_outbound(_State, ?CARRIER_LOCAL, _ForceOutbound) ->
    force_local_outbound();
is_force_outbound(_State, _Module, ForceOutbound) ->
    ForceOutbound.

-spec force_outbound_feature(knm_phone_number:knm_phone_number()) -> boolean().
force_outbound_feature(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, <<"force_outbound">>) of
        'undefined' -> default_force_outbound();
        FO -> kz_util:is_true(FO)
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
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_local_outbound">>, 'true').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_force_outbound() -> boolean().
default_force_outbound() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"default_force_outbound">>, 'false').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_account_from_ports(ne_binary(), {'error', any()}) ->
                                      lookup_account_return().
fetch_account_from_ports(NormalizedNum, Error) ->
    case
        kz_datamgr:get_results(
          ?KZ_PORT_REQUESTS_DB
          ,<<"port_requests/port_in_numbers">>
          ,[{'key', NormalizedNum}]
         )
    of
        {'ok', []} ->
            lager:debug("no port for ~s: ~p", [NormalizedNum, Error]),
            Error;
        {'ok', [Port]} ->
            AccountId = kz_json:get_value(<<"value">>, Port),
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
-spec wrap_phone_number_return(knm_phone_number_return() | knm_phone_number:knm_phone_number()) ->
                                      knm_number_return().
-spec wrap_phone_number_return(knm_phone_number_return() | knm_phone_number:knm_phone_number(), knm_number()) ->
                                      knm_number_return().
wrap_phone_number_return(Result) ->
    wrap_phone_number_return(Result, new()).

wrap_phone_number_return({'error', _}=E, _Number) -> E;
wrap_phone_number_return({'ok', PhoneNumber}, Number) ->
    {'ok', set_phone_number(Number, PhoneNumber)};
wrap_phone_number_return(PhoneNumber, Number) ->
    {'ok', set_phone_number(Number, PhoneNumber)}.

-spec phone_number(knm_number()) -> knm_phone_number:knm_phone_number().
-spec set_phone_number(knm_number(), knm_phone_number:knm_phone_number()) ->
                              knm_number().
phone_number(#knm_number{knm_phone_number=PhoneNumber}) -> PhoneNumber.
set_phone_number(Number, PhoneNumber) ->
    Number#knm_number{knm_phone_number=PhoneNumber}.

-spec services(knm_number()) -> kz_services:services() | 'undefined'.
services(#knm_number{services=Services}) -> Services.

-spec set_services(knm_number(), kz_services:services()) -> knm_number().
set_services(#knm_number{}=Number, Services) ->
    Number#knm_number{services=Services}.

-spec billing_id(knm_number()) -> api_binary().
billing_id(#knm_number{billing_id=BillingId}) ->
    BillingId.

-spec set_billing_id(knm_number(), ne_binary()) -> knm_number().
set_billing_id(#knm_number{}=Number, BillingId) ->
    Number#knm_number{billing_id=BillingId}.

-spec transactions(knm_number()) -> kz_transaction:transactions().
transactions(#knm_number{transactions=Transactions}) -> Transactions.

-spec add_transaction(knm_number(), kz_transaction:transaction()) -> knm_number().
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

-spec to_public_json(knm_number()) -> kz_json:object().
to_public_json(#knm_number{}=Number) ->
    knm_phone_number:to_public_json(phone_number(Number)).

-spec attempt(fun(), list()) -> knm_number_return() |
                                knm_phone_number_return().
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

-spec num_to_did(api_binary() | knm_number() | knm_phone_number:knm_phone_number()) ->
                        api_binary().
num_to_did('undefined') -> 'undefined';
num_to_did(?NE_BINARY = DID) -> DID;
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
