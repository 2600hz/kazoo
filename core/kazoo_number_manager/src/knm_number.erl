%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz INC
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
        ,delete/2
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
        ,force_outbound_feature/1
        ]).

-ifdef(TEST).
-export([attempt/2]).
-export([ensure_can_load_to_create/1]).
-export([ensure_can_create/2]).
-export([create_or_load/3]).
-export([update_phone_number/2, update_phone_number/3]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kazoo_json/include/kazoo_json.hrl").
-include("knm.hrl").

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_phone_number()
                    ,services :: kz_services:services()
                    ,billing_id :: api_binary()
                    ,transactions = [] :: kz_transaction:transactions()
                    ,errors = [] :: []
                    ,charges = [] :: [{ne_binary(), integer()}]
                    }).
-opaque knm_number() :: #knm_number{}.
-type knm_numbers() :: [knm_number()].

-export_type([knm_number/0
             ,knm_numbers/0
             ,knm_number_return/0
             ,dry_run_return/0
             ]).

-type dry_run_or_number_return() :: knm_number() | dry_run_return().

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
%% @doc
%% Attempts to get a number from DB.
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
        'true' ->
            wrap_phone_number_return(
              attempt(fun knm_phone_number:fetch/2, [Num, Options])
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to create a new number in DB or modify an existing one.
%% Note: `assign_to' number option MUST be set.
%% Note: creating numbers with `ported_in` option set to true will
%%   attempt to create them with state in_service.
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), knm_number_options:options()) ->
                    knm_number_return().
create(Num, Options) ->
    ?MATCH_ACCOUNT_RAW(_) = knm_number_options:assign_to(Options),
    case knm_converters:is_reconcilable(Num) of
        'false' -> {'error', knm_errors:to_json('not_reconcilable', Num)};
        'true' ->
            attempt(fun do_create/2, [Num, Options])
    end.

-spec do_create(ne_binary(), knm_number_options:options()) -> dry_run_or_number_return().
do_create(Num, Options) ->
    case knm_number_options:assign_to(Options) of
        ?MATCH_ACCOUNT_RAW(_) -> create_or_load(Num, Options);
        _ -> knm_errors:assign_failure(Num, assign_to)
    end.

-spec create_or_load(ne_binary(), knm_number_options:options()) -> dry_run_or_number_return().
create_or_load(Num, Options0) ->
    ToState = state_for_create(Options0),
    lager:debug("picked ~s state ~s for ~s", [Num, ToState, knm_number_options:assign_to(Options0)]),
    Options = [{'state', ToState} | Options0],
    create_or_load(Num, Options, knm_phone_number:fetch(Num)).

-spec state_for_create(knm_number_options:options()) -> ne_binary().
state_for_create(Options) ->
    case {knm_number_options:state(Options, ?NUMBER_STATE_RESERVED)
         ,knm_number_options:ported_in(Options)
         ,knm_number_options:module_name(Options)
         }
    of
        {?NUMBER_STATE_PORT_IN=PortIn, _, _} -> PortIn;
        {_, true, _} -> ?NUMBER_STATE_IN_SERVICE;
        {_, _, ?CARRIER_MDN} -> ?NUMBER_STATE_IN_SERVICE;
        {State, _, _} ->
            true = lists:member(State, allowed_creation_states(knm_number_options:auth_by(Options))),
            State
    end.

-spec allowed_creation_states(api_ne_binary()) -> ne_binaries().
allowed_creation_states(undefined) -> [];
allowed_creation_states(AuthBy) ->
    %% Note: AuthBy can be ?KNM_DEFAULT_AUTH_BY
    case knm_phone_number:is_admin(AuthBy) of
        true ->
            [?NUMBER_STATE_AGING
            ,?NUMBER_STATE_AVAILABLE
            ,?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_PORT_IN
            ,?NUMBER_STATE_RESERVED
            ];
        false ->
            case is_reseller(AuthBy) of
                false -> [?NUMBER_STATE_RESERVED];
                true ->
                    [?NUMBER_STATE_RESERVED
                    ,?NUMBER_STATE_IN_SERVICE
                    ]
            end
    end.

-ifdef(TEST).
is_reseller(?MASTER_ACCOUNT_ID) -> true;
is_reseller(?RESELLER_ACCOUNT_ID) -> true;
is_reseller(?MATCH_ACCOUNT_RAW(_)) -> false.
-else.
is_reseller(?MATCH_ACCOUNT_RAW(AccountId)) ->
    kz_services:is_reseller(AccountId).
-endif.

-spec create_or_load(ne_binary(), knm_number_options:options(), knm_phone_number_return()) ->
                            dry_run_or_number_return().
ifdef(TEST).
-define(OPTIONS_FOR_LOAD(Num, Options),
        case knm_number_options:ported_in(Options) of
            false -> Options;
            true ->
                case Num of
                    ?TEST_PORT_IN2_NUM -> [{module_name, <<"knm_telnyx">>}|Options];
                    ?TEST_AVAILABLE_NUM -> [{module_name, <<"knm_bandwidth2">>}|Options];
                    _ -> [{module_name, ?PORT_IN_MODULE_NAME}|Options]
                end
        end).
-else.
-define(OPTIONS_FOR_LOAD(_Num, Options),
        case knm_number_options:ported_in(Options) of
            false -> Options;
            true -> [{module_name, ?PORT_IN_MODULE_NAME}|Options]
        end).
-endif.

create_or_load(_Num, Options, {'ok', PN}) ->
    ensure_can_load_to_create(PN),
    Updates = knm_number_options:to_phone_number_setters(
                ?OPTIONS_FOR_LOAD(_Num, props:delete(state, Options))
               ),
    {'ok', NewPN} = knm_phone_number:setters(PN, Updates),
    create_phone_number(Options, set_phone_number(new(), NewPN));
create_or_load(Num, Options, {'error', 'not_found'}) ->
    ensure_can_create(Num, Options),
    PhoneNumber = knm_phone_number:from_number_with_options(Num, Options),
    create_phone_number(Options, set_phone_number(new(), PhoneNumber)).

-spec ensure_can_load_to_create(knm_phone_number:knm_phone_number()) -> 'true'.
ensure_can_load_to_create(PhoneNumber) ->
    ensure_state(PhoneNumber, [?NUMBER_STATE_AVAILABLE
                              ,?NUMBER_STATE_PORT_IN
                              ]).

-spec ensure_state(knm_phone_number:knm_phone_number(), ne_binaries()) -> 'true'.
ensure_state(PhoneNumber, AllowedStates) ->
    State = knm_phone_number:state(PhoneNumber),
    case lists:member(State, AllowedStates) of
        true -> true;
        false ->
            Num = knm_phone_number:number(PhoneNumber),
            lager:error("~s wrong state ~s, expected one of ~p", [Num, State, AllowedStates]),
            knm_errors:number_exists(Num)
    end.

-spec create_phone_number(knm_number_options:options(), knm_number()) ->
                                 dry_run_or_number_return().
create_phone_number(Options, Number) ->
    Number1 = maybe_set_ported_in(Options, Number),
    TargetState = knm_number_options:state(Options),
    Routines = [fun (N) -> knm_number_states:to_state(N, TargetState) end
               ,fun save_number/1
               ],
    apply_number_routines(Number1, Routines).

maybe_set_ported_in(Options, N) ->
    case knm_number_options:ported_in(Options) of
        false -> N;
        true ->
            Routines = [{fun knm_phone_number:set_ported_in/2, true}],
            {ok, NewPN} = knm_phone_number:setters(phone_number(N), Routines),
            set_phone_number(N, NewPN)
    end.

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
    Routines = [fun fail_if_assign_to_is_not_an_account_id/1
               ,fun knm_number_states:to_reserved/1
               ,fun save_number/1
               ],
    apply_number_routines(Number, Routines).

-spec fail_if_assign_to_is_not_an_account_id(knm_number()) -> knm_number().
fail_if_assign_to_is_not_an_account_id(N) ->
    PN = phone_number(N),
    case knm_phone_number:assign_to(PN) of
        ?MATCH_ACCOUNT_RAW(_) -> N;
        _ -> knm_errors:assign_failure(PN, assign_to)
    end.

-spec save_number(knm_number()) -> knm_number().
save_number(Number) ->
    Routines = [fun knm_providers:save/1
               ,fun save_phone_number/1
               ,fun knm_services:update_services/1
               ,fun dry_run_or_number/1
               ],
    apply_number_routines(Number, Routines).

-spec save_phone_number(knm_number()) -> knm_number().
save_phone_number(Number) ->
    set_phone_number(Number, knm_phone_number:save(phone_number(Number))).

-spec save_wrap_phone_number(knm_phone_number:knm_phone_number(), knm_number()) -> knm_number_return().
save_wrap_phone_number(PhoneNumber, Number) ->
    wrap_phone_number_return(knm_phone_number:save(PhoneNumber), Number).

-spec dry_run_or_number(knm_number()) -> dry_run_or_number_return().
dry_run_or_number(Number) ->
    case knm_phone_number:dry_run(phone_number(Number)) of
        'false' -> Number;
        'true' ->
            Charges = knm_services:phone_number_activation_charges(Number),
            {'dry_run', services(Number), Charges}
    end.

-spec ensure_can_create(ne_binary(), knm_number_options:options()) -> 'true'.
ensure_can_create(Num, Options) ->
    ensure_account_can_create(Options, knm_number_options:auth_by(Options))
        andalso ensure_number_is_not_porting(Num, Options).

-ifdef(TEST).
-define(LOAD_ACCOUNT(Options, _AccountId),
        {'ok', props:get_value(<<"auth_by_account">>, Options)}).
-else.
-define(LOAD_ACCOUNT(_Options, AccountId),
        kz_account:fetch(AccountId)).
-endif.

ensure_account_can_create(_, ?KNM_DEFAULT_AUTH_BY) ->
    lager:info("bypassing auth"),
    'true';
ensure_account_can_create(Options, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    knm_number_options:ported_in(Options)
        orelse knm_number_options:state(Options) =:= ?NUMBER_STATE_PORT_IN
        orelse begin
                   {'ok', JObj} = ?LOAD_ACCOUNT(Options, AccountId),
                   kz_account:allow_number_additions(JObj)
               end
        orelse knm_phone_number:is_admin(AccountId)
        orelse knm_errors:unauthorized();
ensure_account_can_create(_, _NotAnAccountId) ->
    ?LOG_DEBUG("'~p' is not an account id", [_NotAnAccountId]),
    knm_errors:unauthorized().

-spec ensure_number_is_not_porting(ne_binary(), knm_number_options:options()) -> 'true'.
-ifdef(TEST).
ensure_number_is_not_porting(?TEST_CREATE_NUM, _Options) -> 'true';
ensure_number_is_not_porting(?TEST_AVAILABLE_NUM = Num, _Options) ->
    knm_errors:number_is_porting(Num).
-else.
ensure_number_is_not_porting(Num, Options) ->
    JustPorted = knm_number_options:ported_in(Options),
    case JustPorted
        orelse knm_port_request:get(Num)
    of
        'true' -> 'true';
        {'ok', _Doc} -> knm_errors:number_is_porting(Num);
        {'error', 'not_found'} -> 'true'
    end.
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binary(), ne_binary()) -> knm_number_return().
-spec move(ne_binary(), ne_binary(), knm_number_options:options()) -> knm_number_return().
move(Num, MoveTo) ->
    move(Num, MoveTo, knm_number_options:default()).

move(Num, ?MATCH_ACCOUNT_RAW(MoveTo), Options0) ->
    Options = [{'assign_to', MoveTo} | Options0],
    Updates = knm_number_options:to_phone_number_setters(Options),
    case get(Num, Options) of
        {'ok', Number} ->
            {'ok', PN} = knm_phone_number:setters(phone_number(Number), Updates),
            attempt(fun move_to/1, [set_phone_number(Number, PN)]);
        {'error', 'not_found'} -> maybe_from_discovery(Num, Options);
        {'error', _R}=E -> E
    end.


-spec maybe_from_discovery(ne_binary(), knm_number_options:options()) -> knm_number_return().
maybe_from_discovery(Num, Options) ->
    case knm_search:discovery(Num, Options) of
        {'ok', Number} -> attempt(fun move_to/1, [Number]);
        {'error', _R}=E -> E
    end.

-spec move_to(knm_number()) -> knm_number_return().
move_to(Number) ->
    Routines = [fun knm_number_states:to_in_service/1
               ,fun save_number/1
               ],
    apply_number_routines(Number, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to update some phone_number fields.
%% Note: will always result in a phone_number save.
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
            attempt(fun update_phone_number0/3, [Number, Routines, []])
    end.

-ifdef(TEST).
-spec update_phone_number(knm_number(), knm_phone_number:set_functions()) ->
                                 knm_number_return().
-spec update_phone_number(knm_number(), knm_phone_number:set_functions(), knm_number_options:options()) ->
                                 knm_number_return().
update_phone_number(Number, Routines) ->
    update_phone_number(Number, Routines, []).
update_phone_number(N, Routines, Options) ->
    Updates = [{fun knm_phone_number:set_is_dirty/2, false}] ++ Routines,
    update_phone_number0(N, Updates, Options).
-endif.

update_phone_number0(Number, Routines, Options) ->
    Fix = knm_number_options:to_phone_number_setters(Options),
    PhoneNumber = phone_number(Number),
    case knm_phone_number:setters(PhoneNumber, Fix++Routines) of
        {'error', _R}=Error -> Error;
        {'ok', NewPN} ->
            {'ok', save_number(set_phone_number(Number, NewPN))}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number()) -> knm_number_return().
save(Number) ->
    N =
        case is_carrier_search_result(Number) of
            'false' -> knm_services:update_services(Number);
            'true' ->
                %% Number was created as a result of carrier search
                %%  thus has no services associated with it
                Number
        end,
    PN = knm_phone_number:save(phone_number(N)),
    wrap_phone_number_return(PN, N).

%% @private
-spec is_carrier_search_result(knm_number()) -> boolean().
is_carrier_search_result(Number) ->
    PhoneNumber = phone_number(Number),
    SearchableStates = [?NUMBER_STATE_DISCOVERY
                       ,?NUMBER_STATE_AVAILABLE
                       ,?NUMBER_STATE_RESERVED
                       ],
    'undefined' == knm_phone_number:assigned_to(PhoneNumber)
        andalso lists:member(knm_phone_number:state(PhoneNumber), SearchableStates).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Note: option 'assign_to' needs to be set.
%% @end
%%--------------------------------------------------------------------
-spec reconcile(ne_binary(), knm_number_options:options()) -> knm_number_return().
reconcile(DID, Options) ->
    knm_number_options:assign_to(Options) == 'undefined'
        andalso knm_errors:assign_failure(Options, 'field_undefined'),
    NewOptions = [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
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

-spec reconcile_number(knm_number(), knm_number_options:options()) -> knm_number_return().
reconcile_number(Number, Options) ->
    PhoneNumber = phone_number(Number),
    Updaters = case props:is_defined(module_name, Options) of
                   false -> [];
                   true ->
                       [{knm_number_options:module_name(Options)
                        ,knm_phone_number:module_name(PhoneNumber)
                        ,fun knm_phone_number:set_module_name/2
                        }]
               end ++
        [{knm_number_options:assign_to(Options)
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
        {'false', _PhoneNumber} -> {'ok', Number};
        {'true', UpdatedPhoneNumber} ->
            save_wrap_phone_number(UpdatedPhoneNumber, Number)
    end.

-spec updates_require_save(knm_phone_number:knm_phone_number(), up_req_els()) -> up_req_acc().
updates_require_save(PhoneNumber, Updaters) ->
    Acc0 = {'false', PhoneNumber},
    lists:foldl(fun update_requires_save/2, Acc0, Updaters).

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
            attempt(fun release_number/1, [Number])
    end.

-spec release_number(knm_number()) -> knm_number_return().
release_number(Number) ->
    Routines = [fun knm_phone_number:release/1
               ],
    {'ok', PhoneNumber} = knm_phone_number:setters(phone_number(Number), Routines),
    N1 = knm_providers:delete(set_phone_number(Number, PhoneNumber)),
    N2 = unwind_or_disconnect(N1),
    NewPN = phone_number(N2),
    case ?NUMBER_STATE_DELETED =:= knm_phone_number:state(NewPN) of
        true -> {ok, N2};
        false -> save_wrap_phone_number(NewPN, N2)
    end.

-spec unwind_or_disconnect(knm_number()) -> knm_number().
unwind_or_disconnect(Number) ->
    PhoneNumber = knm_phone_number:unwind_reserve_history(phone_number(Number)),
    N = set_phone_number(Number, PhoneNumber),
    case knm_phone_number:reserve_history(PhoneNumber) of
        [] -> disconnect(N);
        History -> unwind(N, History)
    end.

-spec unwind(knm_phone_number:phone_number(), ne_binaries()) -> knm_number().
unwind(Number, [NewAssignedTo|_]) ->
    Routines = [{fun knm_phone_number:set_assigned_to/2, NewAssignedTo}
               ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}
               ],
    {'ok', PhoneNumber} = knm_phone_number:setters(phone_number(Number), Routines),
    set_phone_number(Number, PhoneNumber).

-spec disconnect(knm_number()) -> knm_number().
disconnect(Number) ->
    ModuleName = knm_phone_number:module_name(phone_number(Number)),
    ShouldDelete = knm_config:should_permanently_delete()
        orelse ?CARRIER_LOCAL =:= ModuleName
        orelse ?CARRIER_MDN =:= ModuleName,
    try knm_carriers:disconnect(Number) of
        N when ShouldDelete -> delete_phone_number(N);
        N -> maybe_age(N)
    catch
        _E:_R when ShouldDelete ->
            ?LOG_WARN("failed to disconnect number: ~s: ~p", [_E, _R]),
            delete_phone_number(Number)
    end.

-spec delete_phone_number(knm_number()) -> knm_number().
delete_phone_number(Number) ->
    PN = knm_phone_number:delete(phone_number(Number)),
    set_phone_number(Number, PN).

-spec maybe_age(knm_number()) -> knm_number().
maybe_age(Number) ->
    case knm_config:should_age()
        andalso knm_phone_number:state(phone_number(Number))
    of
        ?NUMBER_STATE_AVAILABLE ->
            lager:debug("aging available number for some time"),
            knm_number_states:to_aging(Number);
        _ -> Number
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Remove a number from the system without doing any checking.
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary(), knm_number_options:options()) -> knm_number_return().
delete(Num, Options) ->
    case get(Num, Options) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            AuthBy = knm_number_options:auth_by(Options),
            attempt(fun delete_number/2, [Number, AuthBy])
    end.

-spec delete_number(knm_number(), ne_binary()) -> knm_number_return().
delete_number(Number, AuthBy) ->
    case knm_phone_number:is_admin(AuthBy) of
        'false' -> knm_errors:unauthorized();
        'true' ->
            N = knm_providers:delete(Number),
            PN = knm_phone_number:delete(phone_number(N)),
            wrap_phone_number_return(PN, N)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binary(), api_binary()) -> knm_number_return().
-spec assign_to_app(ne_binary(), api_binary(), knm_number_options:options()) -> knm_number_return().
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
            lager:debug("assigning ~s to ~s", [knm_phone_number:number(PhoneNumber), NewApp]),
            save_wrap_phone_number(knm_phone_number:set_used_by(PhoneNumber, NewApp)
                                  ,Number
                                  )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup_account(api_ne_binary()) -> lookup_account_return().
-ifdef(TEST).
lookup_account(undefined) -> {error, not_reconcilable};
lookup_account(Num) ->
    %%FIXME: use knm_converters:is_reconcilable/1
    NormalizedNum = knm_converters:normalize(Num),
    fetch_account_from_number(NormalizedNum).
-else.
lookup_account(undefined) -> {error, not_reconcilable};
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
-endif.

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
        {'error', _}=Error -> maybe_fetch_account_from_ports(NormalizedNum, Error);
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
            States = [?NUMBER_STATE_PORT_IN
                     ,?NUMBER_STATE_IN_SERVICE
                     ,?NUMBER_STATE_PORT_OUT
                     ,?NUMBER_STATE_RESERVED
                     ],
            case lists:member(knm_phone_number:state(PhoneNumber), States) of
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
    case is_account_enabled(AssignedTo) of
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

-ifdef(TEST).
is_account_enabled(?MATCH_ACCOUNT_RAW(_)) -> true.
-else.
is_account_enabled(AccountId) -> kz_util:is_account_enabled(AccountId).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_prepend(knm_phone_number:knm_phone_number()) -> api_binary().
feature_prepend(PhoneNumber) ->
    Prepend = knm_phone_number:feature(PhoneNumber, ?FEATURE_PREPEND),
    case kz_json:is_true(?PREPEND_ENABLED, Prepend) of
        'false' -> 'undefined';
        'true' -> kz_json:get_ne_value(?PREPEND_NAME, Prepend)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature_inbound_cname(knm_phone_number:knm_phone_number()) -> boolean().
feature_inbound_cname(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND) of
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
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_EARLY, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:knm_phone_number()) -> api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_TRANSFER, RingBack).

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
is_force_outbound(_State, ?CARRIER_MDN, _ForceOutbound) ->
    force_local_outbound();
is_force_outbound(_State, _Module, ForceOutbound) ->
    ForceOutbound.

%% FIXME: move to kpn
-spec force_outbound_feature(knm_phone_number:knm_phone_number()) -> boolean().
force_outbound_feature(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, ?FEATURE_FORCE_OUTBOUND) of
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
force_module_outbound(?CARRIER_MDN) -> force_local_outbound();
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
-spec maybe_fetch_account_from_ports(ne_binary(), {'error', any()}) ->
                                            lookup_account_return().
maybe_fetch_account_from_ports(NormalizedNum, Error) ->
    case kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"fetch_account_from_ports">>, 'true') of
        'true' -> fetch_account_from_ports(NormalizedNum, Error);
        'false' -> Error
    end.

-spec fetch_account_from_ports(ne_binary(), {'error', any()}) ->
                                      lookup_account_return().
fetch_account_from_ports(NormalizedNum, Error) ->
    case knm_port_request:get(NormalizedNum) of
        {'error', _E} -> Error;
        {'ok', Port} ->
            AccountId = kz_doc:account_id(Port),
            Props = [{'pending_port', 'true'}
                    ,{'local', 'true'}
                    ,{'number', NormalizedNum}
                    ,{'account_id', AccountId}
                     %% No prepend
                    ,{'inbound_cnam', 'false'}
                     %% No ringback_media
                     %% No transfer_media
                    ,{'force_outbound', 'true'}
                    ],
            {'ok', AccountId, Props}
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

wrap_phone_number_return({'error', _R}=E, #knm_number{knm_phone_number = _PhoneNumber})
  when _PhoneNumber /= 'undefined' ->
    lager:debug("number ~s (~s) error: ~p"
               ,[knm_phone_number:number(_PhoneNumber), knm_phone_number:state(_PhoneNumber), _R]),
    E;
wrap_phone_number_return({'error', _R}=E, _) ->
    lager:debug("number error: ~p", [_R]),
    E;
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

-type number_routine() :: fun((knm_number()) -> dry_run_or_number_return()).
-type number_routines() :: [number_routine()].
-spec apply_number_routines(knm_number(), number_routines()) ->
                                   dry_run_or_number_return().
apply_number_routines(Number, Routines) ->
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines).
