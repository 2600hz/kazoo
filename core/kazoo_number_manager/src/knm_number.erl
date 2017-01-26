%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_number).

-export([new/0, new/1
        ,get/1, get/2
        ,create/2
        ,move/2, move/3
        ,update/2, update/3
        ,release/1, release/2
        ,delete/2
        ,assign_to_app/2, assign_to_app/3
        ,lookup_account/1
        ,reconcile/2
        ,reserve/2
        ]).

-export([phone_number/1, set_phone_number/2
        ,services/1, set_services/2
        ,transactions/1
        ,add_transaction/2
        ,charges/2, set_charges/3
        ,to_public_json/1
        ,is_number/1
        ]).

-export([attempt/2
        ,ensure_can_create/1
        ,ensure_can_load_to_create/1
        ,state_for_create/2
        ]).

-ifdef(TEST).
-export([ensure_can_create/2]).
-endif.

-include_lib("kazoo_json/include/kazoo_json.hrl").
-include("knm.hrl").

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_phone_number()
                    ,services :: kz_services:services()
                    ,transactions = [] :: kz_transaction:transactions()
                    ,charges = [] :: [{ne_binary(), non_neg_integer()}]
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


-define(TRY_CLAUSES(Num),
        {false, #{ok := [Number]}} ->
               {ok, Number};
            {true, T=#{ok := [_Number], services := Services}} ->
               Charges = knm_services:phone_number_activation_charges(T),
               {dry_run, Services, Charges};
            {_, #{ko := ErrorM}} ->
               {error, hd(maps:values(ErrorM))}).

-define(TRY2(F, Num, Options),
        case {knm_number_options:dry_run(Options)
             ,knm_numbers:F([Num], Options)
             }
        of ?TRY_CLAUSES(Num) end).

-define(TRY3(F, Num, Arg2, Options),
        case {knm_number_options:dry_run(Options)
             ,knm_numbers:F([Num], Arg2, Options)
             }
        of ?TRY_CLAUSES(Num) end).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> knm_number().
new() -> #knm_number{}.

-spec new(knm_numbers:collection()) -> knm_numbers:collection();
         (knm_phone_number:knm_phone_number()) -> knm_number().
new(T=#{todo := PNs}) ->
    Numbers = [new(PN) || PN <- PNs],
    knm_numbers:ok(Numbers, T);
new(PN) ->
    set_phone_number(new(), PN).

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
    case knm_numbers:get([Num], Options) of
        #{ok := [Number]} -> {ok, Number};
        #{ko := M} -> {error, hd(maps:values(M))}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to create a new number in DB or modify an existing one.
%% Note: `assign_to' number option MUST be set.
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), knm_number_options:options()) -> knm_number_return().
create(Num, Options) ->
    ?TRY2(create, Num, Options).

-spec state_for_create(ne_binary(), knm_number_options:options()) -> ne_binary().
-ifdef(TEST).
state_for_create(AccountId, Options) ->
    case {knm_number_options:state(Options)
         ,knm_number_options:module_name(Options)
         ,AccountId
         }
    of
        {?NUMBER_STATE_PORT_IN=PortIn, _, _} -> PortIn;
        {_, ?CARRIER_MDN, _} -> ?NUMBER_STATE_IN_SERVICE;
        {_, _, ?MASTER_ACCOUNT_ID} -> ?NUMBER_STATE_AVAILABLE;
        {_, _, ?RESELLER_ACCOUNT_ID} -> ?NUMBER_STATE_RESERVED;
        _ -> ?NUMBER_STATE_IN_SERVICE
    end.
-else.
state_for_create(AccountId, Options) ->
    case knm_number_options:state(Options) of
        ?NUMBER_STATE_PORT_IN=PortIn -> PortIn;
        _ ->
            case ?CARRIER_MDN =:= knm_number_options:module_name(Options) of
                true -> ?NUMBER_STATE_IN_SERVICE;
                _ ->
                    case kz_services:is_reseller(AccountId)
                        andalso kapps_util:get_master_account_id()
                    of
                        'false' -> ?NUMBER_STATE_IN_SERVICE;
                        {'ok', AccountId} -> ?NUMBER_STATE_AVAILABLE;
                        {'ok', _} -> ?NUMBER_STATE_RESERVED
                    end
            end
    end.
-endif.

-spec ensure_can_load_to_create(knm_phone_number:knm_phone_number()) -> 'true';
                               (knm_numbers:collection()) -> knm_numbers:collection().
ensure_can_load_to_create(T0=#{todo := PNs}) ->
    F = fun (PN, T) ->
                case attempt(fun ensure_can_load_to_create/1, [PN]) of
                    true -> knm_numbers:ok(PN, T);
                    {error, R} ->
                        Num = knm_phone_number:number(PN),
                        knm_numbers:ko(Num, R, T)
                end
        end,
    lists:foldl(F, T0, PNs);
ensure_can_load_to_create(PhoneNumber) ->
    ensure_state(PhoneNumber, ?NUMBER_STATE_AVAILABLE).

-spec ensure_state(knm_phone_number:knm_phone_number(), ne_binary()) -> 'true'.
ensure_state(PhoneNumber, ExpectedState) ->
    case knm_phone_number:state(PhoneNumber) of
        ExpectedState -> 'true';
        _State ->
            lager:debug("wrong state: expected ~s, got ~s", [ExpectedState, _State]),
            knm_errors:number_exists(knm_phone_number:number(PhoneNumber))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches then transitions an existing number to the reserved state.
%% @end
%%--------------------------------------------------------------------
-spec reserve(ne_binary(), knm_number_options:options()) -> knm_number_return().
reserve(Num, Options) ->
    ?TRY2(reserve, Num, Options).

-spec ensure_can_create(knm_numbers:collection()) -> knm_numbers:collection().
ensure_can_create(T0=#{todo := Nums, options := Options}) ->
    F = fun (Num, T) ->
                case attempt(fun ensure_can_create/2, [Num, Options]) of
                    {error, R} -> knm_numbers:ko(Num, R, T);
                    true -> knm_numbers:ok(knm_phone_number:new(Num, Options), T)
                end
        end,
    lists:foldl(F, T0, Nums).

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
-define(LOAD_ACCOUNT(Props, _AccountId),
        {'ok', props:get_value(<<"auth_by_account">>, Props)}).
-else.
-define(LOAD_ACCOUNT(_Options, AccountId),
        kz_account:fetch(AccountId)).
-endif.

ensure_account_is_allowed_to_create(_, ?KNM_DEFAULT_AUTH_BY) ->
    lager:info("bypassing auth"),
    'true';
ensure_account_is_allowed_to_create(_Options, _AccountId) ->
    case knm_number_options:state(_Options) of
        ?NUMBER_STATE_PORT_IN -> 'true';
        _Else ->
            {'ok', JObj} = ?LOAD_ACCOUNT(_Options, _AccountId),
            kz_account:allow_number_additions(JObj)
                orelse knm_errors:unauthorized()
    end.

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

move(Num, MoveTo, Options) ->
    ?TRY3(move, Num, MoveTo, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to update some phone_number fields.
%% Note: will always result in a phone_number save.
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), knm_phone_number:set_functions()) -> knm_number_return().
-spec update(ne_binary(), knm_phone_number:set_functions(), knm_number_options:options()) ->
                    knm_number_return().
update(Num, Routines) ->
    update(Num, Routines, knm_number_options:default()).

update(Num, Routines, Options) ->
    ?TRY3(update, Num, Routines, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Note: option 'assign_to' needs to be set.
%% @end
%%--------------------------------------------------------------------
-spec reconcile(ne_binary(), knm_number_options:options()) -> knm_number_return().
reconcile(DID, Options) ->
    ?TRY2(reconcile, DID, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec release(ne_binary()) -> knm_number_return().
-spec release(ne_binary(), knm_number_options:options()) -> knm_number_return().
release(Num) ->
    release(Num, knm_number_options:default()).

release(Num, Options) ->
    ?TRY2(release, Num, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Remove a number from the system without doing any state checking.
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary(), knm_number_options:options()) -> knm_number_return().
delete(Num, Options) ->
    ?TRY2(delete, Num, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binary(), api_ne_binary()) -> knm_number_return().
-spec assign_to_app(ne_binary(), api_ne_binary(), knm_number_options:options()) -> knm_number_return().
assign_to_app(Num, App) ->
    assign_to_app(Num, App, knm_number_options:default()).

assign_to_app(Num, App, Options) ->
    ?TRY3(assign_to_app, Num, App, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup_account(api_binary()) -> lookup_account_return().
lookup_account('undefined') ->
    {'error', 'not_reconcilable'};
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
    case kz_term:is_empty(AssignedTo) of
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
    Prepend = knm_phone_number:feature(PhoneNumber, ?FEATURE_PREPEND),
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
    case knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND) of
        'undefined' -> 'false';
        _ ->
            Mod = knm_phone_number:module_name(PhoneNumber),
            Module = kz_term:to_atom(Mod, 'true'),
            try Module:should_lookup_cnam() of
                Boolean -> kz_term:is_true(Boolean)
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
    kz_json:get_ne_value(<<"early">>, RingBack).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:knm_phone_number()) -> api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
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
    is_force_outbound(State, Module, kz_term:is_true(ForceOutbound)).

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
    case knm_phone_number:feature(PhoneNumber, ?FEATURE_FORCE_OUTBOUND) of
        'undefined' -> default_force_outbound();
        FO -> kz_term:is_true(FO)
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
            Props = [{'force_outbound', 'true'}
                    ,{'pending_port', 'true'}
                    ,{'local', 'true'}
                    ,{'inbound_cnam', 'false'}
                    ,{'number', NormalizedNum}
                    ,{'account_id', AccountId}
                    ],
            {'ok', AccountId, Props}
    end.

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

-spec transactions(knm_number()) -> kz_transaction:transactions().
transactions(#knm_number{transactions=Transactions}) -> Transactions.

-spec add_transaction(knm_number(), kz_transaction:transaction()) -> knm_number().
add_transaction(#knm_number{transactions=Transactions}=Number, Transaction) ->
    Number#knm_number{transactions=[Transaction|Transactions]}.

-spec charges(knm_number(), ne_binary()) -> non_neg_integer().
-spec set_charges(knm_number(), ne_binary(), non_neg_integer()) -> knm_number().
charges(#knm_number{charges=Charges}, Key) ->
    props:get_value(Key, Charges, 0).

set_charges(#knm_number{charges=Charges}=Number, Key, Amount) ->
    Number#knm_number{charges=props:set_value(Key, Amount, Charges)}.

-spec to_public_json(knm_number()) -> kz_json:object().
to_public_json(#knm_number{}=Number) ->
    knm_phone_number:to_public_json(phone_number(Number)).

-spec attempt(fun(), list()) -> knm_number_return() |
                                knm_phone_number_return() |
                                true.
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

-spec num_to_did(api_binary() | knm_number() | knm_phone_number:knm_phone_number()) -> api_ne_binary().
num_to_did('undefined') -> 'undefined';
num_to_did(?NE_BINARY = DID) -> DID;
num_to_did(#knm_number{}=Number) -> num_to_did(phone_number(Number));
num_to_did(PhoneNumber) -> knm_phone_number:number(PhoneNumber).
