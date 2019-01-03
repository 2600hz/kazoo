%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
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
        ,force_outbound_feature/1
        ]).

-export([attempt/2
        ,ensure_can_create/1
        ,ensure_can_load_to_create/1
        ,state_for_create/1, allowed_creation_states/1, allowed_creation_states/2
        ]).

-ifdef(TEST).
-export([ensure_can_create/2]).
-export([is_force_outbound/1]).
-endif.

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include("knm.hrl").

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_phone_number() | 'undefined'
                    ,services :: kz_services:services() | 'undefined'
                    ,transactions = [] :: kz_transaction:transactions()
                    ,charges = [] :: [{kz_term:ne_binary(), non_neg_integer()}]
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
                        {'not_in_service', kz_term:ne_binary()} |
                        {'account_disabled', kz_term:ne_binary()}.

-type lookup_account_return() :: {'ok', kz_term:ne_binary(), knm_number_options:extra_options()} |
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


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Attempts to get a number from DB.
%%
%% <div class="notice">Number parameter has to be normalized.</div>
%%
%% <div class="notice">{@link get/1}, {@link get/2} should not throw,
%% instead they should return: `{ok,_} | {error,_} | ...'.</div>
%% @end
%%------------------------------------------------------------------------------

-spec get(kz_term:ne_binary()) -> knm_number_return().
get(Num) ->
    get(Num, knm_number_options:default()).

-spec get(kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
get(Num, Options) ->
    case knm_numbers:get([Num], Options) of
        #{ok := [Number]} -> {ok, Number};
        #{ko := M} -> {error, hd(maps:values(M))}
    end.

%%------------------------------------------------------------------------------
%% @doc Attempts to create a new number in DB or modify an existing one.
%%
%% <div class="notice">`assign_to' number option MUST be set.</div>
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
create(Num, Options) ->
    ?TRY2(create, Num, Options).

-spec state_for_create(knm_number_options:options()) -> kz_term:ne_binary().
state_for_create(Options) ->
    case {knm_number_options:state(Options, ?NUMBER_STATE_IN_SERVICE)
         ,knm_number_options:ported_in(Options)
         ,knm_number_options:module_name(Options)
         }
    of
        {?NUMBER_STATE_PORT_IN=PortIn, _, _} -> PortIn;
        {_, true, _} -> ?NUMBER_STATE_IN_SERVICE;
        {_, _, ?CARRIER_MDN} -> ?NUMBER_STATE_IN_SERVICE;
        {State, _, _} ->
            AuthBy = knm_number_options:auth_by(Options),
            lists:member(State, allowed_creation_states(Options, AuthBy))
                orelse knm_errors:unauthorized(),
            ?LOG_DEBUG("allowing picking state ~s for ~s", [State, AuthBy]),
            State
    end.

-spec allowed_creation_states(kz_term:api_ne_binary()) -> kz_term:ne_binaries().
allowed_creation_states(AuthBy) ->
    allowed_creation_states([], AuthBy).

-spec allowed_creation_states(knm_number_options:options(), kz_term:api_ne_binary()) -> kz_term:ne_binaries().
allowed_creation_states(_, undefined) -> [];
allowed_creation_states(Options, AuthBy) ->
    case {knm_phone_number:is_admin(AuthBy)
         ,allow_number_additions(Options, AuthBy)
         }
    of
        {true, _} ->
            [?NUMBER_STATE_AGING
            ,?NUMBER_STATE_AVAILABLE
            ,?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_PORT_IN
            ,?NUMBER_STATE_RESERVED
            ];
        {false, true} ->
            [?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_RESERVED
            ];
        _ -> []
    end.

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
ensure_can_load_to_create(PN) ->
    ensure_state(PN, [?NUMBER_STATE_AVAILABLE
                     ,?NUMBER_STATE_PORT_IN
                     ]).

-spec ensure_state(knm_phone_number:knm_phone_number(), kz_term:ne_binaries()) -> true.
ensure_state(PN, AllowedStates) ->
    State = knm_phone_number:state(PN),
    case lists:member(State, AllowedStates) of
        true -> true;
        false ->
            Num = knm_phone_number:number(PN),
            lager:error("~s wrong state ~s, expected one of ~p", [Num, State, AllowedStates]),
            knm_errors:number_exists(Num)
    end.

%%------------------------------------------------------------------------------
%% @doc Fetches then transitions an existing number to the reserved state.
%% @end
%%------------------------------------------------------------------------------
-spec reserve(kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
reserve(Num, Options) ->
    ?TRY2(reserve, Num, Options).

-spec ensure_can_create(knm_numbers:collection()) -> knm_numbers:collection().
ensure_can_create(T0=#{todo := Nums, options := Options}) ->
    F = fun (Num, T) ->
                case attempt(fun ensure_can_create/2, [Num, Options]) of
                    {error, R} -> knm_numbers:ko(Num, R, T);
                    true ->
                        PN = knm_phone_number:from_number_with_options(Num, Options),
                        knm_numbers:ok(PN, T)
                end
        end,
    lists:foldl(F, T0, Nums).

-spec ensure_can_create(kz_term:ne_binary(), knm_number_options:options()) -> 'true'.
ensure_can_create(Num, Options) ->
    ensure_account_can_create(Options, knm_number_options:auth_by(Options))
        andalso ensure_number_is_not_porting(Num, Options).

-ifdef(TEST).
-define(LOAD_ACCOUNT(Options, _AccountId)
       ,{'ok', props:get_value(<<"auth_by_account">>, Options)}
       ).
-else.
-define(LOAD_ACCOUNT(_Options, AccountId)
       ,kzd_accounts:fetch(AccountId)
       ).
-endif.

-spec allow_number_additions(knm_number_options:options(), kz_term:ne_binary()) -> boolean().
allow_number_additions(_Options, ?KNM_DEFAULT_AUTH_BY) ->
    'true';
allow_number_additions(_Options, _AccountId) ->
    {'ok', JObj} = ?LOAD_ACCOUNT(_Options, _AccountId),
    kzd_accounts:allow_number_additions(JObj).

ensure_account_can_create(_, ?KNM_DEFAULT_AUTH_BY) ->
    lager:info("bypassing auth"),
    'true';
ensure_account_can_create(Options, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    knm_number_options:ported_in(Options)
        orelse knm_number_options:state(Options) =:= ?NUMBER_STATE_PORT_IN
        orelse allow_number_additions(Options, AccountId)
        orelse knm_phone_number:is_admin(AccountId)
        orelse knm_errors:unauthorized();
ensure_account_can_create(_, _NotAnAccountId) ->
    ?LOG_DEBUG("'~p' is not an account id", [_NotAnAccountId]),
    knm_errors:unauthorized().

-spec ensure_number_is_not_porting(kz_term:ne_binary(), knm_number_options:options()) -> 'true'.
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
        {'error', 'not_found'} -> 'true';
        {'error', _} -> 'true'
    end.
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec move(kz_term:ne_binary(), kz_term:ne_binary()) -> knm_number_return().
move(Num, MoveTo) ->
    move(Num, MoveTo, knm_number_options:default()).

-spec move(kz_term:ne_binary(), kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
move(Num, MoveTo, Options) ->
    ?TRY3(move, Num, MoveTo, Options).

%%------------------------------------------------------------------------------
%% @doc Attempts to update some phone_number fields.
%%
%% <div class="notice">will always result in a phone_number save.</div>
%% @end
%%------------------------------------------------------------------------------

-spec update(kz_term:ne_binary(), knm_phone_number:set_functions()) -> knm_number_return().
update(Num, Routines) ->
    update(Num, Routines, knm_number_options:default()).

-spec update(kz_term:ne_binary(), knm_phone_number:set_functions(), knm_number_options:options()) ->
                    knm_number_return().
update(Num, Routines, Options) ->
    ?TRY3(update, Num, Routines, Options).

%%------------------------------------------------------------------------------
%% @doc Note: option 'assign_to' needs to be set.
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
reconcile(DID, Options) ->
    ?TRY2(reconcile, DID, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec release(kz_term:ne_binary()) -> knm_number_return().
release(Num) ->
    release(Num, knm_number_options:default()).

-spec release(kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
release(Num, Options) ->
    ?TRY2(release, Num, Options).

%%------------------------------------------------------------------------------
%% @doc Remove a number from the system without doing any state checking.
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_term:ne_binary(), knm_number_options:options()) -> knm_number_return().
delete(Num, Options) ->
    ?TRY2(delete, Num, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec assign_to_app(kz_term:ne_binary(), kz_term:api_ne_binary()) -> knm_number_return().
assign_to_app(Num, App) ->
    assign_to_app(Num, App, knm_number_options:default()).

-spec assign_to_app(kz_term:ne_binary(), kz_term:api_ne_binary(), knm_number_options:options()) -> knm_number_return().
assign_to_app(Num, App, Options) ->
    ?TRY3(assign_to_app, Num, App, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec lookup_account(kz_term:api_ne_binary()) -> lookup_account_return().
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
    Key = {account_lookup, NormalizedNum},
    case kz_cache:peek_local(?CACHE_NAME, Key) of
        {ok, Ok} -> Ok;
        {error, not_found} ->
            case fetch_account_from_number(NormalizedNum) of
                {ok, _, _}=Ok ->
                    NumberDb = knm_converters:to_db(NormalizedNum),
                    CacheProps = [{origin, [{db, NumberDb, NormalizedNum}]}],
                    kz_cache:store_local(?CACHE_NAME, Key, Ok, CacheProps),
                    Ok;
                Else -> Else
            end
    end.
-endif.

fetch_account_from_number(Num) ->
    case knm_phone_number:fetch(Num) of
        {ok, PN} -> check_number(PN);
        {error, _}=Error -> maybe_fetch_account_from_ports(Num, Error)
    end.

check_number(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case kz_term:is_empty(AssignedTo) of
        true -> {error, unassigned};
        false ->
            States = [?NUMBER_STATE_PORT_IN
                     ,?NUMBER_STATE_IN_SERVICE
                     ,?NUMBER_STATE_PORT_OUT
                     ,?NUMBER_STATE_RESERVED
                     ],
            case lists:member(knm_phone_number:state(PN), States) of
                true -> check_account(PN);
                false -> {error, {not_in_service, AssignedTo}}
            end
    end.

-ifdef(TEST).
is_account_enabled(?MATCH_ACCOUNT_RAW(_)) -> true.
-else.
is_account_enabled(AccountId) -> kz_util:is_account_enabled(AccountId).
-endif.

check_account(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case is_account_enabled(AssignedTo) of
        false -> {error, {account_disabled, AssignedTo}};
        true ->
            Props = [{pending_port, knm_phone_number:state(PN) =:= ?NUMBER_STATE_PORT_IN}
                    ,{local, knm_phone_number:module_name(PN) =:= ?CARRIER_LOCAL}
                    ,{number, knm_phone_number:number(PN)}
                    ,{account_id, AssignedTo}
                    ,{prepend, feature_prepend(PN)}
                    ,{inbound_cnam, feature_inbound_cname(PN)}
                    ,{ringback_media, find_early_ringback(PN)}
                    ,{transfer_media, find_transfer_ringback(PN)}
                    ,{force_outbound, is_force_outbound(PN)}
                    ],
            {ok, AssignedTo, Props}
    end.

-spec maybe_fetch_account_from_ports(kz_term:ne_binary(), {error, any()}) -> lookup_account_return().
maybe_fetch_account_from_ports(Num, Error) ->
    case knm_config:should_fetch_account_from_ports() of
        'false' -> Error;
        'true' -> fetch_account_from_ports(Num, Error)
    end.

-spec fetch_account_from_ports(kz_term:ne_binary(), {'error', any()}) -> lookup_account_return().
fetch_account_from_ports(Num, Error) ->
    case knm_port_request:get(Num) of
        {error, _E} -> Error;
        {ok, Port} ->
            AccountId = kz_doc:account_id(Port),
            Props = [{pending_port, true}
                    ,{local, true}
                    ,{number, Num}
                    ,{account_id, AccountId}
                     %% No prepend
                    ,{inbound_cnam, false}
                     %% No ringback_media
                     %% No transfer_media
                    ,{force_outbound, true}
                    ],
            {ok, AccountId, Props}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature_prepend(knm_phone_number:knm_phone_number()) -> kz_term:api_binary().
feature_prepend(PhoneNumber) ->
    Prepend = knm_phone_number:feature(PhoneNumber, ?FEATURE_PREPEND),
    case kz_json:is_true(?PREPEND_ENABLED, Prepend) of
        'false' -> 'undefined';
        'true' -> kz_json:get_ne_value(?PREPEND_NAME, Prepend)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_early_ringback(knm_phone_number:knm_phone_number()) -> kz_term:api_binary().
find_early_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_EARLY, RingBack).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:knm_phone_number()) -> kz_term:api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_TRANSFER, RingBack).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_force_outbound(knm_phone_number:knm_phone_number()) -> boolean().
is_force_outbound(PN) ->
    is_force_outbound(knm_phone_number:state(PN)
                     ,knm_phone_number:module_name(PN)
                     ,force_outbound_feature(PN)
                     ).

-spec is_force_outbound(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> boolean().
is_force_outbound(?NUMBER_STATE_PORT_IN, Module, _ForceOutbound) ->
    knm_config:should_force_port_in_outbound()
        orelse force_module_outbound(Module);
is_force_outbound(?NUMBER_STATE_PORT_OUT, Module, _ForceOutbound) ->
    knm_config:should_force_port_out_outbound()
        orelse force_module_outbound(Module);
is_force_outbound(_State, ?CARRIER_LOCAL, _ForceOutbound) ->
    force_local_outbound();
is_force_outbound(_State, ?CARRIER_MDN, _ForceOutbound) ->
    force_local_outbound();
is_force_outbound(_State, _Module, ForceOutbound) ->
    ForceOutbound.

%% FIXME: move to kpn
-spec force_outbound_feature(knm_phone_number:knm_phone_number()) -> boolean().
force_outbound_feature(PN) ->
    case knm_phone_number:feature(PN, ?FEATURE_FORCE_OUTBOUND) of
        'undefined' -> knm_config:should_force_outbound();
        FO -> kz_term:is_true(FO)
    end.

-spec force_module_outbound(kz_term:ne_binary()) -> boolean().
force_module_outbound(?CARRIER_LOCAL) -> force_local_outbound();
force_module_outbound(?CARRIER_MDN) -> force_local_outbound();
force_module_outbound(_Mod) -> 'false'.

-spec force_local_outbound() -> boolean().
force_local_outbound() ->
    knm_config:should_force_local_outbound().

-spec phone_number(knm_number()) -> knm_phone_number:knm_phone_number().
phone_number(#knm_number{knm_phone_number=PhoneNumber}) -> PhoneNumber.

-spec set_phone_number(knm_number(), knm_phone_number:knm_phone_number()) ->
                              knm_number().
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

-spec charges(knm_number(), kz_term:ne_binary()) -> non_neg_integer().
charges(#knm_number{charges=Charges}, Key) ->
    props:get_value(Key, Charges, 0).

-spec set_charges(knm_number(), kz_term:ne_binary(), non_neg_integer()) -> knm_number().
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

-spec num_to_did(kz_term:api_binary() | knm_number() | knm_phone_number:knm_phone_number()) -> kz_term:api_ne_binary().
num_to_did('undefined') -> 'undefined';
num_to_did(?NE_BINARY = DID) -> DID;
num_to_did(#knm_number{}=Number) -> num_to_did(phone_number(Number));
num_to_did(PhoneNumber) -> knm_phone_number:number(PhoneNumber).
