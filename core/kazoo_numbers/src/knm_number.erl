%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_number).

-export([get/1, get/2
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

-export([force_outbound_feature/1]).

-export([ensure_can_create/1
        ,ensure_can_load_to_create/1
        ,state_for_create/1
        ,allowed_creation_states/1, allowed_creation_states/2
        ]).

-ifdef(TEST).
%% TODO: Remove me after fixturedb has save feature
-export([ensure_can_create/2]).
-export([is_force_outbound/1]).
-endif.

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include("knm.hrl").

-type lookup_error() :: 'not_reconcilable' |
                        'not_found' |
                        'unassigned' |
                        {'not_in_service', kz_term:ne_binary()} |
                        {'account_disabled', kz_term:ne_binary()}.

-type lookup_account_return() :: {'ok', kz_term:ne_binary(), knm_number_options:extra_options()} |
                                 {'error', lookup_error()}.

-type dry_run_return() :: {'dry_run', knm_pipe:quotes()}.
-type either_obj() :: kz_either:either(knm_errors:error() | atom(), knm_phone_number:record()).
-type either_objs() :: kz_either:either(knm_errors:error() | atom(), knm_phone_number:records()).

-type return() :: either_obj() | dry_run_return().

-export_type([return/0
             ,either_obj/0
             ,either_objs/0
             ]).

%% FIXME: opaque
-define(KNM_NUMBERS_CLAUSES(Num)
       ,{'false', #{'succeeded' := [Number]}} ->
               {'ok', Number};
            {'true', #{'succeeded' := [_Number], 'quotes' := Quotes}} ->
               {'dry_run', Quotes};
            {_, #{'failed' := ErrorM}} ->
               {'error', hd(maps:values(ErrorM))}
                   ).

-define(RUN_KNM_NUMBERS_FUN(F, Num, Options)
       ,case {knm_number_options:dry_run(Options)
             ,knm_numbers:F([Num], Options)
             }
        of ?KNM_NUMBERS_CLAUSES(Num)
            end
       ).

-define(RUN_KNM_NUMBERS_FUN_ARGS(F, Num, Arg2, Options),
        case {knm_number_options:dry_run(Options)
             ,knm_numbers:F([Num], Arg2, Options)
             }
        of ?KNM_NUMBERS_CLAUSES(Num)
            end
       ).

%%------------------------------------------------------------------------------
%% @doc Attempts to get a number from DB.
%%
%% <div class="notice">Number parameter has to be normalized.</div>
%%
%% <div class="notice">{@link get/1}, {@link get/2} should not throw,
%% instead they should return: `{ok,_} | {error,_} | ...'.</div>
%% @end
%%------------------------------------------------------------------------------
-spec get(kz_term:ne_binary()) -> return().
get(Num) ->
    get(Num, knm_number_options:default()).

-spec get(kz_term:ne_binary(), knm_number_options:options()) -> return().
get(Num, Options) ->
    case knm_numbers:get([Num], Options) of
        %% FIXME: opaque
        #{'succeeded' := [PN]} -> {'ok', PN};
        #{'failed' := M} -> {'error', hd(maps:values(M))}
    end.

%%------------------------------------------------------------------------------
%% @doc Attempts to create a new number in DB or modify an existing one.
%%
%% <div class="notice">`assign_to' number option MUST be set.</div>
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), knm_number_options:options()) -> return().
create(Num, Options) ->
    ?RUN_KNM_NUMBERS_FUN('create', Num, Options).

-spec state_for_create(knm_number_options:options()) -> kz_term:ne_binary().
state_for_create(Options) ->
    case {knm_number_options:state(Options, ?NUMBER_STATE_IN_SERVICE)
         ,knm_number_options:ported_in(Options)
         ,knm_number_options:module_name(Options)
         }
    of
        {?NUMBER_STATE_PORT_IN=PortIn, _, _} -> PortIn;
        {_, 'true', _} -> ?NUMBER_STATE_IN_SERVICE;
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
allowed_creation_states(_, 'undefined') -> [];
allowed_creation_states(Options, AuthBy) ->
    case {knm_phone_number:is_admin(AuthBy)
         ,allow_number_additions(Options, AuthBy)
         }
    of
        {'true', _} ->
            [?NUMBER_STATE_AGING
            ,?NUMBER_STATE_AVAILABLE
            ,?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_PORT_IN
            ,?NUMBER_STATE_RESERVED
            ];
        {'false', 'true'} ->
            [?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_RESERVED
            ];
        _ -> []
    end.

-spec ensure_can_load_to_create(knm_pipe:collection()) -> knm_pipe:collection();
                               (knm_phone_number:record()) -> 'true'.
%% FIXME: opaque
ensure_can_load_to_create(T0=#{'todo' := PNs}) ->
    F = fun (PN, T) ->
                case knm_pipe:attempt(fun ensure_can_load_to_create/1, [PN]) of
                    'true' -> knm_pipe:set_succeeded(T, PN);
                    {'error', R} ->
                        Num = knm_phone_number:number(PN),
                        knm_pipe:set_failed(T, Num, R)
                end
        end,
    lists:foldl(F, T0, PNs);
ensure_can_load_to_create(PN) ->
    ensure_state(PN, [?NUMBER_STATE_AGING
                     ,?NUMBER_STATE_AVAILABLE
                     ,?NUMBER_STATE_PORT_IN
                     ]).

-spec ensure_state(knm_phone_number:record(), kz_term:ne_binaries()) -> 'true'.
ensure_state(PN, AllowedStates) ->
    State = knm_phone_number:state(PN),
    case lists:member(State, AllowedStates) of
        'true' -> 'true';
        'false' ->
            Num = knm_phone_number:number(PN),
            ?LOG_ERROR("~s wrong state ~s, expected one of ~p", [Num, State, AllowedStates]),
            knm_errors:number_exists(Num)
    end.

%%------------------------------------------------------------------------------
%% @doc Fetches then transitions an existing number to the reserved state.
%% @end
%%------------------------------------------------------------------------------
-spec reserve(kz_term:ne_binary(), knm_number_options:options()) -> return().
reserve(Num, Options) ->
    ?RUN_KNM_NUMBERS_FUN('reserve', Num, Options).

-spec ensure_can_create(knm_pipe:collection()) -> knm_pipe:collection().
%% FIXME: opaque
ensure_can_create(T0=#{'todo' := Nums, 'options' := Options}) ->
    F = fun (Num, T) ->
                case knm_pipe:attempt(fun ensure_can_create/2, [Num, Options]) of
                    {'error', R} -> knm_pipe:set_failed(T, Num, R);
                    'true' ->
                        PN = knm_phone_number:from_number_with_options(Num, Options),
                        knm_pipe:set_succeeded(T, PN)
                end
        end,
    lists:foldl(F, T0, Nums).

-spec ensure_can_create(kz_term:ne_binary(), knm_number_options:options()) -> 'true'.
ensure_can_create(Num, Options) ->
    ensure_account_can_create(Options, knm_number_options:auth_by(Options))
        andalso ensure_number_is_not_porting(Num, Options).

-ifdef(TEST).
%% TODO: this is required to simulate reseller account without number_allowed_addition
%% Remove this after fixturedb supports save operation
-define(LOAD_ACCOUNT(Options, AccountId)
       ,(case props:get_value(<<"auth_by_account">>, Options) of
             'undefined' -> kzd_accounts:fetch(AccountId);
             AccountJObj ->
                 {'ok', Fetched} = kzd_accounts:fetch(AccountId),
                 {'ok', kz_json:merge(Fetched, AccountJObj)}
         end)
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
    ?LOG_INFO("bypassing auth"),
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
%% TODO: Remove me after fixturedb has save feature
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

-spec move(kz_term:ne_binary(), kz_term:ne_binary()) -> return().
move(Num, MoveTo) ->
    move(Num, MoveTo, knm_number_options:default()).

-spec move(kz_term:ne_binary(), kz_term:ne_binary(), knm_number_options:options()) -> return().
move(?NE_BINARY=Num, ?NE_BINARY=MoveTo, Options) ->
    ?RUN_KNM_NUMBERS_FUN_ARGS('move', Num, MoveTo, Options).

%%------------------------------------------------------------------------------
%% @doc Attempts to update some phone_number fields.
%%
%% <div class="notice">will always result in a phone_number save.</div>
%% @end
%%------------------------------------------------------------------------------

-spec update(kz_term:ne_binary(), knm_phone_number:set_functions()) -> return().
update(Num, Routines) ->
    update(Num, Routines, knm_number_options:default()).

-spec update(kz_term:ne_binary(), knm_phone_number:set_functions(), knm_number_options:options()) -> return().
update(Num, Routines, Options) ->
    ?RUN_KNM_NUMBERS_FUN_ARGS('update', Num, Routines, Options).

%%------------------------------------------------------------------------------
%% @doc Note: option 'assign_to' needs to be set.
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_term:ne_binary(), knm_number_options:options()) -> return().
reconcile(DID, Options) ->
    ?RUN_KNM_NUMBERS_FUN('reconcile', DID, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec release(kz_term:ne_binary()) -> return().
release(Num) ->
    release(Num, knm_number_options:default()).

-spec release(kz_term:ne_binary(), knm_number_options:options()) -> return().
release(Num, Options) ->
    ?RUN_KNM_NUMBERS_FUN('release', Num, Options).

%%------------------------------------------------------------------------------
%% @doc Remove a number from the system without doing any state checking.
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_term:ne_binary(), knm_number_options:options()) -> return().
delete(Num, Options) ->
    ?RUN_KNM_NUMBERS_FUN('delete', Num, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec assign_to_app(kz_term:ne_binary(), kz_term:api_ne_binary()) -> return().
assign_to_app(Num, App) ->
    assign_to_app(Num, App, knm_number_options:default()).

-spec assign_to_app(kz_term:ne_binary(), kz_term:api_ne_binary(), knm_number_options:options()) -> return().
assign_to_app(Num, App, Options) ->
    ?RUN_KNM_NUMBERS_FUN_ARGS('assign_to_app', Num, App, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec lookup_account(kz_term:api_ne_binary()) -> lookup_account_return().
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

-spec fetch_account_from_number(kz_term:ne_binary()) -> lookup_account_return().
fetch_account_from_number(Num) ->
    case knm_phone_number:fetch(Num) of
        {'ok', PN} -> check_number(PN);
        {'error', _}=Error -> maybe_fetch_account_from_ports(Num, Error)
    end.

-spec check_number(knm_phone_number:record()) -> lookup_account_return().
check_number(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case kz_term:is_empty(AssignedTo) of
        'true' -> {'error', 'unassigned'};
        'false' ->
            States = [?NUMBER_STATE_PORT_IN
                     ,?NUMBER_STATE_IN_SERVICE
                     ,?NUMBER_STATE_PORT_OUT
                     ,?NUMBER_STATE_RESERVED
                     ],
            case lists:member(knm_phone_number:state(PN), States) of
                'true' -> check_account(PN);
                'false' -> {'error', {'not_in_service', AssignedTo}}
            end
    end.

-spec check_account(knm_phone_number:record()) -> lookup_account_return().
check_account(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case kzd_accounts:is_enabled(AssignedTo) of
        'false' -> {'error', {'account_disabled', AssignedTo}};
        'true' ->
            Props = [{'pending_port', knm_phone_number:state(PN) =:= ?NUMBER_STATE_PORT_IN}
                    ,{'local', knm_phone_number:module_name(PN) =:= ?CARRIER_LOCAL}
                    ,{'number', knm_phone_number:number(PN)}
                    ,{'account_id', AssignedTo}
                    ,{'prepend', feature_prepend(PN)}
                    ,{'inbound_cnam', feature_inbound_cname(PN)}
                    ,{'ringback_media', find_early_ringback(PN)}
                    ,{'transfer_media', find_transfer_ringback(PN)}
                    ,{'force_outbound', is_force_outbound(PN)}
                    ],
            {'ok', AssignedTo, Props}
    end.

-spec maybe_fetch_account_from_ports(kz_term:ne_binary(), {'error', any()}) -> lookup_account_return().
maybe_fetch_account_from_ports(Num, Error) ->
    case knm_config:should_fetch_account_from_ports() of
        'false' -> Error;
        'true' -> fetch_account_from_ports(Num, Error)
    end.

-spec fetch_account_from_ports(kz_term:ne_binary(), {'error', any()}) -> lookup_account_return().
fetch_account_from_ports(Num, Error) ->
    case knm_port_request:get(Num) of
        {'error', _E} -> Error;
        {'ok', Port} ->
            AccountId = kz_doc:account_id(Port),
            Props = [{'pending_port', 'true'}
                    ,{'local', 'true'}
                    ,{'number', Num}
                    ,{'account_id', AccountId}
                     %% No prepend
                    ,{'inbound_cnam', 'false'}
                     %% No ringback_media
                     %% No transfer_media
                    ,{'force_outbound', 'true'}
                    ],
            {'ok', AccountId, Props}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature_prepend(knm_phone_number:record()) -> kz_term:api_binary().
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
-spec feature_inbound_cname(knm_phone_number:record()) -> boolean().
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
-spec find_early_ringback(knm_phone_number:record()) -> kz_term:api_binary().
find_early_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_EARLY, RingBack).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:record()) -> kz_term:api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_TRANSFER, RingBack).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_force_outbound(knm_phone_number:record()) -> boolean().
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
-spec force_outbound_feature(knm_phone_number:record()) -> boolean().
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
