%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_carriers).

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include("knm.hrl").

-compile({'no_auto_import',[apply/3]}).

-export([check/1
        ,available_carriers/1, all_modules/0, info/3
        ,default_carriers/0, default_carrier/0
        ,acquire/1
        ,disconnect/1

        ,prefix/1, prefix/2
        ,dialcode/1
        ,country/1
        ,offset/1
        ,blocks/1
        ,account_id/1
        ,reseller_id/1

        ,is_number_billable/1
        ,is_local/1
        ]).

%% FIXME:`?' CHILD_ACCOUNT_ID is not a reseller but that's okay:
%% [?CARRIER_LOCAL, <<"knm_bandwidth2">>]
-define(CARRIER_MODULES(AccountId)
       ,kapps_account_config:get_ne_binaries(AccountId, ?KNM_CONFIG_CAT, <<"carrier_modules">>, ?CARRIER_MODULES)
       ).

-define(CARRIER_MODULES
       ,kapps_config:get_ne_binaries(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)
       ).

-define(DEFAULT_CARRIER_MODULE
       ,kapps_config:get_binary(?KNM_CONFIG_CAT, <<"available_module_name">>, ?CARRIER_LOCAL)
       ).

-define(DEFAULT_CARRIER_MODULES, [?DEFAULT_CARRIER_MODULE]).

-ifdef(TEST).
%% TODO: write testable code so we don't depend on passing these
%% for testing
-type option() :: {'quantity', pos_integer()} |
                  {'carriers', kz_term:ne_binaries()} |
                  {'phonebook_url', kz_term:ne_binary()} |
                  {'tollfree', boolean()} |
                  {'prefix', kz_term:ne_binary()} |
                  {'country', knm_util:country_iso3166a2()} |
                  {'offset', non_neg_integer()} |
                  {'blocks', boolean()} |
                  {'account_id', kz_term:ne_binary()} |
                  {'reseller_id', kz_term:ne_binary()}.
-else.
-type option() :: {'quantity', pos_integer()} |
                  {'prefix', kz_term:ne_binary()} |
                  {'dialcode', kz_term:ne_binary()} |
                  {'country', knm_util:country_iso3166a2()} |
                  {'offset', non_neg_integer()} |
                  {'blocks', boolean()} |
                  {'account_id', kz_term:ne_binary()} |
                  {'reseller_id', kz_term:ne_binary()}.
-endif.
-type options() :: [option()].
-export_type([option/0, options/0]).


%%------------------------------------------------------------------------------
%% @doc Normalize then query the various providers for available numbers.
%% @end
%%------------------------------------------------------------------------------
-spec check(kz_term:ne_binaries()) -> kz_json:object().
check(Numbers) ->
    Nums = lists:usort([knm_converters:normalize(Num) || Num <- Numbers]),
    lager:info("attempting to check ~p ", [Nums]),
    {_, OKs, Failed} =
        lists:foldl(fun check_fold/2, {Nums, #{}, #{}}, available_carriers([])),
    kz_json:from_map(maps:merge(Failed, OKs)).

-spec check_fold(module(), {kz_term:ne_binaries(), JObj::map(), NumErrorMap::map()}) ->
          {kz_term:ne_binaries(), JObj::map(), NumErrorMap::map()}.
check_fold(_, {[], _, _}=Acc) -> Acc;
check_fold(Module, {Nums, OKs0, Failed0}) ->
    {OKs, Failed} = check_numbers(Module, Nums),
    OKNums = maps:keys(OKs0),
    {Nums -- OKNums
    ,maps:merge(OKs, OKs0)
    ,maps:merge(maps:without(OKNums, Failed), Failed0)
    }.

-spec check_numbers(module(), kz_term:ne_binaries()) -> {JObj::map(), NumErrorMap::map()}.
check_numbers(Module, Nums) ->
    try apply(Module, 'check_numbers', [Nums]) of
        {'ok', JObj} -> {kz_json:to_map(JObj), #{}};
        {'error', _} -> {#{}, maps:from_list([{Num, <<"error">>} || Num <- Nums])}
    catch
        ?STACKTRACE(_, _, ST)
        kz_log:log_stacktrace(ST),
        {#{}, maps:from_list([{Num, <<"error">>} || Num <- Nums])}
        end.

%%------------------------------------------------------------------------------
%% @doc Create a list of all carrier modules available to a subaccount.
%% @end
%%------------------------------------------------------------------------------
-spec available_carriers(options()) -> kz_term:atoms().
-ifdef(TEST).
available_carriers(Options) ->
    case props:get_value('carriers', Options) of
        Cs=[_|_] -> keep_only_reachable(Cs);
        _ -> get_available_carriers(Options)
    end.
-else.
available_carriers(Options) ->
    get_available_carriers(Options).
-endif.

-spec get_available_carriers(options()) -> kz_term:atoms().
get_available_carriers(Options) ->
    case account_id(Options) =:= 'undefined'
        orelse reseller_id(Options) =:= 'undefined'
    of
        'true' -> keep_only_reachable(?CARRIER_MODULES);
        'false' ->
            ResellerId = reseller_id(Options),
            First = [?CARRIER_RESERVED, ?CARRIER_RESERVED_RESELLER, ?CARRIER_LOCAL],
            keep_only_reachable(First ++ (?CARRIER_MODULES(ResellerId) -- First))
    end.

-spec default_carriers() -> kz_term:atoms().
default_carriers() ->
    keep_only_reachable(?DEFAULT_CARRIER_MODULES).

-spec default_carrier() -> kz_term:ne_binary().
default_carrier() ->
    ?DEFAULT_CARRIER_MODULE.

%%------------------------------------------------------------------------------
%% @doc List all carrier modules.
%% @end
%%------------------------------------------------------------------------------
-spec all_modules() -> kz_term:ne_binaries().
all_modules() ->
    [<<"knm_bandwidth2">>
    ,<<"knm_bandwidth">>
    ,<<"knm_inum">>
    ,<<"knm_local">>
    ,<<"knm_inventory">>
    ,<<"knm_managed">>
    ,<<"knm_mdn">>
    ,<<"knm_other">>
    ,<<"knm_reserved">>
    ,<<"knm_reserved_reseller">>
    ,<<"knm_simwood">>
    ,<<"knm_telnyx">>
    ,<<"knm_vitelity">>
    ,<<"knm_voip_innovations">>
    ,<<"knm_voxbone">>
    ].

%%------------------------------------------------------------------------------
%% @doc Get information on the available carriers
%% @end
%%------------------------------------------------------------------------------
-spec info(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_json:object().
info(AuthAccountId, AccountId, ResellerId) ->
    AvailableCarriers = available_carriers([{'account_id', AccountId}
                                           ,{'reseller_id', ResellerId}
                                           ]),
    Acc0 = #{?CARRIER_INFO_MAX_PREFIX => 15
            },
    Map = lists:foldl(fun info_fold/2, Acc0, AvailableCarriers),
    kz_json:from_map(
      Map#{?CARRIER_INFO_USABLE_CARRIERS => usable_carriers()
          ,?CARRIER_INFO_USABLE_CREATION_STATES => knm_lib:allowed_creation_states(AuthAccountId)
          }
     ).

-spec info_fold(module(), map()) -> map().
info_fold(Module, Info=#{?CARRIER_INFO_MAX_PREFIX := MaxPrefix}) ->
    try apply(Module, 'info', []) of
        #{?CARRIER_INFO_MAX_PREFIX := Lower}
          when is_integer(Lower), Lower < MaxPrefix ->
            Info#{?CARRIER_INFO_MAX_PREFIX => Lower
                 };
        _ -> Info
    catch
        ?STACKTRACE(_E, _R, ST)
        kz_log:log_stacktrace(ST),
        Info
        end.

-spec usable_carriers() -> kz_term:ne_binaries().
usable_carriers() ->
    Modules = all_modules() -- [?CARRIER_RESERVED
                               ,?CARRIER_RESERVED_RESELLER
                               ],
    [CarrierName || <<"knm_",CarrierName/binary>> <- Modules].

%%------------------------------------------------------------------------------
%% @doc Buy a number from its carrier module
%% @end
%%------------------------------------------------------------------------------
-spec acquire(knm_phone_number:record()) -> knm_phone_number:record();
             (knm_pipe:collection()) -> knm_pipe:collection().
%% FIXME: opaque
acquire(T0=#{'todo' := PNs}) ->
    F = fun (PN, T) ->
                case knm_pipe:attempt(fun acquire/1, [PN]) of
                    {'ok', NewPN} -> knm_pipe:set_succeeded(T, NewPN);
                    {'error', R} -> knm_pipe:set_failed(T, PN, R)
                end
        end,
    lists:foldl(F, T0, PNs);
acquire(PN) ->
    Module = knm_phone_number:module_name(PN),
    DryRun = knm_phone_number:dry_run(PN),
    acquire(PN, Module, DryRun).

-spec acquire(knm_phone_number:record(), kz_term:api_ne_binary(), boolean()) ->
          knm_phone_number:record().
acquire(PN, 'undefined', _DryRun) ->
    knm_errors:carrier_not_specified(PN);
acquire(PN, _Mod, 'true') ->
    PN;
acquire(PN, ?NE_BINARY=Mod, 'false') ->
    case 'false' =:= kz_module:ensure_loaded(Mod) of
        'false' -> apply(Mod, 'acquire_number', [PN]);
        'true' ->
            lager:info("carrier '~s' does not exist, skipping", [Mod]),
            PN
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec disconnect(knm_phone_number:record()) -> knm_phone_number:record();
                (knm_pipe:collection()) -> knm_pipe:collection().
%% FIXME: opaque
disconnect(T0=#{'todo' := PNs}) ->
    F = fun (PN, T) ->
                case knm_pipe:attempt(fun disconnect/1, [PN]) of
                    {'ok', NewPN} -> knm_pipe:set_succeeded(T, NewPN);
                    {'error', R} ->
                        Num = knm_phone_number:number(PN),
                        knm_pipe:set_failed(T, Num, R)
                end
        end,
    lists:foldl(F, T0, PNs);
disconnect(PN) ->
    Module = knm_phone_number:module_name(PN),
    try apply(Module, 'disconnect_number', [PN]) of
        Result -> Result
    catch
        'error':_ ->
            lager:debug("nonexistent carrier module ~p, allowing disconnect", [Module]),
            PN
    end.

-spec prefix(options()) -> kz_term:ne_binary().
prefix(Options) ->
    props:get_ne_binary_value('prefix', Options).

-spec prefix(options(), kz_term:ne_binary()) -> kz_term:ne_binary().
prefix(Options, Default) ->
    props:get_ne_binary_value('prefix', Options, Default).

-spec dialcode(options()) -> kz_term:ne_binary().
dialcode(Options) ->
    props:get_ne_binary_value('dialcode', Options).

-spec country(options()) -> knm_util:country_iso3166a2().
country(Options) ->
    case props:get_ne_binary_value('country', Options, ?KNM_DEFAULT_COUNTRY) of
        <<_:8, _:8>>=Country -> Country;
        _Else ->
            lager:debug("~p is not iso3166a2, using default"),
            ?KNM_DEFAULT_COUNTRY
    end.

-spec offset(options()) -> non_neg_integer().
offset(Options) ->
    props:get_integer_value('offset', Options, 0).

-spec blocks(options()) -> boolean().
blocks(Options) ->
    props:get_value('blocks', Options).

-spec account_id(options()) -> kz_term:api_ne_binary().
account_id(Options) ->
    props:get_value('account_id', Options).

-spec reseller_id(options()) -> kz_term:api_ne_binary().
reseller_id(Options) ->
    props:get_value('reseller_id', Options).

%%------------------------------------------------------------------------------
%% @doc Returns whether carrier handles numbers local to the system.
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(PN) ->
    Carrier = knm_phone_number:module_name(PN),
    try apply(Carrier, 'is_number_billable', [PN])
    catch
        'error':_R -> 'true'
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns whether carrier handles numbers local to the system.
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local(kz_term:ne_binary()) -> boolean().
is_local(Carrier) ->
    try apply(Carrier, 'is_local', [])
    catch
        ?STACKTRACE(_E, _R, ST)
        kz_log:log_stacktrace(ST),
        true
        end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec apply(module() | kz_term:ne_binary() | knm_phone_number:record(), atom(), list()) -> any().
apply(Module, FName, Args) when is_atom(Module), Module =/= 'undefined' ->
    lager:debug("contacting carrier ~s for ~s", [Module, FName]),
    erlang:apply(Module, FName, Args);
apply(?NE_BINARY=Carrier, FName, Args) ->
    Module = erlang:binary_to_atom(Carrier, 'utf8'),
    apply(Module, FName, Args);
apply(PN, FName, Args) ->
    Carrier = knm_phone_number:module_name(PN),
    apply(Carrier, FName, Args).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec keep_only_reachable([kz_term:ne_binary()]) -> kz_term:atoms().
keep_only_reachable(ModuleNames) ->
    lager:debug("resolving carrier modules: ~p", [ModuleNames]),
    [Module
     || M <- ModuleNames,
        (Module = kz_module:ensure_loaded(M)) =/= 'false'
    ].
