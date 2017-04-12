%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_carriers).

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include("knm.hrl").

-compile({no_auto_import,[apply/3]}).

-export([check/1
        ,available_carriers/1, all_modules/0
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

-define(DEFAULT_CARRIER_MODULES, [?CARRIER_LOCAL]).

-define(DEFAULT_CARRIER_MODULE
       ,kapps_config:get_binary(?KNM_CONFIG_CAT, <<"available_module_name">>, ?CARRIER_LOCAL)).
-define(CARRIER_MODULES
       ,kapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)).
-define(CARRIER_MODULES(AccountId)
       ,kapps_account_config:get(AccountId, ?KNM_CONFIG_CAT, <<"carrier_modules">>, ?CARRIER_MODULES)).

-ifdef(TEST).
-type option() :: {'quantity', pos_integer()} |
                  {'carriers', ne_binaries()} |
                  {'phonebook_url', ne_binary()} |
                  {'tollfree', boolean()} |
                  {'prefix', ne_binary()} |
                  {'country', knm_util:country_iso3166a2()} |
                  {'offset', non_neg_integer()} |
                  {'blocks', boolean()} |
                  {'account_id', ne_binary()} |
                  {'reseller_id', ne_binary()}.
-else.
-type option() :: {'quantity', pos_integer()} |
                  {'prefix', ne_binary()} |
                  {'dialcode', ne_binary()} |
                  {'country', knm_util:country_iso3166a2()} |
                  {'offset', non_neg_integer()} |
                  {'blocks', boolean()} |
                  {'account_id', ne_binary()} |
                  {'reseller_id', ne_binary()}.
-endif.
-type options() :: [option()].
-export_type([option/0, options/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Normalize then query the various providers for available numbers.
%% @end
%%--------------------------------------------------------------------
-spec check(ne_binaries()) -> kz_json:object().
check(Numbers) ->
    Nums = lists:usort([knm_converters:normalize(Num) || Num <- Numbers]),
    lager:info("attempting to check ~p ", [Nums]),
    {_, OKs, KOs} =
        lists:foldl(fun check_fold/2, {Nums, #{}, #{}}, available_carriers([])),
    kz_json:from_map(maps:merge(KOs, OKs)).

check_fold(_, {[], _, _}=Acc) -> Acc;
check_fold(Module, {Nums, OKs0, KOs0}) ->
    {OKs, KOs} = check_numbers(Module, Nums),
    OKNums = maps:keys(OKs0),
    {Nums -- OKNums
    ,maps:merge(OKs, OKs0)
    ,maps:merge(maps:without(OKNums, KOs), KOs0)
    }.

check_numbers(Module, Nums) ->
    try apply(Module, check_numbers, [Nums]) of
        {ok, JObj} -> {kz_json:to_map(JObj), #{}};
        {error, _} -> {#{}, maps:from_list([{Num,<<"error">>} || Num <- Nums])}
    catch
        _:_ ->
            kz_util:log_stacktrace(),
            {#{}, maps:from_list([{Num,<<"error">>} || Num <- Nums])}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all carrier modules available to a subaccount.
%% @end
%%--------------------------------------------------------------------
-spec available_carriers(options()) -> atoms().
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

-spec get_available_carriers(options()) -> atoms().
get_available_carriers(Options) ->
    case account_id(Options) =:= undefined
        orelse reseller_id(Options) =:= undefined
    of
        true -> keep_only_reachable(?CARRIER_MODULES);
        false ->
            ResellerId = reseller_id(Options),
            First = [?CARRIER_RESERVED, ?CARRIER_RESERVED_RESELLER, ?CARRIER_LOCAL],
            keep_only_reachable(First ++ (?CARRIER_MODULES(ResellerId) -- First))
    end.

-spec default_carriers() -> atoms().
default_carriers() ->
    keep_only_reachable(?DEFAULT_CARRIER_MODULES).

-spec default_carrier() -> ne_binary().
default_carrier() ->
    ?DEFAULT_CARRIER_MODULE.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% List all carrier modules.
%% @end
%%--------------------------------------------------------------------
-spec all_modules() -> ne_binaries().
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
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Buy a number from its carrier module
%% @end
%%--------------------------------------------------------------------
-spec acquire(knm_number:knm_number()) -> knm_number:knm_number();
             (knm_numbers:collection()) -> knm_numbers:collection().
acquire(T0=#{todo := Ns}) ->
    F = fun (N, T) ->
                case knm_number:attempt(fun acquire/1, [N]) of
                    {ok, NewN} -> knm_numbers:ok(NewN, T);
                    {error, R} -> knm_numbers:ko(N, R, T)
                end
        end,
    lists:foldl(F, T0, Ns);
acquire(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Module = knm_phone_number:module_name(PhoneNumber),
    DryRun = knm_phone_number:dry_run(PhoneNumber),
    acquire(Number, Module, DryRun).

-spec acquire(knm_number:knm_number(), api_ne_binary(), boolean()) ->
                     knm_number:knm_number().
acquire(Number, 'undefined', _DryRun) ->
    knm_errors:carrier_not_specified(Number);
acquire(Number, _Mod, 'true') ->
    Number;
acquire(Number, ?NE_BINARY=Mod, 'false') ->
    apply(Mod, acquire_number, [Number]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disconnect(knm_number:knm_number()) -> knm_number:knm_number();
                (knm_numbers:collection()) -> knm_numbers:collection().
disconnect(T0=#{todo := Ns}) ->
    F = fun (N, T) ->
                case knm_number:attempt(fun disconnect/1, [N]) of
                    {ok, NewN} -> knm_numbers:ok(NewN, T);
                    {error, R} ->
                        Num = knm_phone_number:number(knm_number:phone_number(N)),
                        knm_numbers:ko(Num, R, T)
                end
        end,
    lists:foldl(F, T0, Ns);
disconnect(Number) ->
    Module = knm_phone_number:module_name(knm_number:phone_number(Number)),
    try apply(Module, disconnect_number, [Number]) of
        Result -> Result
    catch
        'error':_ ->
            lager:debug("non-existant carrier module ~p, allowing disconnect", [Module]),
            Number
    end.

-spec prefix(options()) -> ne_binary().
-spec prefix(options(), ne_binary()) -> ne_binary().
prefix(Options) ->
    props:get_ne_binary_value('prefix', Options).
prefix(Options, Default) ->
    props:get_ne_binary_value('prefix', Options, Default).

-spec dialcode(options()) -> ne_binary().
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

-spec account_id(options()) -> api_ne_binary().
account_id(Options) ->
    props:get_value('account_id', Options).

-spec reseller_id(options()) -> api_ne_binary().
reseller_id(Options) ->
    props:get_value('reseller_id', Options).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns whether carrier handles numbers local to the system.
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(PhoneNumber) ->
    Carrier = knm_phone_number:module_name(PhoneNumber),
    try apply(Carrier, is_number_billable, [PhoneNumber])
    catch
        'error':_R -> 'true'
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns whether carrier handles numbers local to the system.
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local(ne_binary()) -> boolean().
is_local(Carrier) ->
    try apply(Carrier, is_local, [])
    catch
        _E:_R ->
            kz_util:log_stacktrace(),
            true
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec apply(module() | ne_binary() | knm_number:knm_number(), atom(), list()) -> any().
apply(Module, FName, Args) when is_atom(Module), Module =/= undefined ->
    lager:debug("contacting carrier ~s for ~s", [Module, FName]),
    erlang:apply(Module, FName, Args);
apply(?NE_BINARY=Carrier, FName, Args) ->
    Module = erlang:binary_to_atom(Carrier, utf8),
    apply(Module, FName, Args);
apply(Number, FName, Args) ->
    Carrier = knm_phone_number:module_name(knm_number:phone_number(Number)),
    apply(Carrier, FName, Args).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec keep_only_reachable([ne_binary()]) -> atoms().
keep_only_reachable(ModuleNames) ->
    lager:debug("resolving carrier modules: ~p", [ModuleNames]),
    [Module
     || M <- ModuleNames,
        (Module = kz_util:try_load_module(M)) =/= 'false'
    ].
