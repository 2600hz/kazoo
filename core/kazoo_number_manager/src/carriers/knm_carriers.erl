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

-include_lib("kazoo_json/include/kazoo_json.hrl").
-include("knm.hrl").

-compile({no_auto_import,[apply/3]}).

-export([find/1, find/2
        ,check/1
        ,available_carriers/1, all_modules/0
        ,default_carriers/0, default_carrier/0
        ,acquire/1
        ,disconnect/1

        ,quantity/1
        ,prefix/1, prefix/2
        ,dialcode/1
        ,country/1
        ,offset/1
        ,blocks/1
        ,account_id/1
        ,reseller_id/1

        ,is_number_billable/1
        ]).

-export([options_to_jobj/1]).

-define(DEFAULT_CARRIER_MODULES, [?CARRIER_LOCAL]).

-ifdef(TEST).
-export([process_carrier_results/2
        ,process_bulk_carrier_results/2
        ]).
-endif.

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

-spec options_to_jobj(options()) -> kz_json:object().
options_to_jobj(Options) ->
    lists:foldl(fun option_to_kv/2, kz_json:new(), Options).

option_to_kv({K, V}, JObj) ->
    kz_json:set_value(kz_util:to_binary(K), V, JObj).

-define(DEFAULT_CARRIER_MODULE
       ,kapps_config:get_binary(?KNM_CONFIG_CAT, <<"available_module_name">>, ?CARRIER_LOCAL)).
-define(CARRIER_MODULES
       ,kapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)).
-define(CARRIER_MODULES(AccountId)
       ,kapps_account_config:get(AccountId, ?KNM_CONFIG_CAT, <<"carrier_modules">>, ?CARRIER_MODULES)).

-define(MAX_QUANTITY, kapps_config:get_integer(?KNM_CONFIG_CAT, <<"maximum_search_quantity">>, 50)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary()) -> kz_json:objects().
-spec find(ne_binary(), options()) -> kz_json:objects().

find(Prefix) ->
    find(Prefix, []).

find(Prefix, Options0) ->
    Dialcode = knm_util:prefix_for_country(country(Options0)),
    Options = [{'dialcode', Dialcode} | Options0],
    NormalizedPrefix = <<Dialcode/binary, (prefix(Options, Prefix))/binary>>,
    Quantity = quantity(Options),
    Carriers = available_carriers(Options),
    lager:debug("contacting, in order: ~p", [Carriers]),
    Acc0 = #{found => []
            ,count => 0
            ,left => Quantity
            ,should_continue => 'true'
            },
    #{found := Found
     ,count := _Count
     } =
        lists:foldl(fun(Carrier, Acc) ->
                            find_fold(Carrier, NormalizedPrefix, Options, Acc)
                    end
                   ,Acc0
                   ,Carriers
                   ),
    lager:debug("~s found ~p/~p numbers", [NormalizedPrefix, _Count, Quantity]),
    lists:sort(fun sort_find_result/2, Found).

-type find_acc() :: #{found => kz_json:objects()
                     ,count => non_neg_integer()
                     ,left => pos_integer()
                     ,should_continue => boolean()
                     }.
-spec find_fold(atom(), ne_binary(), options(), find_acc()) -> find_acc().
find_fold(_Carrier, _, _, Acc=#{should_continue := ShouldContinue
                               ,count := _Count
                               ,left := Left
                               })
  when ShouldContinue == 'false'; Left < 1 ->
    lager:debug("stopping ~s with ~p (~p) numbers found", [_Carrier, _Count, Left]),
    Acc;
find_fold(Carrier, Prefix, Options, Acc=#{left := Quantity}) ->
    try apply(Carrier, find_numbers, [Prefix, Quantity, Options]) of
        {'ok', []} -> Acc;
        {'ok', Numbers} -> process_carrier_results(Numbers, Acc);
        {'bulk', []} -> Acc;
        {'bulk', Numbers} -> process_bulk_carrier_results(Numbers, Acc);
        {'stopping_here', Numbers} ->
            NewAcc = process_carrier_results(Numbers, Acc),
            NewAcc#{should_continue => 'false'};
        {'error', _R} ->
            lager:debug("skipping carrier ~s: ~p", [Carrier, _R]),
            Acc
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            ?LOG_WARN("failed to query carrier ~s for ~p numbers: ~s: ~p"
                     ,[Carrier, Quantity, _E, _R]),
            kz_util:log_stacktrace(ST),
            Acc
    end.

-spec process_bulk_carrier_results(knm_number:knm_numbers(), find_acc()) -> find_acc().
process_bulk_carrier_results(Numbers, Acc) ->
    acc_found(Acc, [found_number_to_jobj(Number) || Number <- Numbers]).

-spec process_carrier_results(knm_number:knm_numbers(), find_acc()) -> find_acc().
process_carrier_results(Numbers, Acc) ->
    acc_found(Acc, [process_number_result(Number) || Number <- Numbers]).

-spec acc_found(find_acc(), kz_json:objects()) -> find_acc().
acc_found(Acc=#{found := Found
               ,count := Count
               ,left := Left
               }, NewNumbers) ->
    NewNumbersCount = length(NewNumbers),
    Acc#{found => Found ++ NewNumbers
        ,count => Count + NewNumbersCount
        ,left => Left - NewNumbersCount
        }.

-spec process_number_result(knm_number:knm_number()) -> kz_json:object().
process_number_result(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Carrier = knm_phone_number:module_name(PhoneNumber),
    case is_local(Carrier) of
        'true' -> found_number_to_jobj(Number);
        'false' ->
            DID = knm_phone_number:number(PhoneNumber),
            check_for_existing_did(Number, Carrier, knm_phone_number:fetch(DID))
    end.

-spec check_for_existing_did(knm_number:knm_number(), ne_binary(), knm_phone_number_return()) -> kz_json:object().
check_for_existing_did(Number, _Carrier, {'error', 'not_found'}) ->
    %% This case is only possible for -dTEST: tests don't save to DB (yet)
    %% and we make sure that non-local carriers save discovered numbers to DB.
    io:format(user, "number ~s was not in db\n"
             ,[knm_phone_number:number(knm_number:phone_number(Number))]),
    found_number_to_jobj(Number);
check_for_existing_did(Number, Carrier, {'ok', ExistingPhoneNumber}) ->
    case knm_phone_number:module_name(ExistingPhoneNumber) of
        Carrier -> found_number_to_jobj(Number);
        _OtherCarrier ->
            transition_existing_to_discovery(Number, ExistingPhoneNumber)
    end.

-spec transition_existing_to_discovery(knm_number:knm_number(), knm_phone_number:knm_phone_number()) ->
                                              kz_json:object().
transition_existing_to_discovery(Number, ExistingPhoneNumber) ->
    PhoneNumber0 = knm_number:phone_number(Number),
    Setters = [{fun knm_phone_number:set_module_name/2, knm_phone_number:module_name(PhoneNumber0)}
              ,{fun knm_phone_number:set_carrier_data/2, knm_phone_number:carrier_data(PhoneNumber0)}
              ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
              ],
    {'ok', PhoneNumber} = knm_phone_number:setters(ExistingPhoneNumber, Setters),
    found_number_to_jobj(
      knm_number:set_phone_number(Number, knm_phone_number:save(PhoneNumber))).

-spec found_number_to_jobj(knm_number:knm_number()) -> kz_json:object().
found_number_to_jobj(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    DID = knm_phone_number:number(PhoneNumber),
    kz_json:from_list(
      props:filter_undefined(
        [{<<"number">>, DID}
        ,{<<"activation_charge">>, activation_charge(DID, knm_phone_number:assign_to(PhoneNumber))}
        ,{<<"state">>, knm_phone_number:state(PhoneNumber)}
        ])).

-spec activation_charge(ne_binary(), api_binary()) -> api_number().
-ifdef(TEST).
activation_charge(?START_BLOCK, _AccountId) -> 5.0;
activation_charge(?END_BLOCK, _AccountId) -> 'undefined';
activation_charge(_Number, _AccountId) -> 1.0.
-else.
activation_charge(_DID, 'undefined') -> 'undefined';
activation_charge(DID, AccountId) ->
    kz_services:activation_charges(<<"phone_numbers">>, knm_converters:classify(DID), AccountId).
-endif.

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

-spec quantity(options()) -> pos_integer().
quantity(Options) ->
    Quantity = props:get_integer_value('quantity', Options, 1),
    min(Quantity, ?MAX_QUANTITY).

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
%% Sort whole number search result
%% @end
%%--------------------------------------------------------------------
-spec sort_find_result(kz_json:object(), kz_json:object()) -> boolean().
sort_find_result(A, B) ->
    kz_json:get_value(<<"number">>, A) =< kz_json:get_value(<<"number">>, B).

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
    Module = erlang:binary_to_existing_atom(Carrier, 'utf8'),
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
