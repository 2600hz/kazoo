%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_carriers).

-include_lib("kazoo_json/include/kazoo_json.hrl").
-include("knm.hrl").

-compile({no_auto_import,[apply/3]}).

-export([find/1, find/2
        ,check/1, check/2
        ,available_carriers/1, all_modules/0, info/3
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

-define(DEFAULT_CARRIER_MODULE
       ,kapps_config:get_binary(?KNM_CONFIG_CAT, <<"available_module_name">>, ?CARRIER_LOCAL)).
-define(CARRIER_MODULES
       ,kapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)).
-ifdef(TEST).
-define(CARRIER_MODULES(AccountId)
       ,(fun (?CHILD_ACCOUNT_ID) ->
                 %% CHILD_ACCOUNT_ID is not a reseller but that's okay
                 [?CARRIER_LOCAL, <<"knm_bandwidth2">>];
             (_) ->
                 kapps_account_config:get(AccountId, ?KNM_CONFIG_CAT, <<"carrier_modules">>, ?CARRIER_MODULES)
         end)(AccountId)).
-else.
-define(CARRIER_MODULES(AccountId)
       ,kapps_account_config:get(AccountId, ?KNM_CONFIG_CAT, <<"carrier_modules">>, ?CARRIER_MODULES)).
-endif.

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
            log_stacktrace(ST),
            Acc
    end.

-spec process_bulk_carrier_results(knm_number:knm_numbers(), find_acc()) -> find_acc().
process_bulk_carrier_results(Numbers, Acc) ->
    acc_found(Acc, [found_number_to_jobj(Number) || Number <- Numbers]).

-spec process_carrier_results(knm_number:knm_numbers(), find_acc()) -> find_acc().
process_carrier_results(Numbers, Acc) ->
    acc_found(Acc, lists:foldl(fun process_number_result/2, [], Numbers)).

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

-spec process_number_result(knm_number:knm_number(), kz_json:objects()) -> kz_json:objects().
process_number_result(Number, Acc) ->
    PhoneNumber = knm_number:phone_number(Number),
    Carrier = knm_phone_number:module_name(PhoneNumber),
    case is_local(Carrier) of
        'true' -> [found_number_to_jobj(Number) | Acc];
        'false' ->
            DID = knm_phone_number:number(PhoneNumber),
            check_for_existing_did(Number, Acc, Carrier, knm_phone_number:fetch(DID))
    end.

-spec check_for_existing_did(knm_number:knm_number(), kz_json:objects(), ne_binary()
                            ,knm_phone_number_return()) -> kz_json:objects().
check_for_existing_did(Number, Acc, _Carrier, {'error', 'not_found'}) ->
    %% This case is only possible for -dTEST: tests don't save to DB (yet)
    %% and we make sure that non-local carriers save discovered numbers to DB.
    io:format(user, "number ~s was not in db\n"
             ,[knm_phone_number:number(knm_number:phone_number(Number))]),
    [found_number_to_jobj(Number) | Acc];
check_for_existing_did(Number, Acc, Carrier, {'ok', ExistingPhoneNumber}) ->
    case knm_phone_number:module_name(ExistingPhoneNumber) of
        Carrier -> [found_number_to_jobj(Number) | Acc];
        _OtherCarrier ->
            transition_existing_to_discovery(Number, ExistingPhoneNumber, Acc)
    end.

-spec transition_existing_to_discovery(knm_number:knm_number(), knm_phone_number:knm_phone_number()
                                      ,kz_json:objects()) -> kz_json:objects().
transition_existing_to_discovery(Number, ExistingPhoneNumber, Acc) ->
    PhoneNumber0 = knm_number:phone_number(Number),
    Setters = [{fun knm_phone_number:set_module_name/2, knm_phone_number:module_name(PhoneNumber0)}
              ,{fun knm_phone_number:set_carrier_data/2, knm_phone_number:carrier_data(PhoneNumber0)}
              ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
              ],
    {'ok', PhoneNumber} =
        knm_phone_number:setters(ExistingPhoneNumber, Setters),
    case knm_number:save(knm_number:set_phone_number(Number, PhoneNumber)) of
        {'ok', SavedNumber} -> [found_number_to_jobj(SavedNumber) | Acc];
        {'error', _R} ->
            lager:debug("skipping number ~s: ~p", [knm_phone_number:number(PhoneNumber), _R]),
            Acc
    end.

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
%% @doc Normalize then query the various providers for available numbers.
%%--------------------------------------------------------------------
-type checked_numbers() :: [{module(), {'ok', kz_json:object()} |
                             {'error', any()} |
                             {'EXIT', any()}
                            }].
-spec check(ne_binaries()) -> checked_numbers().
-spec check(ne_binaries(), options()) -> checked_numbers().
check(Numbers) -> check(Numbers, []).

check(Numbers, Options) ->
    FormattedNumbers = [knm_converters:normalize(Num) || Num <- Numbers],
    lager:info("attempting to check ~p ", [FormattedNumbers]),
    [{Module, catch(apply(Module, check_numbers, [FormattedNumbers, Options]))}
     || Module <- available_carriers(Options)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc Create a list of all available carrier modules
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
%% Get information on the available carriers
%% @end
%%--------------------------------------------------------------------
-spec info(api_ne_binary(), api_ne_binary(), api_ne_binary()) -> kz_json:object().
info(AuthAccountId, AccountId, ResellerId) ->
    AvailableCarriers = available_carriers([{account_id, AccountId}
                                           ,{reseller_id, ResellerId}
                                           ]),
    Acc0 = #{?CARRIER_INFO_MAX_PREFIX => 15
            },
    Map = lists:foldl(fun info_fold/2, Acc0, AvailableCarriers),
    kz_json:from_map(
      Map#{?CARRIER_INFO_USABLE_CARRIERS => usable_carriers()
          ,?CARRIER_INFO_USABLE_CREATION_STATES => knm_number:allowed_creation_states(AuthAccountId)
          }
     ).

info_fold(Module, Info=#{?CARRIER_INFO_MAX_PREFIX := MaxPrefix}) ->
    try apply(Module, info, []) of
        #{?CARRIER_INFO_MAX_PREFIX := Lower}
          when is_integer(Lower), Lower < MaxPrefix ->
            Info#{?CARRIER_INFO_MAX_PREFIX => Lower
                 };
        _ -> Info
    catch
        _E:_R ->
            kz_util:log_stacktrace(),
            Info
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Buy a number from its carrier module
%% @end
%%--------------------------------------------------------------------
-spec acquire(knm_number:knm_number()) -> knm_number:knm_number().
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
    case false =:= kz_util:try_load_module(Mod) of
        false -> apply(Mod, acquire_number, [Number]);
        true ->
            lager:info("carrier '~s' does not exist, skipping", [Mod]),
            Number
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec disconnect(knm_number:knm_number()) -> knm_number:knm_number().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
log_stacktrace(ST) ->
    ?LOG_DEBUG("stacktrace:", []),
    _ = [log_stacktrace_mfa(M, F, A, Info)
         || {M, F, A, Info} <- ST
        ],
    'ok'.

log_stacktrace_mfa(M, F, Arity, Info) when is_integer(Arity) ->
    ?LOG_DEBUG("st: ~s:~s/~b at (~b)"
              ,[M, F, Arity, props:get_value('line', Info, 0)]
              );
log_stacktrace_mfa(M, F, Args, Info) ->
    ?LOG_DEBUG("st: ~s:~s at ~p", [M, F, props:get_value('line', Info, 0)]),
    _ = [?LOG_DEBUG("args: ~p", [Arg]) || Arg <- Args],
    'ok'.
