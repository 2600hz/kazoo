%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_carriers).

-include_lib("kazoo/src/kz_json.hrl").
-include("knm.hrl").

-export([find/1, find/2, find/3
         ,check/1, check/2
         ,available_carriers/1
         ,default_carriers/0, default_carrier/0
         ,acquire/1
         ,disconnect/1
        ]).

%%% For knm carriers only
-export([create_discovery/4]).

-define(DEFAULT_CARRIER_MODULES, [?CARRIER_LOCAL]).

-ifdef(TEST).
-export([process_carrier_results/2
         ,process_bulk_carrier_results/2
        ]).
-endif.

-define(DEFAULT_CARRIER_MODULE
       ,kapps_config:get_binary(?KNM_CONFIG_CAT, <<"available_module_name">>, ?CARRIER_LOCAL)).
-define(CARRIER_MODULES
       ,kapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary()) -> kz_json:objects().
-spec find(ne_binary(), integer()) -> kz_json:objects().
-spec find(ne_binary(), integer(), kz_proplist()) -> kz_json:objects().

find(Num) ->
    find(Num, 1).

find(Num, Quantity) ->
    find(Num, Quantity, []).

find(Num, Quantity, Options) ->
    NormalizedNumber = knm_converters:normalize(Num),
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
                            find_fold(Carrier, NormalizedNumber, Options, Acc)
                    end
                   ,Acc0
                   ,Carriers
                   ),
    lager:debug("found ~p/~p numbers", [_Count, Quantity]),
    Found.

-type find_acc() :: #{found => kz_json:objects()
                     ,count => non_neg_integer()
                     ,left => pos_integer()
                     ,should_continue => boolean()
                     }.
-spec find_fold(atom(), ne_binary(), kz_proplist(), find_acc()) -> find_acc().
find_fold(_Carrier, _, _, Acc=#{should_continue := ShouldContinue
                               ,count := _Count
                               ,left := Left
                               })
  when ShouldContinue == 'false'; Left < 1 ->
    lager:debug("stopping ~s with ~p (~p) numbers found", [_Carrier, _Count, Left]),
    Acc;
find_fold(Carrier, NormalizedNumber, Options, Acc=#{left := Quantity}) ->
    try Carrier:find_numbers(NormalizedNumber, Quantity, Options) of
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

-spec process_number_result(knm_number:knm_number(), kz_json:objects()) ->
                                   kz_json:objects().
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
                                      ,kz_json:objects()) ->
                                              kz_json:objects().
transition_existing_to_discovery(Number, ExistingPhoneNumber, Acc) ->
    PhoneNumber0 = knm_number:phone_number(Number),
    {'ok', PhoneNumber} =
        knm_phone_number:setters(
          ExistingPhoneNumber
          ,[{fun knm_phone_number:set_module_name/2, knm_phone_number:module_name(PhoneNumber0)}
           ,{fun knm_phone_number:set_carrier_data/2, knm_phone_number:carrier_data(PhoneNumber0)}
           ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
           ]
         ),
    case knm_number:save(knm_number:set_phone_number(Number, PhoneNumber)) of
        {'ok', SavedNumber} ->
            [found_number_to_jobj(SavedNumber) | Acc];
        {'error', _R} ->
            lager:debug("skipping number ~s: ~p", [knm_phone_number:number(PhoneNumber), _R]),
            Acc
    end.

-spec found_number_to_jobj(knm_number:knm_number()) -> kz_json:object().
found_number_to_jobj(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    CarrierData = knm_phone_number:carrier_data(PhoneNumber),
    DID = knm_phone_number:number(PhoneNumber),
    case knm_phone_number:module_name(PhoneNumber) of
        ?CARRIER_MANAGED ->
            kz_json:from_list(
              props:filter_undefined(
                [{<<"number">>, DID}
                ,{<<"rate">>, kz_json:get_value(<<"rate">>, CarrierData, <<"1">>)}
                ,{<<"activation_charge">>, kz_json:get_value(<<"activation_charge">>, CarrierData, <<"0">>)}
                ,{<<"state">>, knm_phone_number:state(PhoneNumber)}
                ])
             );
        _Carrier ->
            AssignTo = knm_phone_number:assign_to(PhoneNumber),
            Add = props:filter_undefined(
                    [{<<"number">>, DID}
                    ,{<<"activation_charge">>, activation_charge(DID, AssignTo)}
                    ,{<<"state">>, knm_phone_number:state(PhoneNumber)}
                    ]),
            kz_json:set_values(Add, CarrierData)
    end.

-spec activation_charge(ne_binary(), api_binary()) -> api_number().
-ifdef(TEST).
activation_charge(?START_BLOCK, _AccountId) -> 5.0;
activation_charge(?END_BLOCK, _AccountId) -> 'undefined';
activation_charge(_Number, _AccountId) -> 1.0.
-else.
activation_charge(_DID, 'undefined') -> 'undefined';
activation_charge(DID, AccountId) ->
    kz_services:activation_charges(<<"phone_numbers">>
                                   ,knm_converters:classify(DID)
                                   ,AccountId
                                  ).
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
-spec check(ne_binaries(), kz_proplist()) -> checked_numbers().
check(Numbers) ->
    check(Numbers, []).

check(Numbers, Options) ->
    FormattedNumbers = [knm_converters:normalize(Num) || Num <- Numbers],
    lager:info("attempting to check ~p ", [FormattedNumbers]),
    [{Module, catch(Module:check_numbers(FormattedNumbers, Options))}
     || Module <- available_carriers(Options)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc Create a list of all available carrier modules
%%--------------------------------------------------------------------
-spec available_carriers(kz_proplist()) -> atoms().
-ifdef(TEST).
available_carriers(Options) ->
    case props:get_value(<<"carriers">>, Options) of
        Cs=[_|_] -> keep_only_reachable(Cs);
        _ -> get_available_carriers(Options)
    end.
-else.
available_carriers(Options) ->
    get_available_carriers(Options).
-endif.

-spec get_available_carriers(kz_proplist()) -> atoms().
get_available_carriers(Options) ->
    case props:get_value(?KNM_ACCOUNTID_CARRIER, Options) of
        'undefined' ->
            keep_only_reachable(?CARRIER_MODULES);
        AccountId ->
            ResellerId = kz_services:find_reseller_id(AccountId),
            lager:debug("found ~s's reseller: ~p", [AccountId, ResellerId]),
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            case ResellerId == MasterAccountId of
                'true' ->
                    First = [?CARRIER_RESERVED, ?CARRIER_LOCAL],
                    keep_only_reachable(
                      First ++
                          (?CARRIER_MODULES -- (First ++ [?CARRIER_RESERVED_RESELLER]))
                     );
                'false' ->
                    First = [?CARRIER_RESERVED, ?CARRIER_RESERVED_RESELLER],
                    keep_only_reachable(
                      First ++
                          (?CARRIER_MODULES -- (First ++ [?CARRIER_LOCAL]))
                     )
            end
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
%% Buy a number from its carrier module
%% @end
%%--------------------------------------------------------------------
-spec acquire(knm_number:knm_number()) -> knm_number:knm_number().
acquire(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Module = knm_phone_number:module_name(PhoneNumber),
    DryRun = knm_phone_number:dry_run(PhoneNumber),
    acquire(Number, Module, DryRun).

-spec acquire(knm_number:knm_number(), ne_binary(), boolean()) ->
                     knm_number:knm_number().
acquire(Number, 'undefined', _DryRun) ->
    knm_errors:carrier_not_specified(Number);
acquire(Number, _Mod, 'true') ->
    Number;
acquire(Number, ?NE_BINARY = Mod, 'false') ->
    Module = carrier_module(Mod),
    lager:debug("contacting carrier ~s", [Module]),
    Module:acquire_number(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec disconnect(knm_number:knm_number()) ->
                        knm_number:knm_number().
disconnect(Number) ->
    case knm_phone_number:module_name(knm_number:phone_number(Number)) of
        ?NE_BINARY=Mod ->
            Module = kz_util:to_atom(Mod, 'true'),
            lager:debug("contacting carrier ~s", [Module]),
            Module:disconnect_number(Number);
        _Mod ->
            lager:debug("non-existant carrier module ~p, allowing disconnect", [_Mod]),
            Number
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a number in a discovery state.
%% @end
%%--------------------------------------------------------------------
-spec create_discovery(ne_binary(), module(), ne_binary(), kz_json:object()) ->
                              knm_number_return().
create_discovery(DID=?NE_BINARY, Carrier, ?MATCH_ACCOUNT_RAW(AuthBy), Data=?JSON_WRAPPER(_))
  when is_atom(Carrier) ->
    case knm_number:get(DID) of
        {'ok', _Number}=Ok -> Ok;
        {'error', 'not_found'} ->
            NormalizedNum = knm_converters:normalize(DID),
            {'ok', PhoneNumber} =
                knm_phone_number:setters(
                  knm_phone_number:new()
                  ,[{fun knm_phone_number:set_number/2, NormalizedNum}
                   ,{fun knm_phone_number:set_number_db/2, knm_converters:to_db(NormalizedNum)}
                   ,{fun knm_phone_number:set_assign_to/2, 'undefined'}
                   ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
                   ,{fun knm_phone_number:set_module_name/2, kz_util:to_binary(Carrier)}
                   ,{fun knm_phone_number:set_carrier_data/2, Data}
                   ,{fun knm_phone_number:set_auth_by/2, AuthBy}
                   ]),
            knm_number:save(knm_number:set_phone_number(knm_number:new(), PhoneNumber))
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
is_local(?CARRIER_LOCAL) -> 'true';
is_local(?CARRIER_RESERVED) -> 'true';
is_local(?CARRIER_RESERVED_RESELLER) -> 'true';
is_local(?CARRIER_MANAGED) -> 'true';
is_local(?CARRIER_INUM) -> 'true';
is_local(_ForeignCarrier) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec carrier_module(knm_number:knm_number() | ne_binary()) -> atom().
carrier_module(?NE_BINARY = Module) ->
    kz_util:to_atom(Module, 'true');
carrier_module(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    carrier_module(knm_phone_number:module_name(PhoneNumber)).

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
