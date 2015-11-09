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

-include("../knm.hrl").

-export([find/1, find/2, find/3
         ,check/1, check/2
         ,available_carriers/0, available_carriers/1
         ,default_carriers/0, default_carrier/0
         ,acquire/1
         ,disconnect/1
        ]).

-define(DEFAULT_CARRIER_MODULES, [?CARRIER_LOCAL]).

-ifdef(TEST).
-export([process_carrier_results/2
         ,process_bulk_carrier_results/2
        ]).

-define(DEFAULT_CARRIER_MODULE
        ,?CARRIER_LOCAL
       ).
-define(CARRIER_MODULES, ?DEFAULT_CARRIER_MODULES).

-else.
-define(DEFAULT_CARRIER_MODULE
        ,whapps_config:get_binary(?KNM_CONFIG_CAT
                                  ,<<"available_module_name">>
                                  ,?CARRIER_LOCAL
                                 )
       ).
-define(CARRIER_MODULES
        ,whapps_config:get(?KNM_CONFIG_CAT
                           ,<<"carrier_modules">>
                           ,?DEFAULT_CARRIER_MODULES
                          )
       ).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary()) -> wh_json:objects().
-spec find(ne_binary(), integer()) -> wh_json:objects().
-spec find(ne_binary(), integer(), wh_proplist()) -> wh_json:objects().

find(Num) ->
    find(Num, 1).

find(Num, Quantity) ->
    find(Num, Quantity, []).

find(Num, Quantity, Options) ->
    NormalizedNumber = knm_converters:normalize(Num),

    lists:foldl(fun(Carrier, Acc) ->
                        find_fold(Carrier, Acc, NormalizedNumber, Quantity, Options)
                end
                ,[]
                ,?MODULE:available_carriers(Options)
               ).

-spec find_fold(atom(), wh_json:objects(), ne_binary(), non_neg_integer(), wh_proplist()) ->
                       wh_json:objects().
find_fold(Carrier, Acc, NormalizedNumber, Quantity, Options) ->
    try Carrier:find_numbers(NormalizedNumber, Quantity, Options) of
        {'ok', Numbers} -> process_carrier_results(Acc, Numbers);
        {'bulk', Numbers} -> process_bulk_carrier_results(Acc, Numbers);
        {'error', _E} -> Acc
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            ?LOG_WARN("failed to query carrier ~s for ~p numbers: ~s: ~p"
                      ,[Carrier, Quantity, _E, _R]
                     ),
            log_stacktrace(ST),
            Acc
    end.

-spec process_bulk_carrier_results(wh_json:objects(), knm_number:knm_numbers()) ->
                                          wh_json:objects().
process_bulk_carrier_results(Acc, Numbers) ->
    found_numbers_to_jobjs(Numbers) ++ Acc.

-spec process_carrier_results(wh_json:objects(), knm_number:knm_numbers()) ->
                                     wh_json:objects().
process_carrier_results(Acc, []) -> Acc;
process_carrier_results(Acc, Numbers) ->
    lists:reverse(
      lists:foldl(fun process_number_result/2
                  ,[]
                  ,Numbers
                 )
     ) ++ Acc.

-spec process_number_result(knm_number:knm_number(), wh_json:objects()) ->
                                   wh_json:objects().
process_number_result(Number, Acc) ->
    PhoneNumber = knm_number:phone_number(Number),
    process_number_result(Number, Acc, knm_phone_number:module_name(PhoneNumber)).

process_number_result(Number, Acc, ?CARRIER_OTHER) ->
    [found_number_to_jobj(Number) | Acc];
process_number_result(Number, Acc, Carrier) ->
    PhoneNumber = knm_number:phone_number(Number),
    DID = knm_phone_number:number(PhoneNumber),

    check_for_existing_did(Number, Acc, Carrier, knm_phone_number:fetch(DID)).

-spec check_for_existing_did(knm_number:knm_number(), wh_json:objects(), ne_binary(), number_return()) ->
                                    wh_json:objects().
check_for_existing_did(Number, Acc, _Carrier, {'error', 'not_found'}) ->
    create_discovery(Number, Acc);
check_for_existing_did(_Number, Acc, _Carrier, {'error', _}) ->
    Acc;
check_for_existing_did(Number, Acc, Carrier, {'ok', ExistingPhoneNumber}) ->
    case knm_phone_number:module_name(ExistingPhoneNumber) of
        Carrier -> check_existing_phone_number(Number, Acc, ExistingPhoneNumber);
        _OtherCarrier ->
            create_discovery(
              transition_existing_to_discovery(Number, ExistingPhoneNumber, Carrier)
              ,Acc
             )
    end.

-spec create_discovery(knm_number:knm_number(), wh_json:objects()) ->
                              wh_json:objects().
create_discovery(Number, Acc) ->
    DiscoveryUpdates =
        [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}],
    PhoneNumber = knm_number:phone_number(Number),
    DiscoveryNumber =
        knm_number:set_phone_number(
          Number
          ,knm_phone_number:setters(PhoneNumber, DiscoveryUpdates)
         ),
    collect_if_saved(DiscoveryNumber, Acc).

-spec collect_if_saved(knm_number:knm_number(), wh_json:objects()) ->
                              wh_json:objects().
collect_if_saved(DiscoveryNumber, Acc) ->
    case knm_number:save(DiscoveryNumber) of
        {'ok', SavedNumber} ->
            [found_number_to_jobj(SavedNumber) | Acc];
        {'error', _E} ->
            Acc
    end.

-spec transition_existing_to_discovery(knm_number:knm_number(), knm_phone_number:knm_number(), ne_binary()) ->
                                              knm_number:knm_number().
transition_existing_to_discovery(Number, ExistingPhoneNumber, Carrier) ->
    PhoneNumber = knm_number:phone_number(Number),
    knm_number:set_phone_number(
      Number
      ,knm_phone_number:setters(
         ExistingPhoneNumber
         ,[{fun knm_phone_number:set_module_name/2, knm_phone_number:module_name(PhoneNumber)}
           ,{fun knm_phone_number:set_carrier_data/2, knm_phone_number:carrier_data(PhoneNumber)}
           ,{fun knm_phone_number:set_module_name/2, Carrier}
          ]
        )
     ).

-spec check_existing_phone_number(knm_number:knm_number(), wh_json:objects(), knm_phone_number:knm_number()) -> wh_json:objects().
check_existing_phone_number(Number, Acc, PhoneNumber) ->
    case lists:member(knm_phone_number:state(PhoneNumber)
                      ,?KNM_AVAILABLE_STATES
                     )
    of
        'true' -> [found_number_to_jobj(Number) | Acc];
        'false' -> Acc
    end.

-spec found_numbers_to_jobjs(knm_number:knm_numbers()) -> wh_json:objects().
found_numbers_to_jobjs(Numbers) ->
    [found_number_to_jobj(Number) || Number <- Numbers].

-spec found_number_to_jobj(knm_number:knm_number()) -> wh_json:object().
found_number_to_jobj(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    found_number_to_jobj(PhoneNumber, knm_phone_number:module_name(PhoneNumber)).

-spec found_number_to_jobj(knm_phone_number:knm_number(), ne_binary()) ->
                                  wh_json:object().
found_number_to_jobj(PhoneNumber, ?CARRIER_MANAGED) ->
    CarrierData = knm_phone_number:carrier_data(PhoneNumber),
    wh_json:from_list(
      props:filter_undefined(
              [{<<"number">>, knm_phone_number:number(PhoneNumber)}
               ,{<<"rate">>, wh_json:get_value(<<"rate">>, CarrierData, <<"1">>)}
               ,{<<"activation_charge">>, wh_json:get_value(<<"activation_charge">>, CarrierData, <<"0">>)}
              ])
     );
found_number_to_jobj(PhoneNumber, _Carrier) ->
    DID = knm_phone_number:number(PhoneNumber),
    CarrierData = knm_phone_number:carrier_data(PhoneNumber),
    AssignTo = knm_phone_number:assign_to(PhoneNumber),

    wh_json:set_values(
      props:filter_undefined(
        [{<<"number">>, DID}
         ,{<<"activation_charge">>, activation_charge(DID, AssignTo)}
        ]
       )
      ,CarrierData
     ).

-spec activation_charge(ne_binary(), api_binary()) -> api_number().
-ifdef(TEST).
activation_charge(?START_BLOCK, _AccountId) -> 5.0;
activation_charge(?END_BLOCK, _AccountId) -> 'undefined';
activation_charge(_Number, _AccountId) -> 1.0.
-else.
activation_charge(_DID, 'undefined') -> 'undefined';
activation_charge(DID, AccountId) ->
    wh_services:activation_charges(<<"phone_numbers">>
                                   ,knm_converters:classify(DID)
                                   ,AccountId
                                  ).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for available numbers.
%% force leading +
%% @end
%%--------------------------------------------------------------------
-type checked_numbers() :: [{atom(), {'ok', wh_json:object()} |
                             {'error', _} |
                             {'EXIT', _}
                            }].
-spec check(ne_binaries()) -> checked_numbers().
-spec check(ne_binaries(), wh_proplist()) -> checked_numbers().
check(Numbers) ->
    check(Numbers, []).

check(Numbers, Options) ->
    FormattedNumbers = [knm_converters:normalize(Num) || Num <- Numbers],
    lager:info("attempting to check ~p ", [FormattedNumbers]),
    [{Module, catch(Module:check_numbers(FormattedNumbers, Options))}
     || Module <- ?MODULE:available_carriers(Options)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec available_carriers() -> atoms().
available_carriers() ->
    [Module
     || M <- ?CARRIER_MODULES,
        (Module = wh_util:try_load_module(M)) =/= 'false'
    ].

-ifdef(TEST).
available_carriers(Options) ->
    case props:get_value(<<"carriers">>, Options) of
        'undefined' -> available_carriers();
        [] -> available_carriers();
        Cs -> [Module
               || C <- Cs,
                  (Module = wh_util:try_load_module(C)) =/= 'false'
              ]
    end.
-else.
available_carriers(_Options) ->
    available_carriers().
-endif.

-spec default_carriers() -> atoms().
default_carriers() ->
    [Module
     || M <- ?DEFAULT_CARRIER_MODULES,
        (Module = wh_util:try_load_module(M)) =/= 'false'
    ].

-spec default_carrier() -> ne_binary().
default_carrier() ->
    ?DEFAULT_CARRIER_MODULE.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec acquire(knm_number:knm_number()) -> knm_number:knm_number().
-spec acquire(knm_number:knm_number(), ne_binary(), boolean()) ->
                     knm_number:knm_number().
acquire(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Module = knm_phone_number:module_name(PhoneNumber),
    DryRun = knm_phone_number:dry_run(PhoneNumber),
    acquire(Number, Module, DryRun).

acquire(Number, _Mod, 'true') ->
    Number;
acquire(Number, 'undefined', _DryRun) ->
    knm_errors:carrier_not_specified(Number);
acquire(Number, ?NE_BINARY = Mod, 'false') ->
    Module = carrier_module(Mod),
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
    Module = carrier_module(Number),
    Module:disconnect_number(Number).

-spec carrier_module(knm_number:knm_number() | ne_binary()) -> atom().
carrier_module(?NE_BINARY = Module) ->
    wh_util:to_atom(Module, 'true');
carrier_module(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    carrier_module(knm_phone_number:module_name(PhoneNumber)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
