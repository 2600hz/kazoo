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
         ,available_carriers/0, default_carriers/0
         ,acquire/1
         ,disconnect/1
        ]).

-ifdef(TEST).
-define(CARRIER_MODULES, ?DEFAULT_CARRIER_MODULES).
-else.
-define(CARRIER_MODULES
        ,whapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)
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
                ,?MODULE:available_carriers()
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
            lager:warning("failed to query carrier ~s for ~p numbers: ~s: ~p"
                          ,[Carrier, Quantity, _E, _R]
                         ),
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
    lists:foldl(fun process_number_result/2
                ,Acc
                ,Numbers
               ).

-spec process_number_result(knm_number:knm_number(), wh_json:objects()) ->
                                   wh_json:objects().
process_number_result(Number, Acc) ->
    PhoneNumber = knm_number:phone_number(Number),
    process_number_result(Number, Acc, knm_phone_number:module_name(PhoneNumber)).

process_number_result(Number, Acc, ?CARRIER_OTHER) ->
    [found_number_to_jobj(Number) | Acc].

found_numbers_to_jobjs(Numbers) ->
    [found_number_to_jobj(Number) || Number <- Numbers].

found_number_to_jobj(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
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
activation_charge(_DID, 'undefined') -> 'undefined';
activation_charge(DID, AccountId) ->
    wh_services:activation_charges(<<"phone_numbers">>
                                   ,knm_converters:classify(DID)
                                   ,AccountId
                                  ).

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
     || Module <- ?MODULE:available_carriers()
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

-spec default_carriers() -> atoms().
default_carriers() ->
    [Module
     || M <- ?DEFAULT_CARRIER_MODULES,
        (Module = wh_util:try_load_module(M)) =/= 'false'
    ].

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
