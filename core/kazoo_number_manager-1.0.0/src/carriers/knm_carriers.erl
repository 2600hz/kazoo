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
         ,list_modules/0
         ,acquire/1
         ,disconnect/1
        ]).

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

find(Num, Quantity, Opts) ->
    AccountId = props:get_value(<<"Account-ID">>, Opts),
    NormalizedNumber = knm_converters:normalize(Num),

    lager:info(
      "attempting to find ~p numbers with prefix '~s' for account ~p"
      ,[Quantity, NormalizedNumber, AccountId]
     ),

    lists:foldl(
      fun(Mod, Acc) ->
              format_find_resp(
                Mod
                ,attempt_find(Mod, NormalizedNumber, Quantity, Opts)
                ,Acc
               )
      end
      ,[]
      ,?MODULE:list_modules()
     ).

-spec attempt_find(atom(), ne_binary(), integer(), wh_proplist()) ->
                          {'ok', knm_phone_number:knm_numbers()} |
                          {'error', _}.
attempt_find(Mod, NormalizedNumber, Quantity, Opts) ->
    try Mod:find_numbers(NormalizedNumber, Quantity, Opts) of
        Resp -> Resp
    catch
        _E:R ->
            lager:warning("failed to find ~s: ~s: ~p", [NormalizedNumber, _E, R]),
            {'error', R}
    end.

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

check(Numbers, Opts) ->
    FormattedNumbers = [knm_converters:normalize(Num) || Num <- Numbers],
    lager:info("attempting to check ~p ", [FormattedNumbers]),
    [{Module, catch(Module:check_numbers(FormattedNumbers, Opts))}
     || Module <- ?MODULE:list_modules()
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec list_modules() -> atoms().
list_modules() ->
    CarrierModules =
        whapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES),
    [Module
     || M <- CarrierModules,
        (Module = wh_util:try_load_module(M)) =/= 'false'
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec acquire(knm_phone_number:knm_number()) -> knm_number:knm_number().
-spec acquire(knm_phone_number:knm_number(), ne_binary(), boolean()) ->
                     knm_number:knm_number().
acquire(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Module = knm_phone_number:module_name(PhoneNumber),
    DryRun = knm_phone_number:dry_run(PhoneNumber),
    acquire(Number, Module, DryRun).

acquire(Number, 'undefined', _DryRun) ->
    knm_errors:carrier_not_specified(Number);
acquire(Number, _Mod, 'true') ->
    Number;
acquire(Number, <<_/binary>> = Mod, 'false') ->
    Module = wh_util:to_atom(Mod, 'true'),
    {'ok', Number1} = Module:acquire_number(Number),
    Number1.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec disconnect(knm_phone_number:knm_number()) -> number_return().
disconnect(Number) ->
    Mod = knm_phone_number:module_name(Number),
    Module = wh_util:to_atom(Mod, 'true'),
    Module:disconnect_number(Number).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type find_resp() :: {'ok', knm_phone_number:knm_numbers()} |
                     {'error', _}.

-spec format_find_resp(atom(), find_resp(), wh_json:objects()) ->
                              wh_json:objects().
format_find_resp(_Module, {'ok', Numbers}, Acc) ->
    lager:debug("found numbers in ~p", [_Module]),
    lists:reverse([knm_phone_number:to_public_json(Number) || Number <- Numbers]) ++ Acc;
format_find_resp(_Module, {'error', _Reason}, Acc) ->
    lager:error("failed to find number in ~p : ~p", [_Module, _Reason]),
    Acc.
