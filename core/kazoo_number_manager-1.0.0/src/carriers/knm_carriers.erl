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

-export([find/1, find/2, find/3]).
-export([check/1, check/2]).
-export([list_modules/0]).
-export([maybe_acquire/1]).
-export([disconnect/1]).

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
                ,catch(Mod:find_numbers(NormalizedNumber, Quantity, Opts))
                ,Acc
            )
        end
        ,[]
        ,?MODULE:list_modules()
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for available numbers.
%% force leading +
%% @end
%%--------------------------------------------------------------------
-spec check(ne_binaries()) -> wh_json:object().
-spec check(ne_binaries(), wh_proplist()) -> wh_json:object().
check(Numbers) ->
    check(Numbers, []).

check(Numbers, Opts) ->
    FormattedNumbers = [knm_converters:normalize(Num) || Num <- Numbers],
    lager:info("attempting to check ~p ", [FormattedNumbers]),
    [{Module, catch(Module:check_numbers(FormattedNumbers, Opts))}
     || Module <- ?MODULE:list_modules()].

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
    [Module || M <- CarrierModules, (Module = wh_util:try_load_module(M)) =/= 'false'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec maybe_acquire(number()) -> number_return().
-spec maybe_acquire(number(), api_binary(), boolean()) -> number_return().
maybe_acquire(Number) ->
    Module = knm_phone_number:module_name(Number),
    DryRun = knm_phone_number:dry_run(Number),
    maybe_acquire(Number, Module, DryRun).

maybe_acquire(_Number, 'undefined', _DryRun) -> {'error', 'undefined_module'};
maybe_acquire(Number, _Mod, 'true') -> {'ok', Number};
maybe_acquire(Number, Mod, 'false') ->
    Module = wh_util:to_atom(Mod, 'true'),
    Module:acquire_number(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec disconnect(number()) -> number_return().
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
-spec format_find_resp(atom(), any(), wh_json:objects()) -> wh_json:objects().
format_find_resp(Module, {'ok', JObj}, Acc) ->
    format_module_resp(Module, JObj) ++ Acc;
format_find_resp(_Module, {'error', _Reason}, Acc) ->
    lager:error("failed to find number in ~p : ~p", [_Module, _Reason]),
    Acc;
format_find_resp(_Module, _Data, Acc) ->
    lager:error("unknown return for module ~p", [_Module]),
    lager:error("~p", [_Data]),
    Acc.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_module_resp(atom(), wh_json:object()) -> wh_json:objects().
format_module_resp(Module, JObj) ->
    wh_json:foldl(
        fun(Num, Data, Acc) ->
           Number = default_module_resp(Module, Num, Data),
           [specific_module_resp(Module, Number, Data)|Acc]
        end
        ,[]
        ,JObj
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_module_resp(atom(), ne_binary(), wh_json:object()) -> number().
default_module_resp(Module, Num, Data) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates = [
        {fun knm_phone_number:set_number/2, NormalizedNum}
        ,{fun knm_phone_number:set_number_db/2, NumberDb}
        ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(Module)}
        ,{fun knm_phone_number:set_carrier_data/2, Data}
        ,{fun knm_phone_number:set_number_db/2, NumberDb}
    ],
    knm_phone_number:setters(knm_phone_number:new(), Updates).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec specific_module_resp(atom(), ne_binary(), wh_json:object()) -> wh_json:object().
specific_module_resp(Module, Number, _Data) when Module =:= 'wnm_bandwidth'
                                            orelse Module =:= 'wnm_vitelity' ->
    lager:warning("specific format for ~s module ~p", [knm_phone_number:number(Number), Module]),
    Updates = [
        {fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
        ,fun knm_phone_number:to_public_json/1
    ],
    knm_phone_number:setters(Number, Updates);
specific_module_resp(_Module, Number, _Data) ->
    lager:warning("no specific format for ~s module ~p", [knm_phone_number:number(Number), _Module]),
    knm_phone_number:to_public_json(Number).