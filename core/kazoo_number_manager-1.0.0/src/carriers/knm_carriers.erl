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
-spec find(ne_binary()) -> ne_binaries().
-spec find(ne_binary(), integer()) -> ne_binaries().
-spec find(ne_binary(), integer(), wh_proplist()) -> ne_binaries().

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

    [{Module, catch(Module:find_numbers(NormalizedNumber, Quantity, Opts))}
     || Module <- ?MODULE:list_modules()].

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