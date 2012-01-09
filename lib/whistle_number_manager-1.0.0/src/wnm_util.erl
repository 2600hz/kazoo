%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_util).

-export([list_carrier_modules/0]).
-export([get_carrier_module/1]).
-export([number_to_db_name/1]).
-export([normalize_number/1]).

-include("../include/wh_number_manager.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a number doc determine if the carrier module is avaliable
%% and if return the name as well as the data
%% @end
%%--------------------------------------------------------------------
-spec get_carrier_module/1 :: (json_object()) -> {ok, atom(), json_object()} 
                                                     | {error, not_specified | unknown_module}.
get_carrier_module(JObj) ->
    case wh_json:get_value(<<"pvt_module_name">>, JObj) of
        undefined -> {error, not_specified};
        Module ->
            Carriers = list_carrier_modules(),
            Carrier = try_load_carrier_module(Module),
            case lists:member(Carrier, Carriers) of
                true -> {ok, Carrier, wh_json:get_value(<<"pvt_module_data">>, JObj, wh_json:new())};
                false -> {error, unknown_module}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all avaliable carrier modules
%% @end
%%--------------------------------------------------------------------
-spec list_carrier_modules/0 :: () -> [] | [atom(),...].
list_carrier_modules() ->
    CarrierModules = 
        whapps_config:get(?WNM_CONFIG_CAT, <<"carrier_modules">>, ?WNM_DEAFULT_CARRIER_MODULES),
    [Module || M <- CarrierModules, (Module = try_load_carrier_module(M)) =/= false].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a number determine the database name that it should belong to.
%% @end
%%--------------------------------------------------------------------
-spec number_to_db_name/1 :: (ne_binary()) -> ne_binary().
number_to_db_name(<<NumPrefix:5/binary, _/binary>>) ->
    wh_util:to_binary(
      http_uri:encode(
        wh_util:to_list(
          list_to_binary([?WNM_DB_PREFIX, NumPrefix])
         )
       )
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a provided term into a e164 binary string.
%% @end
%%--------------------------------------------------------------------
-spec normalize_number/1 :: (string() | binary()) -> binary().
normalize_number(Number) when is_binary(Number) ->
    wh_util:to_e164(Number);
normalize_number(Number) ->
    normalize_number(wh_util:to_binary(Number)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a module name try to verify its existance, loading it into the
%% the vm if possible.
%% @end
%%--------------------------------------------------------------------
-spec try_load_carrier_module/1 :: (string() | binary()) -> atom() | false.
try_load_carrier_module(Name) ->
    try
        Module  = wh_util:to_atom(Name),
        case erlang:module_loaded(Module) of
            true -> 
                Module;
            false -> 
                {module, Module} = code:ensure_loaded(Module),
                Module
        end
    catch
        error:badarg ->
            ?LOG_SYS("carrier module ~s not found", [Name]),
            case code:where_is_file(wh_util:to_list(<<Name/binary, ".beam">>)) of
                non_existing ->
                    ?LOG_SYS("beam file not found for ~s", [Name]),
                    false;
                _Path ->
                    ?LOG_SYS("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Name, true), %% put atom into atom table
                    try_load_carrier_module(Name)
            end;
        _:_ ->
            false
    end.
