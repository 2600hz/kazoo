%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kz_ledgers).

-include("kzl.hrl").

-export([get/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) ->
                 {'ok', kz_json:object()} |
                 {'error', any()}.
get(Account) ->
    Options = ['reduce'
              ,'group'
              ,{'group_level', 1}
              ],
    case kazoo_modb:get_results(Account, ?TOTAL_BY_SERVICE_LEGACY, Options) of
        {'error', _R}=Error -> Error;
        {'ok', JObjs}->
            build_result(JObjs)
    end.

-spec build_result(kz_json:objects()) -> {'ok', kz_json:object()}.
build_result(JObjs) ->
    Data = lists:foldl(fun add_jobj/2, kz_json:new(), JObjs),
    {'ok', Data}.

-spec add_jobj(kz_json:object(), kz_json:object()) -> kz_json:object().
add_jobj(JObj, Acc) ->
    Key = kz_json:get_value(<<"key">>, JObj),
    Value = kz_json:get_value(<<"value">>, JObj),
    kz_json:set_value(Key, Value, Acc).
