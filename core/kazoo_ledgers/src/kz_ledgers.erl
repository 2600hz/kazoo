%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz, INC
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
-spec get(ne_binary()) -> {'error', any()} | {'ok', kz_json:object()}.
get(Account) ->
    Options = [
               'reduce'
              ,'group'
              ,{'group_level', 1}
              ],
    case kazoo_modb:get_results(Account, ?TOTAL_BY_SERVICE_LEGACY, Options) of
        {'error', _R}=Error -> Error;
        {'ok', JObjs}->
            Data =
                lists:foldl(
                  fun(JObj, Acc) ->
                          Key = kz_json:get_value(<<"key">>, JObj),
                          Value = kz_json:get_value(<<"value">>, JObj),
                          kz_json:set_value(Key, Value, Acc)
                  end
                           ,kz_json:new()
                           ,JObjs
                 ),
            {'ok', Data}
    end.
