%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
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
                 {'ok', wh_json:object()} |
                 {'error', any()}.
get(Account) ->
    Options = ['reduce'
               ,'group'
               ,{'group_level', 1}
              ],
    case kazoo_modb:get_results(Account, ?LIST_BY_TYPE, Options) of
        {'error', _R}=Error -> Error;
        {'ok', JObjs}->
            Data =
                lists:foldl(fun process_result/2
                            ,wh_json:new()
                            ,JObjs
                           ),
            {'ok', Data}
    end.

-spec process_result(wh_json:object(), wh_json:object()) -> wh_json:object().
process_result(JObj, Acc) ->
    Key = wh_json:get_value(<<"key">>, JObj),
    Value = wh_json:get_value(<<"value">>, JObj),
    wh_json:set_value(Key, Value, Acc).
