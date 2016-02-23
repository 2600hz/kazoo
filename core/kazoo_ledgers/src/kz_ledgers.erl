%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600HZ, INC
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
-spec get(ne_binary()) -> {'error', any()} | {'ok', wh_json:object()}.
get(Account) ->
    Options = [
        'reduce'
        ,'group'
        ,{'group_level', 1}
    ],
    case kazoo_modb:get_results(Account, ?LIST_BY_SERVICE, Options) of
        {'error', _R}=Error -> Error;
        {'ok', JObjs}->
            Data =
                lists:foldl(
                    fun(JObj, Acc) ->
                        Key = wh_json:get_value(<<"key">>, JObj),
                        Value = wh_json:get_value(<<"value">>, JObj),
                        wh_json:set_value(Key, Value, Acc)
                    end
                    ,wh_json:new()
                    ,JObjs
                ),
            {'ok', Data}
    end.
