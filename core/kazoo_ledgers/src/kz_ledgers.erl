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

-export([get/1, get/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> {'ok', kz_json:object()} |
                          {'error', atom()}.
-spec get(ne_binary(), api_seconds(), api_seconds()) -> {'ok', kz_json:object()} |
                                                        {'error', atom()}.
get(Account) ->
    get(Account, undefined, undefined).

get(Account, undefined, undefined) ->
    case kazoo_modb:get_results(Account, ?TOTAL_BY_SERVICE_LEGACY, [group]) of
        {'error', _R}=Error -> Error;
        {'ok', JObjs}-> {'ok', build_result(JObjs)}
    end;

get(Account, CreatedFrom, CreatedTo) ->
    MoDBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    lager:debug("from:~p to:~p -> ~p", [CreatedFrom, CreatedTo, MoDBs]),
    try
        Sum = kz_json:sum_jobjs(
                [case kazoo_modb:get_results(MoDB, ?TOTAL_BY_SERVICE_LEGACY, [group]) of
                     {error, Reason} -> throw(Reason);
                     {ok, JObjs} -> build_result(JObjs)
                 end
                 || MoDB <- MoDBs
                ]),
        {ok, Sum}
    catch
        throw:_R -> {error, _R}
    end.

-spec build_result(kz_json:objects()) -> kz_json:object().
build_result(JObjs) ->
    kz_json:from_list(
      [{kz_json:get_value(<<"key">>, JObj), kz_json:get_value(<<"value">>, JObj)}
       || JObj <- JObjs
      ]).
