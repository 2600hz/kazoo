%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kz_ledgers).

-include("kzl.hrl").

-export([get/1, get/3
        ,available_ledgers/1
        ]).

-define(DEFAULT_AVIALABLE_LEDGERS,
        [kz_json:from_list([{<<"name">>, <<"per-minute-voip">>}
                           ,{<<"friendly_name">>, <<"Per Minute VoIP">>}
                           ,{<<"markup_type">>, [<<"percentage">>]}
                           ])
        ]
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec get(kz_term:ne_binary()) -> {'ok', kz_json:object()} |
                                  {'error', atom()}.
get(Account) ->
    get(Account, undefined, undefined).

-spec get(kz_term:ne_binary(), kz_time:api_seconds(), kz_time:api_seconds()) -> {'ok', kz_json:object()} |
                                                                                {'error', atom()}.
get(Account, undefined, undefined) ->
    case kazoo_modb:get_results(Account, ?TOTAL_BY_SERVICE_LEGACY, [group]) of
        {'error', _R}=Error -> Error;
        {'ok', JObjs}->
            LedgersJObj = kz_json:from_list(
                            [{kz_json:get_value(<<"key">>, JObj), kz_json:get_value(<<"value">>, JObj)}
                             || JObj <- JObjs
                            ]),
            {ok, LedgersJObj}
    end;

get(Account, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MoDBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    lager:debug("from:~p to:~p -> ~p", [CreatedFrom, CreatedTo, MoDBs]),
    try
        Sum = kz_json:sum_jobjs(
                [case kazoo_modb:get_results(MoDB, ?LIST_BY_SERVICE_LEGACY, []) of
                     {error, Reason} -> throw(Reason);
                     {ok, JObjs} ->
                         kz_json:sum_jobjs([kz_json:get_value(<<"value">>, JObj)
                                            || JObj <- JObjs,
                                               [_Type, TimeStamp] <- [kz_json:get_value(<<"key">>, JObj)],
                                               CreatedFrom =< TimeStamp,
                                               TimeStamp =< CreatedTo
                                           ])
                 end
                 || MoDB <- MoDBs
                ]),
        {ok, Sum}
    catch
        throw:_R -> {error, _R}
    end.

-spec available_ledgers(kz_term:api_binary()) -> kz_json:objects().
available_ledgers(AccountId) ->
    kapps_account_config:get_global(AccountId, <<"ledgers">>, <<"registered_ledgers">>, ?DEFAULT_AVIALABLE_LEDGERS).
