%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ledgers).

-include("kzl.hrl").

-export([get/1, get/3
        ,get_ranged/1
        ,available_ledgers/1
        ]).

-define(DEFAULT_AVIALABLE_LEDGERS,
        [kz_json:from_list([{<<"name">>, <<"per-minute-voip">>}
                           ,{<<"friendly_name">>, <<"Per Minute VoIP">>}
                           ,{<<"markup_type">>, [<<"percentage">>]}
                           ])
        ]
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

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
    MODBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    Options = [{databases, MODBs}
              ,{startkey, CreatedFrom}
              ,{endkey, CreatedTo}
              ],
    get_ranged(Options).

-spec get_ranged(kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
get_ranged(Options) ->
    MODBs = props:get_value(databases, Options, []),
    ViewOptions = props:filter_undefined(props:delete(databases, Options)),

    lager:debug("from: ~p to: ~p in direction ~p -> ~p"
               ,[props:get_value('startkey', ViewOptions)
                ,props:get_value('endkey', ViewOptions)
                ,props:get_value('direction', ViewOptions, 'ascending')
                ,MODBs
                ]),

    try
        MODBs =:= []
            andalso throw('no_account_db'),
        Sum = kz_json:sum_jobjs(
                [case kazoo_modb:get_results(MODB, <<"ledgers/list_by_timestamp_legacy">>, Options) of
                     {error, Reason} -> throw(Reason);
                     {ok, JObjs} -> kz_json:sum_jobjs([kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs])
                 end
                 || MODB <- MODBs
                ]),
        {ok, Sum}
    catch
        throw:_R -> {error, _R}
    end.

-spec available_ledgers(kz_term:api_binary()) -> kz_json:objects().
available_ledgers(AccountId) ->
    kapps_account_config:get_global(AccountId, <<"ledgers">>, <<"registered_ledgers">>, ?DEFAULT_AVIALABLE_LEDGERS).
