%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stats_handler).

-include("stats.hrl").

-export([get_db/1
        ,handle_event/2
        ,send/1
        ,get_next/3
        ,handle_req/2
        ]).

-include("stats.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'no_node_name' |
          'no_tables' |
          'no_items' |
          'ok'.
handle_req(JObj, _Props) ->
    Items = kz_json:recursive_to_proplist(JObj),
    Nodename = props:get_value(<<"nodename">>, Items),
    store_items(Nodename, table_def(), Items).

-spec handle_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_event(JObj, Props) ->
    lager:debug("event occurred ~p ~p",[JObj, Props]).

-spec get_db(kz_term:ne_binary()) -> list().
get_db(Table) ->
    case kz_cache:peek_local(?CACHE_NAME, Table) of
        {'ok', Records} -> Records;
        _ -> []
    end.

save_db(_, []) -> 'ok';
save_db(Table, Values) ->
    lager:debug("trying to store in ~s: ~p", [Table, Values]),
    kz_cache:store_local(?CACHE_NAME, Table, Values).

store_items('undefined', _, Vals) ->
    lager:debug("cannot find the node name ~p~n",[Vals]),
    'no_node_name';
store_items(_, [], _) -> 'no_tables';
store_items(_, _, []) -> 'no_items';
store_items(NodeName, [{<<"sip-domain">>,_} | Rest], Items) ->
    case props:get_value(<<"sip">>,Items) of
        'undefined' -> 'ok';
        SipItems ->
            NewDb = store_item2([{<<"nodename">>,NodeName} | SipItems],
                                get_db(<<"sip">>), NodeName),
            save_db(<<"sip">>, NewDb),
            update_domaindb(NewDb)
    end,
    store_items(NodeName, Rest, Items);
store_items(NodeName, [{TableName,TableField} | Rest], Items) ->
    case lists:filter(fun({X, _}) ->
                              lists:keymember(X, 1, TableField)
                      end, Items)
    of
        [{<<"nodename">>, _}] -> 'ok';
        TableItems ->
            NewDb = store_item2(TableItems, get_db(TableName), NodeName),
            save_db(TableName, NewDb),
            store_items(NodeName, Rest, Items)
    end.

%%% Store information ordered by node name so table row order is consistent
store_item2(NewItems, [], _) ->
    [NewItems];
store_item2(NewItems, [ Table | Rest ], NodeName ) ->
    case props:get_value(<<"nodename">>, Table) of
        NodeName -> [ NewItems | Rest ];
        Node when NodeName < Node -> [ NewItems , Table | Rest ];
        _ -> [Table | store_item2(NewItems, Rest, NodeName)]
    end.

update_domaindb(OldSip) ->
    NewSip = lists:foldl(fun collect_items/2, [], OldSip),
    NewSip2 = [[{<<"domain-name">>,SipDomain} | Attr]
               || {SipDomain,Attr} <- NewSip
              ],
    save_db(<<"sip-domain">>, NewSip2).

collect_items([], Db) -> Db;
collect_items([{<<"nodename">>,_} | Rest], Db) -> collect_items(Rest, Db);
collect_items([{Domain, Items} | Rest], Db) ->
    DomainData = props:get_value(Domain, Db, []),
    Fun = fun(Key, Value) ->
                  props:get_value(Key, DomainData, 0) + Value
          end,
    NewData = [{Key, Fun(Key,Value)} || {Key, Value} <- Items]
        ++ [{Key, Value} || {Key, Value} <- DomainData,
                            not lists:keymember(Key, 1, Items)
           ],
    collect_items(Rest, lists:keystore(Domain, 1, Db, {Domain, NewData})).

-spec send(kz_json:objects() | kz_term:ne_binary()) -> 'ok'.
send(Payload) when is_list(Payload) -> send(kz_json:encode(Payload));
send(Payload) -> kz_amqp_util:targeted_publish(<<"statistics">>, Payload).

-spec get_next(kz_term:ne_binary(), list(), list()) -> ['endOfTable' | {list(), list()}].
get_next(Table, Row, Col)
  when is_list(Col), is_list(Row), is_binary(Table) ->
    get_next2(Row, Col, get_db(Table), table_order(Table)).

get_next2(_, Cols, [], _) -> ['endOfTable' || _ <- Cols];
get_next2([], _, Table, Order) ->
    [{[1, 1], value(1, 1, Table, Order)}];
get_next2([Row], Cols, Table, Order) ->
    MaxRow = length(Table),
    case Row < MaxRow of
        'true' ->
            [ {[Col, Row + 1], value(Row + 1, Col, Table, Order)}
              || Col <- Cols
            ];
        'false' ->
            MaxCol = length(Order),
            [ case Col < MaxCol of
                  'true' -> {[Col + 1, 1], value(1, Col + 1, Table, Order)};
                  'false' -> 'endOfTable'
              end
              || Col <- Cols
            ]
    end.

value(_, _, Table, []) ->
    lager:error("trying to read from undefined table ~p", [Table]),
    0;
value(Row, Col, Table, Order) ->
    {Key, Default} = lists:nth(Col, Order),
    Val = props:get_value(Key, lists:nth(Row, Table), Default),
    case is_binary(Val) of
        'true' -> binary_to_list(Val);
        'false' -> Val
    end.

%%% Map the OID order of the items in tables in KAZOO-MIB.mib to the tuples
%%% in the lists. The second element is the default value is the tuple is
%%% missing in the list.
table_def() ->
    [{<<"vm">>,
      [{<<"nodename">>, <<"">>}
      ,{<<"erlang-version">>, <<"">>}
      ,{<<"memory-total">>, 0}
      ,{<<"memory-processes">>, 0}
      ,{<<"memory-system">>, 0}
      ,{<<"memory-atom">>, 0}
      ,{<<"memory-binary">>, 0}
      ,{<<"memory-code">>, 0}
      ,{<<"memory-ets">>, 0}
      ,{<<"amqp-error">>, 0}
      ,{<<"amqp-request">>, 0}
      ,{<<"bigcouch-504-error">>, 0}
      ,{<<"bigcouch-other-error">>, 0}
      ,{<<"bigcouch-request">>, 0}
      ]},
     {<<"ecallmgr">>,
      [{<<"nodename">>, <<"">>}
      ,{<<"reduction">>, 0}
      ,{<<"processes">>, 0}
      ,{<<"register-attempt">>, 0}
      ,{<<"register-fail">>, 0}
      ,{<<"presence">>, 0}
      ]},
     {<<"sip-domain">>,
      [{<<"domain-name">>, <<"">>}
      ,{<<"RECOVERY_ON_TIMER_EXPIRE">>, 0}
      ,{<<"PROGRESS_TIMEOUT">>, 0}
      ,{<<"UNALLOCATED_NUMBER">>, 0}
      ,{<<"NO_ROUTE_DESTINATION">>, 0}
      ,{<<"NORMAL_CLEARING">>, 0}
      ,{<<"ORIGINATOR_CANCEL">>, 0}
      ,{<<"DESTINATION_OUT_OF_ORDER">>, 0}
      ,{<<"REQUESTED_CHAN_UNAVAIL">>, 0}
      ,{<<"NO_ANSWER">>, 0}
      ,{<<"INVALID_NUMBER_FORMAT">>, 0}
      ,{<<"INCOMPATIBLE_DESTINATION">>, 0}
      ]}].

table_order(Table) ->
    props:get_value(Table, table_def(), []).
