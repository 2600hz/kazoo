%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% @doc
%%% Master process to gather information send by all kazoo nodes.
%%% It listens to targeted/statistics AMPQ messages, and is accessed by
%%% whistle_snmp for SNMP queries.
%%%-------------------------------------------------------------------
-module(stats_handler).

-define(STATS_CACHE,'stats_cache').

%% API
-export([get_db/1
	 ,handle_event/2
	 ,send/1
	 ,get_next/3
	 ,handle_req/2
	]).

handle_req(JObj,_Props) ->
    Items = wh_json:recursive_to_proplist(JObj),
    lager:debug("Got items ~p~n",[Items]),
    store_items(props:get_value(<<"nodename">>,Items),table_def(),Items).

handle_event(JObj,Props) ->
    lager:debug("Event occurred ~p ~p~n",[JObj,Props]).

get_db(Table) ->
    case wh_cache:peek_local(?STATS_CACHE,Table) of
	{ok, Records} ->
	    Records;
	_ ->
	    []
    end.

save_db(_,[]) ->
    nothing;
save_db(Table,Values) ->
    lager:debug("Trying to store in ~s~n~p~n",[Table,Values]),
    wh_cache:store_local(?STATS_CACHE,Table,Values).

store_items(undefined,_,Vals) ->
    lager:debug("Cannot find the node name ~p~n",[Vals]),
    no_node_name;
store_items(_,[],_) ->
    no_tables;
store_items(_,_,[]) ->
    no_items;
store_items(NodeName, [{<<"sip-domain">>,_} | Rest],Items) ->
    case props:get_value(<<"sip">>,Items) of
	undefined ->
	    nothing;
	SipItems ->
	    NewDb = store_item2([{<<"nodename">>,NodeName} | SipItems],
				get_db(<<"sip">>),NodeName),
	    save_db(<<"sip">>,NewDb),
	    update_domaindb(NewDb)
    end,
    store_items(NodeName,Rest,Items);
store_items(NodeName, [{TableName,TableField} | Rest], Items) ->
    TableItems = lists:filter(fun({X,_}) -> 
				      lists:keymember(X,1,TableField) 
			      end,Items),
    case TableItems of
	[{<<"nodename">>,_}] -> nothing;
	_ ->
	    NewDb = store_item2(TableItems,get_db(TableName),NodeName),
	    save_db(TableName,NewDb),
	    store_items(NodeName,Rest,Items)
    end.

%%% Store information ordered by node name so table row order is consistant
store_item2(NewItems, [], _) ->
    [ NewItems ];
store_item2(NewItems,[ Table | Rest ], NodeName ) ->
    case props:get_value( <<"nodename">>,Table) of
	NodeName ->
	    [ NewItems | Rest ];
	Node ->
	    if NodeName < Node ->
		    [ NewItems , Table | Rest ];
	       true ->
		    [Table | store_item2(NewItems,Rest,NodeName)]
	    end
    end.

update_domaindb(OldSip) ->   
    NewSip = lists:foldl(fun collect_items/2 ,[],OldSip),
    NewSip2 = [[{<<"domain-name">>,SipDomain} | Attr] ||
		  {SipDomain,Attr} <- NewSip],
    save_db(<<"sip-domain">>,NewSip2).

collect_items([],Db) ->
    Db;
collect_items([{<<"nodename">>,_} | Rest],Db) ->
    collect_items(Rest,Db);
collect_items([{Domain,Items} | Rest],Db) ->
    DomainData = props:get_value(Domain,Db,[]),
    Fun = fun(Key,Value) -> props:get_value(Key,DomainData,0) + Value end,
    NewData = [{Key,Fun(Key,Value)} || {Key,Value} <- Items] ++
	[{Key,Value} || {Key,Value} <- DomainData, 
			lists:keymember(Key,1,Items) == false ],
    collect_items(Rest,lists:keystore(Domain,1,Db,{Domain,NewData})).

send(Payload) when is_list(Payload) ->
    send(wh_json:encode(Payload));
send(Payload) ->
    amqp_util:targeted_publish(<<"statistics">>,Payload).

get_next(Table,Row,Col) when is_list(Col),is_list(Row),is_binary(Table) ->
    get_next2(Row,Col,get_db(Table),table_order(Table)).

get_next2(_,Cols,[],_) ->
    [endOfTable || _ <- Cols];
get_next2([],_,Table,Order) ->
    [{[1,1],value(1,1, Table, Order) }];
get_next2([Row],Cols,Table,Order) ->
    MaxRow = length(Table),
    MaxCol = length(Order),
    if Row < MaxRow ->
	    [{[C,Row+1],value(Row+1,C,Table,Order)} || C <- Cols];
       true ->
	    [if C < MaxCol ->
		     {[C+1,1],value(1,C+1,Table,Order) } ;
		true ->
		     'endOfTable'
	     end || C <- Cols]
    end.

value(_,_,Table,[]) ->
    lager:error("Trying to read from undefined table ~p~n",[Table]),
    0;
value(Row,Col,Table,Order) ->
    {Key,Default} = lists:nth(Col,Order),
    Val = props:get_value(Key,lists:nth(Row,Table),Default),
    if is_binary(Val) ->
	    binary_to_list(Val);
       true ->
	    Val
    end.

%%% Map the OID order of the items in tables in KAZOO-MIB.mib to the tuples 
%%% in the lists. The second element is the default value is the tuple is
%%% missing in the list.


table_def() ->
    [{<<"vm">>,
      [{<<"nodename">>,<<"">>}
       ,{<<"erlang-version">>,<<"">>}
       ,{<<"memory-total">>,0}
       ,{<<"memory-processes">>,0}
       ,{<<"memory-system">>,0}
       ,{<<"memory-atom">>,0}
       ,{<<"memory-binary">>,0}
       ,{<<"memory-code">>,0}
       ,{<<"memory-ets">>,0}
       ,{<<"amqp-error">>,0}
       ,{<<"amqp-request">>,0}
       ,{<<"bigcouch-504-error">>,0}
       ,{<<"bigcouch-other-error">>,0}
       ,{<<"bigcouch-request">>,0}
      ]},
     {<<"ecallmgr">>,
      [{<<"nodename">>,<<"">>}
       ,{<<"reduction">>,0}
       ,{<<"processes">>,0}
       ,{<<"register-attempt">>,0}
       ,{<<"register-fail">>,0}
       ,{<<"presence">>,0}
      ]},
     {<<"sip-domain">>,
      [{<<"domain-name">>,<<"">>}
       ,{<<"RECOVERY_ON_TIMER_EXPIRE">>,0}
       ,{<<"PROGRESS_TIMEOUT">>,0}
       ,{<<"UNALLOCATED_NUMBER">>,0}
       ,{<<"NO_ROUTE_DESTINATION">>,0}
       ,{<<"NORMAL_CLEARING">>,0}
       ,{<<"ORIGINATOR_CANCEL">>,0}
       ,{<<"DESTINATION_OUT_OF_ORDER">>,0}
       ,{<<"REQUESTED_CHAN_UNAVAIL">>,0}
       ,{<<"NO_ANSWER">>,0}
       ,{<<"INVALID_NUMBER_FORMAT">>,0}
       ,{<<"INCOMPATIBLE_DESTINATION">>,0}
      ]}].

table_order(Table) -> 
    props:get_value(Table,table_def(),[]).
