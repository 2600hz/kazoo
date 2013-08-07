%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% @doc
%%% Master process to gather information send by all kazoo nodes.
%%% It listens to targeted/statistics AMPQ messages, and is accessed by
%%% whistle_snmp for SNMP queries.
%%%
%%% Created :  3 Jul 2013 by Stephen Gibberd <stephen.gibberd@2600hz.com>
%%%-------------------------------------------------------------------
-module(stats_handler).

-define(STATS_CACHE,'stats_cache').

%% API
-export([get_db/0,get_db/1,simplify_structure/1,handle_event/2,send/1,
	 get_next/3,handle_req/2]).

handle_req(JObj,_Props) ->
    Items = simplify_structure(JObj),
    lager:debug("Got items ~p~n",[Items]),
    store_items(Items).

handle_event(JObj,Props) ->
    lager:debug("Event occurred ~p ~p~n",[JObj,Props]).

get_db() ->
    [{Table,wh_cache:peek_local(?STATS_CACHE,Table)} || 
	Table <- [node,ecall,sip,db]].
get_db(Table) ->
    case wh_cache:peek_local(?STATS_CACHE,Table) of
	{ok, Records} ->
	    Records;
	_ ->
	    []
    end.

save_db(Table,Values) ->
    lager:debug("Trying to store in ~s~n~p~n",[Table,Values]),
    wh_cache:store_local(?STATS_CACHE,Table,Values),
    if Table == <<"sip">> ->
	    update_sipdb(Values);
       true ->
	    nothing
    end.

%%% Structure returned by wh_json:decode() has many {}. Try to remove them
%%% for a simpler list of tuples structure.
simplify_structure({Item}) ->
    simplify_structure(Item);
simplify_structure([H | T]) ->
    [simplify_structure(H) | simplify_structure(T)];
simplify_structure({Key,{List}}) when is_binary(Key),is_list(List) ->
    {Key,simplify_structure(List)};
simplify_structure(Other) ->
    Other.

store_items([]) ->
    finish;
store_items([{TableName, Items} | Rest]) when is_list(Items) ->    
    case props:get_value(<<"nodename">>,Items) of
	undefined ->
	    lager:debug("Can't find the node name ~n~p",[Items]);
	NodeName ->
	    NewDb = store_item2(Items,get_db(TableName),NodeName),
	    save_db(TableName,NewDb)
    end,
    store_items(Rest);
store_items([H,T]) ->
    lager:debug("Ignoring ~p~n",[H]),
    store_items(T);
store_items(Other) -> 
    lager:debug("Ignoring ~p~n",[Other]).

%%% Store information ordered by node name so on restart, snmp monitoring
%%% applications will get table data in the same order.
store_item2(NewItems,[],_NodeName) ->
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

update_sipdb(OldSip) ->   
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
%    lager:debug("maxrow ~w MaxCol ~w Row ~p col ~p",[MaxRow,MaxCol,Row,Cols]),
    if Row < MaxRow ->
	    [{[C,Row+1],value(Row+1,C,Table,Order)} || C <- Cols];
       true ->
	    [if C < MaxCol ->
		     {[C+1,1],value(1,C+1,Table,Order) } ;
		true ->
		     endOfTable
	     end || C <- Cols]
    end.

value(_,_,Table,[]) ->
    lager:debug("Trying to read from undefined table ~p~n",[Table]),
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

table_order(<<"vm">>) -> 
    [{<<"nodename">>,<<"">>},{<<"erlang-version">>,<<"">>},
     {<<"memory_total">>,0},{<<"memory-processes">>,0},{<<"memory-system">>,0},
     {<<"memory-atom">>,0},{<<"memory-binary">>,0},{<<"memory-code">>,0},
     {<<"memory-ets">>,0}];
table_order(<<"ecallmgr">>) -> 
    [{<<"nodename">>,<<"">>},{<<"reduction">>,0},{<<"processes">>,0},
     {<<"register-attempt">>,0},{<<"register-fail">>,0},{<<"presence">>,0}];
table_order(<<"sip-domain">>) -> 
    [{<<"domain-name">>,<<"">>},{<<"RECOVERY_ON_TIMER_EXPIRE">>,0},
     {<<"PROGRESS_TIMEOUT">>,0},{<<"UNALLOCATED_NUMBER">>,0}];
table_order(_) -> 
    [].
    
