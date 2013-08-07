%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% Created :  5 Jul 2013 by Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% This process runs on each node in the Kazoo cluster. It collects information
%%% on each node, and regularly sends the information to whistle_stats_master
%%% For ecallmgr nodes, it also collects ecallmgr information, and 
%%% sip events statistics.
%%%-------------------------------------------------------------------
-module(whistle_stats).

-behaviour(gen_server).

%% API
-export([start_link/0,increment_counter/1,increment_counter/2, send_counter/2,
	 send_absolute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,stop/0]).

-define(SERVER, ?MODULE). 
-define(SEND_STATS,10000).

-record(state, {ecall=[],sip=[],send_stats=?SEND_STATS}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(?SEND_STATS).
start_link(Send_stats) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Send_stats], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}}.
init([Send_stats]) ->
    erlang:send_after(Send_stats, self(), {'send_stats', Send_stats}),
    {ok, #state{send_stats=Send_stats}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_db, _From, State) ->
    {reply, State, State};
handle_call(Other,_From,State) ->
    lager:debug("Call ~p",[Other]),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(stop,State) ->
    {stop,ok,State};
handle_cast({Operation,Key,Val}, State) when Operation==add;Operation==store->
    lager:debug("Got cast ~s ~p ~p",[Operation,Key,Val]),
    NewState = store_value(Operation,Key,Val,State),
    {noreply, NewState};
handle_cast({Operation,Realm,Key,Val}, State) when Operation==add;
						   Operation==store->
    lager:debug("Got cast ~s ~p ~p ~p",[Operation,Realm,Key,Val]),
    NewState = store_value(Operation,Realm,Key,Val,State),
    {noreply, NewState};
handle_cast(Other,State) ->
    lager:debug("Cast: ~p",[Other]),
    {noreply,State}.

stop() ->
    gen_server:cast(?MODULE,stop).

increment_counter(Item) ->
    send_counter(Item,1).
increment_counter(Realm,Item) ->
    gen_server:cast(?MODULE,{add,Realm,Item,1}).

send_counter(Item,Value) when is_integer(Value) ->
    gen_server:cast(?MODULE,{add,Item,Value}).
send_absolute(Item,Value) ->
    gen_server:cast(?MODULE,{store,Item,Value}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'send_stats', Send_stats},State) ->
    send_stats(State#state.ecall,State#state.sip),
    erlang:send_after(Send_stats, self(), {'send_stats', Send_stats}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% #state{}  stores ecallmgr statistics in the ecall field, and sip event
%%% statistics in the sip field. store_value() either increments the existing
%%% value or sets the value.

store_value(add,Key,Value,State=#state{ecall=ECall}) when is_integer(Value) ->
    NewValue =  props:get_value(Key,ECall,0) + Value,
    State#state{ecall = lists:keystore(Key,1,ECall,{Key,NewValue})};
store_value(store,Key,Value,State=#state{ecall=ECall}) ->
    State#state{ecall = lists:keystore(Key,1,ECall,{Key,Value})}.

%%% Sip events are stored according to the sip realm/domain
store_value(Operation,Realm,Key,Value,State=#state{sip=SipL}) when 
      is_integer(Value) ->
    NewData = case props:get_value(Realm,SipL) of
		  undefined ->
		      [{Key,Value}];
		  RData ->
		      NewValue = if Operation == add ->
					 props:get_value(Key,RData,0)+Value;
				    true ->
					 Value
				 end,
		      %lager:debug("RData ~p~nNewVale ~w",[RData,NewValue]),
		      lists:keystore(Key,1,RData,{Key,NewValue})
	      end,
    State#state{sip=lists:keystore(Realm,1,SipL,{Realm,NewData})}.

%%% send_stats collects and sends the statistics to the whistle_stats_master
%%% process. All node (VMs) send memory usage
send_stats(EcallL,SipL) ->
     VM = [{<<"vm">>,
	    prepare_json([{nodename,node()},
			  {'memory-total',erlang:memory(total)},
			  {'memory-processes',erlang:memory(processes)},
			  {'memory-system',erlang:memory(system)},
			  {'memory-atom',erlang:memory(atom)},
			  {'memory-binary',erlang:memory(binary)},
			  {'memory-code',erlang:memory(code)},
			  {'erlang-version',
			   erlang:system_info(system_version)--"\n"},
			  {'memory-ets',erlang:memory(ets)}]) }],
    Ecall = get_ecallmgr_values(EcallL),
    Sip =  get_sip_values(SipL),
    lager:debug("sip is ~p",[Sip]),
    send({VM ++ Ecall ++ Sip}).

get_sip_values([]) ->
    [];
get_sip_values(SipL) ->
    [{<<"sip">>,  prepare_json([{nodename,node()} | SipL])  }].

get_ecallmgr_values([]) ->
    [];
get_ecallmgr_values(Ecall) ->
    RegFail = case {props:get_value("register-attempt",Ecall),
		    props:get_value("register-success",Ecall)} of
		  {undefined,_} -> 0;
		  {_,undefined} -> 0;
		  {Reg,RegSucc} -> lists:max([Reg-RegSucc,0])
	      end,
    Procs = [process_info(X) || 
		{_,X,_,_} <- supervisor:which_children(ecallmgr_sup),is_pid(X)],

%%% Sums the total reductions for all ecallmgr processes.
    Reduction = lists:sum([props:get_value(reductions,X) || 
			      X <- Procs, is_list(X)]),
    [{<<"ecallmgr">>, 
      prepare_json([ {nodename,node()},{reduction,Reduction},
		     {"register-fail",RegFail},
		     {processes, length(processes())} | Ecall])}].

prepare_json(List) when is_list(List) ->
    case lists:all(fun(X) -> is_integer(X) end,List) of
	true ->
	    List;
	false ->
	    {[ {to_binary(K), prepare_json(V)}  || {K,V} <- List]}
    end;
prepare_json(Val) ->
    Val.

to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_atom(X) ->
    atom_to_binary(X,utf8);
to_binary(X) when is_binary(X)->
    X.

send(RawPayload) ->
%    lager:debug("Trying to convert to json ~n~p",[RawPayload]),
    Payload = wh_json:encode(RawPayload),
    lager:debug("Sending to stats_master ~p",[Payload]),
    amqp_util:targeted_publish(<<"statistics">>,Payload).

    
	    
    
    
