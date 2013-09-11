%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% This process runs on each node in the Kazoo cluster. It collects information
%%% on each node, and regularly sends the information the stats application.
%%% For ecallmgr nodes, it also collects ecallmgr information, and 
%%% sip events statistics.
%%%-------------------------------------------------------------------
-module(whistle_stats).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Public APIs
-export([increment_counter/1
	 ,increment_counter/2
	 ,send_counter/2
	 ,send_absolute/2
	 ,stop/0
	 ,getdb/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(SEND_INTERVAL,10000).

-record(state, {variables=[]
		,sip=[]
		,send_stats=?SEND_INTERVAL
	       }).

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
    start_link(?SEND_INTERVAL).
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
    NewState = store_value(Operation,Key,Val,State),
    lager:debug("Got cast ~p ~p ~p",[Operation,Key,Val]),
    {noreply, NewState};
handle_cast({Operation,Realm,Key,Val}, State) when Operation==add;
						   Operation==store->
    lager:debug("Got cast ~p ~p ~p ~p",[Operation,Realm,Key,Val]),
    NewState = store_value(Operation,Realm,Key,Val,State),
    {noreply, NewState};
handle_cast(_,State) ->
    {noreply,State}.

stop() ->
    gen_server:cast(?MODULE,stop).

getdb() ->
    gen_server:call(?MODULE,get_db).

increment_counter(Item) ->
    send_counter(Item,1).

increment_counter(Realm,Item) ->
    gen_server:cast(?MODULE,{'add',Realm,Item,1}).

send_counter(Item,Value) when is_integer(Value) ->
    gen_server:cast(?MODULE,{'add',Item,Value}).

send_absolute(Item,Value) ->
    gen_server:cast(?MODULE,{'store',Item,Value}).
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
handle_info({'send_stats', SendStats},State) ->
    send_stats(State#state.variables,State#state.sip),
    erlang:send_after(SendStats, self(), {'send_stats', SendStats}),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

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
    {'ok', State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% #state{}  stores statistics in the variables field, and sip event
%%% statistics in the sip field. store_value() either increments the existing
%%% value or sets the value.

store_value('add',Key,Value,State=#state{variables=Var}) 
  when is_integer(Value) ->
    NewValue =  props:get_value(Key,Var,0) + Value,
    State#state{variables = lists:keystore(Key,1,Var,{Key,NewValue})};
store_value('store',Key,Value,State=#state{variables=Var}) ->
    State#state{variables = lists:keystore(Key,1,Var,{Key,Value})}.

%%% Sip events are stored according to the sip realm/domain
store_value(Operation,Realm,Key,Value,State=#state{sip=SipList}) when 
      is_integer(Value) ->
    NewData = case props:get_value(Realm,SipList) of
		  'undefined' ->
		      [{Key,Value}];
		  RealmData ->
		      NewValue = if Operation == 'add' ->
					 props:get_value(Key,RealmData,0)+Value;
				    'true' ->
					 Value
				 end,
		      lists:keystore(Key,1,RealmData,{Key,NewValue})
	      end,
    State#state{sip=lists:keystore(Realm,1,SipList,{Realm,NewData})}.

%%% send_stats collects and sends the statistics to the whistle_stats_master
%%% process. All node (VMs) send memory usage
send_stats(VarList,SipList) when is_list(VarList), is_list(SipList) ->
    Vals = [
	    {'nodename',node()},
	    {'memory-total',erlang:memory(total)},
	    {'memory-processes',erlang:memory(processes)},
	    {'memory-system',erlang:memory(system)},
	    {'memory-atom',erlang:memory(atom)},
	    {'memory-binary',erlang:memory(binary)},
	    {'memory-code',erlang:memory(code)},
	    {'erlang-version',erlang:system_info(system_version)--"\n"},
	    {'memory-ets',erlang:memory(ets)} ]
	++ VarList
	++ get_ecallmgr_values(VarList)
	++ get_sip_values(SipList),
    send(Vals).

get_sip_values([]) ->
    [];
get_sip_values(SipList) ->
    [{<<"sip">>, SipList }].

get_ecallmgr_values(VarList) ->
    case atom_to_list(node()) of
	"ecallmgr@" ++ _ ->
	    get_ecallmgr_values2(VarList);
	_ ->
	    []
    end.

get_ecallmgr_values2(VarList) ->
    RegFail = case {props:get_value("register-attempt",VarList),
		    props:get_value("register-success",VarList)} of
		  {undefined,_} -> 0;
		  {_,undefined} -> 0;
		  {Reg,RegSucc} -> lists:max([Reg-RegSucc,0])
	      end,
%%% Sums the total reductions for all ecallmgr processes.
    Procs = [process_info(X) || 
		{_,X,_,_} <- supervisor:which_children(ecallmgr_sup),is_pid(X)],
    Reduction = lists:sum([props:get_value(reductions,X) || 
			      X <- Procs, is_list(X)]),

    [{'reduction',Reduction}
     ,{'register-fail',RegFail}
     ,{'processes', length(processes())} 
    ].

send(RawPayload) ->
%%%   lager:debug("trying to convert to json ~n~p",[RawPayload]),
    Payload = wh_json:encode(wh_json:recursive_from_proplist(RawPayload)),
%%%   lager:debug("sending to stats_master ~p",[Payload]),
    amqp_util:targeted_publish(<<"statistics">>,Payload).
