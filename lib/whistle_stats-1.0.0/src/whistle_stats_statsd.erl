%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%% Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(whistle_stats_statsd).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).
-export([refresh/0]).

-include_lib("whistle/include/wh_types.hrl").

-define(HOST, undefined).
-define(PORT, 8125).
-define(POLLING_INTERVAL, 10000).
 
%% Karl and James - The next three lines aren't important, no need to read them
-define(ECALLMGR_AMQP_POOL, ecallmgr_amqp_pool).
-define(APP_NAME, <<"ecallmgr">>).
-define(APP_VERSION, <<"0.8.0">>).

-record(state, {host = ?HOST :: 'undefined' | nonempty_string()
	        ,port = ?PORT :: pos_integer()
		,polling_interval = ?POLLING_INTERVAL :: pos_integer()
		,node_key :: nonempty_string()
	       }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================	    
-spec refresh/0 :: () -> ok.
refresh() ->
    gen_server:call(?MODULE, refresh),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    State = refresh(#state{node_key=get_node_key()}),
    erlang:send_after(State#state.polling_interval, ?MODULE, interval),
    {ok, State}.

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
handle_call(refresh, _From, State) ->
    {reply, ok, refresh(State)};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(interval, State) ->
    State1 = case State#state.host of
		 undefined ->
		     lager:debug("no url is set for statsd, trying to refresh configuration"),
		     refresh(State);
		 Host ->
		     lager:debug("statsd url is set, sending stats to: ~p", [Host]),
		     send_stats(Host, State#state.port, State#state.node_key),
		     State
	     end,
    erlang:send_after(State1#state.polling_interval, ?MODULE, interval),
    {noreply, State1};
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
-spec get_node_key/0 :: () -> nonempty_string().
get_node_key() ->
    Node = list_to_binary(atom_to_list(node())),
    [User | [Hostname]] = binary:split(Node, <<"@">>),
    [ShortName | [DomainName]] = binary:split(Hostname, <<".">>),
    DomainName1 = binary:replace(DomainName, <<".">>, <<"_">>, [global]),
    binary_to_list(<<DomainName1/binary, ".", ShortName/binary, ".", User/binary>>).

-spec refresh/1 :: (state()) -> state().
refresh(State) ->
    StatsdInfo = case erlang:module_loaded(whapps_config) of 
		     true ->
			 whapps_config:get(<<"whistle_stats">>, <<"statsd">>, wh_json:new());	    
		     false ->
			 get_config(<<"whistle_stats">>, <<"statsd">>, wh_json:new())
		 end,

     State#state{host = wh_json:get_string_value(<<"host">>, StatsdInfo, ?HOST)
		 ,port = wh_json:get_integer_value(<<"port">>, StatsdInfo, ?PORT)
		 ,polling_interval = wh_json:get_integer_value(<<"polling_interval">>, StatsdInfo, ?POLLING_INTERVAL)
               }.

-spec get_config/3 :: (ne_binary(), ne_binary(), term()) -> term(). 
get_config(Cat, Key, Default) ->
    Node = wh_util:to_binary(node()),
    Req = [KV ||
	      {_, V} = KV <- [{<<"Category">>, Cat}
			      ,{<<"Key">>, Key}
			      ,{<<"Default">>, Default}
			      ,{<<"Node">>, Node}
			      ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
			      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
			     ],
	      V =/= undefined],
    
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
				  ,Req
				  ,fun wapi_sysconf:publish_get_req/1
				  ,fun wapi_sysconf:get_resp_v/1),

    case ReqResp of
	{error, _R} ->
	    Default;
	{ok, RespJObj} ->
	    case wh_json:get_value(<<"Value">>, RespJObj) of
		undefined -> Default;
		null -> Default;
		<<"undefined">> -> Default;
		<<"null">> -> Default;
		Value ->
		    Value
	    end
    end.

-spec send_stats/3 :: (nonempty_string(), pos_integer(), nonempty_string()) -> ok.
send_stats(Host, Port, NodeKey) ->
    send_stats(Host, Port, NodeKey, folsom_metrics:get_metrics()).

-spec send_stats/4 :: (nonempty_string(), pos_integer(), nonempty_string(), list()) -> ok.
send_stats(_Host, _Port, _NodeKey, []) ->
    ok;
send_stats(Host, Port, NodeKey, [NamespacedStatKey | Tail]) ->
    [StatType | [StatKey]] = binary:split(NamespacedStatKey, <<".">>),
    StatMod = list_to_atom(binary_to_list(StatType)),
    StatsdKey = lists:flatten([NodeKey
			       ,"."
			       ,binary_to_list(StatKey)
			      ]),
    lager:debug("~p:~p|g", [StatsdKey, StatMod:get(StatKey)]),
    send_udp_message(Host, Port, io_lib:format("~p:~p|g", [StatsdKey
							   ,StatMod:get(StatKey)
							  ])),
    send_stats(Host, Port, NodeKey, Tail).

-spec send_udp_message/3 :: (nonempty_string(), pos_integer(), nonempty_string()) -> ok.
send_udp_message(Host, Port, Msg) when is_integer(Port), is_list(Host) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, Host, Port, Msg),
    gen_udp:close(Socket),
    ok.
