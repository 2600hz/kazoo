%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Serve up registration information
%%% @end
%%% Created : 25 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("ecallmgr.hrl").

-record(state, {
	  is_amqp_up = true :: boolean()
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup/3 :: (Realm, User, Fields) -> proplist() | {error, timeout} when
      Realm :: binary(),
      User :: binary(),
      Fields :: [binary(),...].
lookup(Realm, User, Fields) ->
    gen_server:call(?SERVER, {lookup, Realm, User, Fields}).

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
    Q = start_amqp(),
    {ok, #state{is_amqp_up=is_binary(Q)}}.

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
handle_call({lookup, Realm, User, Fields}, From, State) ->
    spawn(fun() -> gen_server:reply(From, lookup_reg(Realm, User, Fields)) end),
    {noreply, State}.

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

handle_info({cache_registrations, Realm, User, RegFields}, State) ->
    ?LOG_SYS("Storing registration information for ~s@~s", [User, Realm]),
    wh_cache:store({ecall_registrar, Realm, User}, RegFields
		   ,wh_util:to_integer(props:get_value(<<"Expires">>, RegFields, 300)) %% 5 minute default
		  ),
    {noreply, State, hibernate};

handle_info({_, #amqp_msg{payload=Payload}}, State) ->
    spawn(fun() ->
		  JObj = mochijson2:decode(Payload),
		  User = wh_json:get_value(<<"Username">>, JObj),
		  Realm = wh_json:get_value(<<"Realm">>, JObj),
		  ?LOG_SYS("Received successful reg for ~s@~s, erasing cache", [User, Realm]),
		  wh_cache:erase({ecall_registrar, Realm, User})
	  end),

    {noreply, State, hibernate};

handle_info(timeout, #state{is_amqp_up=false}=State) ->
    ?LOG_SYS("AMQP is down, trying"),
    {noreply, State#state{is_amqp_up=is_binary(start_amqp())}, 1000};
handle_info(timeout, #state{is_amqp_up=true}=State) ->
    ?LOG_SYS("AMQP is up, back to good"),
    {noreply, State};

handle_info({amqp_host_down, _H}, State) ->
    ?LOG_SYS("AMQP Host ~s down", [_H]),
    {noreply, State#state{is_amqp_up=false}, 0};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
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
%% Returns a proplist with the keys corresponding to the elements of Fields
-spec lookup_reg/3 :: (Realm, User, Fields) -> proplist() | {error, timeout} when
      Realm :: binary(),
      User :: binary(),
      Fields :: [binary(),...].
lookup_reg(Realm, User, Fields) ->
    ?LOG_SYS("Looking up registration information for ~s@~s", [User, Realm]),
    FilterFun = fun({K, _}=V, Acc) ->
			case lists:member(K, Fields) of
			    true -> [V | Acc];
			    false -> Acc
			end
		end,
    case wh_cache:fetch({ecall_registrar, Realm, User}) of
	{error, not_found} ->
	    ?LOG_SYS("Valid cached registration not found, querying whapps"),
	    RegProp = [{<<"Username">>, User}
		       ,{<<"Realm">>, Realm}
		       ,{<<"Fields">>, []}
		       | wh_api:default_headers(<<>>, <<"directory">>, <<"reg_query">>, <<"ecallmgr">>, <<>>) ],
	    try
		case ecallmgr_amqp_pool:reg_query(RegProp, 1000) of
		    {ok, {struct, RegResp}} ->
			true = wh_api:reg_query_resp_v(RegResp),

			{struct, RegFields} = props:get_value(<<"Fields">>, RegResp, ?EMPTY_JSON_OBJECT),
			?SERVER ! {cache_registrations, Realm, User, RegFields},

			?LOG_SYS("Received registration information"),
			lists:foldr(FilterFun, [], RegFields);
		    timeout ->
			?LOG_SYS("Looking up registration timed out"),
			{error, timeout}
		end
	    catch
		_:_ ->
		    ?LOG_SYS("Looking up registration threw exception"),
		    {error, timeout}
	    end;
	{ok, RegFields} ->
	    ?LOG_SYS("Found cached registration information"),
	    lists:foldr(FilterFun, [], RegFields)
    end.

-spec start_amqp/0 :: () -> binary() | {error, amqp_host_down}.
start_amqp() ->
    case amqp_util:is_host_available() of
	true ->
	    try
		Q = amqp_util:new_queue(),
		ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_SUCCESS),
		ok = amqp_util:basic_consume(Q),
		Q
	    catch
		_:_ -> {error, amqp_host_down}
	    end;
	false ->
	    {error, amqp_host_down}
    end.
