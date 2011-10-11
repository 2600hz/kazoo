%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Serve up registration information
%%% @end
%%% Created : 25 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-behaviour(gen_listener).

%% API
-export([start_link/0, lookup/3, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(RESPONDERS, [
		     {?MODULE, [{<<"directory">>, <<"reg_success">>}]}
		    ]).
-define(BINDINGS, [{registrations, []}]).

-include("ecallmgr.hrl").

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
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, ?BINDINGS}
				     ], []).

-spec lookup/3 :: (Realm, User, Fields) -> proplist() | {error, timeout} when
      Realm :: binary(),
      User :: binary(),
      Fields :: [binary(),...].
lookup(Realm, User, Fields) ->
    {ok, Srv} = ecallmgr_sup:registrar_proc(),
    gen_server:call(Srv, {lookup, Realm, User, Fields}).

-spec handle_req/2 :: (json_object(), proplist()) -> no_return().
handle_req(JObj, _Props) ->
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    User = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    ?LOG_SYS("Received successful reg for ~s@~s, erasing cache", [User, Realm]),
    wh_cache:erase_local(Cache, cache_key(Realm, User)).

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
    ?LOG_SYS("Ecallmgr Registrar starting"),
    {ok, ok}.

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
    {ok, Cache} = ecallmgr_sup:cache_proc(),

    wh_cache:store_local(Cache, cache_key(Realm, User), RegFields
			 ,wh_util:to_integer(props:get_value(<<"Expires">>, RegFields, 300)) %% 5 minute default
			),
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_, _) ->
    {reply, []}.

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
-spec lookup_reg/3 :: (Realm, User, Fields) -> proplist() | {'error', 'timeout'} when
      Realm :: binary(),
      User :: binary(),
      Fields :: [binary(),...].
lookup_reg(Realm, User, Fields) ->
    ?LOG_SYS("Looking up registration information for ~s@~s", [User, Realm]),
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    FilterFun = fun({K, _}=V, Acc) ->
			case lists:member(K, Fields) of
			    true -> [V | Acc];
			    false -> Acc
			end
		end,
    case wh_cache:fetch_local(Cache, cache_key(Realm, User)) of
	{error, not_found} ->
	    ?LOG_SYS("Valid cached registration not found, querying whapps"),
	    RegProp = [{<<"Username">>, User}
		       ,{<<"Realm">>, Realm}
		       ,{<<"Fields">>, []}
		       | wh_api:default_headers(<<>>, <<"directory">>, <<"reg_query">>, <<"ecallmgr">>, <<>>) ],
	    try
		case ecallmgr_amqp_pool:reg_query(RegProp, 1000) of
		    {ok, RegJObj} ->
			true = wh_api:reg_query_resp_v(RegJObj),

			RegFields = wh_json:to_proplist(wh_json:get_value(<<"Fields">>, RegJObj, wh_json:new())),
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

cache_key(Realm, User) ->
    {?MODULE, Realm, User}.
