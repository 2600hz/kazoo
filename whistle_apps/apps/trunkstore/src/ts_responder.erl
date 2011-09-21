%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Trunk-Store responder waits for Auth and Route requests on the broadcast
%%% Exchange, and delievers the requests to the corresponding handler.
%%% TS responder also receives responses from the handlers and returns them
%%% to the requester.
%%% Each request received by TS_RESPONDER should be put into a new spawn()
%%% to avoid blocking on each request.
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_responder).

-behaviour(gen_listener).

%% API
-export([start_link/0, start_responder/0, stop/1, transfer_auth/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("ts.hrl").

-define(RESPONDERS, [
		     {route_req, [{<<"dialplan">>, <<"route_req">>}]}
		    ]).
-define(BINDINGS, [
		   {routing, []}
		  ]).

-define(SERVER, ?MODULE).
-define(ROUTE_QUEUE_NAME, <<"ts_responder.route.queue">>).
-define(ROUTE_QUEUE_OPTIONS, [{exclusive, false}]).
-define(ROUTE_CONSUME_OPTIONS, [{exclusive, false}]).

-record(state, {
	  is_amqp_up = false :: boolean()
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
    spawn(fun() ->
		  _ = transfer_auth(),
		  [ ts_responder_sup:start_handler() || _ <- [1,2,3] ]
	  end),
    ignore.

start_responder() ->
    ?LOG("Starting responder"),
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, ?BINDINGS}
				      ,{queue_name, ?ROUTE_QUEUE_NAME}
				      ,{queue_options, ?ROUTE_QUEUE_OPTIONS}
				      ,{consume_options, ?ROUTE_CONSUME_OPTIONS}
				      ,{basic_qos, 1}
				     ], []).

stop(Srv) ->
    gen_listener:stop(Srv).

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
    ?LOG("Started responder"),
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

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
handle_info(_Unhandled, State) ->
    ?LOG_SYS("Unknown message: ~p~n", [_Unhandled]),
    {noreply, State, 1000}.

handle_event(JObj, _State) ->
    ?LOG("Recv jobj"),
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
terminate(_Reason, _) ->
    ?LOG_SYS("Terminating: ~p~n", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    ?LOG_SYS("Code Change from ~p~n", [_OldVsn]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec transfer_auth/0 :: () -> ok.
transfer_auth() ->
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_USERAUTHREALM, []) of
	{ok, AuthJObjs} ->
	    ?LOG_SYS("Importing ~b accounts into sip_auth", [length(AuthJObjs)]),
	    _ = [ transfer_auth(AuthJObj) || AuthJObj <- AuthJObjs],
	    ok;
	_E ->
	    ok
    end.

-spec transfer_auth/1 :: (AuthJObj) -> no_return() when
      AuthJObj :: json_object().
transfer_auth(AuthJObj) ->
    ID = wh_json:get_value(<<"id">>, AuthJObj),
    spawn(fun() -> ?LOG_SYS("del doc ~s: ~p", [ID, couch_mgr:del_doc(<<"sip_auth">>, ID)]) end), %% cleanup old imports

    AuthData = {struct, AuthProps}
        = wh_json:get_value(<<"value">>, AuthJObj, ?EMPTY_JSON_OBJECT),

    DocID = <<ID/binary, (wh_json:get_binary_value(<<"server_id">>, AuthData, <<"0">>))/binary>>,

    SipAuthDoc = {struct, [{<<"_id">>, DocID}
			   ,{<<"sip">>, {struct, [
						  {<<"realm">>, wh_json:get_value(<<"auth_realm">>, AuthData, <<"">>)}
						  ,{<<"method">>, wh_json:get_value(<<"auth_method">>, AuthData, <<"">>)}
						  ,{<<"username">>, wh_json:get_value(<<"auth_user">>, AuthData, <<"">>)}
						  ,{<<"password">>, wh_json:get_value(<<"auth_password">>, AuthData, <<"">>)}
					]}
			    }]},
    SAD1 = lists:foldl(fun({K, V}, JObj) ->
                              wh_json:set_value(K, V, JObj)
                      end, SipAuthDoc, [Pvt || {K, _}=Pvt <- AuthProps
                                                   ,binary:match(K, <<"pvt_">>) =/= nomatch]),
    %% trunkstore just has to be different, or are we all different and trunkstore normal...
    %% after all it was first ;)
    SAD2 = wh_json:set_value(<<"pvt_account_id">>, ID, SAD1),
    couch_mgr:ensure_saved(<<"sip_auth">>, SAD2).
