%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Receive AMQP requests for media, spawn a handler for the response
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(media_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("media_mgr.hrl").

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(MEDIA_DB, "media_files").

-record(state, {
	  streams = [] :: list(tuple(binary(), pid(), reference())) | [] % MediaID, StreamPid, Ref
	  ,amqp_q = <<>> :: binary()
          ,is_amqp_up = true :: boolean()
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
    H = whapps_controller:get_amqp_host(),

    amqp_util:callmgr_exchange(H),
    amqp_util:targeted_exchange(H),

    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #state{}, 0}.

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
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(timeout, #state{amqp_q = <<>>}=S) ->
    Q = start_amqp(whapps_controller:get_amqp_host()),
    format_log(info, "MEDIA_SRV(~p): Listening on ~p~n", [self(), Q]),
    {noreply, S#state{amqp_q=Q, is_amqp_up=is_binary(Q)}, 2000};
handle_info(timeout, #state{amqp_q={error, _}=QE}=S) ->
    H = whapps_controller:get_amqp_host(),
    format_log(info, "MEDIA_SRV(~p): Failed to start q (~p), trying again on ~p~n", [self(), QE, H]),
    case amqp_util:is_host_available(H) of
	true -> {noreply, S#state{is_amqp_up=true, amqp_q=start_amqp(H)}};
	false -> {noreply, S#state{is_amqp_up=false}, 1000}
    end;
handle_info(timeout, #state{amqp_q=Q, is_amqp_up=false}=S) ->
    H = whapps_controller:get_amqp_host(),
    amqp_util:delete_queue(H, Q),
    NewQ = start_amqp(H),
    format_log(info, "MEDIA_SRV(~p): Stopping ~p, listening on ~p @ ~p~n", [self(), Q, NewQ, H]),
    {noreply, S#state{amqp_q=NewQ, is_amqp_up=is_binary(NewQ)}};

handle_info({amqp_host_down, _}, S) ->
    {noreply, S#state{amqp_q={error, amqp_down}, is_amqp_up=false}, 0};

handle_info({_, #amqp_msg{payload = Payload}}, S) ->
    spawn(fun() -> handle_req(mochijson2:decode(Payload)) end),
    {noreply, S};

handle_info(_Info, State) ->
    format_log(info, "MEDIA_SRV(~p): Unhandled info ~p~n", [self(), _Info]),
    {noreply, State, 1000}.

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
start_amqp(H) ->
    Q = amqp_util:new_queue(H, <<>>),

    amqp_util:bind_q_to_callevt(H, Q, ?KEY_CALL_MEDIA_REQ),
    amqp_util:bind_q_to_targeted(H, Q, Q),

    amqp_util:basic_consume(H, Q),
    Q.


handle_req(JObj) ->
    true = whistle_api:media_req_v(JObj),
    ok.
