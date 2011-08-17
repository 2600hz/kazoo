%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle updating devices and emails about voicemails
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hotornot_responder).

-behaviour(gen_server).

%% API
-export([start_link/0, add_responder/2, rm_responder/1, rm_responder/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("hotornot.hrl").

-define(SERVER, ?MODULE).
-define(RESPONDERS, [
		     {hon_rater, [{<<"call">>, <<"rating_req">>}]}
		    ]).

-record(state, {
	  responders = [] :: [listener_utils:responder(),...] | [] %% { {EvtCat, EvtName}, Module }
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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_responder/2 :: (Responder, Key) -> ok when
      Responder :: atom(),
      Key :: {binary(), binary()} | [{binary(), binary()},...].
add_responder(Responder, Key) when not is_list(Key) ->
    add_responder(Responder, [Key]);
add_responder(Responder, [{_,_}|_] = Keys) ->
    gen_server:cast(?SERVER, {add_responder, Responder, Keys}).

-spec rm_responder/1 :: (Responder) -> ok when
      Responder :: atom().
-spec rm_responder/2 :: (Responder, Key) -> ok when
      Responder :: atom(),
      Key :: [{binary(), binary()},...] | []. %% empty list removes all
rm_responder(Responder) ->
    rm_responder(Responder, []).
rm_responder(Responder, {_,_}=Key) ->
    rm_responder(Responder, [Key]);
rm_responder(Responder, Keys) ->
    gen_server:cast(?SERVER, {rm_responder, Responder, Keys}).

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
    ?LOG_SYS("starting hotornot process"),
    process_flag(trap_exit, true),
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
handle_cast({add_responder, Responder, Keys}, #state{responders=Responders}=State) ->
    {noreply, State#state{responders=listener_utils:add_responder(Responders, Responder, Keys)}};
handle_cast({rm_responder, Responder, Keys}, #state{responders=Responders}=State) ->
    {noreply, State#state{responders=listener_utils:rm_responder(Responders, Responder, Keys)}}.

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
handle_info({#'basic.deliver'{}, #amqp_msg{props = #'P_basic'{content_type = <<"application/json">>}, payload = Payload}}
	    ,#state{responders=Ns}=State) ->
    spawn_link(fun() ->
		       JObj = mochijson2:decode(Payload),
		       whapps_util:put_callid(JObj),
		       ?LOG_START("JSON received: ~s", [Payload]),
		       _ = process_req(Ns, JObj)
	       end),
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    ?LOG_SYS("~p exited with ~p", [_Pid, _Reason]),
    {noreply, State};

handle_info(timeout, State) ->
    %% cast shouldn't block
    spawn(fun() -> [?MODULE:add_responder(Module, Evts) || {Module, Evts} <- ?RESPONDERS] end),
    case start_amqp() of
	{ok, _} -> {noreply, State};
	_ -> {noreply, State, 1000}
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {noreply, State, 1000};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

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
    ?LOG_SYS("vm hotornot process ~p termination", [_Reason]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec start_amqp/0 :: () -> {ok, binary()} | {error, amqp_error}.
start_amqp() ->
    try
        Q = amqp_util:new_queue(),
        amqp_util:bind_q_to_callmgr(Q, ?KEY_RATING_REQ),
        amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec process_req/2 :: (Ns, JObj) -> ok when
      Ns :: [listener_utils:responder(),...] | [],
      JObj :: json_object().
process_req(Ns, JObj) ->
    Key = whapps_util:get_event_type(JObj),
    _ = [ catch(Module:handle_req(JObj)) || {Evt, Module} <- Ns, Key =:= Evt ],
    ok.
