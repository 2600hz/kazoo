%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_test).

-behaviour(gen_server).

%% API
-export([start/1, start_link/0, set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../callflow.hrl").

-define(SERVER, ?MODULE).
-define(APP_NAME, <<"cf_test">>).
-define(APP_VERSION, <<"1.0">>).

-record(state, {
         amqp_host = "localhost"  :: string()
        ,cmgr_q = undefined  :: binary() | undefined
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
start(AHost) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [AHost], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_amqp_host(AHost) ->
    gen_server:call(?SERVER, {set_amqp_host, AHost}, infinity).

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
-spec(init/1 :: (_) -> tuple(ok, #state{})).
init([]) ->
    State = #state{},
    AHost = State#state.amqp_host,
    format_log(info, "CF_TEST(~w): Starting server with amqp host ~w~n", [self(), AHost]),
    {ok, Cmgr_Q} = start_amqp(AHost),
    {ok, State#state{cmgr_q = Cmgr_Q}};

init([AHost]) ->
    format_log(info, "CF_TEST(~w): Starting server with amqp host ~w~n", [self(), AHost]),
    {ok, Cmgr_Q} = start_amqp(AHost),
    {ok, #state{amqp_host=AHost, cmgr_q=Cmgr_Q}}.

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
handle_call({set_amqp_host, AHost1}, _From, #state{amqp_host=AHost2}=State) ->
    format_log(info, "CF_TEST(~w): Updating amqp host from ~w to ~w~n", [self(), AHost2, AHost1]),
    amqp_manager:close_channel(self(), AHost2),
    {ok, Cmgr_Q} = start_amqp(AHost1),
    {reply, ok, State#state{amqp_host = AHost1, cmgr_q = Cmgr_Q}};
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
    io:format("Unhandled ~w", [_Msg]),
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
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    case amqp_util:is_json(Props) of
        true ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            format_log(info, "CF_TEST(~w): Received message~nPayload: ~w~n", [self(), Msg]),
            spawn(fun() -> process_req(amqp_util:get_msg_type(Msg), Msg, State) end);
        _ ->
            format_log(info, "CF_TEST(~w): Recieved non JSON AMQP msg content type~n", [self()])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "CF_TEST(~w): unhandled info ~w~n", [self(), _Info]),
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures the monitor exchange exists, then creates a named queue
%% and places a consumer on it
%% @end
%%--------------------------------------------------------------------
-spec(start_amqp/1 :: (AHost :: string()) -> tuple(ok, binary())).
start_amqp(AHost) ->
    amqp_util:callmgr_exchange(AHost),
    amqp_util:targeted_exchange(AHost),
    amqp_util:callevt_exchange(AHost),
    Cmgr_Q = amqp_util:new_callmgr_queue(AHost, <<>>),
    amqp_util:bind_q_to_callmgr(AHost, Cmgr_Q, ?KEY_AUTH_REQ),
    amqp_util:bind_q_to_callmgr(AHost, Cmgr_Q, ?KEY_ROUTE_REQ),
    amqp_util:bind_q_to_targeted(AHost, Cmgr_Q, Cmgr_Q),
    amqp_util:basic_consume(AHost, Cmgr_Q),
    {ok, Cmgr_Q}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process the requests recieved from AMQP
%% @end
%%--------------------------------------------------------------------
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), Prop :: proplist(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, Prop, #state{cmgr_q=Cmgr_Q}=State) ->
    RespQ = proplists:get_value(<<"Server-ID">>, Prop),
    Resp = [
             {<<"Msg-ID">>, proplists:get_value(<<"Msg-ID">>, Prop)}
            ,{<<"Auth-Method">>, <<"password">>}
            ,{<<"Auth-Password">>, proplists:get_value(<<"Auth-User">>, Prop)}
            | whistle_api:default_headers(Cmgr_Q, <<"directory">>, <<"auth_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    format_log(info, "CF_TEST(~w): Respond to auth_req~nPayload: ~w~n", [self(), Resp]),
    {ok, Json} = whistle_api:auth_resp(Resp),
    send_resp(Json, RespQ, State);

process_req({<<"dialplan">>, <<"route_req">>}, Prop, #state{cmgr_q=Cmgr_Q}=State) ->
    RespQ = proplists:get_value(<<"Server-ID">>, Prop),
    Resp = [
             {<<"Msg-ID">>, proplists:get_value(<<"Msg-ID">>, Prop)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            | whistle_api:default_headers(Cmgr_Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    format_log(info, "CF_TEST(~w): Respond to route_req~nPayload: ~w~n", [self(), Resp]),
    {ok, Json} = whistle_api:route_resp(Resp);
%%    send_resp(Json, RespQ, State);

process_req({<<"dialplan">>, <<"route_win">>}, Prop, State) ->
    format_log(info, "CF_TEST(~w): Recieved route_win!~nPayload: ~w~n", [self(), Prop]),
    Call = #cf_call{
      amqp_h = State#state.amqp_host
      ,ctrl_q = proplists:get_value(<<"Control-Queue">>, Prop)
      ,call_id = proplists:get_value(<<"Call-ID">>, Prop)
      ,cf_pid = self()
     },
    Payload = {struct, [
                         {<<"database">>, <<"crossbar%2Fclients%2F58%2F39%2Fa3ed904ecdaeedebc8af6a1b7a1f">>} 
                        ,{<<"endpoint">>, <<"7818711acb94ddc2f5ac26da6c5d8eaa">>}
                        ,{<<"timeout">>, <<"15">>}
                       ]},
    Pid = spawn(cf_voicemail, handle, [Payload, Call]),
    format_log(info, "CF_TEST(~w): Spawned cf_devices at ~w~n", [self(), Pid]),
    receive
        continue -> 
            format_log(info, "CF_TEST(~w): Received continue atom~n", [self()]),
            ok;
        stop -> 
            format_log(error, "CF_TEST(~w): Received stop atom~n", [self()]),
            ok
    after
        100000 -> error
    end,
    hangup_call(Call);

process_req(_MsgType, _Msg, _State) ->
    format_log(info, "CF_TEST(~w): Unhandled Msg ~w~nPayload: ~w~n", [self(), _MsgType, _Msg]).

hangup_call(#cf_call{call_id=CallId} = Call) ->                               
    Command = [
                {<<"Application-Name">>, <<"hangup">>}
               ,{<<"Call-ID">>, CallId}
             | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    format_log(info, "CF_TEST(~w): Sending command~nPayload: ~w~n", [self(), Command]),
    {ok, Json} = whistle_api:hangup_req(Command),
    send_callctrl(Json, Call).

-spec(send_resp/3 :: (JSON :: iolist(), RespQ :: binary(), tuple()) -> no_return()).
send_resp(Json, RespQ, #state{amqp_host=AHost}) ->
    amqp_util:targeted_publish(AHost, RespQ, Json, <<"application/json">>).

send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    format_log(info, "CF_DEVICES(~w): Sent to ~w~n", [self(), CtrlQ]),
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
