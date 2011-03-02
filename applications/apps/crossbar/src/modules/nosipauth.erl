%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% NoSipAuth module
%%%
%%% Authenticates everyone! PARTY TIME!
%%%
%%% This is just a gen_server that can be used to authenticate calls
%%% so we can build route handlers until sip registration checks user
%%% creds.  It will authenticate any call/registration that has the
%%% the equal username and passwords.
%%%
%%% To start this gen_server run (in your erlang shell): nosipauth:start("localhost").
%%% Where localhost is the hostname of the AMQP server to connect to
%%%
%%% @end
%%% Created : 24 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(nosipauth).

-behaviour(gen_server).

%% API
-export([start/1, start_link/0, set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).

-record(state, {
         amqp_host = "localhost"  :: string()
        ,auth_q = undefined  :: binary() | undefined
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
    format_log(info, "NO_SIP_AUTH(~w): Starting server with amqp host ~w~n", [self(), AHost]),
    {ok, Auth_Q} = start_amqp(AHost),
    {ok, State#state{auth_q = Auth_Q}};

init([AHost]) ->
    format_log(info, "NO_SIP_AUTH(~w): Starting server with amqp host ~w~n", [self(), AHost]),
    {ok, Auth_Q} = start_amqp(AHost),
    {ok, #state{amqp_host=AHost, auth_q=Auth_Q}}.

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
    format_log(info, "NO_SIP_AUTH(~w): Updating amqp host from ~w to ~w~n", [self(), AHost2, AHost1]),
    amqp_manager:close_channel(self(), AHost2),
    {ok, Auth_Q} = start_amqp(AHost1),
    {reply, ok, State#state{amqp_host = AHost1, auth_q = Auth_Q}};
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
            spawn(fun() -> process_req(amqp_util:get_msg_type(Msg), Msg, State) end);
        _ ->
            format_log(info, "NO_SIP_AUTH(~w): Recieved non JSON AMQP msg content type~n", [self()])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "NO_SIP_AUTH(~w): unhandled info ~w~n", [self(), _Info]),
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
    Auth_Q = amqp_util:new_callmgr_queue(AHost, <<>>),
    amqp_util:bind_q_to_callmgr(AHost, Auth_Q, ?KEY_AUTH_REQ),
    amqp_util:basic_consume(AHost, Auth_Q),
    {ok, Auth_Q}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process the requests recieved from AMQP
%% @end
%%--------------------------------------------------------------------
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), Prop :: proplist(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, Prop, State) ->
    Resp = [
         {<<"Event-Category">>, <<"directory">>}
        ,{<<"Event-Name">>, <<"auth_resp">>}
        ,{<<"Server-ID">>, <<"">>}
        ,{<<"App-Name">>, <<"nosipauth">>}
        ,{<<"App-Version">>, <<"1.0">>}
        ,{<<"Msg-ID">>, proplists:get_value(<<"Msg-ID">>, Prop)}
        ,{<<"Auth-Method">>, <<"password">>}
        ,{<<"Auth-Password">>, proplists:get_value(<<"Auth-User">>, Prop)}
    ],
    {ok, Json} = whistle_api:auth_resp(Resp),
    RespQ = proplists:get_value(<<"Server-ID">>, Prop),
    send_resp(Json, RespQ, State);

process_req(_MsgType, _Msg, _State) ->
    format_log(error, "NO_SIP_AUTH(~w): Unhandled Msg ~w~nJSON: ~w~n", [self(), _MsgType, _Msg]).

-spec(send_resp/3 :: (JSON :: iolist(), RespQ :: binary(), tuple()) -> no_return()).
send_resp(Json, RespQ, #state{amqp_host=AHost}) ->
    amqp_util:targeted_publish(AHost, RespQ, Json, <<"application/json">>).
