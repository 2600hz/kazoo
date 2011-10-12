%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Manage the XMPP interactions
%%% @end
%%% Created : 11 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(vx_xmpp).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("voxeo.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_CONFIG, [code:priv_dir(voxeo), "/startup.config"]).

-record(state, {
	  xmpp_session
	  ,xmpp_server
	  ,xmpp_port
	  ,rayo_jid
	  ,rayo_password
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
    case file:consult(?STARTUP_CONFIG) of
	{ok, Configs} ->
	    self() ! authenticate,
	    {ok, #state{
	       xmpp_session = exmpp_component:start_link()
	       ,rayo_jid = props:get_value(rayo_jid, Configs)
	       ,rayo_password = props:get_value(rayo_password, Configs)
	       ,xmpp_server = props:get_value(xmpp_server, Configs)
	       ,xmpp_port = props:get_value(xmpp_port, Configs, ?DEFAULT_XMPP_PORT)
	      }};
	_ ->
	    ?LOG("Failed to load ~s", [?STARTUP_CONFIG]),
	    {stop, normal}
    end.

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
handle_info(authenticate, #state{xmpp_session=Session, xmpp_server=Server, xmpp_port=Port
				 ,rayo_jid=JID, rayo_password=Pass}=State) ->
    try
	exmpp_component:auth(Session, JID, Pass),
	_StreamId = exmpp_component:connect(Session, Server, Port),
	ok = exmpp_component:handshake(Session),

	exmpp_session:send_packet(Session,
				  exmpp_presence:set_status(
				    exmpp_presence:available(), "VX Whapp Ready")),
	{noreply, State}
    catch
	_T:_R ->
	    ?LOG("failed to login: ~p:~p", [_T, _R]),
	    {stop, normal, State}
    end;

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
