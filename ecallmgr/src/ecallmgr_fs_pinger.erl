%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_pinger).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: proplist()
                ,timeout = 2000
               }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

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
init([Node, Props]) ->
    put(callid, Node),
    self() ! initialize_pinger,
    lager:info("node ~s not responding, periodically retrying connection", [Node]),
    {ok, #state{node=Node, options=Props}}.

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
%% #state{nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call(_Request, _From, #state{timeout=Timeout}=State) ->
    {reply, {error, not_implemented}, State, Timeout}.

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
handle_cast(_Msg, #state{timeout=Timeout}=State) ->
    {noreply, State, Timeout}.

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
handle_info(initialize_pinger, #state{node=Node, options=Props}=State) ->
    wh_notify:system_alert("node ~s disconnected from ~s", [Node, node()]),
    _ = case props:get_value(cookie, Props) of
            undefined -> ok;
            Cookie when is_atom(Cookie) ->
                lager:debug("setting cookie to ~s for ~s", [Cookie, Node]),
                erlang:set_cookie(Node, Cookie)
        end,
    GracePeriod = wh_util:to_integer(ecallmgr_config:get(<<"node_down_grace_period">>, 10000)),
    erlang:send_after(GracePeriod, self(), {flush_channels, Node}),
    self() ! check_node_status,
    {noreply, State};
handle_info({flush_channels, Node}, State) ->
    lager:info("node ~s has been down past the grace period, flushing channels", [Node]),
    ecallmgr_fs_nodes:flush_node_channels(Node),
    {noreply, State};
handle_info(check_node_status, #state{node=Node, timeout=Timeout}=State) ->
    case net_adm:ping(Node) of
        pong ->
            %% give the node a moment to init
            timer:sleep(1000),
            wh_notify:system_alert("node ~s connected to ~s", [Node, node()]),
            ok = ecallmgr_fs_nodes:nodeup(Node),
            {stop, normal, State};
        _Else ->
            lager:debug("node ~s not responding, waiting ~b seconds to ping again", [Node, Timeout div 1000]),
            erlang:send_after(Timeout, self(), check_node_status),
            {noreply, State}
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
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs pinger ~p to '~s' termination", [_Reason, Node]).

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
