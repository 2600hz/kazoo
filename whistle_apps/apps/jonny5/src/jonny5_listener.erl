%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for authn_req, reg_success, and reg_query AMQP requests
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(jonny5_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, stop/1, set_blacklist_provider/2
	,add_responder/3, add_binding/2, is_blacklisted/1]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("jonny5.hrl").

-record(state, {
	  cache = undefined :: 'undefined' | pid()
         ,cache_ref = make_ref() :: reference()
	 ,bl_provider_mod = 'default_blacklist' :: atom()
         ,bl_provider_pid = 'undefined' :: 'undefined' | pid()
         ,bl_provider_ref = 'undefined' :: 'undefined' | reference()
	 }).

-define(RESPONDERS, [ {authz_req, [{<<"dialplan">>, <<"authz_req">>}]} ]).
-define(BINDINGS, [ {authorization, []} ]).

-define(SERVER, ?MODULE).
-define(SETTINGS_FILE, [code:priv_dir(jonny5), "/settings.conf"]).
-define(DEFAULT_BL_PROVIDER, default_blacklist).

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

-spec stop/1 :: (Srv) -> ok when
      Srv :: atom() | pid().
stop(Srv) ->
    gen_listener:stop(Srv).

set_blacklist_provider(Srv, Provider) ->
    gen_listener:cast(Srv, {set_blacklist_provider, Provider}).

-spec add_responder/3 :: (Srv, Responder, Key) -> 'ok' when
      Srv :: pid() | atom(),
      Responder :: atom(),
      Key :: {binary(), binary()}.
add_responder(Srv, Responder, Key) ->
    gen_listener:add_responder(Srv, Responder, Key).

-spec add_binding/2 :: (Srv, Binding) -> 'ok' when
      Srv :: pid() | atom(),
      Binding :: atom().
add_binding(Srv, Binding) ->
    gen_listener:add_binding(Srv, {Binding, []}).

-spec is_blacklisted/1 :: (AccountId) -> {'true', binary()} | 'false' when
      AccountId :: binary().
is_blacklisted(AccountId) ->
    {Mod, Srv} = jonny5_sup:get_blacklist_server(),
    Mod:is_blacklisted(Srv, AccountId).

%%%===================================================================
%%% gen_listener callbacks
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
    CPid = whereis(j5_cache),
    Ref = erlang:monitor(process, CPid),

    Provider = case file:consult(?SETTINGS_FILE) of
		   {ok, Config} ->
		       case props:get_value(blacklist_provider, Config) of
			   undefined -> ?DEFAULT_BL_PROVIDER;
			   P -> P
		       end;
		   _ -> ?DEFAULT_BL_PROVIDER
	       end,
    ?LOG_SYS("Blacklist provider: ~s", [Provider]),

    {ok, #state{cache=CPid, cache_ref=Ref
		,bl_provider_mod=Provider
	       }, 0}.

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
handle_call(_, _From, State) ->
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
handle_cast({set_blacklist_provider, Provider}, #state{bl_provider_mod=PMod, bl_provider_pid=PPid}=State) ->
    PMod:stop(PPid),
    ?LOG_SYS("Switching blacklist provider from ~s to ~s", [PMod, Provider]),
    {noreply, State#state{bl_provider_mod=Provider, bl_provider_pid=undefined}}.

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
handle_info({'DOWN', MRef, process, Cache, _Reason}, #state{cache=Cache}=State) ->
    ?LOG_SYS("registrar cache process went down, attempting to reconnect"),
    erlang:demonitor(MRef),
    {noreply, State#state{cache=undefined}, 50};

handle_info({'DOWN', PRef, process, _PPid, _Reason}, #state{bl_provider_ref=PRef, bl_provider_mod=PMod}=State) ->
    ?LOG_SYS("blacklist provider ~s down: ~p", [PMod, _Reason]),
    try
	PPid1 = find_bl_provider_pid(PMod),
	PRef1 = erlang:monitor(process, PPid1),
	{noreply, State#state{bl_provider_ref=PRef1, bl_provider_pid=PPid1}}
    catch
	_:_ ->
	    {noreply, State#state{bl_provider_pid=undefined}}
    end;

handle_info(timeout, #state{bl_provider_mod=PMod, bl_provider_pid=undefined}=State) ->
    ?LOG_SYS("blacklist provider ~s unknown, starting", [PMod]),
    PPid1 = find_bl_provider_pid(PMod),
    PRef1 = erlang:monitor(process, PPid1),
    {noreply, State#state{bl_provider_ref=PRef1, bl_provider_pid=PPid1}};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{cache=Cache}) ->
    {reply, [{cache, Cache}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), #state{}) -> ok.
terminate(_Reason, _) ->
    ?LOG_SYS("jonny5 server ~p termination", [_Reason]).

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

-spec find_bl_provider_pid/1 :: (PMod) -> pid() when
      PMod :: atom().
find_bl_provider_pid(PMod) ->
    ?LOG_SYS("trying to start ~s" , [PMod]),
    {module, PMod} = code:ensure_loaded(PMod),
    case jonny5_sup:start_child(PMod) of
	{ok, undefined} -> ?LOG("ignored"), undefined;
	{ok, Pid} -> ?LOG("started"), Pid;
	{ok, Pid, _Info} -> ?LOG("started with ~p", [_Info]), Pid;
	{error, already_present} ->
	    ?LOG_SYS("already present"),
	    %% supervisor has the child but it isn't running
	    timer:sleep(100),
	    find_bl_provider_pid(PMod);
	{error, {already_started, Pid}} ->
	    ?LOG("already started"),
	    Pid
    end.
