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
-export([start_link/0, stop/1]).

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
    ?LOG("cache ~p", [CPid]),
    ?LOG("settings file: ~s", [?SETTINGS_FILE]),
    Provider = case file:consult(?SETTINGS_FILE) of
		   {ok, Config} ->
		       case props:get_value(blacklist_provider, Config) of
			   undefined -> default_blacklist;
			   P -> P
		       end;
		   _ -> default_blacklist
	       end,
    ?LOG_SYS("Blacklist provider: ~s", [Provider]),

    ProviderPid = find_bl_provider_pid(Provider),
    ?LOG_SYS("BL provider pid: ~p", [ProviderPid]),

    ProviderRef = erlang:monitor(process, ProviderPid),

    {ok, #state{cache=CPid, cache_ref=Ref
		,bl_provider_mod=Provider
		,bl_provider_pid=ProviderPid
		,bl_provider_ref=ProviderRef
	       }}.

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
handle_info({'DOWN', MRef, process, Cache, _Reason}, #state{cache=Cache}=State) ->
    ?LOG_SYS("registrar cache process went down, attempting to reconnect"),
    erlang:demonitor(MRef),
    {noreply, State#state{cache=undefined}, 50};

handle_info({'DOWN', PRef, process, PPid, _Reason}, #state{bl_provider_ref=PRef, bl_provider_mod=PMod}=State) ->
    PPid1 = find_bl_provider_pid(PMod),
    PRef1 = erlang:monitor(process, PPid),
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
