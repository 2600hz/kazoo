%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_call_events_listener).

-behaviour(gen_listener).

-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("../blackhole.hrl").

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, []).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {pids, account_id}).

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
start_link(Args) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], Args).

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
init(AccountId) ->
    lager:debug('new call event listener ~p', [AccountId]),
    wh_hooks_listener:register(AccountId),
    {'ok', #state{account_id = AccountId, pids = sets:new()}}.

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
handle_call('get_socket', {Pid, _}, State) ->
    Reply = lists:member(Pid, State),
    {'reply', Reply, State};
handle_call('get_state', _, State) ->
    {'reply', State, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast({'disconnect_socket', Pid}, #state{pids=Pids}=State) ->
    NewPids = sets:del_element(Pid, Pids),
    case sets:size(NewPids) of
        0 ->
            {'stop', 'normal', 'ok', State#state{pids = NewPids}};
        _Size ->
            blackhole_sockets:send_event(sets:to_list(Pids), <<"calls">>, [<<"ok">>]),
            {'reply', 'ok', State#state{pids = NewPids}}
    end;
handle_cast({'connect_socket', Pid}, #state{pids=Pids}=State) ->
    lager:debug("connecting socket...."),
    NewPidSet = sets:add_element(Pid, Pids),
    blackhole_sockets:send_event( sets:to_list(NewPidSet), <<"calls">>, []),
    {'noreply', State#state{pids = NewPidSet}};
handle_cast(_Message, State) ->
    {'noreply', State}.

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
handle_info(?HOOK_EVT(AccountId, EventType, JObj), #state{pids=Pids}=State) ->
    handle_call_event(AccountId, EventType, JObj, Pids),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    lager:debug("got event: ~p", [_JObj]),
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle_call_event(ne_binary(), ne_binary(), json:object(), set()) -> 'ok'.
handle_call_event(_AccountId, _Event, JObj, Pids) ->
    lager:debug("Event Obj: ~p", [JObj]),
    blackhole_sockets:send_event(sets:to_list(Pids)
                                ,<<"calls">>
                                ,[JObj]).
