%%%-------------------------------------------------------------------
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

-export([handle_call_event/2]).

-include("../blackhole.hrl").

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'call', [{'restrict_to', ['new_channel', 'destroy_channel', 'answered_channel']}]}]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                     ,[{<<"channel">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {account_id :: api_binary()
                ,pids :: pids()
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
start_link(Args) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], Args).

handle_call_event(JObj, _Props) ->
    lager:debug("Handling Call Event: ~p", [JObj]).

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
init(AcctId) ->
    lager:debug('new call event listener ~p', [AcctId]),
    %%gen_listener:add_binding(self()
    %%                         ,'conference'
    %%                         ,[{'restrict_to', ['participant_event'
    %%                                            ,{'conference', CallId}]}
    %%                           ,{<<"Conference-ID">>, CallId}
    %%                          ]),
    {'ok', #state{account_id=AcctId, pids=[]}}.

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
handle_call({'disconnect_socket', User}, {Pid, _}, State) ->
    case lists:delete(Pid, State) of
        [] ->
            {'stop', 'normal', 'ok', []};
        NewState ->
            blackhole_sockets:send_event(NewState, <<"disconnected">>, []),
            {'reply', 'ok', NewState}
    end;
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
handle_cast({'connect_socket', Pid}, #state{pids=Pids}=State) ->
    lager:debug("connecting socket...."),
    blackhole_sockets:send_event([Pid|Pids], <<"connected">>, []),
    {'noreply', State#state{pids=[Pid|Pids]}};
handle_cast(_Message, State) ->
    io:format("got: ~p~n", [_Message]),
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
    io:format("~p~n", [_JObj]),
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
