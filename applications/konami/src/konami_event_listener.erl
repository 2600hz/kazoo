%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_event_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,add_call_binding/1, add_call_binding/2
         ,rm_call_binding/1, rm_call_binding/2
         ,handle_call_event/2
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("konami.hrl").

%% {callid, [event,...]}
-record(state, {bindings = dict:new() :: dict()}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, []).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                      ,[{<<"call_event">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DYN_BINDINGS(CallId), {'call', [{'restrict_to', ['DTMF', 'CHANNEL_DESTROY']}
                                        ,{'callid', CallId}
                                       ]}).
-define(DYN_BINDINGS(CallId, Events), {'call', [{'restrict_to', Events}
                                                ,{'callid', CallId}
                                               ]}).

-define(KONAMI_REG(CallId), {'p', 'l', {'event', CallId}}).

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
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                             ]
                            ,[]
                           ).

-spec add_call_binding(api_binary() | whapps_call:call()) -> 'ok'.
-spec add_call_binding(api_binary() | whapps_call:call(), ne_binaries() | atoms()) -> 'ok'.
add_call_binding('undefined') -> 'ok';
add_call_binding(CallId) when is_binary(CallId) ->
    Events = ['DTMF', 'CHANNEL_DESTROY'],
    lager:debug("add fsm binding for call ~s: ~p", [CallId, Events]),
    gproc:reg(?KONAMI_REG({'fsm', CallId})),
    gen_listener:cast(?MODULE, {'add_bindings', CallId, Events}),
    gen_listener:add_binding(?MODULE, ?DYN_BINDINGS(CallId, Events));
add_call_binding(Call) ->
    add_call_binding(whapps_call:call_id_direct(Call)).

add_call_binding('undefined', _) -> 'ok';
add_call_binding(CallId, Events) when is_binary(CallId) ->
    lager:debug("add pid binding for call ~s: ~p", [CallId, Events]),
    gproc:reg(?KONAMI_REG({'pid', CallId})),
    gen_listener:cast(?MODULE, {'add_bindings', CallId, Events}),
    gen_listener:add_binding(?MODULE, ?DYN_BINDINGS(CallId, Events));
add_call_binding(Call, Events) ->
    add_call_binding(whapps_call:call_id_direct(Call), Events).

-spec rm_call_binding(api_binary() | whapps_call:call()) -> 'ok'.
rm_call_binding('undefined') -> 'ok';
rm_call_binding(CallId) ->
    gen_listener:cast(?MODULE, {'rm_bindings', CallId}).

rm_call_binding('undefined', _) -> 'ok';
rm_call_binding(CallId, Events) when is_binary(CallId) ->
    gen_listener:cast(?MODULE, {'rm_bindings', CallId, Events});
rm_call_binding(Call, Events) ->
    rm_call_binding(whapps_call:call_id_direct(Call), Events).

-spec handle_call_event(wh_json:object(), wh_proplist()) -> any().
handle_call_event(JObj, Props) ->
    'true' = wapi_call:event_v(JObj),
    handle_call_event(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).
handle_call_event(JObj, _Props, <<"CHANNEL_DESTROY">>) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    rm_call_binding(CallId),
    relay_to_pids(CallId, JObj);
handle_call_event(JObj, _Props, Event) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    relay_to_fsms(CallId, Event, JObj),
    relay_to_pids(CallId, JObj).

-spec relay_to_fsms(ne_binary(), ne_binary(), wh_json:object()) -> any().
relay_to_fsms(CallId, Event, JObj) ->
    [konami_code_fsm:event(FSM, CallId, Event, JObj)
     || FSM <- gproc:lookup_pids(?KONAMI_REG({'fsm', CallId}))
    ].

-spec relay_to_pids(ne_binary(), wh_json:object()) -> any().
relay_to_pids(CallId, JObj) ->
    [whapps_call_command:relay_event(Pid, JObj)
     || Pid <- gproc:lookup_pids(?KONAMI_REG({'pid', CallId}))
    ].

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
    {'ok', #state{}}.

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
handle_cast({'add_bindings', CallId, Events}, #state{bindings=Bs}=State) ->
    lager:debug("adding events for ~s: ~p", [CallId, Events]),
    EventsSet = sets:from_list(Events),
    CurrentEventsSet = call_events(CallId, Bs),
    NewEventsSet = sets:union(CurrentEventsSet, EventsSet),
    {'noreply', State#state{bindings=dict:store(CallId, NewEventsSet, Bs)}, 'hibernate'};
handle_cast({'rm_bindings', CallId, Events}, #state{bindings=Bs}=State) ->
    lager:debug("removing events for ~s: ~p", [CallId, Events]),
    EventsSet = sets:from_list(Events),
    CurrentEventsSet = call_events(CallId, Bs),
    NewEventsSet = sets:subtract(CurrentEventsSet, EventsSet),
    RemoveEventsSet = sets:intersection(CurrentEventsSet, EventsSet),

    _ = gen_listener:rm_binding(self(), ?DYN_BINDINGS(CallId, sets:to_list(RemoveEventsSet))),
    lager:debug("removing from ~s: ~p", [CallId, sets:to_list(RemoveEventsSet)]),

    Bs1 = case sets:size(NewEventsSet) of
              0 -> dict:erase(CallId, Bs);
              _Size -> dict:store(CallId, NewEventsSet, Bs)
          end,

    {'noreply', State#state{bindings=Bs1}, 'hibernate'};
handle_cast({'rm_bindings', CallId}, #state{bindings=Bs}=State) ->
    EventsSet = call_events(CallId, Bs),
    case sets:to_list(EventsSet) of
        [] -> {'noreply', State};
        Events ->
            lager:debug("removing all events for ~s: ~p", [CallId, Events]),
            gen_listener:rm_binding(self(), ?DYN_BINDINGS(CallId, Events)),
            {'noreply', State#state{bindings=dict:erase(CallId, Bs)}, 'hibernate'}
    end;

handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
-spec call_events(ne_binary(), dict()) -> set().
call_events(CallId, Bindings) ->
    try dict:fetch(CallId, Bindings) of
        Events -> Events
    catch
        'error':'badarg' -> sets:new()
    end.
