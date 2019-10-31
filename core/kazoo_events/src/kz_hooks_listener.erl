%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Listens for a list of events and gproc-sends them out to folks who
%%% want them
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_hooks_listener).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_events.hrl").
-include("kz_hooks.hrl").

-define(SERVER, ?MODULE).

%% Three main call events
-define(ALL_EVENTS, [<<"CHANNEL_CREATE">>
                    ,<<"CHANNEL_ANSWER">>
                    ,<<"CHANNEL_DESTROY">>
                    ,<<"CHANNEL_DISCONNECTED">>
                    ,<<"CHANNEL_BRIDGE">>
                    ]).
-define(CALL_BINDING(Events), {'call', [{'restrict_to', Events}
                                       ,'federate'
                                       ]}).
-define(BINDINGS, []).
-define(RESPONDERS, [{{'kz_hooks_util', 'handle_call_event'}
                     ,[{<<"call_event">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call_events = [] :: kz_term:ne_binaries()}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    kapi_call:declare_exchanges(),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'maybe_add_binding', 'all'}, #state{call_events=Events}=State) ->
    case [E || E <- ?ALL_EVENTS, not lists:member(E, Events)] of
        [] -> {'noreply', State};
        Es ->
            lager:debug("adding bindings for ~p", [Es]),
            gen_listener:add_binding(self(), ?CALL_BINDING(Es)),
            {'noreply', State#state{call_events=Es ++ Events}}
    end;
handle_cast({'maybe_add_binding', Event}, #state{call_events=Events}=State) ->
    case lists:member(Event, Events) of
        'true' -> {'noreply', State};
        'false' ->
            lager:debug("adding bindings for ~s", [Event]),
            gen_listener:add_binding(self(), ?CALL_BINDING([Event])),
            {'noreply', State#state{call_events=[Event | Events]}}
    end;
handle_cast({'maybe_remove_binding', 'all'}, #state{call_events=Events}=State) ->
    case [E || E <- ?ALL_EVENTS, lists:member(E, Events)] of
        [] -> {'noreply', State};
        Es ->
            lager:debug("removing bindings for ~p", [Es]),
            gen_listener:rm_binding(self(), ?CALL_BINDING(Es)),
            {'noreply', State#state{call_events=Events -- Es}}
    end;
handle_cast({'maybe_remove_binding', Event}, #state{call_events=Events}=State) ->
    case lists:member(Event, Events) of
        'true' -> {'noreply', State};
        'false' ->
            lager:debug("removing bindings for ~s", [Event]),
            gen_listener:rm_binding(self(), ?CALL_BINDING([Event])),
            {'noreply', State#state{call_events=lists:delete(Event, Events)}}
    end;
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', [{'rr', 'false'}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
