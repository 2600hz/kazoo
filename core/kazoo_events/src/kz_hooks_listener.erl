%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2022, 2600Hz
%%% @doc Listens for a list of events and gproc-sends them out to folks who
%%% want them
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_hooks_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,maybe_add_binding/1, maybe_remove_binding/1
        ,wait_until_consuming/1
        ]).

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
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

-spec maybe_add_binding('all' | kz_term:ne_binary()) -> 'ok'.
maybe_add_binding(EventName) ->
    Events = call_events(gen_listener:bindings(?MODULE)),
    maybe_add_binding(EventName, Events).

-spec maybe_remove_binding('all' | kz_term:ne_binary()) -> 'ok'.
maybe_remove_binding(EventName) ->
    Events = call_events(gen_listener:bindings(?MODULE)),
    maybe_remove_binding(EventName, Events).

-spec wait_until_consuming(timeout()) -> 'ok' | {'error', 'timeout'}.
wait_until_consuming(Timeout) ->
    gen_listener:wait_until_consuming(?MODULE, Timeout).

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
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'federators_consuming', _IsConsuming}}, State) ->
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
maybe_add_binding('all', Events) ->
    case [E || E <- ?ALL_EVENTS, not lists:member(E, Events)] of
        [] -> 'ok';
        Es ->
            lager:info("adding bindings for ~p", [Es]),
            gen_listener:b_add_binding(?MODULE, ?CALL_BINDING(Es))
    end;
maybe_add_binding(Event, Events) ->
    case lists:member(Event, Events) of
        'true' -> 'ok';
        'false' ->
            lager:info("adding bindings for ~p", [Event]),
            gen_listener:b_add_binding(?MODULE, ?CALL_BINDING([Event]))
    end.

maybe_remove_binding('all', Events) ->
    case [E || E <- ?ALL_EVENTS, lists:member(E, Events)] of
        [] -> lager:debug("all events not in ~p", [Events]);
        Es ->
            lager:debug("removing bindings for events ~p", [Es]),
            gen_listener:b_rm_binding(?MODULE, ?CALL_BINDING(Es))
    end;
maybe_remove_binding(Event, Events) ->
    case lists:member(Event, Events) of
        'false' -> 'ok';
        'true' ->
            lager:debug("removing bindings for ~p", [Event]),
            gen_listener:b_rm_binding(?MODULE, ?CALL_BINDING([Event]))
    end.

-spec call_events([{kz_term:ne_binary(), kz_term:proplist()}]) -> kz_term:ne_binaries().
call_events(Bindings) ->
    lists:foldl(fun binding_to_call_event/2, [], Bindings).

-spec binding_to_call_event({kz_term:ne_binary(), kz_term:proplist()}, kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
binding_to_call_event({<<"call">>, CallProps}, CallEvents) ->
    case props:get_value('restrict_to', CallProps, [<<"*">>]) of
        [] -> CallEvents;
        [<<"*">>] -> lists:usort(CallEvents ++ ?ALL_EVENTS);
        Events -> lists:usort(Events ++ CallEvents)
    end;
binding_to_call_event({_Binding, _BindProps}, CallEvents) -> CallEvents.
