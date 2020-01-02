%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Listener for route requests that can be fulfilled by Callflows.
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_event_listener).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/3
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_events.hrl").

-type state() :: map().

-define(SERVER, ?MODULE).

-define(RESPONDERS, []).
-define(BINDINGS, [{'bind', [{'exchange', ?KZ_CALL_EVENTS_EXCHANGE}
                            ,{'routing', <<"20">>}
                            ]
                   }
                  ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(map()) -> {'ok', state()}.
init(State) ->
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:info("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling AMQP event objects
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, Props, _State) ->
    CallId = kz_call_event:call_id(JObj),
    gproc:send({'p', 'l', {'call_event', CallId}}, {'kapi', kapi:delivery_message(JObj, Props)}),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("kazoo event listener ~p termination", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
