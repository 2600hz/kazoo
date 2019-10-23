%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_inbound_listener).
-behaviour(gen_listener).

-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ,format_status/2
        ,handle_debug/3
        ]).

-include("doodle.hrl").

-define(SERVER, ?MODULE).

-record(state, {connection :: amqp_listener_connection()
               }).
-type state() :: #state{}.

-define(BINDINGS(Ex), [{'sms', [{'exchange', Ex}
                               ,{'restrict_to', ['inbound']}
                               ]}
                      ,{'self', []}
                      ]).
-define(RESPONDERS, [{'doodle_inbound_handler'
                     ,[{<<"message">>, <<"inbound">>}]
                     }
                    ]).

-define(QUEUE_OPTIONS, [{'exclusive', 'false'}
                       ,{'durable', 'true'}
                       ,{'auto_delete', 'false'}
                       ,{'arguments', [{<<"x-message-ttl">>, 'infinity'}
                                      ,{<<"x-max-length">>, 'infinity'}
                                      ]}
                       ]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}
                         ,{'no_ack', 'false'}
                         ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(amqp_listener_connection()) -> kz_types:startlink_ret().
start_link(#amqp_listener_connection{broker=Broker
                                    ,exchange=Exchange
                                    ,type=Type
                                    ,queue=Queue
                                    ,options=Options
                                    }=C) ->
    Exchanges = [{Exchange, Type, Options}],
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(Exchange)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', Queue}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ,{'declare_exchanges', Exchanges}
                            ,{'broker', Broker}
                            ]
                           ,[C]
                           ,[{'debug', [{'install', {fun handle_debug/3, 'mystate'}}]
                             }
                            ]
                           ).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([amqp_listener_connection()]) -> {'ok', state()}.
init([#amqp_listener_connection{}=Connection]) ->
    {'ok', #state{connection=Connection}}.

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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("inbound listener unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("inbound listener unhandled info: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate('shutdown', _State) ->
    lager:debug("inbound listener terminating");
terminate(Reason, #state{connection=Connection}) ->
    lager:error("inbound listener unexpected termination : ~p", [Reason]),
    kz_process:spawn(fun()->
                             timer:sleep(10000),
                             doodle_inbound_listener_sup:start_inbound_listener(Connection)
                     end).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec format_status(any(), list()) -> [].
format_status(_Opt, [_PDict, _State]) -> [].

-spec handle_debug(A, any(), any()) -> A.
handle_debug(FuncState, _Event, _ProcState) -> FuncState.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
