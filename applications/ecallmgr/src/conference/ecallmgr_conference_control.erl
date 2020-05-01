%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Execute conference commands
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_conference_control).
-behaviour(gen_listener).

%% API
-export([start_link/3
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{fun handle_conference_command/2
                     ,[{<<"conference">>, <<"command">>}]
                     }
                    ]).

-define(BINDINGS(Id), [{'conference', [{'restrict_to', [{'command', Id}]}
                                      ,'federate'
                                      ]}
                      ]).

%% This queue is used to round-robin conference commands among ALL the
%% conference listeners with the hopes that the one receiving the command
%% can send it to the focus (barring network connectivity)...
-define(QUEUE_NAME(ConfId), <<"conference_controller_", ConfId/binary>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-record(state, {node :: atom()
               ,conference_id :: kz_term:ne_binary()
               ,instance_id :: kz_term:ne_binary()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(Node, ConferenceId, InstanceId) ->
    lager:debug("starting conference ~s control instance ~s for node ~s in ~s"
               ,[ConferenceId, InstanceId, Node, node()]
               ),
    gen_listener:start_link(?SERVER
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS(ConferenceId)}
                            ,{'queue_name', ?QUEUE_NAME(ConferenceId)}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, ConferenceId, InstanceId]
                           ).

-spec handle_conference_command(kz_json:object(), kz_term:proplist()) -> any().
handle_conference_command(JObj, Props) ->
    Node = props:get_value('node', Props),
    ConferenceId = props:get_value('conference_id', Props),
    ecallmgr_conference_command:exec_cmd(Node, ConferenceId, JObj).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:ne_binary()]) -> {'ok', state()}.
init([Node, ConferenceId, InstanceId]) ->
    kz_log:put_callid(ConferenceId),
    lager:info("starting new conference control for ~s", [ConferenceId]),
    {'ok', #state{node=Node
                 ,conference_id=ConferenceId
                 ,instance_id=InstanceId
                 }
    }.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueName}}, State) ->
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
handle_info({'stop', {Node, ConferenceId, InstanceId}}, #state{node=Node
                                                              ,conference_id=ConferenceId
                                                              ,instance_id=InstanceId
                                                              }=State) ->
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all amqp messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node
                          ,conference_id=ConferenceId
                          ,instance_id=InstanceId
                          }) ->
    {'reply', [{'node', Node}
              ,{'conference_id', ConferenceId}
              ,{'instance_id', InstanceId}
              ]
    }.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{conference_id=ConferenceId}) ->
    lager:debug("ecallmgr conference control for ~s terminating: ~p", [ConferenceId, _Reason]).

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
