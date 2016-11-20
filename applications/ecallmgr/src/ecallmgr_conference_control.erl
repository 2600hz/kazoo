%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016 2600Hz INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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
                    ,{ fun handle_conference_event/2
                     ,[{<<"conference">>, <<"event">>}]
                     }
                    ]).

-define(BINDINGS(Id), [{'conference', [{'restrict_to', [{'command', Id}
                                                       ,{'event', [{'conference_id', Id}
                                                                  ,{'event', <<"conference-destroy">>}
                                                                  ]}
                                                       ]}
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
               ,conference_id :: ne_binary()
               ,instance_id :: ne_binary()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(Node, ConferenceId, InstanceId) ->
    gen_listener:start_link(?SERVER,
                            [{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS(ConferenceId)}
                            ,{'queue_name', ?QUEUE_NAME(ConferenceId)}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ], [Node, ConferenceId, InstanceId]).

-spec handle_conference_command(kz_json:object(), kz_proplist()) -> any().
handle_conference_command(JObj, Props) ->
    Node = props:get_value('node', Props),
    ConferenceId = props:get_value('conference_id', Props),
    ecallmgr_conference_command:exec_cmd(Node, ConferenceId, JObj).

-spec handle_conference_event(kz_json:object(), kz_proplist()) -> any().
handle_conference_event(JObj, Props) ->
    'true' = kapi_conference:event_v(JObj),
    Node = props:get_value('node', Props),
    Server = props:get_value('server', Props),
    ConferenceId = props:get_value('conference_id', Props),
    Event = kz_json:get_value(<<"Event">>, JObj),
    handle_conference_event(Node, Server, ConferenceId, Event, JObj).

handle_conference_event(_Node, Server, ConferenceId, <<"conference-destroy">>, _JObj) ->
    lager:debug("received conference-destroy for ~s, terminating", [ConferenceId]),
    gen_server:stop(Server);
handle_conference_event(_Node, _Server, ConferenceId, Event, _JObj) ->
    lager:debug("received not handled event ~s for ~s", [Event, ConferenceId]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([atom() | ne_binary()]) -> {'ok', state()}.
init([Node, ConferenceId, InstanceId]) ->
    kz_util:put_callid(ConferenceId),
    lager:info("starting new conference control for ~s", [ConferenceId]),
    {'ok', #state{node=Node
                 ,conference_id=ConferenceId
                 ,instance_id=InstanceId
                 }
    }.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'gen_listener',{'created_queue',_QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all amqp messages
%%
%% @spec handle_event(JObj, Props) -> {reply, Props} |
%%                                    ignore
%% @end
%%--------------------------------------------------------------------
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{conference_id=ConfereceId}) ->
    lager:debug("ecallmgr conference control for ~s terminating: ~p", [ConfereceId, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
