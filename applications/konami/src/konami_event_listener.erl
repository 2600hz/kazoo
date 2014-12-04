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
         ,handle_originate_event/2
         ,handle_metaflow_req/2
         ,queue_name/0
         ,bindings/0, bindings/1
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
-include_lib("whistle_apps/include/wh_hooks.hrl").

%% {callid, [event,...]}
-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                      ,[{<<"call_event">>, <<"*">>}]
                     }
                     ,{{?MODULE, 'handle_originate_event'}
                       ,[{<<"resource">>, <<"*">>}
                         ,{<<"error">>, <<"*">>}
                        ]
                      }
                     ,{{?MODULE, 'handle_metaflow_req'}
                       ,[{<<"metaflow">>, <<"req">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(TRACKED_CALL_EVENTS, ['DTMF', 'CHANNEL_BRIDGE', 'CHANNEL_DESTROY']).

-define(DYN_BINDINGS(CallId), {'call', [{'restrict_to', ?TRACKED_CALL_EVENTS}
                                        ,{'callid', CallId}
                                       ]}).
-define(DYN_BINDINGS(CallId, Events), {'call', [{'restrict_to', Events}
                                                ,{'callid', CallId}
                                               ]}).
-define(META_BINDINGS(CallId), {'metaflow', [{'callid', CallId}
                                             ,{'action', <<"*">>}
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

-spec bindings() -> [{ne_binary(), atoms() | ne_binaries()},...] | [].
-spec bindings(ne_binary()) -> [{ne_binary(), atoms() | ne_binaries()},...] | [].
bindings() ->
    [{props:get_value('callid', Props), props:get_value('restrict_to', Props)}
     || {'call', Props} <- gen_listener:bindings(?MODULE)
    ].

bindings(CallId) ->
    [{CallId, props:get_value('restrict_to', Props)}
     || {'call', Props} <- gen_listener:bindings(?MODULE),
        props:get_value('callid', Props) =:= CallId
    ].

-spec add_call_binding(api_binary() | whapps_call:call()) -> 'ok'.
-spec add_call_binding(api_binary() | whapps_call:call(), ne_binaries() | atoms()) -> 'ok'.
add_call_binding('undefined') -> 'ok';
add_call_binding(CallId) when is_binary(CallId) ->
    lager:debug("add fsm binding for call ~s: ~p", [CallId, ?TRACKED_CALL_EVENTS]),
    gproc:reg(?KONAMI_REG({'fsm', CallId})),
    gen_listener:add_binding(?MODULE, ?DYN_BINDINGS(CallId, ?TRACKED_CALL_EVENTS)),
    gen_listener:add_binding(?MODULE, ?META_BINDINGS(CallId));
add_call_binding(Call) ->
    gen_listener:cast(?MODULE, {'add_account_events', whapps_call:account_id(Call)}),
    gproc:reg(?KONAMI_REG({'fsm', whapps_call:account_id(Call)})),
    add_call_binding(whapps_call:call_id_direct(Call)).

add_call_binding('undefined', _) -> 'ok';
add_call_binding(CallId, Events) when is_binary(CallId) ->
    lager:debug("add pid binding for call ~s: ~p", [CallId, Events]),
    gproc:reg(?KONAMI_REG({'pid', CallId})),
    gen_listener:add_binding(?MODULE, ?DYN_BINDINGS(CallId, Events)),
    gen_listener:add_binding(?MODULE, ?META_BINDINGS(CallId));
add_call_binding(Call, Events) ->
    gen_listener:cast(?MODULE, {'add_account_events', whapps_call:account_id(Call)}),
    gproc:reg(?KONAMI_REG({'fsm', whapps_call:account_id(Call)})),
    add_call_binding(whapps_call:call_id_direct(Call), Events).

-spec rm_call_binding(api_binary() | whapps_call:call()) -> 'ok'.
rm_call_binding('undefined') -> 'ok';
rm_call_binding(CallId) ->
    gen_listener:rm_binding(?MODULE, ?DYN_BINDINGS(CallId, ?TRACKED_CALL_EVENTS)),
    gen_listener:rm_binding(?MODULE, ?META_BINDINGS(CallId)).

rm_call_binding('undefined', _) -> 'ok';
rm_call_binding(CallId, Events) when is_binary(CallId) ->
    gen_listener:rm_binding(?MODULE, ?DYN_BINDINGS(CallId, Events)),
    gen_listener:rm_binding(?MODULE, ?META_BINDINGS(CallId));
rm_call_binding(Call, Events) ->
    rm_call_binding(whapps_call:call_id_direct(Call), Events).

-spec handle_call_event(wh_json:object(), wh_proplist()) -> any().
handle_call_event(JObj, Props) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    handle_call_event(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).
handle_call_event(JObj, _Props, <<"CHANNEL_DESTROY">> = Event) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    rm_call_binding(CallId),
    relay_to_pids(CallId, JObj),
    relay_to_fsms(CallId, Event, JObj);
handle_call_event(JObj, _Props, Event) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    relay_to_fsms(CallId, Event, JObj),
    relay_to_pids(CallId, JObj).

-spec handle_originate_event(wh_json:object(), wh_proplist()) -> any().
handle_originate_event(JObj, _Props) ->
    CallId = wh_json:get_first_defined([<<"Call-ID">>, <<"Outbound-Call-ID">>], JObj),
    relay_to_pids(CallId, JObj).

-spec handle_metaflow_req(wh_json:object(), wh_proplist()) -> any().
handle_metaflow_req(JObj, _Props) ->
    'true' = wapi_metaflow:req_v(JObj),

    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Evt = wh_json:from_list(
            [{<<"module">>, wh_json:get_value(<<"Action">>, JObj)}
             ,{<<"data">>, wh_json:set_value(<<"dtmf_leg">>, CallId, wh_json:get_value(<<"Data">>, JObj))}
            ]),
    relay_to_fsm(CallId, <<"metaflow_exe">>, Evt).

-spec queue_name() -> ne_binary().
queue_name() -> gen_listener:queue_name(?MODULE).

-spec relay_to_fsms(ne_binary(), ne_binary(), wh_json:object()) -> any().
relay_to_fsms(CallId, Event, JObj) ->
    lager:debug("relaying ~s to fsms: ~p", [Event, gproc:lookup_pids(?KONAMI_REG({'fsm', CallId}))]),
    [konami_code_fsm:event(FSM, CallId, Event, JObj)
     || FSM <- gproc:lookup_pids(?KONAMI_REG({'fsm', CallId}))
    ].

-spec relay_to_fsm(ne_binary(), ne_binary(), wh_json:object()) -> any().
relay_to_fsm(CallId, Event, JObj) ->
    [FSM | _] = gproc:lookup_pids(?KONAMI_REG({'fsm', CallId})),
    lager:debug("relaying ~s to fsm: ~p", [Event, FSM]),
    konami_code_fsm:event(FSM, CallId, Event, JObj).

-spec relay_to_pids(ne_binary(), wh_json:object()) -> any().
relay_to_pids(CallId, JObj) ->
    lager:debug("relaying ~s to pids: ~p", [wh_json:get_value(<<"Event-Name">>, JObj)
                                            ,gproc:lookup_pids(?KONAMI_REG({'pid', CallId}))
                                           ]),
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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'add_account_events', AccountId}, State) ->
    lager:debug("registering for account events for ~s", [AccountId]),
    wh_hooks:register(AccountId, <<"CHANNEL_ANSWER">>),
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
handle_info(?HOOK_EVT(AccountId, EventName, Event), State) ->
    relay_to_fsms(AccountId, EventName, Event),
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
