%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_event_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,add_call_binding/1, add_call_binding/2
        ,rm_call_binding/1, rm_call_binding/2
        ,add_konami_binding/1, rm_konami_binding/1
        ,handle_call_event/2
        ,handle_originate_event/2
        ,handle_metaflow_req/2
        ,handle_konami/2
        ,queue_name/0
        ,bindings/0, bindings/1
        ,originate/1

        ,fsms/0, metaflows/0
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3

        ,cleanup_bindings/1
        ]).

-include("konami.hrl").
-include_lib("kazoo_events/include/kz_hooks.hrl").

-define(SERVER, ?MODULE).

-record(state, {cleanup_ref :: reference()
               }).
-type state() :: #state{}.

-define(CLEANUP_TIMEOUT
       ,kapps_config:get_integer(?APP_NAME, <<"event_cleanup_timeout_ms">>, ?MILLISECONDS_IN_HOUR)
       ).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                     ,[{<<"call_event">>, <<"*">>}
                      ,{<<"error">>, <<"*">>}
                      ]
                     }
                    ,{{?MODULE, 'handle_originate_event'}
                     ,[{<<"resource">>, <<"*">>}
                      ,{<<"error">>, <<"*">>}
                      ]
                     }
                    ,{{?MODULE, 'handle_metaflow_req'}
                     ,[{<<"metaflow">>, <<"action">>}]
                     }
                    ,{{?MODULE, 'handle_konami'}
                     ,[{?APP_NAME, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(TRACKED_CALL_EVENTS, [<<"CHANNEL_ANSWER">>
                             ,<<"CHANNEL_BRIDGE">>
                             ,<<"CHANNEL_DESTROY">>
                             ,<<"CHANNEL_REPLACED">>
                             ,<<"CHANNEL_TRANSFEREE">>
                             ,<<"DTMF">>
                             ]).

-define(DYN_BINDINGS(CallId), 'call', [{'restrict_to', ?TRACKED_CALL_EVENTS}
                                      ,{'callid', CallId}
                                      ]
       ).
-define(DYN_BINDINGS(CallId, Events), 'call', [{'restrict_to', Events}
                                              ,{'callid', CallId}
                                              ]
       ).
-define(META_BINDINGS(CallId), 'metaflow', [{'callid', CallId}
                                           ,{'action', <<"*">>}
                                           ,{'restrict_to', ['action']}
                                           ,'federate'
                                           ]
       ).
-define(KONAMI_BINDINGS(CallId), 'konami', [{'callid', CallId}
                                           ,{'restrict_to', ['transferred']}
                                           ]
       ).
-define(KONAMI_REG(CallId), {'p', 'l', {'konami_event', CallId}}).

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
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ]
                           ,[]
                           ).

-spec bindings() -> [{kz_term:ne_binary(), kz_term:atoms() | kz_term:ne_binaries()}].
bindings() ->
    [{props:get_value('callid', Props), props:get_value('restrict_to', Props)}
     || {'call', Props} <- gen_listener:bindings(?SERVER)
    ].

-spec bindings(kz_term:ne_binary()) -> [{kz_term:ne_binary(), kz_term:atoms() | kz_term:ne_binaries()}].
bindings(CallId) ->
    [{CallId, props:get_value('restrict_to', Props)}
     || {'call', Props} <- gen_listener:bindings(?SERVER),
        props:get_value('callid', Props) =:= CallId
    ].

-spec add_konami_binding(kz_term:api_binary()) -> 'ok'.
add_konami_binding('undefined') -> 'ok';
add_konami_binding(CallId) ->
    gen_listener:add_binding(?SERVER, ?KONAMI_BINDINGS(CallId)).

-spec rm_konami_binding(kz_term:api_binary()) -> 'ok'.
rm_konami_binding('undefined') -> 'ok';
rm_konami_binding(<<_/binary>> = CallId) ->
    gen_listener:rm_binding(?SERVER, ?KONAMI_BINDINGS(CallId)).

-spec add_call_binding(kz_term:api_ne_binary() | kapps_call:call()) -> 'ok'.
add_call_binding('undefined') -> 'ok';
add_call_binding(CallId) when is_binary(CallId) ->
    lager:debug("add fsm binding for call ~s: ~p", [CallId, ?TRACKED_CALL_EVENTS]),
    catch gproc:reg(?KONAMI_REG({'fsm', CallId})),
    gen_listener:b_add_binding(?SERVER, ?DYN_BINDINGS(CallId, ?TRACKED_CALL_EVENTS)),
    gen_listener:b_add_binding(?SERVER, ?META_BINDINGS(CallId));
add_call_binding(Call) ->
    gen_listener:cast(?SERVER, {'add_account_events', kapps_call:account_id(Call)}),
    catch gproc:reg(?KONAMI_REG({'fsm', kapps_call:account_id(Call)})),
    add_call_binding(kapps_call:call_id_direct(Call)).

-spec add_call_binding(kz_term:api_ne_binary() | kapps_call:call(), kz_term:ne_binaries() | kz_term:atoms()) -> 'ok'.
add_call_binding('undefined', _) -> 'ok';
add_call_binding(CallId, Events) when is_binary(CallId) ->
    lager:debug("add pid binding for call ~s: ~p", [CallId, Events]),
    catch gproc:reg(?KONAMI_REG({'pid', CallId})),
    gen_listener:b_add_binding(?SERVER, ?DYN_BINDINGS(CallId, Events)),
    gen_listener:b_add_binding(?SERVER, ?META_BINDINGS(CallId));
add_call_binding(Call, Events) ->
    gen_listener:cast(?SERVER, {'add_account_events', kapps_call:account_id(Call)}),
    catch gproc:reg(?KONAMI_REG({'pid', kapps_call:account_id(Call)})),
    add_call_binding(kapps_call:call_id_direct(Call), Events).

-spec rm_call_binding(kz_term:api_ne_binary() | kapps_call:call()) -> 'ok'.
rm_call_binding('undefined') -> 'ok';
rm_call_binding(CallId) ->
    catch gproc:unreg(?KONAMI_REG({'fsm', CallId})),
    case call_has_listeners(CallId) of
        'true' -> 'ok';
        'false' -> really_remove_call_bindings(CallId)
    end.

-spec rm_call_binding(kz_term:api_binary(), kz_term:ne_binaries()) -> 'ok'.
rm_call_binding('undefined', _Evts) -> 'ok';
rm_call_binding(CallId, Events) ->
    catch gproc:unreg(?KONAMI_REG({'pid', CallId})),
    case call_has_listeners(CallId) of
        'true' -> 'ok';
        'false' -> really_remove_call_bindings(CallId, Events)
    end.

-spec call_has_listeners(kz_term:ne_binary()) -> boolean().
call_has_listeners(CallId) ->
    Self = self(),
    case {fsms_for_callid(CallId), pids_for_callid(CallId)} of
        {[], []} -> 'false';
        {[Self], []} -> 'false';
        {[], [Self]} -> 'false';
        {[Self], [Self]} -> 'false';
        _ -> 'true'
    end.

-spec really_remove_call_bindings(kz_term:ne_binary()) -> 'ok'.
really_remove_call_bindings(CallId) ->
    really_remove_call_bindings(CallId, ?TRACKED_CALL_EVENTS).

-spec really_remove_call_bindings(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
really_remove_call_bindings(CallId, Events) ->
    gen_listener:rm_binding(?SERVER, ?DYN_BINDINGS(CallId, Events)),
    gen_listener:rm_binding(?SERVER, ?META_BINDINGS(CallId)).

-spec handle_call_event(kz_json:object(), kz_term:proplist()) -> any().
handle_call_event(JObj, Props) ->
    'true' = kapi_call:event_v(JObj)
        orelse kapi_dialplan:error_v(JObj),
    kz_log:put_callid(JObj),
    handle_call_event(JObj, Props, kz_call_event:event_name(JObj)).

handle_call_event(JObj, _Props, <<"CHANNEL_DESTROY">> = Event) ->
    CallId = kz_call_event:call_id(JObj),
    _ = relay_to_pids(CallId, JObj),
    _ = relay_to_fsms(CallId, Event, JObj),
    rm_call_binding(CallId);
handle_call_event(JObj, _Props, Event) ->
    CallId = kz_call_event:call_id(JObj),

    _ = relay_to_fsms(CallId, Event, JObj),
    relay_to_pids(CallId, JObj).

-spec handle_originate_event(kz_json:object(), kz_term:proplist()) -> any().
handle_originate_event(JObj, _Props) ->
    CallId = kz_json:get_first_defined([<<"Call-ID">>, <<"Outbound-Call-ID">>], JObj),
    relay_to_pids(CallId, JObj).

-spec handle_metaflow_req(kz_json:object(), kz_term:proplist()) -> any().
handle_metaflow_req(JObj, _Props) ->
    'true' = kapi_metaflow:action_v(JObj),

    CallId = kz_call_event:call_id(JObj),
    Evt = kz_json:from_list(
            [{<<"module">>, kz_json:get_value(<<"Action">>, JObj)}
            ,{<<"data">>, kz_json:set_value(<<"dtmf_leg">>
                                           ,CallId
                                           ,kz_json:get_value(<<"Data">>, JObj, kz_json:new())
                                           )
             }
            ]),
    relay_to_fsm(CallId, <<"metaflow_exe">>, Evt).

-spec handle_konami(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_konami(JObj, _Props) ->
    handle_konami_api(JObj, kz_json:get_value(<<"Event-Name">>, JObj)).

-spec handle_konami_api(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_konami_api(JObj, <<"transferred">> = Event) ->
    'true' = kapi_konami:transferred_v(JObj),
    Target = kz_json:get_value(<<"Target">>, JObj),
    relay_to_fsm(Target, Event, JObj).

-spec queue_name() -> kz_term:ne_binary().
queue_name() -> gen_listener:queue_name(?SERVER).

-spec relay_to_fsms(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> any().
relay_to_fsms(CallId, Event, JObj) ->
    [konami_code_statem:event(FSM, CallId, Event, JObj)
     || FSM <- fsms_for_callid(CallId)
    ].

-spec fsms_for_callid(kz_term:api_ne_binary() | '_') -> kz_term:pids().
fsms_for_callid('undefined') -> [];
fsms_for_callid(CallId) ->
    %% {{'p', 'l', Key}, PidToMatch, ValueToMatch}
    MatchHead = {?KONAMI_REG({'fsm', CallId}), '$1', '_'},
    Guard = [],
    Result = '$1',

    gproc:select('p', [{MatchHead, Guard, [Result]}]).

-spec fsms() -> kz_term:pids().
fsms() ->
    fsms_for_callid('_').

-spec metaflows() -> kz_term:pids().
metaflows() ->
    pids_for_callid('_').

-spec relay_to_fsm(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> any().
relay_to_fsm(CallId, Event, JObj) ->
    [FSM | _] = fsms_for_callid(CallId),
    konami_code_statem:event(FSM, CallId, Event, JObj).

-spec relay_to_pids(kz_term:ne_binary(), kz_json:object()) -> any().
relay_to_pids(CallId, JObj) ->
    [begin
         kapps_call_command:relay_event(Pid, JObj),
         lager:debug("relaying ~p to ~p", [kz_util:get_event_type(JObj), Pid])
     end
     || Pid <- pids_for_callid(CallId)
    ].

-spec pids_for_callid(kz_term:ne_binary() | '_') -> kz_term:pids().
pids_for_callid(CallId) ->
    %% {{'p', 'l', Key}, PidToMatch, ValueToMatch}
    MatchHead = {?KONAMI_REG({'pid', CallId}), '$1', '_'},
    Guard = [],
    Result = '$1',

    gproc:select('p', [{MatchHead, Guard, [Result]}]).

-spec originate(kz_term:api_terms()) -> 'ok'.
originate(Req) ->
    gen_listener:cast(?SERVER, {'originate', Req}).
%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{cleanup_ref=cleanup_timer()}}.

cleanup_timer() ->
    erlang:start_timer(?CLEANUP_TIMEOUT, self(), 'ok').

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
handle_cast({'originate', Req}, State) ->
    catch kapi_resource:publish_originate_req(Req),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'add_account_events', AccountId}, State) ->
    lager:debug("registering for account events for ~s", [AccountId]),
    kz_hooks:register(AccountId, <<"CHANNEL_ANSWER">>),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(?HOOK_EVT(AccountId, <<"CHANNEL_ANSWER">> = EventName, Event), State) ->
    _ = relay_to_fsms(AccountId, EventName, Event),
    {'noreply', State};
handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref=Ref}=State) ->
    _P = kz_process:spawn(fun cleanup_bindings/1, [self()]),
    {'noreply', State#state{cleanup_ref=cleanup_timer()}};
handle_info(_Info, State) ->
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
-spec cleanup_bindings(kz_types:server_ref()) -> 'ok'.
cleanup_bindings(Srv) ->
    kz_log:put_callid(?MODULE),
    cleanup_bindings(Srv, gen_listener:bindings(Srv)).

-spec cleanup_bindings(kz_types:server_ref(), gen_listener:bindings()) -> 'ok'.
cleanup_bindings(_Srv, []) -> 'ok';
cleanup_bindings(Srv, [{Binding, Props}|Bindings]) ->
    maybe_remove_binding(Srv, Binding, Props, props:get_value('callid', Props)),
    cleanup_bindings(Srv, Bindings).

-spec maybe_remove_binding(kz_types:server_ref(), atom(), kz_term:proplist(), kz_term:api_binary()) -> 'ok'.
maybe_remove_binding(_Srv, _Binding, _Props, 'undefined') -> 'ok';
maybe_remove_binding(Srv, Binding, Props, CallId) ->
    case {fsms_for_callid(CallId), pids_for_callid(CallId)} of
        {[], []} ->
            lager:debug("~p: no pids for call-id '~s', removing binding '~s'", [Srv, CallId, Binding]),
            gen_listener:rm_binding(Srv, Binding, Props);
        {_FSMs, _Pids} ->
            lager:debug("binding ~p still has FSMs: ~p and pids: ~p", [Binding, _FSMs, _Pids])
    end.
