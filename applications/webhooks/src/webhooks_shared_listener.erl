%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(webhooks_shared_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_channel_event/2
         ,maybe_handle_channel_event/3
         ,hooks_configured/0
         ,hooks_configured/1
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("webhooks.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(FAX_NOTIFY_RESTRICT_TO, ['outbound_fax'
                                 ,'outbound_fax_error'
                                ]).

-define(WEBHOOKS_NOTIFY_RESTRICT_TO, ['webhook']).

-define(BINDINGS, [%% channel events that toggle presence lights
                   {'call', [{'restrict_to', ['CHANNEL_CREATE'
                                              ,'CHANNEL_ANSWER'
                                              ,'CHANNEL_DESTROY'
                                              ,'CHANNEL_DISCONNECTED'
                                             ]}
                             ,'federate'
                            ]}
                   ,{'notifications', [{'restrict_to', ?FAX_NOTIFY_RESTRICT_TO}]}
                   ,{'notifications', [{'restrict_to', ?WEBHOOKS_NOTIFY_RESTRICT_TO}]}
                  ]).

-define(RESPONDERS, [{{?MODULE, 'handle_config'}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                     ,{{?MODULE, 'handle_channel_event'}
                       ,[{<<"call_event">>, <<"*">>}]
                      }
                     ,{{'webhooks_fax', 'handle_req'}
                       ,[{<<"notification">>, <<"outbound_fax">>}
                         ,{<<"notification">>, <<"outbound_fax_error">>}
                        ]
                      }
                     ,{{'webhooks_callflow', 'handle_req'}
                       ,[{<<"notification">>, <<"webhook">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<"webhooks_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[]
                           ).

-spec handle_channel_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_channel_event(JObj, _Props) ->
    HookEvent = hook_event_name(wh_json:get_value(<<"Event-Name">>, JObj)),
    case wh_hooks_util:lookup_account_id(JObj) of
        {'error', _R} ->
            lager:debug("failed to determine account id for ~s", [HookEvent]);
        {'ok', AccountId} ->
            lager:debug("determined account id for ~s is ~s", [HookEvent, AccountId]),
            J = wh_json:set_value([<<"Custom-Channel-Vars">>
                                   ,<<"Account-ID">>
                                  ], AccountId, JObj),
            maybe_handle_channel_event(AccountId, HookEvent, J)
    end.

-spec maybe_handle_channel_event(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_handle_channel_event(AccountId, HookEvent, JObj) ->
    lager:debug("evt ~s for ~s", [HookEvent, AccountId]),
    case webhooks_util:find_webhooks(HookEvent, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [HookEvent, AccountId]);
        Hooks -> webhooks_util:fire_hooks(format_event(JObj, AccountId, HookEvent), Hooks)
    end.

-spec hook_event_name(ne_binary()) -> ne_binary().
hook_event_name(<<"CHANNEL_DISCONNECTED">>) -> <<"CHANNEL_DESTROY">>;
hook_event_name(Event) -> Event.

-spec format_event(wh_json:object(), api_binary(), ne_binary()) ->
                          wh_json:object().
format_event(JObj, AccountId, <<"CHANNEL_CREATE">>) ->
    wh_json:set_value(<<"hook_event">>, <<"channel_create">>
                      ,base_hook_event(JObj, AccountId)
                     );
format_event(JObj, AccountId, <<"CHANNEL_ANSWER">>) ->
    wh_json:set_value(<<"hook_event">>, <<"channel_answer">>
                      ,base_hook_event(JObj, AccountId)
                     );
format_event(JObj, AccountId, <<"CHANNEL_DESTROY">>) ->
    base_hook_event(JObj, AccountId
                    ,[{<<"hook_event">>, <<"channel_destroy">>}
                      ,{<<"hangup_cause">>, wh_json:get_value(<<"Hangup-Cause">>, JObj)}
                      ,{<<"hangup_code">>, wh_json:get_value(<<"Hangup-Code">>, JObj)}
                     ]).

-spec base_hook_event(wh_json:object(), api_binary()) -> wh_json:object().
-spec base_hook_event(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
base_hook_event(JObj, AccountId) ->
    base_hook_event(JObj, AccountId, []).
base_hook_event(JObj, AccountId, Acc) ->
    WasGlobal = wh_util:is_true(ccv(JObj, <<"Global-Resource">>)),

    wh_json:from_list(
      props:filter_undefined(
        [{<<"call_direction">>, wh_json:get_value(<<"Call-Direction">>, JObj)}
         ,{<<"timestamp">>, wh_json:get_value(<<"Timestamp">>, JObj)}
         ,{<<"account_id">>, AccountId}
         ,{<<"request">>, wh_json:get_value(<<"Request">>, JObj)}
         ,{<<"to">>, wh_json:get_value(<<"To">>, JObj)}
         ,{<<"from">>, wh_json:get_value(<<"From">>, JObj)}
         ,{<<"inception">>, wh_json:get_value(<<"Inception">>, JObj)}
         ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, JObj)}
         ,{<<"other_leg_call_id">>, wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)}
         ,{<<"caller_id_name">>, wh_json:get_value(<<"Caller-ID-Name">>, JObj)}
         ,{<<"caller_id_number">>, wh_json:get_value(<<"Caller-ID-Number">>, JObj)}
         ,{<<"callee_id_name">>, wh_json:get_value(<<"Callee-ID-Name">>, JObj)}
         ,{<<"callee_id_number">>, wh_json:get_value(<<"Callee-ID-Number">>, JObj)}
         ,{<<"owner_id">>, ccv(JObj, <<"Owner-ID">>)}
         ,{<<"reseller_id">>, wh_services:find_reseller_id(AccountId)}
         ,{<<"authorizing_id">>, ccv(JObj, <<"Authorizing-ID">>)}
         ,{<<"authorizing_type">>, ccv(JObj, <<"Authorizing-Type">>)}
         ,{<<"local_resource_used">>, (not WasGlobal)}
         ,{<<"local_resource_id">>, resource_used(WasGlobal, JObj)}
         ,{<<"emergency_resource_used">>, wh_util:is_true(ccv(JObj, <<"Emergency-Resource">>))}
         | Acc
        ])).

-spec resource_used(boolean(), wh_json:object()) -> api_binary().
resource_used('true', _JObj) -> 'undefined';
resource_used('false', JObj) -> ccv(JObj, <<"Resource-ID">>).

-spec ccv(wh_json:object(), wh_json:key()) -> api_binary().
ccv(JObj, Key) ->
    wh_json:get_value([<<"Custom-Channel-Vars">>, Key], JObj).

-spec hooks_configured() -> 'ok'.
-spec hooks_configured(ne_binary()) -> 'ok'.
hooks_configured() ->
    MatchSpec = [{#webhook{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_summary(ets:select(webhooks_util:table_id(), MatchSpec, 1)).
hooks_configured(AccountId) ->
    MatchSpec = [{#webhook{account_id = '$1'
                           ,_ = '_'
                          }
                  ,[{'=:=', '$1', {'const', AccountId}}]
                  ,['$_']
                 }],
    print_summary(ets:select(webhooks_util:table_id(), MatchSpec, 1)).

-define(FORMAT_STRING_SUMMARY, "| ~-45s | ~-5s | ~-20s | ~-10s | ~-32s |~n").

-spec print_summary('$end_of_table' | {webhooks(), term()}) -> 'ok'.
-spec print_summary('$end_of_table' | {webhooks(), term()}, non_neg_integer()) -> 'ok'.
print_summary('$end_of_table') ->
    io:format("no webhooks configured~n", []);
print_summary(Match) ->
    io:format(?FORMAT_STRING_SUMMARY
                ,[<<"URI">>, <<"VERB">>, <<"EVENT">>, <<"RETRIES">>, <<"ACCOUNT ID">>]
                ),
    print_summary(Match, 0).

print_summary('$end_of_table', Count) ->
    io:format("found ~p webhooks~n", [Count]);
print_summary({[#webhook{uri=URI
                        ,http_verb=Verb
                        ,hook_event=Event
                        ,retries=Retries
                        ,account_id=AccountId
                       }]
               ,Continuation
              }
              ,Count) ->
    io:format(?FORMAT_STRING_SUMMARY
              ,[URI, Verb, Event, wh_util:to_binary(Retries), AccountId]
             ),
    print_summary(ets:select(Continuation), Count+1).

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
-spec init([]) -> {'ok', state()}.
init([]) ->
    wh_util:put_callid(?MODULE),
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
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    lager:debug("starting to consume: ~s", [_IsConsuming]),
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
handle_info(?HOOK_EVT(AccountId, EventType, JObj), State) ->
    _ = spawn(?MODULE
              ,'maybe_handle_channel_event'
              ,[AccountId, EventType, JObj]
             ),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
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
    lager:debug("shared listener terminating: ~p", [_Reason]).

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
