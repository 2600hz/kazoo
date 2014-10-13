%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnipresence_shared_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_channel_event/2
         ,cached_terminated_callids/0
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("omnipresence.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-record(state, {subs_pid :: pid()
                ,subs_ref :: reference()
               }).

-define(BINDINGS, [{'self', []}
                   %% channel events that toggle presence lights
                   ,{'call', [{'restrict_to', ['CHANNEL_CREATE'
                                               ,'CHANNEL_ANSWER'
                                               ,'CHANNEL_DESTROY'
                                               ,'CHANNEL_CONNECTED'
                                               ,'CHANNEL_DISCONNECTED'
                                              ]}
                              ,'federate'
                             ]}
                   ,{'presence', [{'restrict_to', ['update'
                                                   ,'mwi_update'
                                                   ,'reset'
                                                  ]}
                                  ,'federate'
                                 ]}
                   ,{'omnipresence', [{'restrict_to', ['subscribe']}]}
                  ]).
-define(RESPONDERS, [{{'omnip_subscriptions', 'handle_channel_event'}
                      ,[{<<"call_event">>, <<"*">>}]
                     }
                     ,{{'omnip_subscriptions', 'handle_presence_update'}
                       ,[{<<"presence">>, <<"update">>}]
                      }
                     ,{{'omnip_subscriptions', 'handle_mwi_update'}
                       ,[{<<"presence">>, <<"mwi_update">>}]
                      }
                     ,{{'omnip_subscriptions', 'handle_reset'}
                       ,[{<<"presence">>, <<"reset">>}]
                      }
                     ,{{'omnip_subscriptions', 'handle_kamailio_subscribe'}
                       ,[{<<"presence">>, <<"subscription">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<"omnip_shared_listener">>).
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
start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

-define(CACHE_TERMINATED_CALLID, whapps_config:get_integer(?CONFIG_CAT, <<"cache_terminated_callid_s">>, 60)).

-spec handle_channel_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_channel_event(JObj, _Props) ->
    EventName = wh_json:get_value(<<"Event-Name">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    wh_util:put_callid(CallId),

    maybe_handle_event(JObj, CallId, EventName).

-spec maybe_handle_event(wh_json:object(), ne_binary(), ne_binary()) -> 'ok'.
maybe_handle_event(JObj, CallId, <<"CHANNEL_DESTROY">> = EventName) ->
    lager:debug("caching CHANNEL_DESTROY for ~s", [CallId]),
    wh_cache:store_local(?CACHE_NAME
                         ,terminated_cache_key(CallId)
                         ,'terminated'
                         ,[{'expires', ?CACHE_TERMINATED_CALLID}]
                        ),
    handle_the_event(JObj, EventName);
maybe_handle_event(JObj, CallId, <<"CHANNEL_CREATE">> = EventName) ->
    case wh_cache:fetch_local(?CACHE_NAME, terminated_cache_key(CallId)) of
        {'error', 'not_found'} -> handle_the_event(JObj, EventName);
        {'ok', 'terminated'} -> lager:warning("received ~s but call is terminated already, dropping", [EventName])
    end;
maybe_handle_event(JObj, CallId, <<"CHANNEL_ANSWER">> = EventName) ->
    case wh_cache:fetch_local(?CACHE_NAME, terminated_cache_key(CallId)) of
        'undefined' -> handle_the_event(JObj, EventName);
        'terminated' -> lager:warning("received ~s but call is terminated already, dropping", [EventName])
    end;
maybe_handle_event(JObj, _CallId, EventName) ->
    handle_the_event(JObj, EventName).

-spec handle_the_event(wh_json:object(), ne_binary()) -> 'ok'.
handle_the_event(JObj, EventName) ->
    omnip_subscriptions:handle_channel_event(EventName, JObj).

-spec terminated_cache_key(CallId) -> {'terminated', CallId}.
terminated_cache_key(CallId) ->
    {'terminated', CallId}.

-spec cached_terminated_callids() -> ne_binaries().
cached_terminated_callids() ->
    [CallId || {'terminated', CallId} <- wh_cache:fetch_keys_local(?CACHE_NAME)].


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
    put('callid', ?MODULE),
    gen_listener:cast(self(), 'find_subscriptions_srv'),
    lager:debug("omnipresence_listener started"),
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
handle_cast('find_subscriptions_srv', #state{subs_pid=_Pid}=State) ->
    case omnipresence_sup:subscriptions_srv() of
        'undefined' ->
            lager:debug("no subs_pid"),
            gen_listener:cast(self(), 'find_subscriptions_srv'),
            timer:sleep(500),
            {'noreply', State#state{subs_pid='undefined'}};
        P when is_pid(P) ->
            lager:debug("new subs pid: ~p", [P]),
            {'noreply', State#state{subs_pid=P
                                    ,subs_ref=erlang:monitor('process', P)
                                   }}
    end;
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
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
handle_info({'DOWN', Ref, 'process', Pid, _R}, #state{subs_pid=Pid
                                                      ,subs_ref=Ref
                                                     }=State) ->
    gen_listener:cast(self(), 'find_subscriptions_srv'),
    {'noreply', State#state{subs_pid='undefined'
                            ,subs_ref='undefined'
                           }};
handle_info(?HOOK_EVT(_, EventName, JObj), State) ->
    _ = spawn('omnip_subscriptions', 'handle_channel_event', [EventName, JObj]),
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
handle_event(_JObj, #state{subs_pid=S}) ->
    {'reply', [{'omnip_subscriptions', S}]}.

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
