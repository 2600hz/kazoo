%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(webhooks_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_config/2
         ,check_failed_attempts/0
         ,find_failures/0
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

-define(EXPIRY_MSG, 'failure_check').
-record(state, {failure_tref :: reference()}).
-type state() :: #state{}.

-define(BINDINGS, [{'conf', [{'action', <<"*">>}
                             ,{'db', ?KZ_WEBHOOKS_DB}
                             ,{'type', <<"webhook">>}
                             ,{'id', <<"*">>}
                             ,'federate'
                            ]}
                  ]).

-define(RESPONDERS, [{{?MODULE, 'handle_config'}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).
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

-spec handle_config(wh_json:object(), wh_proplist()) -> 'ok'.
-spec handle_config(wh_json:object(), pid(), ne_binary()) -> 'ok'.
handle_config(JObj, Props) ->
    handle_config(JObj
                  ,props:get_value('server', Props)
                  ,wh_json:get_value(<<"Event-Name">>, JObj)
                 ).

handle_config(JObj, Srv, <<"doc_created">>) ->
    case wapi_conf:get_doc(JObj) of
        'undefined' -> find_and_add_hook(JObj, Srv);
        Hook -> webhooks_util:load_hook(Srv, Hook)
    end;
handle_config(JObj, Srv, <<"doc_edited">>) ->
    case wapi_conf:get_id(JObj) of
        'undefined' -> find_and_update_hook(JObj, Srv);
        HookId ->
            {'ok', Hook} = couch_mgr:open_cache_doc(?KZ_WEBHOOKS_DB, HookId),
            case kzd_webhook:is_enabled(Hook) of
                'true' ->
                    gen_listener:cast(Srv, {'update_hook', webhooks_util:jobj_to_rec(Hook)});
                'false' ->
                    gen_listener:cast(Srv, {'remove_hook', webhooks_util:jobj_to_rec(Hook)})
            end
    end;
handle_config(JObj, Srv, <<"doc_deleted">>) ->
    case wapi_conf:get_doc(JObj) of
        'undefined' -> find_and_remove_hook(JObj, Srv);
        Hook ->
            gen_listener:cast(Srv, {'remove_hook', webhooks_util:jobj_to_rec(Hook)})
    end.

-spec find_and_add_hook(wh_json:object(), pid()) -> 'ok'.
find_and_add_hook(JObj, Srv) ->
    case find_hook(JObj) of
        {'ok', Hook} ->
            gen_listener:cast(Srv, {'add_hook', webhooks_util:jobj_to_rec(Hook)});
        {'error', _E} ->
            lager:debug("failed to add hook ~s: ~p", [webhooks_util:hook_id(JObj), _E])
    end.

-spec find_and_update_hook(wh_json:object(), pid()) -> 'ok'.
find_and_update_hook(JObj, Srv) ->
    case find_hook(JObj) of
        {'ok', Hook} ->
            gen_listener:cast(Srv, {'update_hook', webhooks_util:jobj_to_rec(Hook)});
        {'error', _E} ->
            lager:debug("failed to update hook ~s: ~p", [webhooks_util:hook_id(JObj), _E])
    end.

-spec find_and_remove_hook(wh_json:object(), pid()) -> 'ok'.
find_and_remove_hook(JObj, Srv) ->
    gen_listener:cast(Srv, {'remove_hook', webhooks_util:hook_id(JObj)}).

-spec find_hook(wh_json:object()) ->
                       {'ok', wh_json:object()} |
                       {'error', _}.
find_hook(JObj) ->
    couch_mgr:open_cache_doc(?KZ_WEBHOOKS_DB
                             ,wapi_conf:get_id(JObj)
                            ).

-spec check_failed_attempts() -> 'ok'.
check_failed_attempts() ->
    Failures = find_failures(),
    check_failures(Failures).

-type failure() :: {{ne_binary(), ne_binary()}, integer()}.
-type failures() :: [failure(),...] | [].

-spec find_failures() -> failures().
-spec find_failures([tuple()]) -> failures().
find_failures() ->
    Keys = wh_cache:fetch_keys_local(?CACHE_NAME),
    find_failures(Keys).

find_failures(Keys) ->
    dict:to_list(lists:foldl(fun process_failed_key/2, dict:new(), Keys)).

-spec process_failed_key(tuple(), dict()) -> dict().
process_failed_key(?FAILURE_CACHE_KEY(AccountId, HookId, _Timestamp)
                   ,Dict
                  ) ->
    dict:update_counter({AccountId, HookId}, 1, Dict);
process_failed_key(_Key, Dict) ->
    Dict.

-spec check_failures(failures()) -> 'ok'.
check_failures(Failures) ->
    [check_failure(AccountId, HookId, Count)
     || {{AccountId, HookId}, Count} <- Failures
    ],
    'ok'.

-spec check_failure(ne_binary(), ne_binary(), pos_integer()) -> 'ok'.
check_failure(AccountId, HookId, Count) ->
    try wh_util:to_integer(whapps_account_config:get_global(AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY, 6)) of
        N when N =< Count ->
            disable_hook(AccountId, HookId);
        _ -> 'ok'
    catch
        _:_ ->
            lager:warning("account ~s has an non-integer for ~s/~s", [AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY]),
            case Count > 6 of
                'true' ->
                    disable_hook(AccountId, HookId);
                'false' -> 'ok'
            end
    end.

-spec disable_hook(ne_binary(), ne_binary()) -> 'ok'.
disable_hook(AccountId, HookId) ->
    case couch_mgr:open_cache_doc(?KZ_WEBHOOKS_DB, HookId) of
        {'ok', HookJObj} ->
            Disabled = kzd_webhook:disable(HookJObj, <<"too many failed attempts">>),
            _ = couch_mgr:ensure_saved(?KZ_WEBHOOKS_DB, Disabled),
            filter_cache(AccountId, HookId),
            send_notification(AccountId, HookId),
            lager:debug("auto-disabled and saved hook ~s/~s", [AccountId, HookId]);
        {'error', _E} ->
            lager:debug("failed to find ~s/~s to disable: ~p", [AccountId, HookId, _E])
    end.

-spec filter_cache(ne_binary(), ne_binary()) -> non_neg_integer().
filter_cache(AccountId, HookId) ->
    wh_cache:filter_erase_local(?CACHE_NAME
                                ,fun(?FAILURE_CACHE_KEY(A, H, _), _) ->
                                         lager:debug("maybe remove ~s/~s", [A, H]),
                                         A =:= AccountId andalso H =:= HookId;
                                    (_K, _V) -> 'false'
                                 end
                               ).

-spec send_notification(ne_binary(), ne_binary()) -> 'ok'.
send_notification(AccountId, HookId) ->
    API = [{<<"Account-ID">>, AccountId}
           ,{<<"Hook-ID">>, HookId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wh_amqp_worker:cast(API, fun wapi_notifications:publish_webhook_disabled/1).

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
    {'ok', #state{failure_tref=start_failure_check_timer()}}.

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
handle_cast({'add_hook', #webhook{id=_Id}=Hook}, State) ->
    lager:debug("adding hook ~s", [_Id]),
    ets:insert_new(webhooks_util:table_id(), Hook),
    {'noreply', State};
handle_cast({'update_hook', #webhook{id=_Id}=Hook}, State) ->
    lager:debug("updating hook ~s", [_Id]),
    _ = ets:insert(webhooks_util:table_id(), Hook),
    {'noreply', State};
handle_cast({'remove_hook', #webhook{id=Id}}, State) ->
    handle_cast({'remove_hook', Id}, State);
handle_cast({'remove_hook', <<_/binary>> = Id}, State) ->
    lager:debug("removing hook ~s", [Id]),
    ets:delete(webhooks_util:table_id(), Id),
    {'noreply', State};
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
handle_info({'ETS-TRANSFER', _TblId, _From, _Data}, State) ->
    lager:info("write access to table '~p' available", [_TblId]),
    Self = self(),
    _ = spawn(fun() ->
                      wh_util:put_callid(?MODULE),
                      webhooks_util:load_hooks(Self),
                      webhooks_util:init_mods()
              end),
    {'noreply', State};
handle_info({'timeout', Ref, ?EXPIRY_MSG}, #state{failure_tref=Ref}=State) ->
    _ = spawn(?MODULE, 'check_failed_attempts', []),
    {'noreply', State#state{failure_tref=start_failure_check_timer()}};
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

-spec start_failure_check_timer() -> reference().
start_failure_check_timer() ->
    Expiry = webhooks_util:system_expires_time(),
    erlang:start_timer(Expiry, self(), ?EXPIRY_MSG).
