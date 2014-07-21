%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
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

%% ETS Management
-export([table_id/0
         ,table_options/0
         ,find_me/0
         ,gift_data/0
        ]).

%% Internal Exports
-export([load_hooks/1]).

-include("webhooks.hrl").

-record(state, {}).
-type state() :: #state{}.

%% Three main call events
-define(BINDINGS, [{'conf', [{'action', <<"*">>}
                             ,{'db', ?KZ_WEBHOOKS_DB}
                             ,{'type', <<"webhook">>}
                             ,{'id', <<"*">>}
                             ,'federate'
                            ]}
                   %% channel events that toggle presence lights
                   ,{'call', [{'restrict_to', ['CHANNEL_CREATE'
                                               ,'CHANNEL_ANSWER'
                                               ,'CHANNEL_DESTROY'
                                               ,'CHANNEL_DISCONNECTED'
                                              ]}
                              ,'federate'
                             ]}
                   ,{'notifications', [{'restrict_to', ?FAX_NOTIFY_RESTRICT_TO}]}
                   ,{'notifications', [{'restrict_to', ?WEBHOOKS_NOTIFY_RESTRICT_TO}]}
                  ]).

-define(FAX_NOTIFY_RESTRICT_TO, ['outbound_fax'
                                 ,'outbound_fax_error'
                                ]).

-define(WEBHOOKS_NOTIFY_RESTRICT_TO, ['webhook']).

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
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() -> ['set'
                    ,'protected'
                    ,{'keypos', #webhook.id}
                    ,'named_table'
                   ].

-spec find_me() -> api_pid().
find_me() -> whereis(?MODULE).

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

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
        Hook -> load_hook(Srv, Hook)
    end;
handle_config(JObj, Srv, <<"doc_edited">>) ->
    case wapi_conf:get_doc(JObj) of
        'undefined' -> find_and_update_hook(JObj, Srv);
        Hook ->
            gen_listener:cast(Srv, {'update_hook', jobj_to_rec(Hook)})
    end;
handle_config(JObj, Srv, <<"doc_deleted">>) ->
    case wapi_conf:get_doc(JObj) of
        'undefined' -> find_and_remove_hook(JObj, Srv);
        Hook ->
            gen_listener:cast(Srv, {'remove_hook', jobj_to_rec(Hook)})
    end.

-spec find_and_add_hook(wh_json:object(), pid()) -> 'ok'.
find_and_add_hook(JObj, Srv) ->
    case find_hook(JObj) of
        {'ok', Hook} ->
            gen_listener:cast(Srv, {'add_hook', jobj_to_rec(Hook)});
        {'error', _E} ->
            lager:debug("failed to add hook ~s: ~p", [hook_id(JObj), _E])
    end.

-spec find_and_update_hook(wh_json:object(), pid()) -> 'ok'.
find_and_update_hook(JObj, Srv) ->
    case find_hook(JObj) of
        {'ok', Hook} ->
            gen_listener:cast(Srv, {'update_hook', jobj_to_rec(Hook)});
        {'error', _E} ->
            lager:debug("failed to update hook ~s: ~p", [hook_id(JObj), _E])
    end.

-spec find_and_remove_hook(wh_json:object(), pid()) -> 'ok'.
find_and_remove_hook(JObj, Srv) ->
    case find_hook(JObj) of
        {'ok', Hook} ->
            gen_listener:cast(Srv, {'remove_hook', jobj_to_rec(Hook)});
        {'error', _E} ->
            lager:debug("failed to remove hook ~s: ~p", [hook_id(JObj), _E])
    end.

-spec find_hook(wh_json:object()) ->
                       {'ok', wh_json:object()} |
                       {'error', _}.
find_hook(JObj) ->
    couch_mgr:open_doc(?KZ_WEBHOOKS_DB
                       ,wapi_conf:get_id(JObj)
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
    wh_json:set_value(<<"hook_event">>, <<"channel_destroy">>
                      ,base_hook_event(JObj, AccountId)
                     ).

-spec base_hook_event(wh_json:object(), api_binary()) -> wh_json:object().
base_hook_event(JObj, AccountId) ->
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
        ])).

-spec hooks_configured() -> 'ok'.
-spec hooks_configured(ne_binary()) -> 'ok'.
hooks_configured() ->
    MatchSpec = [{#webhook{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_summary(ets:select(?MODULE, MatchSpec, 1)).
hooks_configured(AccountId) ->
    MatchSpec = [{#webhook{account_id = '$1'
                           ,_ = '_'
                          }
                  ,[{'=:=', '$1', {'const', AccountId}}]
                  ,['$_']
                 }],
    print_summary(ets:select(?MODULE, MatchSpec, 1)).

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
                       }], Continuation}, Count) ->
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
    lager:debug("started ~s", [?MODULE]),
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
handle_cast({'add_hook', #webhook{id=_Id}=Hook}, State) ->
    lager:debug("adding hook ~s", [_Id]),
    ets:insert_new(?MODULE, Hook),
    {'noreply', State};
handle_cast({'update_hook', #webhook{id=_Id}=Hook}, State) ->
    lager:debug("updating hook ~s", [_Id]),
    ets:insert(?MODULE, Hook),
    {'noreply', State};
handle_cast({'remove_hook', #webhook{id=Id}}, State) ->
    lager:debug("removing hook ~s", [Id]),
    ets:delete(?MODULE, Id),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
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
    lager:debug("write access to table '~p' available", [_TblId]),
    spawn(?MODULE, 'load_hooks', [self()]),
    {'noreply', State};
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
-spec load_hooks(pid()) -> 'ok'.
load_hooks(Srv) ->
    wh_util:put_callid(?MODULE),
    lager:debug("loading hooks into memory"),
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/crossbar_listing">>
                               ,['include_docs']
                              )
    of
        {'ok', []} ->
            lager:debug("no configured webhooks");
        {'ok', WebHooks} ->
            load_hooks(Srv, WebHooks);
        {'error', 'not_found'} ->
            lager:debug("db or view not found, initing"),
            init_webhooks(),
            load_hooks(Srv);
        {'error', _E} ->
            lager:debug("failed to load webhooks: ~p", [_E])
    end,
    init_mods().

-spec init_mods() -> 'ok'.
-spec init_mods(wh_json:objects()) -> 'ok'.
init_mods() ->
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,[{'group_level', 1}]
                              )
    of
        {'ok', []} ->
            lager:debug("no accounts to load views into the MODs");
        {'ok', Accts} ->
            init_mods(Accts);
        {'error', _E} ->
            lager:debug("failed to load accounts_listing: ~p", [_E])
    end.

init_mods(Accts) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(wh_util:current_tstamp()),
    init_mods(Accts, Year, Month).
init_mods([], _, _) -> 'ok';
init_mods([Acct|Accts], Year, Month) ->
    init_mod(Acct, Year, Month),
    init_mods(Accts, Year, Month).

-spec init_mod(wh_json:object(), wh_year(), wh_month()) -> 'ok'.
init_mod(Acct, Year, Month) ->
    Db = wh_util:format_account_id(wh_json:get_value(<<"key">>, Acct), Year, Month),
    kazoo_modb:create(Db),
    lager:debug("updated account_mod ~s", [Db]).

-spec init_webhooks() -> 'ok'.
init_webhooks() ->
    _ = couch_mgr:db_create(?KZ_WEBHOOKS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    _ = couch_mgr:revise_doc_from_file(?WH_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),
    'ok'.

-spec load_hooks(pid(), wh_json:objects()) -> 'ok'.
load_hooks(Srv, WebHooks) ->
    [load_hook(Srv, wh_json:get_value(<<"doc">>, Hook)) || Hook <- WebHooks],
    lager:debug("sent hooks into server ~p", [Srv]).

-spec load_hook(pid(), wh_json:object()) -> 'ok'.
load_hook(Srv, WebHook) ->
    try jobj_to_rec(WebHook) of
        Hook -> gen_listener:cast(Srv, {'add_hook', Hook})
    catch
        'throw':{'bad_hook', HookEvent} ->
            lager:debug("failed to load hook ~s.~s: bad_hook: ~s"
                        ,[wh_json:get_value(<<"pvt_account_id">>, WebHook)
                          ,wh_json:get_value(<<"_id">>, WebHook)
                          ,HookEvent
                         ])
    end.

-spec jobj_to_rec(wh_json:object()) -> webhook().
jobj_to_rec(Hook) ->
    #webhook{id = hook_id(Hook)
             ,uri = wh_json:get_value(<<"uri">>, Hook)
             ,http_verb = http_verb(wh_json:get_value(<<"http_verb">>, Hook))
             ,hook_event = hook_event(wh_json:get_value(<<"hook">>, Hook))
             ,hook_id = wh_json:get_first_defined([<<"_id">>, <<"ID">>], Hook)
             ,retries = retries(wh_json:get_integer_value(<<"retries">>, Hook, 3))
             ,account_id = wh_json:get_value(<<"pvt_account_id">>, Hook)
             ,custom_data = wh_json:get_ne_value(<<"custom_data">>, Hook)
            }.

-spec hook_id(wh_json:object()) -> ne_binary().
-spec hook_id(ne_binary(), ne_binary()) -> ne_binary().
hook_id(JObj) ->
    hook_id(wh_json:get_first_defined([<<"pvt_account_id">>
                                       ,<<"Account-ID">>
                                      ], JObj)
            ,wh_json:get_first_defined([<<"_id">>, <<"ID">>], JObj)
           ).
hook_id(AccountId, Id) ->
    <<AccountId/binary, ".", Id/binary>>.

-spec http_verb(atom() | binary()) -> http_verb().
http_verb('get') -> 'get';
http_verb('post') -> 'post';
http_verb(Bin) when is_binary(Bin) ->
    try wh_util:to_atom(wh_util:to_lower_binary(Bin)) of
        Atom -> http_verb(Atom)
    catch
        'error':'badarg' -> 'get'
    end;
http_verb(_) -> 'get'.

-spec hook_event(ne_binary()) -> ne_binary().
-spec hook_event_lowered(ne_binary()) -> ne_binary().
hook_event(Bin) -> hook_event_lowered(wh_util:to_lower_binary(Bin)).

hook_event_lowered(<<"channel_create">>) -> <<"CHANNEL_CREATE">>;
hook_event_lowered(<<"channel_answer">>) -> <<"CHANNEL_ANSWER">>;
hook_event_lowered(<<"channel_destroy">>) -> <<"CHANNEL_DESTROY">>;
hook_event_lowered(<<"all">>) -> <<"all">>;
hook_event_lowered(Bin) -> Bin.

-spec retries(integer()) -> hook_retries().
retries(N) when N > 0, N < 5 -> N;
retries(N) when N < 1 -> 1;
retries(_) -> 5.
