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
         ,hooks_configured/0
         ,hooks_configured/1
         ,handle_doc_type_update/2

         ,add_object_bindings/1
         ,remove_object_bindings/1
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

%% responsible for reloading auto-disabled webhooks
-define(BINDINGS, [{'conf', [{'restrict_to', ['doc_type_updates']}
                             ,{'type', kzd_webhook:type()}
                            ]
                   }
                  ]).

-define(RESPONDERS, [{{?MODULE, 'handle_doc_type_update'}
                       ,[{<<"configuration">>, <<"doc_type_update">>}]
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
    {Bindings, Responders} = load_module_bindings_and_responders(),
    gen_listener:start_link(?MODULE
                            ,[{'bindings', Bindings}
                              ,{'responders', Responders}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[]
                           ).

-type load_acc() :: {gen_listener:bindings()
                     ,gen_listener:responders()
                    }.

-spec load_module_bindings_and_responders() -> load_acc().
load_module_bindings_and_responders() ->
    lists:foldl(fun load_module_fold/2
                ,{?BINDINGS, ?RESPONDERS}
                ,webhooks_init:existing_modules()
               ).

-spec load_module_fold(atom(), load_acc()) -> load_acc().
load_module_fold(Module, {Bindings, Responders}=Acc) ->
    try Module:bindings_and_responders() of
        {ModBindings, ModResponders} ->
            lager:debug("added ~s bindings and responders", [Module]),
            {ModBindings ++ Bindings
             ,ModResponders ++ Responders
            }
    catch
        'error':'undef' ->
            lager:debug("~s doesn't supply bindings or responders", [Module]),
            Acc;
        _E:_R ->
            lager:debug("~s failed to load bindings or responders: ~s: ~p"
                        ,[Module, _E, _R]
                       ),
            Acc
    end.

-spec handle_doc_type_update(wh_json:object(), wh_proplist()) -> 'ok'.
handle_doc_type_update(JObj, _Props) ->
    'true' = wapi_conf:doc_type_update_v(JObj),
    wh_util:put_callid(JObj),

    lager:debug("re-enabling hooks for ~s: ~s"
                ,[wapi_conf:get_account_id(JObj)
                  ,wapi_conf:get_action(JObj)
                 ]),
    webhooks_util:reenable(wapi_conf:get_account_id(JObj)
                           ,wapi_conf:get_action(JObj)
                          ),

    ServerId = wh_api:server_id(JObj),
    lager:debug("publishing resp to ~s", [ServerId]),
    wh_amqp_worker:cast([{<<"status">>, <<"success">>}
                         ,{<<"Event-Category">>, <<"configuration">>}
                         ,{<<"Event-Name">>, <<"doc_type_updated">>}
                         ,{<<"Msg-ID">>, wh_api:msg_id(JObj)}
                         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                        ]
                        ,fun(P) -> wapi_self:publish_message(ServerId, P) end
                       ).

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

-spec print_summary('$end_of_table' | {webhooks(), _}) -> 'ok'.
-spec print_summary('$end_of_table' | {webhooks(), _}, non_neg_integer()) -> 'ok'.
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

-spec add_object_bindings(ne_binary()) -> 'ok'.
add_object_bindings(AccountId) ->
    Bindings = webhooks_object:account_bindings(AccountId),
    Srv = webhooks_sup:shared_listener(),

    _ = [gen_listener:add_binding(Srv, Binding)
         || Binding <- Bindings
        ],
    'ok'.

-spec remove_object_bindings(ne_binary()) -> 'ok'.
remove_object_bindings(AccountId) ->
    Bindings = webhooks_object:account_bindings(AccountId),
    Srv = webhooks_sup:shared_listener(),

    _ = [gen_listener:rm_binding(Srv, Binding)
         || Binding <- Bindings
        ],
    'ok'.

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
    _ = wh_util:spawn(?MODULE
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
