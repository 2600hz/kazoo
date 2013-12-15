%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_config/2
         ,handle_call_event/2
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
         ,find_me_function/0
         ,gift_data/0
        ]).

%% Internal Exports
-export([load_hooks/1]).

-include("webhooks.hrl").

-record(state, {}).

-type http_verb() :: 'get' | 'post'.

-record(webhook, {
          id :: ne_binary()
          ,uri :: ne_binary()
          ,http_verb :: http_verb()
          ,hook_event :: ne_binary()
          ,retries = 3 :: 1..5
          ,account_id :: ne_binary()
         }).

%% Three main call events
-define(BINDINGS, [{'call', [{'restrict_to', ['new_channel'
                                              ,'answered_channel'
                                              ,'destroy_channel'
                                             ]}]}
                   ,{'conf', [{'action', <<"*">>}
                              ,{'db', ?KZ_WEBHOOKS_DB}
                              ,{'type', <<"webhook">>}
                              ,{'id', <<"*">>}
                             ]}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_config'}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                     ,{{?MODULE, 'handle_call_event'}
                       ,[{<<"channel">>, <<"*">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<"webhooks_listener">>).
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

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() -> ['set', 'protected', {'keypos', #webhook.id}].

-spec find_me_function() -> api_pid().
find_me_function() -> whereis(?MODULE).

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

-spec handle_config(wh_json:object(), wh_proplist()) -> 'ok'.
-spec handle_config(wh_json:object(), pid(), ne_binary()) -> 'ok'.
handle_config(JObj, Props) ->
    handle_config(JObj
                  ,props:get_value('server', Props)
                  ,wh_json:get_value(<<"Event-Type">>, JObj)
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
    couch_mgr:open_doc(wapi_conf:get_account_db(JObj)
                       ,wapi_conf:get_id(JObj)
                      ).

-spec handle_call_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, _Props) ->
    HookEvent = wh_json:get_value(<<"Event-Name">>, JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                                  ,JObj
                                 ),
    lager:debug("handle event ~s: ~s", [HookEvent, AccountId]).

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
handle_cast({'wh_amqp_channel', {'new_channel', _IsNew}}, State) ->
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
    lager:debug("write access to table ~s available", [_TblId]),
    spawn(?MODULE, 'load_hooks', [self()]),
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
        {'error', _E} ->
            lager:debug("failed to load webhooks: ~p", [_E])
    end.

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

-spec jobj_to_rec(wh_json:object()) -> #webhook{}.
jobj_to_rec(Hook) ->
    #webhook{id = hook_id(Hook)
             ,uri = wh_json:get_value(<<"uri">>, Hook)
             ,http_verb = http_verb(wh_json:get_value(<<"http_verb">>, Hook))
             ,hook_event = hook_event(wh_json:get_value(<<"hook">>, Hook))
             ,retries = retries(wh_json:get_integer_value(<<"retries">>, Hook, 3))
             ,account_id = wh_json:get_value(<<"pvt_account_id">>, Hook)
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

hook_event_lowered(<<"new_channel">>) -> <<"new_channel">>;
hook_event_lowered(<<"answered_channel">>) -> <<"answered_channel">>;
hook_event_lowered(<<"destroy_channel">>) -> <<"destroy_channel">>;
hook_event_lowered(<<"all">>) -> <<"all">>;
hook_event_lowered(Bin) -> throw({'bad_hook', Bin}).

-spec retries(integer()) -> 1..5.
retries(N) when N > 0, N < 5 -> N;
retries(N) when N < 1 -> 1;
retries(_) -> 5.

