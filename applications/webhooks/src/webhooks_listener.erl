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
    case wapi_conf:get_doc(JObj) of
        'undefined' -> find_and_update_hook(JObj, Srv);
        Hook ->
            gen_listener:cast(Srv, {'update_hook', webhooks_util:jobj_to_rec(Hook)})
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
    case find_hook(JObj) of
        {'ok', Hook} ->
            gen_listener:cast(Srv, {'remove_hook', webhooks_util:jobj_to_rec(Hook)});
        {'error', _E} ->
            lager:debug("failed to remove hook ~s: ~p", [webhooks_util:hook_id(JObj), _E])
    end.

-spec find_hook(wh_json:object()) ->
                       {'ok', wh_json:object()} |
                       {'error', _}.
find_hook(JObj) ->
    couch_mgr:open_cache_doc(?KZ_WEBHOOKS_DB
                             ,wapi_conf:get_id(JObj)
                            ).

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
handle_cast({'add_hook', #webhook{id=_Id}=Hook}, State) ->
    lager:debug("adding hook ~s", [_Id]),
    ets:insert_new(webhooks_util:table_id(), Hook),
    {'noreply', State};
handle_cast({'update_hook', #webhook{id=_Id}=Hook}, State) ->
    lager:debug("updating hook ~s", [_Id]),
    ets:insert(webhooks_util:table_id(), Hook),
    {'noreply', State};
handle_cast({'remove_hook', #webhook{id=Id}}, State) ->
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
