-module(kz_cache_listener).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

%% API
-export([maybe_start_listener/2
        ,handle_event/2
        ]).

-include("kz_caches.hrl").

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).
-define(DATABASE_BINDING, [{'type', <<"database">>}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_start_listener(atom(), kz_term:proplist()) -> 'ok' | kz_types:startlink_ret().
maybe_start_listener(KzCacheName, Props) ->
    maybe_start_listener(KzCacheName, Props, props:get_value('origin_bindings', Props)).

-spec maybe_start_listener(atom()
                          ,kz_term:proplist()
                          ,kz_term:proplist() | 'undefined'
                          ) -> 'ok' | kz_types:startlink_ret().
maybe_start_listener(_KzCacheName, _Props, 'undefined') ->
    ok;
maybe_start_listener(KzCacheName, Props, BindingProps) ->
    ListenerNameBin = <<(kz_term:to_binary(KzCacheName))/binary, "_listener">>,
    ListenerName = kz_term:to_atom(ListenerNameBin, 'true'),
    lager:debug("started new cache process (gen_listener): ~s", [ListenerName]),
    Bindings = [{'conf', ['federate' | P]} || P <- maybe_add_db_binding(BindingProps)],
    gen_listener:start_link({'local', ListenerName}
                           ,'kz_cache_listener'
                           ,[{'bindings', Bindings}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[KzCacheName, Props]
                           ).

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_cache:state()) -> gen_listener:handle_event_return().
handle_event(JObj, State) ->
    Tab = kz_cache:tab(State),
    case (V=kapi_conf:doc_update_v(JObj))
        andalso (kz_api:node(JObj) =/= kz_term:to_binary(node())
                 orelse kz_json:get_atom_value(<<"Origin-Cache">>, JObj) =/= ets:info(Tab, 'name')
                )
    of
        'true' -> handle_document_change(JObj, State);
        'false' when V -> 'ok';
        'false' -> lager:error("payload invalid for kapi_conf: ~p", [JObj])
    end,
    'ignore'.

%%%=============================================================================
%%% `gen_server' callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', kz_cache:state()}.
init([KzCacheName, _Props]) ->
    kapi_conf:declare_exchanges(),
    {ok, kz_cache:init_state(KzCacheName
                            ,kz_cache:pointer_tab(KzCacheName)
                            ,kz_cache:monitor_tab(KzCacheName)
                            )}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), kz_cache:state()) -> kz_types:handle_call_ret_state(kz_cache:state()).
handle_call('stop', _From, State) ->
    lager:debug("recv stop from ~p", [_From]),
    {'stop', 'normal', State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), kz_cache:state()) -> kz_types:handle_cast_ret_state(kz_cache:state()).
handle_cast(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), kz_cache:state()) -> kz_types:handle_info_ret_state(kz_cache:state()).
handle_info(_Request, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), kz_cache:state()) -> 'ok'.
terminate(_Reason, State) ->
    Tab = kz_cache:tab(State),
    lager:debug("terminating ~p(~p)", [self(), Tab]),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), kz_cache:state(), any()) -> {'ok', kz_cache:state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_db_binding(kz_term:proplists()) -> kz_term:proplists().
maybe_add_db_binding([]) -> [];
maybe_add_db_binding([[]]) -> [[]];
maybe_add_db_binding(BindingProps) ->
    [?DATABASE_BINDING | BindingProps].

-spec handle_document_change(kz_json:object(), kz_cache:state()) -> 'ok' | 'false'.
handle_document_change(JObj, State) ->
    'true' = kapi_conf:doc_update_v(JObj),
    Db = kz_json:get_value(<<"Database">>, JObj),
    Type = kz_json:get_value(<<"Type">>, JObj),
    Id = kz_json:get_value(<<"ID">>, JObj),
    Keys = handle_document_change(Db, Type, Id, State),
    Keys =/= []
        andalso lager:debug("removed ~p keys for ~s/~s/~s", [length(Keys), Db, Id, Type]).

-spec handle_document_change(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_cache:state()) ->
                                    list().
handle_document_change(Db, <<"database">>, _Id, State) ->
    PTab = kz_cache:pointer_tab(State),
    MatchSpec = match_db_changed(Db),
    lists:foldl(fun(Obj, Removed) -> erase_changed(Obj, Removed, State) end
               ,[]
               ,ets:select(PTab, MatchSpec)
               );
handle_document_change(Db, Type, Id, State) ->
    PTab = kz_cache:pointer_tab(State),
    MatchSpec = match_doc_changed(Db, Type, Id),
    Objects = ets:select(PTab, MatchSpec),
    lists:foldl(fun(Obj, Removed) -> erase_changed(Obj, Removed, State) end, [], Objects).

-spec match_db_changed(kz_term:ne_binary()) -> ets:match_spec().
match_db_changed(Db) ->
    [{#cache_obj{'origin' = {'db', Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{'origin' = {'db', Db, '_'}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{'origin' = {'type', <<"database">>, Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ].

-spec match_doc_changed(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> ets:match_spec().
match_doc_changed(Db, Type, Id) ->
    [{#cache_obj{'origin' = {'db', Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{'origin' = {'db', Db, Type}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{'origin' = {'db', Db, Id}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{'origin' = {'type', Type, Id}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{'origin' = {'type', Type}, _ = '_'}
     ,[]
     ,['$_']
     }
    ].

-spec erase_changed(cache_obj(), list(), kz_cache:state()) -> list().
erase_changed(#cache_obj{'key'=Key}, Removed, State) ->
    case lists:member(Key, Removed) of
        'true' ->
            Removed;
        'false' ->
            lager:debug("removing updated cache object ~-300p", [Key]),
            'true' = kz_cache:erase_changed(Key, State),
            [Key | Removed]
    end.
