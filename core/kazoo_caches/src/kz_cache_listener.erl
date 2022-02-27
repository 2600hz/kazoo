%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Bind for AMQP configuration change events and remove entries from cache
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_listener).
-behaviour(gen_listener).

-export([start_link/2
        ,add_origin_pointers/3
        ,get_origin_pointers/2
        ,listener_name/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-ifdef(TEST).
-export([handle_document_change/2]).
-endif.

-include("kz_caches.hrl").

-ifdef(TEST).
-define(BINDINGS, []).
-else.
-define(BINDINGS, [{'self', []}]).
-endif.

-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DATABASE_BINDING, [{'type', <<"database">>}]).

-record(state, {name :: atom()
               ,new_channel_flush :: boolean()
               ,channel_reconnect_flush :: boolean()
               }).
-type state() :: #state{}.

-spec start_link(atom(), kz_cache:start_options()) -> kz_types:startlink_ret().
-ifdef(TEST).
start_link(Name, _Props) ->
    gen_server:start_link({'local', listener_name(Name)}, ?MODULE, [#state{name=Name}], []).
-else.
start_link(Name, Props) ->
    BindingProps = props:get_value('origin_bindings', Props),
    Bindings = [{'conf', ['federate' | P]} || P <- maybe_add_db_binding(BindingProps)],
    gen_listener:start_link({'local', listener_name(Name)}
                           ,?MODULE
                           ,[{'bindings', Bindings}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[#state{name=Name
                                   ,new_channel_flush=props:is_true('new_channel_flush', Props)
                                   ,channel_reconnect_flush=props:is_true('channel_reconnect_flush', Props)
                                   }
                            ]
                           ).
-endif.

-spec add_origin_pointers(atom(), cache_obj(), 'undefined' | origin_tuple() | origin_tuples()) -> 'ok'.
add_origin_pointers(_Name, _CacheObj, 'undefined') -> 'ok';
add_origin_pointers(Name, CacheObj, Origin) when not is_list(Origin) ->
    add_origin_pointers(Name, CacheObj, [Origin]);
add_origin_pointers(Name, CacheObj, Origins) ->
    PointerTab = kz_cache_ets:pointer_tab(Name),
    _ = [add_origin_pointer(PointerTab, CacheObj, Origin)
         || Origin <- Origins
        ],
    'ok'.

-spec add_origin_pointer(atom(), cache_obj(), origin_tuple()) -> 'true'.
add_origin_pointer(PointerTab, #cache_obj{key=Key}=CacheObj, Origin) ->
    ets:insert(PointerTab
              ,CacheObj#cache_obj{key=Key
                                 ,value=Key
                                 ,origin=Origin
                                 ,callback='undefined'
                                 ,expires_s='infinity'
                                 }
              ).

-spec get_origin_pointers(atom(), any()) -> list().
get_origin_pointers(PointerTab, Key) ->
    ets:lookup(PointerTab, Key).

-spec listener_name(atom()) -> atom().
listener_name(Name) ->
    kz_term:to_atom(atom_to_list(Name) ++ "_listener", 'true').

-ifndef(TEST).
-spec maybe_add_db_binding(kz_term:proplists()) -> kz_term:proplists().
maybe_add_db_binding([]) -> [];
maybe_add_db_binding([[]]) -> [[]];
maybe_add_db_binding(BindingProps) ->
    [?DATABASE_BINDING | BindingProps].
-endif.

-spec init([state()]) -> {'ok', state()}.
init([#state{name=Name}=State]) ->
    kz_util:put_callid(listener_name(Name)),
    {'ok', State}.

-spec handle_call(any(), kz_types:pid_ref(), state()) -> {'noreply', state()}.
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'kz_amqp_channel', {'new_channel', 'false'}}
           ,#state{name=Name
                  ,new_channel_flush='true'
                  }=State
           ) ->
    lager:debug("new channel, flush ~s", [Name]),
    kz_cache_ets:flush(Name),
    {'noreply', State};
handle_cast({'kz_amqp_channel', {'new_channel', 'true'}}
           ,#state{name=Name
                  ,channel_reconnect_flush='true'
                  }=State
           ) ->
    lager:debug("reconnected channel, flush everything from ~s", [Name]),
    kz_cache_ets:flush(Name),
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'federators_consuming', _AreFederatorsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> 'ignore'.
handle_event(JObj, #state{name=Name}) ->
    IsValid = kapi_conf:doc_update_v(JObj),
    FromOtherNode = kz_api:node(JObj) =/= kz_term:to_binary(node()),
    FromOtherCache = kz_json:get_atom_value(<<"Origin-Cache">>, JObj) =/= ets:info(Name, 'name'),

    _ = case IsValid
            andalso (FromOtherNode
                     orelse FromOtherCache
                    )
        of
            'true' ->
                handle_document_change(JObj, Name);
            'false' when IsValid ->
                _P = kz_util:spawn(fun exec_bindings/2, [kz_term:to_binary(Name), JObj]);
            'false' ->
                lager:error("payload invalid for kapi_conf: ~p", [JObj])
        end,
    'ignore'.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec exec_bindings(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
exec_bindings(Name, JObj) ->
    Db = kz_json:get_ne_binary_value(<<"Database">>, JObj),
    Type = kz_json:get_ne_binary_value(<<"Type">>, JObj),
    Id = kz_json:get_ne_binary_value(<<"ID">>, JObj),
    Event = kz_api:event_name(JObj),
    RK = kz_binary:join([<<"kapi.conf">>, Name, Db, Type, Event, Id], <<".">>),
    _ = kazoo_bindings:pmap(RK, JObj),
    lager:debug("executed pmap routing key ~s", [RK]).

-spec handle_document_change(kz_json:object(), atom()) -> 'ok' | 'false'.
handle_document_change(JObj, Name) ->
    'true' = kapi_conf:doc_update_v(JObj),

    Db = kz_json:get_ne_binary_value(<<"Database">>, JObj),
    Type = kz_json:get_ne_binary_value(<<"Type">>, JObj),
    Id = kz_json:get_ne_binary_value(<<"ID">>, JObj),

    _P = kz_util:spawn(fun exec_bindings/2, [kz_term:to_binary(Name), JObj]),

    _Keys = handle_document_change(Db, Type, Id, Name),
    _Keys =/= []
        andalso lager:debug("removed ~p keys for ~s/~s/~s", [length(_Keys), Db, Id, Type]).

-spec handle_document_change(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), atom()) ->
          list().
handle_document_change(Db, <<"database">>, _Id, Name) ->
    MatchSpec = match_db_changed(Db),
    lists:foldl(fun(Obj, Removed) ->
                        erase_changed(Obj, Removed, Name)
                end
               ,[]
               ,ets:select(kz_cache_ets:pointer_tab(Name), MatchSpec)
               );
handle_document_change(Db, Type, Id, Name) ->
    MatchSpec = match_doc_changed(Db, Type, Id),
    Objects = ets:select(kz_cache_ets:pointer_tab(Name), MatchSpec),

    lists:foldl(fun(Obj, Removed) ->
                        erase_changed(Obj, Removed, Name)
                end
               ,[]
               ,Objects
               ).

-spec match_db_changed(kz_term:ne_binary()) -> ets:match_spec().
match_db_changed(Db) ->
    [{#cache_obj{origin = {'db', Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'db', Db, '_'}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'type', <<"database">>, Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ].

-spec match_doc_changed(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> ets:match_spec().
match_doc_changed(Db, Type, Id) ->
    [{#cache_obj{origin = {'db', Db}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'db', Db, Type}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'db', Db, Id}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'type', Type, Id}, _ = '_'}
     ,[]
     ,['$_']
     }
    ,{#cache_obj{origin = {'type', Type}, _ = '_'}
     ,[]
     ,['$_']
     }
    ].

-spec erase_changed(cache_obj(), list(), atom()) -> list().
erase_changed(#cache_obj{key=Key}, Removed, Name) ->
    case lists:member(Key, Removed) of
        'true' -> Removed;
        'false' ->
            lager:debug("removing updated cache object ~-300p", [Key]),
            'ok' = kz_cache_ets:erase(Name, Key),
            [Key | Removed]
    end.
