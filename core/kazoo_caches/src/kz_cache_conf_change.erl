%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc Handles document change AMQP payloads and flushes caches appropriately
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_conf_change).

-export([start_link/2
        ,new_channel_flush/1
        ,channel_reconnect_flush/1

        ,handle_change/2
        ]).

-include("kz_caches.hrl").

-ifdef(TEST).
-export([handle_document_change/2]).
-endif.

-spec start_link(atom(), kz_cache:start_options()) -> 'ignore'.
start_link(Name, CacheOptions) ->
    kz_log:put_callid(listener_name(Name)),
    BindingProps = props:get_value('origin_bindings', CacheOptions, []),
    _ = [kz_cache_listener:add_binding(Name, BP)
         || BP <- BindingProps
        ],

    NewChannelFlush = props:is_true('new_channel_flush', CacheOptions, 'false'),
    ChannelReconnectFlush = props:is_true('channel_reconnect_flush', CacheOptions, 'false'),
    _ = add_bindings(Name, NewChannelFlush, ChannelReconnectFlush),
    'ignore'.

-spec listener_name(atom()) -> atom().
listener_name(Name) ->
    kz_term:to_atom(atom_to_list(Name) ++ "_listener", 'true').

add_bindings(Name, NewChannelFlush, ChannelReconnectFlush) ->
    _ = maybe_bind_for_new_channel(Name, NewChannelFlush),
    maybe_bind_for_reconnected_channel(Name, ChannelReconnectFlush).

maybe_bind_for_new_channel(_Name, 'false') -> 'ok';
maybe_bind_for_new_channel(Name, 'true') ->
    kazoo_bindings:bind(kz_cache_listener:new_channel_flush_binding(), ?MODULE, 'new_channel_flush', [Name]).

maybe_bind_for_reconnected_channel(_Name, 'false') -> 'ok';
maybe_bind_for_reconnected_channel(Name, 'true') ->
    kazoo_bindings:bind(kz_cache_listener:channel_reconnect_flush_binding(), ?MODULE, 'channel_reconnect_flush', [Name]).

-spec new_channel_flush(atom()) -> 'ok'.
new_channel_flush(Name) ->
    lager:debug("new channel, flush ~s", [Name]),
    kz_cache_ets:flush(Name).

-spec channel_reconnect_flush(atom()) -> 'ok'.
channel_reconnect_flush(Name) ->
    lager:debug("reconnected channel, flush everything from ~s", [Name]),
    kz_cache_ets:flush(Name).

-spec handle_change(atom(), kapi_conf:doc()) -> 'ok'.
handle_change([Name], JObj) ->
    handle_change(Name, JObj);
handle_change(Name, JObj) ->
    handle_change(Name, JObj, kz_json:is_json_object(JObj)).

-spec handle_change(atom(), kapi_conf:doc(), boolean()) -> 'ok'.
handle_change(_Name, _Other, 'false') ->
    lager:debug("not handling non-JSON for ~s: ~p", [_Name, _Other]);
handle_change(Name, JObj, 'true') ->
    handle_if_valid(Name, JObj, kapi_conf:doc_update_v(JObj)).

-spec handle_if_valid(atom(), kapi_conf:doc(), boolean()) -> 'ok'.
handle_if_valid(_Name, _JObj, 'false') ->
    lager:debug("not handling invalid JSON for ~s", [_Name]);
handle_if_valid(Name, JObj, 'true') ->
    kz_log:put_callid(Name),
    IsFromSameNode = kz_api:node(JObj) =:= kz_term:to_binary(node()),
    IsFromSameCache = kz_json:get_atom_value(<<"Origin-Cache">>, JObj) =:= Name,

    lager:debug("change is from same node(~s) or same cache(~s)", [IsFromSameNode, IsFromSameCache]),

    _ = exec_bindings(Name, JObj),

    case (not IsFromSameNode)
        orelse (not IsFromSameCache)
    of
        'true' -> handle_document_change(Name, JObj);
        'false' -> 'ok'
    end.

-spec handle_document_change(atom(), kapi_conf:doc()) -> 'ok' | 'false'.
handle_document_change(Name, JObj) ->
    Db = kapi_conf:get_database(JObj),
    Type = kapi_conf:get_type(JObj),
    Id = kapi_conf:get_id(JObj),

    Keys = handle_document_change(Db, Type, Id, Name),
    log_changed(Keys, Db, Id, Type).

log_changed([], _, _, _) -> 'ok';
log_changed(Keys, Db, Id, Type) ->
    lager:debug("removed ~p keys for ~s/~s/~s", [length(Keys), Db, Id, Type]).

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

-spec exec_bindings(atom(), kz_json:object()) -> list().
exec_bindings(Name, JObj) ->
    Db = kapi_conf:get_database(JObj),
    Type = kapi_conf:get_type(JObj),
    Id = kapi_conf:get_id(JObj),
    Event = kz_api:event_name(JObj),

    RK = kz_binary:join([<<"kapi.conf">>, Name, Db, Type, Event, Id], <<".">>),

    _ = kazoo_bindings:pmap(RK, JObj).
