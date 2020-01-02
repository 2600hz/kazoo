%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_amqp_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include("kz_caches.hrl").

-define(CHANGE_TYPE, <<"some_doc_type">>).

amqp_test() ->
    {'timeout'
    ,10
    ,{'setup'
     ,fun init/0
     ,fun cleanup/1
     ,fun(_CachePid) ->
              [{"erase changed doc", fun erase_changed_doc/0}
              ,{"erase docs in db", fun erase_docs_in_db/0}
              ]
      end
     }
    }.

init() ->
    {'ok', BindingsPid} = kazoo_bindings_sup:start_link(),
    {'ok', CachePid} = kz_cache_sup:start_link(?MODULE, ?MILLISECONDS_IN_SECOND, [{'origin_bindings', [[{'type', ?CHANGE_TYPE}]]}]),
    {CachePid, BindingsPid}.

cleanup({CachePid, BindingsPid}) ->
    exit(CachePid, 'shutdown'),
    exit(BindingsPid, 'shutdown').

erase_changed_doc() ->
    Value = kz_binary:rand_hex(5),

    Db = kz_binary:rand_hex(5),
    DocId = kz_binary:rand_hex(5),
    DocId2 = kz_binary:rand_hex(5),

    cache_doc(Db, DocId, Value),
    cache_doc(Db, DocId2, Value),

    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId})),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId2})),

    send_document_change(Db, DocId, ?DOC_EDITED),

    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, {Db, DocId})),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId2})),

    cache_doc(Db, DocId, Value),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId})),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId2})),

    send_document_change(Db, DocId, ?DOC_DELETED),
    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, {Db, DocId})),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId2})).

cache_doc(Db, DocId, Value) ->
    CacheProps = [{'origin', {'db', Db, DocId}}],
    kz_cache:store_local(?MODULE, {Db, DocId}, Value, CacheProps).

erase_docs_in_db() ->
    Value = kz_binary:rand_hex(5),

    Db = kz_binary:rand_hex(5),
    Db2 = kz_binary:rand_hex(5),

    DocIds = [kz_binary:rand_hex(5) || _ <- lists:seq(1,5)],

    [begin
         cache_doc(Db, DocId, Value),
         cache_doc(Db2, DocId, Value)
     end
     || DocId <- DocIds
    ],

    [begin
         ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db, DocId})),
         ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db2, DocId}))
     end
     || DocId <- DocIds
    ],

    send_db_change(Db, ?DB_DELETED),

    [?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, {Db, DocId}))
     || DocId <- DocIds
    ],
    [?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, {Db2, DocId}))
     || DocId <- DocIds
    ].

send_db_change(Db, ChangeType) ->
    JObj = create_db_payload(Db, ChangeType),
    RK = kz_amqp_util:document_routing_key(ChangeType, Db, ?CHANGE_TYPE, Db),
    kz_cache_listener:handle_change(JObj, [], RK).

create_db_payload(Db, ChangeType) ->
    kz_json:from_list([{<<"Type">>, <<"database">>}
                      ,{<<"ID">>, Db}
                      ,{<<"Database">>, Db}
                      ,{<<"Msg-ID">>, kz_binary:rand_hex(5)}
                       | kz_api:default_headers(?KAPI_CONF_CATEGORY, ChangeType, <<"test">>, <<"1">>)
                      ]).

send_document_change(Db, DocId, ChangeType) ->
    JObj = create_payload_json(Db, DocId, ChangeType),
    RK = kz_amqp_util:document_routing_key(ChangeType, Db, ?CHANGE_TYPE, DocId),
    kz_cache_listener:handle_change(JObj, [], RK).

create_payload_json(Db, DocId, ChangeType) ->
    kz_json:from_list([{<<"ID">>, DocId}
                      ,{<<"Database">>, Db}
                      ,{<<"Type">>, ?CHANGE_TYPE}
                      ,{<<"Msg-ID">>, kz_binary:rand_hex(5)}
                       | kz_api:default_headers(?KAPI_CONF_CATEGORY, ChangeType, <<"test">>, <<"1">>)
                      ]).
