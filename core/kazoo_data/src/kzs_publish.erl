%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_publish).


-export([maybe_publish_doc/3
        ,maybe_publish_docs/3
        ,publish_db/2
        ,publish_doc/3
        ,publish_fields/1, publish_fields/2
        ,publish/3
        ]).

-include("kz_data.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-spec maybe_publish_docs(ne_binary(), kz_json:objects(), kz_json:objects()) -> 'ok'.
maybe_publish_docs(Db, Docs, JObjs) ->
    _ = kz_datamgr:change_notice()
        andalso should_publish_db_changes(Db)
        andalso publish_docs(Db, Docs, JObjs),
    kzs_cache:flush_cache_docs(Db, JObjs).

-spec publish_docs(ne_binary(), kz_json:objects(), kz_json:objects()) -> 'ok'.
publish_docs(Db, Docs, JObjs) ->
    _ = kz_util:spawn(
          fun() ->
                  [publish_doc(Db, Doc, JObj)
                   || {Doc, JObj} <- lists:zip(Docs, JObjs),
                      should_publish_doc(Doc)
                  ]
          end),
    'ok'.

-spec maybe_publish_doc(ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
maybe_publish_doc(Db, Doc, JObj) ->
    _ = kz_datamgr:change_notice()
        andalso should_publish_db_changes(Db)
        andalso should_publish_doc(Doc)
        andalso kz_util:spawn(fun publish_doc/3, [Db, Doc, JObj]),
    kzs_cache:flush_cache_doc(Db, JObj).

-spec publish_db(ne_binary(), kapi_conf:action()) -> boolean().
publish_db(DbName, Action) ->
    _ = kz_util:spawn(fun() -> do_publish_db(DbName, Action) end),
    'true'.

-spec should_publish_doc(kz_json:object()) -> boolean().
should_publish_doc(Doc) ->
    case kz_doc:id(Doc) of
        <<"_design/", _/binary>> = _D -> 'false';
        <<"_design%2F", _/binary>> = _D -> 'false';
        <<"_design%2f", _/binary>> = _D -> 'false';
        _Else -> 'true'
    end.

-spec should_publish_db_changes(ne_binary()) -> boolean().
should_publish_db_changes(DbName) ->
    Key = <<"publish_", (kz_util:to_binary(kzs_util:db_classification(DbName)))/binary, "_changes">>,
    kapps_config:get_is_true(?CONFIG_CAT, Key, 'true').

-spec publish_doc(ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
publish_doc(DbName, Doc, JObj) ->
    case kz_doc:is_soft_deleted(Doc)
        orelse kz_doc:is_deleted(Doc)
        orelse kz_doc:revision(JObj)
    of
        'true' ->
            publish('deleted', kz_util:to_binary(DbName), publish_fields(Doc, JObj));
        <<"1-", _/binary>> ->
            publish('created', kz_util:to_binary(DbName), publish_fields(Doc, JObj));
        _Else ->
            publish('edited', kz_util:to_binary(DbName), publish_fields(Doc, JObj))
    end.

-spec do_publish_db(ne_binary(), kapi_conf:action()) -> 'ok'.
do_publish_db(DbName, Action) ->
    Props =
        [{<<"Type">>, 'database'}
        ,{<<"ID">>, DbName}
        ,{<<"Database">>, DbName}
         | kz_api:default_headers(<<"configuration">>
                                 ,<<"db_", (kz_util:to_binary(Action))/binary>>
                                 ,?CONFIG_CAT
                                 ,<<"1.0.0">>
                                 )
        ],
    Fun = fun(P) -> kapi_conf:publish_db_update(Action, DbName, P) end,
    kapps_util:amqp_pool_send(Props, Fun).

-spec publish_fields(kz_json:object()) -> kz_proplist().
-spec publish_fields(kz_json:object(), kz_json:object()) -> kz_json:object().
publish_fields(Doc) ->
    [{Key, V} ||
        Key <- ?PUBLISH_FIELDS,
        kz_util:is_not_empty(V = kz_json:get_value(Key, Doc))
    ].

publish_fields(Doc, JObj) ->
    kz_json:set_values(publish_fields(Doc), JObj).

-spec publish(kapi_conf:action(), ne_binary(), kz_json:object()) -> 'ok'.
publish(Action, Db, Doc) ->
    Type = kz_doc:type(Doc),
    Id = kz_doc:id(Doc),

    IsSoftDeleted = kz_doc:is_soft_deleted(Doc),
    IsHardDeleted = kz_doc:is_deleted(Doc),

    EventName = doc_change_event_name(Action, IsSoftDeleted or IsHardDeleted),

    Props = props:filter_undefined(
              [{<<"ID">>, Id}
              ,{<<"Origin-Cache">>, ?CACHE_NAME}
              ,{<<"Type">>, Type}
              ,{<<"Database">>, Db}
              ,{<<"Rev">>, kz_doc:revision(Doc)}
              ,{<<"Account-ID">>, doc_acct_id(Db, Doc)}
              ,{<<"Date-Modified">>, kz_doc:created(Doc)}
              ,{<<"Date-Created">>, kz_doc:modified(Doc)}
              ,{<<"Is-Soft-Deleted">>, IsSoftDeleted}
               | kz_api:default_headers(<<"configuration">>
                                       ,EventName
                                       ,?APP_NAME
                                       ,?APP_VERSION
                                       )
              ]),
    Fun = fun(P) -> kapi_conf:publish_doc_update(Action, Db, Type, Id, P) end,
    kz_amqp_worker:cast(Props, Fun).

-spec doc_change_event_name(kapi_conf:action(), boolean()) -> ne_binary().
doc_change_event_name(_Action, 'true') ->
    ?DOC_DELETED;
doc_change_event_name(Action, 'false') ->
    <<"doc_", (kz_util:to_binary(Action))/binary>>.

-spec doc_acct_id(ne_binary(), kz_json:object()) -> ne_binary().
doc_acct_id(Db, Doc) ->
    case kz_doc:account_id(Doc) of
        ?MATCH_ACCOUNT_RAW(AccountId) -> AccountId;
        _ -> maybe_account_id_from_db(kzs_util:db_classification(Db), Db)
    end.

-spec maybe_account_id_from_db(atom(), ne_binary()) -> api_binary().
maybe_account_id_from_db('account', Db) ->
    kz_util:format_account_id(Db);
maybe_account_id_from_db('modb', Db) ->
    kz_util:format_account_id(Db);
maybe_account_id_from_db(_, _) ->
    'undefined'.
