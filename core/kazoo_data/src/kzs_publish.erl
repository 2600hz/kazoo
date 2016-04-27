%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_publish).


-export([maybe_publish_db/2
         ,maybe_publish_doc/3
         ,maybe_publish_docs/3
         ,publish_db/2
         ,publish_doc/3
         ,publish_fields/1, publish_fields/2
         ,publish/3
        ]).

-include("kz_data.hrl").
-include_lib("whistle/include/wapi_conf.hrl").

-spec maybe_publish_docs(ne_binary(), wh_json:objects(), wh_json:objects()) -> 'ok'.
maybe_publish_docs(Db, Docs, JObjs) ->
    case kz_datamgr:change_notice()
        andalso should_publish_db_changes(Db)
    of
        'true' ->
            _ = wh_util:spawn(
                  fun() ->
                          [publish_doc(Db, Doc, JObj)
                           || {Doc, JObj} <- lists:zip(Docs, JObjs)
                                  , should_publish_doc(Doc)
                          ]
                  end),
            'ok';
        'false' -> 'ok'
    end.

-spec maybe_publish_doc(ne_binary(), wh_json:object(), wh_json:object()) -> 'ok'.
maybe_publish_doc(Db, Doc, JObj) ->
    case kz_datamgr:change_notice()
        andalso should_publish_db_changes(Db)
        andalso should_publish_doc(Doc)
    of
        'true' ->
            _ = wh_util:spawn(fun() -> publish_doc(Db, Doc, JObj) end),
            'ok';
        'false' -> 'ok'
    end.

-spec maybe_publish_db(ne_binary(), wapi_conf:action()) -> 'ok'.
maybe_publish_db(DbName, Action) ->
    case kz_datamgr:change_notice() of
        'true' ->
            _ = wh_util:spawn(fun() -> publish_db(DbName, Action) end),
            'ok';
        'false' -> 'ok'
    end.

-spec should_publish_doc(wh_json:object()) -> boolean().
should_publish_doc(Doc) ->
    case wh_doc:id(Doc) of
        <<"_design/", _/binary>> = _D -> 'false';
        _Else -> 'true'
    end.

-spec should_publish_db_changes(ne_binary()) -> boolean().
should_publish_db_changes(DbName) ->
    Key = <<"publish_", (wh_util:to_binary(kzs_util:db_classification(DbName)))/binary, "_changes">>,
    whapps_config:get_is_true(?CONFIG_CAT, Key, 'true').

-spec publish_doc(ne_binary(), wh_json:object(), wh_json:object()) -> 'ok'.
publish_doc(DbName, Doc, JObj) ->
    case wh_doc:is_soft_deleted(Doc)
        orelse wh_doc:is_deleted(Doc)
    of
        'true' ->
            publish('deleted', wh_util:to_binary(DbName), publish_fields(Doc, JObj));
        'false' ->
            case wh_doc:revision(JObj) of
                <<"1-", _/binary>> ->
                    publish('created', wh_util:to_binary(DbName), publish_fields(Doc, JObj));
                _Else ->
                    publish('edited', wh_util:to_binary(DbName), publish_fields(Doc, JObj))
            end
    end.

-spec publish_db(ne_binary(), wapi_conf:action()) -> 'ok'.
publish_db(DbName, Action) ->
    Props =
        [{<<"Type">>, 'database'}
         ,{<<"ID">>, DbName}
         ,{<<"Database">>, DbName}
         | wh_api:default_headers(<<"configuration">>
                                  ,<<"db_", (wh_util:to_binary(Action))/binary>>
                                  ,?CONFIG_CAT
                                  ,<<"1.0.0">>
                                 )
        ],
    Fun = fun(P) -> wapi_conf:publish_db_update(Action, DbName, P) end,
    whapps_util:amqp_pool_send(Props, Fun).

-spec publish_fields(wh_json:object()) -> wh_proplist().
-spec publish_fields(wh_json:object(), wh_json:object()) -> wh_json:object().
publish_fields(Doc) ->
    [{Key, V} ||
        Key <- ?PUBLISH_FIELDS,
        wh_util:is_not_empty(V = wh_json:get_value(Key, Doc))
    ].

publish_fields(Doc, JObj) ->
    wh_json:set_values(publish_fields(Doc), JObj).

-spec publish(wapi_conf:action(), ne_binary(), wh_json:object()) -> 'ok'.
publish(Action, Db, Doc) ->
    Type = wh_doc:type(Doc),
    Id = wh_doc:id(Doc),

    IsSoftDeleted = wh_doc:is_soft_deleted(Doc),
    IsHardDeleted = wh_doc:is_deleted(Doc),

    EventName = doc_change_event_name(Action, IsSoftDeleted orelse IsHardDeleted),

    Props =
        [{<<"ID">>, Id}
        ,{<<"Origin-Cache">>, ?KZ_DATA_CACHE}
        ,{<<"Type">>, Type}
        ,{<<"Database">>, Db}
        ,{<<"Rev">>, wh_doc:revision(Doc)}
        ,{<<"Account-ID">>, doc_acct_id(Db, Doc)}
        ,{<<"Date-Modified">>, wh_doc:created(Doc)}
        ,{<<"Date-Created">>, wh_doc:modified(Doc)}
        ,{<<"Is-Soft-Deleted">>, IsSoftDeleted}
         | wh_api:default_headers(<<"configuration">>
                                 ,EventName
                                 ,?APP_NAME
                                 ,?APP_VERSION
                                 )
        ],
    Fun = fun(P) -> wapi_conf:publish_doc_update(Action, Db, Type, Id, P) end,
    wh_amqp_worker:cast(Props, Fun).

-spec doc_change_event_name(wapi_conf:action(), boolean()) -> ne_binary().
doc_change_event_name(_Action, 'true') ->
    ?DOC_DELETED;
doc_change_event_name(Action, 'false') ->
    <<"doc_", (wh_util:to_binary(Action))/binary>>.

-spec doc_acct_id(ne_binary(), wh_json:object()) -> ne_binary().
doc_acct_id(Db, Doc) ->
    case wh_doc:account_id(Doc) of
        'undefined' -> wh_util:format_account_id(Db, 'raw');
        AccountId -> AccountId
    end.
