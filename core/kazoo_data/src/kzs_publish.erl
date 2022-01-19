%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2022, 2600Hz
%%% @doc data adapter behaviour
%%% @end
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

-spec maybe_publish_docs(kz_term:ne_binary(), kz_json:objects(), kz_json:objects()) -> 'ok'.
-ifdef(TEST).
maybe_publish_docs(_, _, _) -> 'ok'.
-else.
maybe_publish_docs(Db, PreDbDocs, PostDbDocs) ->
    _ = kz_datamgr:change_notice()
        andalso should_publish_db_changes(Db)
        andalso publish_docs(Db, PreDbDocs, PostDbDocs),
    kzs_cache:flush_cache_docs(Db, PostDbDocs).

-spec publish_docs(kz_term:ne_binary(), kz_json:objects(), kz_json:objects()) -> 'ok'.
publish_docs(Db, PreDbDocs, PostDbDocs) ->
    _ = kz_util:spawn(
          fun() ->
                  [publish_doc(Db, PreDbDoc, PostDbDoc)
                   || {PreDbDoc, PostDbDoc} <- lists:zip(PreDbDocs, PostDbDocs),
                      should_publish_doc(PreDbDoc)
                  ]
          end),
    'ok'.
-endif.

-spec maybe_publish_doc(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
-ifdef(TEST).
maybe_publish_doc(_, _, _) -> 'ok'.
-else.
maybe_publish_doc(Db, PreDbDoc, PostDbDoc) ->
    _PidOrNot = kz_datamgr:change_notice()
        andalso should_publish_db_changes(Db)
        andalso should_publish_doc(PreDbDoc)
        andalso kz_util:spawn(fun publish_doc/3, [Db, PreDbDoc, PostDbDoc]),
    lager:debug("maybe publishing db/doc change: ~p", [_PidOrNot]).
-endif.

-spec publish_db(kz_term:ne_binary(), kapi_conf:action()) -> boolean().
-ifdef(TEST).
publish_db(_, _) -> 'true'.
-else.
publish_db(DbName, Action) ->
    SupressChange = kz_datamgr:change_notice(),
    _ = kz_util:spawn(fun() -> do_publish_db(SupressChange, DbName, Action) end),
    'true'.

-spec should_publish_doc(kz_json:object()) -> boolean().
should_publish_doc(Doc) ->
    ExcludeList = kapps_config:get_ne_binaries(?CONFIG_CAT
                                              ,<<"change_notice_exclude_types">>
                                              ,?DEFAULT_PUBLISH_EXCLUDE_TYPES
                                              ),
    Type = kz_doc:type(Doc),
    case kz_doc:id(Doc) of
        <<"_design/", _/binary>> = _D -> 'false';
        <<"_design%2F", _/binary>> = _D -> 'false';
        <<"_design%2f", _/binary>> = _D -> 'false';
        _Else -> not lists:member(Type, ExcludeList)
    end.

-spec should_publish_db_changes(kz_term:ne_binary()) -> boolean().
should_publish_db_changes(DbName) ->
    Key = <<"publish_", (kz_term:to_binary(kzs_util:db_classification(DbName)))/binary, "_changes">>,
    kazoo_data_config:get_is_true(Key, 'true').
-endif.

%%------------------------------------------------------------------------------
%% @doc Publish doc event to amqp
%% PreDbDoc - The doc before the database save or delete operation (Pre db operation).
%% PostDbDoc - The doc returned from the database save or delete operation (Post db operation).
%% The document event (deleted vs created vs edited) is labeled as 'deleted' if
%% the deleted flag is set in 'PreDbDoc', 'created'.
%% if 'PreDbDoc' has no rev set and 'edited' if 'PreDbDoc' has a revision.
%% @end
%%------------------------------------------------------------------------------
-spec publish_doc(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
publish_doc(DbName, PreDbDoc, PostDbDoc) ->
    case kz_doc:is_soft_deleted(PreDbDoc)
        orelse kz_doc:is_deleted(PreDbDoc)
        orelse kz_doc:revision(PreDbDoc)
    of
        'true' ->
            publish('deleted', kz_term:to_binary(DbName), publish_fields(PreDbDoc, PostDbDoc));
        'undefined' ->
            %% PreDbDoc rev is not set so its a create action
            publish('created', kz_term:to_binary(DbName), publish_fields(PreDbDoc, PostDbDoc));
        _RevIsSet ->
            %% PreDbDoc rev is set so its a update / edited action
            publish('edited', kz_term:to_binary(DbName), publish_fields(PreDbDoc, PostDbDoc))
    end.

-ifndef(TEST).
-spec do_publish_db(boolean(), kz_term:ne_binary(), kapi_conf:action()) -> 'ok'.
do_publish_db('false', _DbName, _Action) -> 'ok';
do_publish_db('true', DbName, Action) ->
    Props =
        [{<<"Type">>, <<"database">>}
        ,{<<"ID">>, DbName}
        ,{<<"Database">>, DbName}
         | kz_api:default_headers(<<"configuration">>
                                 ,<<"db_", (kz_term:to_binary(Action))/binary>>
                                 ,?CONFIG_CAT
                                 ,<<"1.0.0">>
                                 )
        ],
    Fun = fun(P) -> kapi_conf:publish_db_update(Action, DbName, P) end,
    kz_amqp_worker:cast(Props, Fun).
-endif.

%%------------------------------------------------------------------------------
%% @doc Return a predefined list of Key-Values from a doc.
%% Keys are defined by `?PUBLISH_FIELDS'.
%% @end
%%------------------------------------------------------------------------------
-spec publish_fields(kz_json:object()) -> kz_term:proplist().
publish_fields(Doc) ->
    [{Key, V} ||
        Key <- ?PUBLISH_FIELDS,
        kz_term:is_not_empty(V = kz_json:get_value(Key, Doc))
    ].

%%------------------------------------------------------------------------------
%% @doc Set select values defined by publish_fields/1 from PreDbDoc on PostDbDoc.
%% PreDbDoc - The doc before the database save or delete operation (Pre db operation).
%% PostDbDoc - The doc returned from the database save or delete operation (Post db operation).
%% @end
%%------------------------------------------------------------------------------
-spec publish_fields(kz_json:object(), kz_json:object()) -> kz_json:object().
publish_fields(PreDbDoc, PostDbDoc) ->
    kz_json:set_values(publish_fields(PreDbDoc), PostDbDoc).

-spec publish(kapi_conf:action(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
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

-spec doc_change_event_name(kapi_conf:action(), boolean()) -> kz_term:ne_binary().
doc_change_event_name(_Action, 'true') ->
    ?DOC_DELETED;
doc_change_event_name(Action, 'false') ->
    <<"doc_", (kz_term:to_binary(Action))/binary>>.

-spec doc_acct_id(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
doc_acct_id(Db, Doc) ->
    case kz_doc:account_id(Doc) of
        ?MATCH_ACCOUNT_RAW(AccountId) -> AccountId;
        _ -> maybe_account_id_from_db(kzs_util:db_classification(Db), Db)
    end.

-spec maybe_account_id_from_db(atom(), kz_term:ne_binary()) -> kz_term:api_binary().
maybe_account_id_from_db('account', Db) ->
    kz_util:format_account_id(Db);
maybe_account_id_from_db('modb', Db) ->
    kz_util:format_account_id(Db);
maybe_account_id_from_db(_, _) ->
    'undefined'.
