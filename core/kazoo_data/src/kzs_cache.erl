%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_cache).



%% Doc related
-export([open_cache_doc/3, open_cache_doc/4
        ,add_to_doc_cache/3
        ,flush_cache_doc/2
        ,flush_cache_doc/3
        ,flush_cache_docs/0
        ,flush_cache_docs/1
        ,flush_cache_docs/2
        ,flush_cache_docs/3
        ]).


-include("kz_data.hrl").

-define(DEFAULT_NO_CACHING_TYPES, [<<"media">>, <<"private_media">>, <<"call_recording">>
                                  ,<<"fax">>, <<"mailbox_message">>
                                  ]).
-define(NO_CACHING_TYPES, kapps_config:get(?CONFIG_CAT
                                          ,<<"no_caching_doc_types">>
                                          ,?DEFAULT_NO_CACHING_TYPES
                                          )).
-define(DEFAULT_CACHE_PERIOD, 15 * ?SECONDS_IN_MINUTE).
-define(DEFAULT_CACHING_POLICY, kz_json:from_list(
                                  [{<<"deprecated">>, ?DEFAULT_CACHE_PERIOD}
                                  ,{<<"aggregate">>, ?DEFAULT_CACHE_PERIOD}
                                  ,{<<"numbers">>, ?DEFAULT_CACHE_PERIOD}
                                  ,{<<"modb">>, ?DEFAULT_CACHE_PERIOD}
                                  ,{<<"account">>, ?DEFAULT_CACHE_PERIOD}
                                  ,{<<"system">>, ?DEFAULT_CACHE_PERIOD}
                                  ,{<<"system_config">>, 'infinity'}
                                  ,{<<"system_data">>, 'infinity'}
                                  ])).

-spec open_cache_doc(text(), ne_binary(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.
open_cache_doc(DbName, DocId, Options) ->
    case kz_cache:fetch_local(?CACHE_NAME, {?MODULE, DbName, DocId}) of
        {'ok', {'error', _}=E} -> E;
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            case kz_datamgr:open_doc(DbName, DocId, remove_cache_options(Options)) of
                {'error', _}=E ->
                    maybe_cache_failure(DbName, DocId, Options, E),
                    E;
                {'ok', JObj}=Ok ->
                    add_to_doc_cache(DbName, DocId, JObj),
                    Ok
            end
    end.

-spec open_cache_doc(map(), text(), ne_binary(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.
open_cache_doc(Server, DbName, DocId, Options) ->
    case kz_cache:fetch_local(?CACHE_NAME, {?MODULE, DbName, DocId}) of
        {'ok', {'error', _}=E} -> E;
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            case kzs_doc:open_doc(Server, DbName, DocId, remove_cache_options(Options)) of
                {'error', _}=E ->
                    maybe_cache_failure(DbName, DocId, Options, E),
                    E;
                {'ok', JObj}=Ok ->
                    add_to_doc_cache(DbName, DocId, JObj),
                    Ok
            end
    end.

-spec remove_cache_options(kz_proplist()) -> kz_proplist().
remove_cache_options(Options) ->
    props:delete_keys(['cache_failures'], Options).

-spec maybe_cache_failure(ne_binary(), ne_binary(), kz_proplist(), data_error()) -> 'ok'.
-spec maybe_cache_failure(ne_binary(), ne_binary(), kz_proplist(), data_error(), atoms()) -> 'ok'.
maybe_cache_failure(DbName, DocId, Options, Error) ->
    case props:get_value('cache_failures', Options) of
        ErrorCodes when is_list(ErrorCodes) ->
            maybe_cache_failure(DbName, DocId, Options, Error, ErrorCodes);
        'true' -> add_to_doc_cache(DbName, DocId, Error);
        _ -> 'ok'
    end.

maybe_cache_failure(DbName, DocId, _Options, {'error', ErrorCode}=Error, ErrorCodes) ->
    case lists:member(ErrorCode, ErrorCodes) of
        'true' -> add_to_doc_cache(DbName, DocId, Error);
        'false' -> 'ok'
    end.

-spec add_to_doc_cache(ne_binary(), ne_binary(), kz_json:object() | data_error()) -> 'ok'.
add_to_doc_cache(DbName, DocId, CacheValue) ->
    kz_cache:erase_local(?CACHE_NAME, {?MODULE, DbName, DocId}),
    CacheProps = [{'origin', {'db', DbName, DocId}}
                 ,{'expires', expires_policy_value(DbName, CacheValue)}
                 ],
    case kz_json:is_json_object(CacheValue) of
        'true' ->
            cache_if_not_media(CacheProps, DbName, DocId, CacheValue);
        'false' ->
            kz_cache:store_local(?CACHE_NAME, {?MODULE, DbName, DocId}, CacheValue, CacheProps)
    end.

-spec cache_if_not_media(kz_proplist(), ne_binary(), ne_binary(), kz_json:object() | data_error()) -> 'ok'.
cache_if_not_media(CacheProps, DbName, DocId, CacheValue) ->
    %% NOTE: this is currently necessary because when a http_put is issued to
    %%   freeswitch and the media is uploaded it goes directly to bigcouch
    %%   and therefore no doc change notice is pushed.  This results in the
    %%   doc cache containing a document tha thas no attachements (or the wrong
    %%   attachments).  What needs to happen is a change notice get sent on the
    %%   message bus anytime a http_put is issued (or maybe if the store
    %%   url is built in media IF everything uses that helper function,
                                                %    which is not currently the case...)
    case kzs_util:db_classification(DbName) =/= 'system'
        andalso lists:member(kz_doc:type(CacheValue), ?NO_CACHING_TYPES) of
        'true' -> 'ok';
        'false' -> kz_cache:store_local(?CACHE_NAME
                                       ,{?MODULE, DbName, DocId}
                                       ,CacheValue
                                       ,CacheProps
                                       )
    end.

-spec expires_policy_value(ne_binary(), kz_json:object()) -> kz_timeout().
expires_policy_value(DbName, CacheValue) ->
    Classification = kz_util:to_binary(kzs_util:db_classification(DbName)),
    Type = kz_doc:type(CacheValue, <<"no_type">>),
    expires_policy_value(DbName, Classification, Type).

-spec expires_policy_value(ne_binary(), ne_binary(), ne_binary()) -> kz_timeout().
expires_policy_value(<<"system_config">>, _, _) -> 'infinity';
expires_policy_value(<<"system_data">>, _, _) -> 'infinity';
expires_policy_value(DbName, Classification, Type) ->
    CachePolicy = kapps_config:get(?CONFIG_CAT, <<"cache_policy">>, ?DEFAULT_CACHING_POLICY),
    case kz_json:get_first_defined([[DbName, Type]
                                   ,[DbName, <<"any">>]
                                   ,[Classification, Type]
                                   ,[Classification, <<"any">>]
                                   ,[DbName]
                                   ,[Type]
                                   ,[Classification]
                                   ], CachePolicy, ?DEFAULT_CACHE_PERIOD) of
        <<"infinity">> -> 'infinity';
        Timeout -> kz_util:to_integer(Timeout)
    end.

-spec flush_cache_doc(ne_binary() | db(), ne_binary() | kz_json:object()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc) ->
    flush_cache_doc(#db{name=Name}, Doc, []);
flush_cache_doc(Db, Doc) when is_binary(Db) ->
    flush_cache_doc(Db, Doc, []).

-spec flush_cache_doc(ne_binary() | db(), ne_binary() | kz_json:object(), kz_proplist()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc, Options) ->
    flush_cache_doc(kz_util:to_binary(Name), Doc, Options);
flush_cache_doc(DbName, DocId, _Options) when is_binary(DocId) ->
    kz_cache:erase_local(?CACHE_NAME, {?MODULE, DbName, DocId});
flush_cache_doc(DbName, Doc, Options) ->
    flush_cache_doc(DbName, kz_doc:id(Doc), Options).

-spec flush_cache_docs() -> 'ok'.
flush_cache_docs() -> kz_cache:flush_local(?CACHE_NAME).

-spec flush_cache_docs(ne_binary() | db()) -> 'ok'.
flush_cache_docs(#db{name=Name}) ->
    flush_cache_docs(kz_util:to_binary(Name));
flush_cache_docs(DbName) ->
    Filter = fun({?MODULE, DbName1, _DocId}=K, _) when DbName1 =:= DbName ->
                     kz_cache:erase_local(?CACHE_NAME, K),
                     'true';
                (_, _) -> 'false'
             end,
    _ = kz_cache:filter_local(?CACHE_NAME, Filter),
    'ok'.

-spec flush_cache_docs(ne_binary() | db(), ne_binaries() | kz_json:objects()) -> 'ok'.
flush_cache_docs(Db, Docs) ->
    flush_cache_docs(Db, Docs, []).

-spec flush_cache_docs(ne_binary() | db(), ne_binaries() | kz_json:objects(), kz_proplist()) -> 'ok'.
flush_cache_docs(Db, Docs, Options) ->
    _ = [flush_cache_doc(Db, Doc, Options)
         || Doc <- Docs
        ],
    'ok'.

