%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_cache).

%% Doc related
-export([open_cache_doc/3, open_cache_doc/4
        ,open_cache_docs/3
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
-define(NO_CACHING_TYPES
       ,kazoo_data_config:get_ne_binaries(<<"no_caching_doc_types">>, ?DEFAULT_NO_CACHING_TYPES)
       ).

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
            R = kz_datamgr:open_doc(DbName, DocId, remove_cache_options(Options)),
            _ = maybe_cache(DbName, DocId, Options, R),
            R
    end.

-spec maybe_cache(ne_binary(), ne_binary(), kz_proplist(), data_error() | {'ok', kz_json:object()}) ->
                         'ok'.
maybe_cache(DbName, DocId, Options, {'error', _}=E) ->
    maybe_cache_failure(DbName, DocId, Options, E);
maybe_cache(DbName, DocId, _, {'ok', JObj}) ->
    add_to_doc_cache(DbName, DocId, JObj).

-spec open_cache_doc(map(), text(), ne_binary(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.
open_cache_doc(Server, DbName, DocId, Options) ->
    case kz_cache:fetch_local(?CACHE_NAME, {?MODULE, DbName, DocId}) of
        {'ok', {'error', _}=E} -> E;
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            R = kzs_doc:open_doc(Server, DbName, DocId, remove_cache_options(Options)),
            _ = maybe_cache(DbName, DocId, Options, R),
            R
    end.

-spec open_cache_docs(text(), ne_binaries(), kz_proplist()) ->
                             {'ok', kz_json:objects()} |
                             data_error().
open_cache_docs(DbName, DocIds, Options) ->
    {Cached, MissedDocIds} = fetch_locals(DbName, DocIds),
    lager:debug("misses: ~p", [MissedDocIds]),
    case kz_datamgr:open_docs(DbName, MissedDocIds, remove_cache_options(Options)) of
        {error, _}=E -> E;
        {ok, Opened} ->
            FromBulk = disassemble_jobjs(DbName, Options, Opened),
            {ok, assemble_jobjs(DocIds, Cached, FromBulk)}
    end.

fetch_locals(DbName, DocIds) ->
    F = fun (DocId, {Cached, Missed}) ->
                case kz_cache:fetch_local(?CACHE_NAME, {?MODULE, DbName, DocId}) of
                    {error, not_found} -> {Cached, [DocId|Missed]};
                    {ok, {error, Reason}} ->
                        {[{DocId, error, Reason}|Cached], Missed};
                    {ok, Doc} ->
                        {[{DocId, ok, Doc}|Cached], Missed}
                end
        end,
    lists:foldl(F, {[], []}, DocIds).

-type doc_returned() :: {ne_binary(), ok, kz_json:object()} |
                        {ne_binary(), error, ne_binary()}.
-type docs_returned() :: [doc_returned()].
-spec disassemble_jobjs(ne_binary(), kz_proplist(), kz_json:objects()) -> docs_returned().
disassemble_jobjs(DbName, Options, JObjs) ->
    [case kz_json:get_json_value(<<"doc">>, JObj) of
         'undefined' ->
             %% Reason is not_found for when documents were deleted.
             Reason = kz_json:get_ne_binary_value(<<"error">>, JObj, <<"not_found">>),
             _ = maybe_cache_failure(DbName, DocId, Options, {'error', kz_term:to_atom(Reason)}),
             {DocId, 'error', Reason};
         Doc ->
             _ = add_to_doc_cache(DbName, DocId, Doc),
             {DocId, 'ok', Doc}
     end
     || JObj <- JObjs,
        DocId <- [kz_json:get_ne_value(<<"key">>, JObj)]
    ].

-spec assemble_jobjs(ne_binaries(), docs_returned(), docs_returned()) -> kz_json:objects().
assemble_jobjs(DocIds, Cached, DocsReturned) ->
    JObjs1 = [to_nonbulk_format(Returned) || Returned <- Cached],
    JObjs2 = [to_nonbulk_format(Returned) || Returned <- DocsReturned],
    kz_json:order_by([<<"key">>], DocIds, [JObjs1,JObjs2]).

-spec to_nonbulk_format(doc_returned()) -> kz_json:object().
to_nonbulk_format({DocId, ok, Doc}) ->
    kz_json:from_list(
      [{<<"key">>, DocId}
      ,{<<"doc">>, Doc}
      ]);
to_nonbulk_format({DocId, error, Reason}) ->
    kz_json:from_list(
      [{<<"key">>, DocId}
      ,{<<"error">>, kz_term:to_atom(Reason, true)}
      ]).

-spec remove_cache_options(kz_proplist()) -> kz_proplist().
remove_cache_options(Options) ->
    props:delete_keys(['cache_failures'], Options).

-spec maybe_cache_failure(ne_binary(), ne_binary(), kz_proplist(), data_error()) -> 'ok'.
-spec maybe_cache_failure(ne_binary(), ne_binary(), kz_proplist(), data_error(), atoms()) -> 'ok'.
maybe_cache_failure(DbName, DocId, Options, {'error', _}=Error) ->
    case props:get_value('cache_failures', Options) of
        ErrorCodes when is_list(ErrorCodes) ->
            maybe_cache_failure(DbName, DocId, Options, Error, ErrorCodes);
        'true' ->
            maybe_cache_failure(DbName, DocId, Options, Error, ['not_found']);
        _ -> 'ok'
    end.

maybe_cache_failure(DbName, DocId, _Options, {'error', ErrorCode}=Error, ErrorCodes) ->
    _ = lists:member(ErrorCode, ErrorCodes)
        andalso add_to_doc_cache(DbName, DocId, Error),
    'ok'.

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

-spec cache_if_not_media(kz_proplist(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
cache_if_not_media(CacheProps, DbName, DocId, CacheValue) ->
    %% NOTE: this is currently necessary because when a http_put is issued to
    %%   freeswitch and the media is uploaded it goes directly to bigcouch
    %%   and therefore no doc change notice is pushed.  This results in the
    %%   doc cache containing a document that has no attachements (or the wrong
    %%   attachments). What needs to happen is a change notice get sent on the
    %%   message bus anytime a http_put is issued (or maybe if the store
    %%   url is built in media IF everything uses that helper function,
    %%   which is not currently the case...)
    case kzs_util:db_classification(DbName) =/= 'system'
        andalso lists:member(kz_doc:type(CacheValue), ?NO_CACHING_TYPES) of
        'true' -> 'ok';
        'false' -> kz_cache:store_local(?CACHE_NAME
                                       ,{?MODULE, DbName, DocId}
                                       ,CacheValue
                                       ,CacheProps
                                       )
    end.

-spec expires_policy_value(ne_binary(), kz_json:object() | data_error()) -> kz_timeout().
expires_policy_value(_DbName, {'error', _}) ->
    ?DEFAULT_CACHE_PERIOD;
expires_policy_value(DbName, CacheValue) ->
    Classification = kz_term:to_binary(kzs_util:db_classification(DbName)),
    Type = kz_doc:type(CacheValue, <<"no_type">>),
    expires_policy_value(DbName, Classification, Type).

-spec expires_policy_value(ne_binary(), ne_binary(), ne_binary()) -> kz_timeout().
expires_policy_value(<<"system_config">>, _, _) -> 'infinity';
expires_policy_value(<<"system_data">>, _, _) -> 'infinity';
expires_policy_value(DbName, Classification, Type) ->
    CachePolicy = kazoo_data_config:get_json(<<"cache_policy">>, ?DEFAULT_CACHING_POLICY),
    case kz_json:get_first_defined([[DbName, Type]
                                   ,[DbName, <<"any">>]
                                   ,[Classification, Type]
                                   ,[Classification, <<"any">>]
                                   ,[DbName]
                                   ,[Type]
                                   ,[Classification]
                                   ], CachePolicy, ?DEFAULT_CACHE_PERIOD) of
        <<"infinity">> -> 'infinity';
        Timeout -> kz_term:to_integer(Timeout)
    end.

-spec flush_cache_doc(ne_binary() | db(), ne_binary() | kz_json:object()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc) ->
    flush_cache_doc(#db{name=Name}, Doc, []);
flush_cache_doc(Db, Doc) when is_binary(Db) ->
    flush_cache_doc(Db, Doc, []).

-spec flush_cache_doc(ne_binary() | db(), ne_binary() | kz_json:object(), kz_proplist()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc, Options) ->
    flush_cache_doc(kz_term:to_binary(Name), Doc, Options);
flush_cache_doc(DbName, DocId, _Options) when is_binary(DocId) ->
    kz_cache:erase_local(?CACHE_NAME, {?MODULE, DbName, DocId});
flush_cache_doc(DbName, Doc, Options) ->
    flush_cache_doc(DbName, kz_doc:id(Doc), Options).

-spec flush_cache_docs() -> 'ok'.
flush_cache_docs() -> kz_cache:flush_local(?CACHE_NAME).

-spec flush_cache_docs(ne_binary() | db()) -> 'ok'.
flush_cache_docs(#db{name=Name}) ->
    flush_cache_docs(kz_term:to_binary(Name));
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
