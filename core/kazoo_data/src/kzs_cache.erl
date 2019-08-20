%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc data adapter behaviour
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_cache).

%% Doc related
-export([open_cache_doc/3, open_cache_doc/4
        ,open_cache_docs/3
        ,add_to_doc_cache/3, add_to_doc_cache/4
        ,flush_cache_doc/2
        ,flush_cache_doc/3
        ,flush_cache_docs/0
        ,flush_cache_docs/1
        ,flush_cache_docs/2
        ,flush_cache_docs/3
        ]).

-export([cache_strategy/0
        ,set_cache_strategy/1
        ]).

-export_type([cache_strategy/0]).

-include("kz_data.hrl").

-define(DEFAULT_NO_CACHING_TYPES, [<<"media">>, <<"private_media">>, <<"call_recording">>
                                  ,<<"fax">>, <<"mailbox_message">>
                                  ]).
-define(NO_CACHING_TYPES
       ,kazoo_data_config:get_ne_binaries(<<"no_caching_doc_types">>, ?DEFAULT_NO_CACHING_TYPES)
       ).

-define(DEFAULT_CACHE_PERIOD, 15 * ?SECONDS_IN_MINUTE).
-define(ANY_TYPE_CACHING_POLICY, kz_json:from_list([{<<"any">>, ?DEFAULT_CACHE_PERIOD}])).
-define(ANY_TYPE_CACHING_POLICY(T), kz_json:from_list([{<<"any">>, T}])).
-define(DEFAULT_CACHING_POLICY, kz_json:from_list(
                                  [{<<"deprecated">>, ?ANY_TYPE_CACHING_POLICY}
                                  ,{<<"aggregate">>, ?ANY_TYPE_CACHING_POLICY}
                                  ,{<<"numbers">>, ?ANY_TYPE_CACHING_POLICY}
                                  ,{<<"modb">>, ?ANY_TYPE_CACHING_POLICY}
                                  ,{<<"account">>, ?ANY_TYPE_CACHING_POLICY}
                                  ,{<<"system">>, ?ANY_TYPE_CACHING_POLICY}
                                  ,{<<"system_config">>, ?ANY_TYPE_CACHING_POLICY(<<"infinity">>)}
                                  ,{<<"system_data">>, ?ANY_TYPE_CACHING_POLICY(<<"infinity">>)}
                                  ])).

-define(STAMPEDE_WAIT_MS, 1500). % see kz_cache_stampede_tests
-define(CACHE_KEY(DbName, DocId), {?MODULE, DbName, DocId}).

-spec open_cache_doc(kz_term:text(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.
open_cache_doc(DbName, DocId, Options) ->
    MitigationKey = kz_cache:mitigation_key(),
    CacheStrategy = cache_strategy(),

    case kz_cache:fetch_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId)) of
        {MitigationKey, _Pid} when CacheStrategy =:= 'stampede' ->
            kz_cache:wait_for_stampede_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId), ?STAMPEDE_WAIT_MS);
        {MitigationKey, _Pid} ->
            fetch_doc(DbName, DocId, Options, CacheStrategy);
        {'ok', {'error', _}=E} -> E;
        {'ok', _Doc}=Ok -> Ok;
        {'error', 'not_found'} when CacheStrategy =:= 'stampede' ->
            mitigate_stampede(DbName, DocId, Options, CacheStrategy);
        {'error', 'not_found'} ->
            fetch_doc(DbName, DocId, Options, CacheStrategy)
    end.

-spec mitigate_stampede(kz_term:text(), kz_term:ne_binary(), kz_term:proplist(), cache_strategy()) ->
                               {'ok', kz_json:object()} |
                               data_error() |
                               {'error', 'not_found'}.
mitigate_stampede(DbName, DocId, Options, CacheStrategy) ->
    CacheKey = ?CACHE_KEY(DbName, DocId),
    case kz_cache:mitigate_stampede_local(?CACHE_NAME, CacheKey) of
        'ok' ->
            fetch_doc(DbName, DocId, Options, CacheStrategy);
        'error' ->
            kz_cache:wait_for_stampede_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId), ?STAMPEDE_WAIT_MS)
    end.

-spec fetch_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), cache_strategy()) ->
                       {'ok', kz_json:object()} |
                       data_error() |
                       {'error', 'not_found'}.
fetch_doc(DbName, DocId, Options, CacheStrategy) ->
    R = kz_datamgr:open_doc(DbName, DocId, remove_cache_options(Options)),
    _ = maybe_cache(DbName, DocId, Options, R, CacheStrategy),
    R.

-spec maybe_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), data_error() | {'ok', kz_json:object()}, cache_strategy()) ->
                         'ok'.
maybe_cache(DbName, DocId, _Options, {'error', 'conflict'}, _CacheStrategy) ->
    flush_cache_doc(DbName, DocId);
maybe_cache(DbName, DocId, Options, {'error', _}=E, CacheStrategy) ->
    maybe_cache_failure(DbName, DocId, Options, E, CacheStrategy);
maybe_cache(DbName, DocId, _, {'ok', JObj}, CacheStrategy) ->
    add_to_doc_cache(DbName, DocId, JObj, CacheStrategy).

-spec open_cache_doc(map(), kz_term:text(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.
open_cache_doc(Server, DbName, DocId, Options) ->
    MitigationKey = kz_cache:mitigation_key(),
    CacheStrategy = cache_strategy(),

    case kz_cache:fetch_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId)) of
        {MitigationKey, _Pid} when CacheStrategy =:= 'stampede' ->
            kz_cache:wait_for_stampede_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId));
        {MitigationKey, _Pid} ->
            fetch_doc(Server, DbName, DocId, remove_cache_options(Options), CacheStrategy);
        {'ok', {'error', _}=E} -> E;
        {'ok', _Doc}=Ok -> Ok;
        {'error', 'not_found'} ->
            fetch_doc(Server, DbName, DocId, remove_cache_options(Options), CacheStrategy)
    end.

fetch_doc(Server, DbName, DocId, Options, CacheStrategy) ->
    R = kzs_doc:open_doc(Server, DbName, DocId, Options),
    _ = maybe_cache(DbName, DocId, Options, R, CacheStrategy),
    R.

-spec open_cache_docs(kz_term:text(), kz_term:ne_binaries(), kz_term:proplist()) ->
                             {'ok', kz_json:objects()} |
                             data_error().
open_cache_docs(DbName, DocIds, Options) ->
    {Cached, MissedDocIds} = fetch_locals(DbName, DocIds),
    ?LOG_INFO("fetched cached ~p and missing ~p", [Cached, MissedDocIds]),
    open_non_cached_docs(DbName, DocIds, Options, Cached, MissedDocIds).

open_non_cached_docs(DbName, DocIds, Options, Cached, []) ->
    prepare_jobjs(DbName, DocIds, Options, Cached, []);
open_non_cached_docs(DbName, DocIds, Options, Cached, MissedDocIds) ->
    case kz_datamgr:open_docs(DbName, MissedDocIds, remove_cache_options(Options)) of
        {'error', _}=E -> E;
        {'ok', Found} ->
            prepare_jobjs(DbName, DocIds, Options, Cached, Found)
    end.

-spec prepare_jobjs(kz_term:text(), kz_term:ne_binaries(), kz_term:proplist(), docs_returned(), kz_json:objects()) ->
                           {'ok', kz_json:objects()} |
                           data_error().
prepare_jobjs(DbName, DocIds, Options, Cached, Opened) ->
    FromBulk = disassemble_jobjs(DbName, Options, Opened),
    {'ok', assemble_jobjs(DocIds, Cached, FromBulk)}.

-spec fetch_locals(kz_term:ne_binary(), kz_term:ne_binaries()) -> {docs_returned(), kz_term:ne_binaries()}.
fetch_locals(DbName, DocIds) ->
    F = fun (DocId, {Cached, Missed}) ->
                case kz_cache:fetch_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId)) of
                    {'error', 'not_found'} -> {Cached, [DocId|Missed]};
                    {'ok', {'error', Reason}} ->
                        {[{DocId, 'error', Reason}|Cached], Missed};
                    {'ok', Doc} ->
                        {[{DocId, 'ok', Doc}|Cached], Missed}
                end
        end,
    lists:foldl(F, {[], []}, DocIds).

-type doc_returned() :: {kz_term:ne_binary(), 'ok', kz_json:object()} |
                        {kz_term:ne_binary(), 'error', kz_term:ne_binary()}.
-type docs_returned() :: [doc_returned()].
-spec disassemble_jobjs(kz_term:ne_binary(), kz_term:proplist(), kz_json:objects()) -> docs_returned().
disassemble_jobjs(DbName, Options, JObjs) ->
    [case kz_json:get_json_value(<<"doc">>, JObj) of
         'undefined' ->
             %% Reason is not_found for when documents were deleted.
             Reason = kz_json:get_ne_binary_value(<<"error">>, JObj, <<"not_found">>),
             _ = maybe_cache_failure(DbName, DocId, Options, {'error', kz_term:to_atom(Reason)}, cache_strategy()),
             {DocId, 'error', Reason};
         Doc ->
             _ = add_to_doc_cache(DbName, DocId, Doc),
             {DocId, 'ok', Doc}
     end
     || JObj <- JObjs,
        DocId <- [kz_json:get_ne_value(<<"key">>, JObj)]
    ].

-spec assemble_jobjs(kz_term:ne_binaries(), docs_returned(), docs_returned()) -> kz_json:objects().
assemble_jobjs(DocIds, Cached, DocsReturned) ->
    JObjs1 = [to_nonbulk_format(Returned) || Returned <- Cached],
    JObjs2 = [to_nonbulk_format(Returned) || Returned <- DocsReturned],
    kz_json:order_by([<<"key">>], DocIds, [JObjs1,JObjs2]).

-spec to_nonbulk_format(doc_returned()) -> kz_json:object().
to_nonbulk_format({DocId, 'ok', Doc}) ->
    kz_json:from_list(
      [{<<"key">>, DocId}
      ,{<<"doc">>, Doc}
      ]);
to_nonbulk_format({DocId, 'error', Reason}) ->
    kz_json:from_list(
      [{<<"key">>, DocId}
      ,{<<"error">>, kz_term:to_atom(Reason, 'true')}
      ]).

-spec remove_cache_options(kz_term:proplist()) -> kz_term:proplist().
remove_cache_options(Options) ->
    props:delete_keys(['cache_failures'], Options).

-spec maybe_cache_failure(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), data_error(), cache_strategy()) -> 'ok'.
maybe_cache_failure(DbName, DocId, Options, {'error', _}=Error, CacheStrategy) ->
    case props:get_value('cache_failures', Options) of
        ErrorCodes when is_list(ErrorCodes) ->
            maybe_cache_failure(DbName, DocId, Options, Error, ErrorCodes, CacheStrategy);
        'true' ->
            maybe_cache_failure(DbName, DocId, Options, Error, ['not_found'], CacheStrategy);
        _ ->
            kz_cache:erase_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId))
    end.

-spec maybe_cache_failure(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), data_error(), kz_term:atoms(), cache_strategy()) -> 'ok'.
maybe_cache_failure(DbName, DocId, _Options, {'error', ErrorCode}=Error, ErrorCodes, CacheStrategy) ->
    case lists:member(ErrorCode, ErrorCodes) of
        'true' -> add_to_doc_cache(DbName, DocId, Error, CacheStrategy);
        'false' ->
            kz_cache:erase_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId))
    end.

-spec add_to_doc_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object() | data_error()) -> 'ok'.
add_to_doc_cache(DbName, DocId, CacheValue) ->
    add_to_doc_cache(DbName, DocId, CacheValue, 'none').

-spec add_to_doc_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object() | data_error(), cache_strategy()) -> 'ok'.
add_to_doc_cache(DbName, DocId, CacheValue, CacheStrategy) ->
    CacheProps = [{'origin', {'db', DbName, DocId}}
                 ,{'expires', expires_policy_value(DbName, CacheValue)}
                 ],
    case kz_json:is_json_object(CacheValue) of
        'true' ->
            cache_if_not_media(CacheProps, DbName, DocId, CacheValue, CacheStrategy);
        'false' when CacheStrategy =:= 'none';
                     CacheStrategy =:= 'stampede' ->
            kz_cache:store_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId), CacheValue, CacheProps);
        'false' when CacheStrategy =:= 'async' ->
            kz_cache:store_local_async(?CACHE_NAME, ?CACHE_KEY(DbName, DocId), CacheValue, CacheProps);
        'false' when CacheStrategy =:= 'stampede_async' ->
            maybe_cache(DbName, DocId, CacheValue, CacheProps)
    end.

-spec maybe_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object() | data_error(), kz_cache:store_options()) -> 'ok'.
maybe_cache(DbName, DocId, CacheValue, CacheProps) ->
    case kz_cache:mitigate_stampede_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId)) of
        'ok' ->
            kz_cache:store_local_async(?CACHE_NAME, ?CACHE_KEY(DbName, DocId), CacheValue, CacheProps);
        'error' -> 'ok'
    end.

-spec cache_if_not_media(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), cache_strategy()) -> 'ok'.
cache_if_not_media(CacheProps, DbName, DocId, CacheValue, CacheStrategy) ->
    %% NOTE: this is currently necessary because when a http_put is issued to
    %%   freeswitch and the media is uploaded it goes directly to bigcouch
    %%   and therefore no doc change notice is pushed.  This results in the
    %%   doc cache containing a document that has no attachments (or the wrong
    %%   attachments). What needs to happen is a change notice get sent on the
    %%   message bus anytime a http_put is issued (or maybe if the store
    %%   url is built in media IF everything uses that helper function,
    %%   which is not currently the case...)
    case kzs_util:db_classification(DbName) =/= 'system'
        andalso lists:member(kz_doc:type(CacheValue), ?NO_CACHING_TYPES)
    of
        'true' ->
            kz_cache:erase_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId));
        'false' when CacheStrategy =:= 'none';
                     CacheStrategy =:= 'stampede' ->
            check_if_newer_revision(CacheProps, DbName, DocId, CacheValue);
        'false' when CacheStrategy =:= 'stampede_async' ->
            maybe_cache(DbName, DocId, CacheValue, CacheProps);
        'false' ->
            kz_cache:store_local_async(?CACHE_NAME, ?CACHE_KEY(DbName, DocId), CacheValue, CacheProps)
    end.

-spec check_if_newer_revision(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
check_if_newer_revision(CacheProps, DbName, DocId, CacheValue) ->
    case kz_cache:peek_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId)) of
        {'ok', Current} ->
            case kz_doc:revision_id(Current) > kz_doc:revision_id(CacheValue) of
                'true' -> 'ok';
                'false' -> cache_it(CacheProps, DbName, DocId, CacheValue)
            end;
        {_, _} -> cache_it(CacheProps, DbName, DocId, CacheValue)
    end.

-spec cache_it(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
cache_it(CacheProps, DbName, DocId, CacheValue) ->
    kz_cache:store_local(?CACHE_NAME
                        ,?CACHE_KEY(DbName, DocId)
                        ,CacheValue
                        ,CacheProps
                        ).

-spec expires_policy_value(kz_term:ne_binary(), kz_json:object() | data_error()) -> timeout().
expires_policy_value(_DbName, {'error', _}) ->
    ?DEFAULT_CACHE_PERIOD;
expires_policy_value(DbName, CacheValue) ->
    Classification = kz_term:to_binary(kzs_util:db_classification(DbName)),
    Type = kz_doc:type(CacheValue, <<"no_type">>),
    expires_policy_value(DbName, Classification, Type).

-spec expires_policy_value(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> timeout().
expires_policy_value(<<"system_config">>, _, _) -> 'infinity';
expires_policy_value(<<"system_data">>, _, _) -> 'infinity';
expires_policy_value(DbName, Classification, Type) ->
    CachePolicy = kazoo_data_config:get_json(<<"cache_policy">>, ?DEFAULT_CACHING_POLICY),
    case kz_json:get_first_defined([[DbName, Type]
                                   ,[DbName, <<"any">>]
                                   ,[Classification, Type]
                                   ,[Classification, <<"any">>]
                                   ,[<<"type">>, Type]
                                   ], CachePolicy, ?DEFAULT_CACHE_PERIOD) of
        <<"infinity">> -> 'infinity';
        Timeout -> kz_term:to_integer(Timeout)
    end.

-spec flush_cache_doc(kz_term:ne_binary() | db(), kz_term:api_ne_binary() | kz_json:object()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc) ->
    flush_cache_doc(#db{name=Name}, Doc, []);
flush_cache_doc(Db, Doc) when is_binary(Db) ->
    flush_cache_doc(Db, Doc, []).

-spec flush_cache_doc(kz_term:ne_binary() | db(), kz_term:api_ne_binary() | kz_json:object(), kz_term:proplist()) -> 'ok'.
flush_cache_doc(_Db, 'undefined', _Options) ->
    lager:debug("failed to supply a doc id for flushing, ignoring");
flush_cache_doc(#db{name=Name}, Doc, Options) ->
    flush_cache_doc(kz_term:to_binary(Name), Doc, Options);
flush_cache_doc(DbName, <<DocId/binary>>, _Options) ->
    kz_cache:erase_local(?CACHE_NAME, ?CACHE_KEY(DbName, DocId));
flush_cache_doc(DbName, Doc, Options) ->
    flush_cache_doc(DbName, kz_doc:id(Doc), Options).

-spec flush_cache_docs() -> 'ok'.
flush_cache_docs() -> kz_cache:flush_local(?CACHE_NAME).

-spec flush_cache_docs(kz_term:ne_binary() | db()) -> 'ok'.
flush_cache_docs(#db{name=Name}) ->
    flush_cache_docs(kz_term:to_binary(Name));
flush_cache_docs(DbName) ->
    Filter = fun(?CACHE_KEY(DbName1, _DocId)=Key, _) when DbName1 =:= DbName ->
                     kz_cache:erase_local(?CACHE_NAME, Key),
                     'true';
                (_, _) -> 'false'
             end,
    _ = kz_cache:filter_local(?CACHE_NAME, Filter),
    'ok'.

-spec flush_cache_docs(kz_term:ne_binary() | db(), kz_term:ne_binaries() | kz_json:objects()) -> 'ok'.
flush_cache_docs(Db, Docs) ->
    flush_cache_docs(Db, Docs, []).

-spec flush_cache_docs(kz_term:ne_binary() | db(), kz_term:ne_binaries() | kz_json:objects(), kz_term:proplist()) -> 'ok'.
flush_cache_docs(Db, Docs, Options) ->
    _ = [flush_cache_doc(Db, Doc, Options)
         || Doc <- Docs
        ],
    'ok'.

-type cache_strategy() :: 'none' |
                          'stampede' |
                          'async' |
                          'stampede_async'.
-spec cache_strategy() -> cache_strategy().
cache_strategy() ->
    application:get_env('kazoo_data', 'cache_strategy', 'none').

-spec set_cache_strategy(cache_strategy()) -> 'ok'.
set_cache_strategy(Strategy)
  when Strategy =:= 'none';
       Strategy =:= 'stampede';
       Strategy =:= 'async';
       Strategy =:= 'stampede_async' ->
    application:set_env('kazoo_data', 'cache_strategy', Strategy).
