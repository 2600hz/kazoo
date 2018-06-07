%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc data adapter behaviour
%%% @end
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
        ,load_test/0
        ,load_test/2
        ]).


-include("kz_data.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

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

-spec open_cache_doc(kz_term:text(), kz_term:ne_binary(), kz_term:proplist()) ->
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

-spec maybe_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), data_error() | {'ok', kz_json:object()}) ->
                         'ok'.
maybe_cache(DbName, DocId, Options, {'error', _}=E) ->
    maybe_cache_failure(DbName, DocId, Options, E);
maybe_cache(DbName, DocId, _, {'ok', JObj}) ->
    add_to_doc_cache(DbName, DocId, JObj).

-spec open_cache_doc(map(), kz_term:text(), kz_term:ne_binary(), kz_term:proplist()) ->
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

-spec open_cache_docs(kz_term:text(), kz_term:ne_binaries(), kz_term:proplist()) ->
                             {'ok', kz_json:objects()} |
                             data_error().
open_cache_docs(DbName, DocIds, Options) ->
    {Cached, MissedDocIds} = fetch_locals(DbName, DocIds),
    case MissedDocIds =/= []
        andalso kz_datamgr:open_docs(DbName, MissedDocIds, remove_cache_options(Options))
    of
        {error, _}=E -> E;
        Other ->
            prepare_jobjs(DbName, DocIds, Options, Cached, Other)
    end.

-spec prepare_jobjs(kz_term:text(), kz_term:ne_binaries(), kz_term:proplist(), docs_returned(), {ok, kz_json:objects()} | 'false') ->
                           {'ok', kz_json:objects()} |
                           data_error().
prepare_jobjs(DbName, DocIds, Options, Cached, false) ->
    prepare_jobjs(DbName, DocIds, Options, Cached, {ok, []});
prepare_jobjs(DbName, DocIds, Options, Cached, {ok, Opened}) ->
    FromBulk = disassemble_jobjs(DbName, Options, Opened),
    {ok, assemble_jobjs(DocIds, Cached, FromBulk)}.

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

-type doc_returned() :: {kz_term:ne_binary(), ok, kz_json:object()} |
                        {kz_term:ne_binary(), error, kz_term:ne_binary()}.
-type docs_returned() :: [doc_returned()].
-spec disassemble_jobjs(kz_term:ne_binary(), kz_term:proplist(), kz_json:objects()) -> docs_returned().
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

-spec assemble_jobjs(kz_term:ne_binaries(), docs_returned(), docs_returned()) -> kz_json:objects().
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

-spec remove_cache_options(kz_term:proplist()) -> kz_term:proplist().
remove_cache_options(Options) ->
    props:delete_keys(['cache_failures'], Options).

-spec maybe_cache_failure(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), data_error()) -> 'ok'.
maybe_cache_failure(DbName, DocId, Options, {'error', _}=Error) ->
    case props:get_value('cache_failures', Options) of
        ErrorCodes when is_list(ErrorCodes) ->
            maybe_cache_failure(DbName, DocId, Options, Error, ErrorCodes);
        'true' ->
            maybe_cache_failure(DbName, DocId, Options, Error, ['not_found']);
        _ -> 'ok'
    end.

-spec maybe_cache_failure(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), data_error(), kz_term:atoms()) -> 'ok'.
maybe_cache_failure(DbName, DocId, _Options, {'error', ErrorCode}=Error, ErrorCodes) ->
    _ = lists:member(ErrorCode, ErrorCodes)
        andalso add_to_doc_cache(DbName, DocId, Error),
    'ok'.

-spec add_to_doc_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object() | data_error()) -> 'ok'.
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

-spec cache_if_not_media(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
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

-spec flush_cache_doc(kz_term:ne_binary() | db(), kz_term:ne_binary() | kz_json:object()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc) ->
    flush_cache_doc(#db{name=Name}, Doc, []);
flush_cache_doc(Db, Doc) when is_binary(Db) ->
    flush_cache_doc(Db, Doc, []).

-spec flush_cache_doc(kz_term:ne_binary() | db(), kz_term:ne_binary() | kz_json:object(), kz_term:proplist()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc, Options) ->
    flush_cache_doc(kz_term:to_binary(Name), Doc, Options);
flush_cache_doc(DbName, DocId, _Options) when is_binary(DocId) ->
    kz_cache:erase_local(?CACHE_NAME, {?MODULE, DbName, DocId});
flush_cache_doc(DbName, Doc, Options) ->
    flush_cache_doc(DbName, kz_doc:id(Doc), Options).

-spec flush_cache_docs() -> 'ok'.
flush_cache_docs() -> kz_cache:flush_local(?CACHE_NAME).

-spec flush_cache_docs(kz_term:ne_binary() | db()) -> 'ok'.
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

-spec flush_cache_docs(kz_term:ne_binary() | db(), kz_term:ne_binaries() | kz_json:objects()) -> 'ok'.
flush_cache_docs(Db, Docs) ->
    flush_cache_docs(Db, Docs, []).

-spec flush_cache_docs(kz_term:ne_binary() | db(), kz_term:ne_binaries() | kz_json:objects(), kz_term:proplist()) -> 'ok'.
flush_cache_docs(Db, Docs, Options) ->
    _ = [flush_cache_doc(Db, Doc, Options)
         || Doc <- Docs
        ],
    'ok'.

-spec load_test() -> 'true'.
load_test() ->
    load_test(100, 50).

-spec load_test(pos_integer(), pos_integer()) -> 'ok'.
load_test(NumOfAgents, MaxMsgs) when NumOfAgents > 0
                                     andalso MaxMsgs > 0 ->
    DbName = <<"cache_load_tests">>,
    CacheProps = [{'origin_bindings', []}],
    %% Start the cache process with `origin_bindings' declared so we get a
    %% gen_listener process.
    {'ok', CachePid} = kz_cache:start_link(binary_to_atom(DbName, 'utf8'), CacheProps),
    %% Make sure the cache process has 0 queued messages.
    {'message_queue_len', 0} = process_info(CachePid, 'message_queue_len'),
    StartTime = kz_time:now_ms(), % Used to calculate the uptime.
    %% Spawn agent(s) to simulate load.
    Self = self(),
    Fun = fun() -> load_test_agent(CachePid, DbName, MaxMsgs, Self, 0) end,
    Spawns = [spawn_monitor(Fun) || _  <- lists:seq(1, NumOfAgents)],
    %% Wait for all the agents to finish, this way we know none of the agents got stuck
    %% or timed out.
    'ok' = wait_for(Spawns),
    %% Calculate msg rate per agent.
    UptimeMS = kz_time:elapsed_ms(StartTime, kz_time:now_ms()),
    Loops = collect_loops(0), % All agents' loops count
    MsgRate = (2 * Loops) / UptimeMS,
    lager:info("Uptime: ~pms, TotalLoops: ~p", [UptimeMS, Loops]),
    lager:info("MsgRate: ~.2f", [MsgRate]),
    'ok' = kz_cache:stop_local(CachePid).

-spec load_test_agent(pid(), kz_term:text(), pos_integer(), pid(), non_neg_integer()) ->
    'no_return'.
load_test_agent(CachePid, DbName, MaxMsgs, Parent, LoopCount) ->
    %% Check that there is some space in he queue before sending a new msg
    PInfoKey = 'message_queue_len',
    case process_info(CachePid, PInfoKey) of
        {PInfoKey, N} when N < MaxMsgs ->
            'ok';
        {PInfoKey, N} ->
            lager:error("Cache already has ~p queued messages, exiting (~p loops)",
                        [N, LoopCount]),
            Parent ! {'loops', self(), LoopCount},
            exit('normal')
    end,
    %% Insert a key/value pair into the cache.
    DocId = kz_binary:rand_hex(16),
    Type = 'undefined',
    Category = <<"configuration">>,
    AMQPPayload = load_test_build_amqp_payload(<<"doc_created">>, DbName, Type, DocId, Category),
    Payload = #cache_obj{'key'=DocId
                        ,'value'=AMQPPayload
                        ,'expires'=1000
                        ,'callback'='undefined'
                        ,'origin'={'db', DbName, DocId}
                        },
    CachePid ! {'$gen_call', {self(), make_ref()}, Payload},
    %% Flush the agent's mailbox (because of the '$gen_call').
    'ok' = receive after 0 -> 'ok' end,
    %% "doc edited"
    'ok' = load_test_publish(CachePid, <<"doc_edited">>, DbName, Type, DocId, Category),
    load_test_agent(CachePid, DbName, MaxMsgs, Parent, LoopCount + 1).

-spec wait_for([] | [{pid(), reference()}]) -> 'ok' | 'no_return'.
wait_for([]) ->
    'ok';
wait_for([{Pid, Ref} | Spawns]) ->
    receive
        {'DOWN', Ref, 'process', Pid, 'normal'} ->
            wait_for(Spawns)
        after 1000 ->
            lager:error("~p failed to return, ~p workers left.", [Pid, length(Spawns)]),
            exit("Failed to return")
    end.

-spec collect_loops(non_neg_integer()) -> pos_integer().
collect_loops(CurrentCount) ->
    receive
        {'loops', _Pid, LoopCount} ->
            collect_loops(CurrentCount + LoopCount)
    after 1000 ->
        CurrentCount
    end.

-spec load_test_publish(pid()
                       ,kz_term:ne_binary()
                       ,kz_term:ne_binary()
                       ,kz_term:ne_binary()
                       ,kz_term:ne_binary()
                       ,kz_term:ne_binary()) -> 'ok'.
load_test_publish(CachePid, EvName, Db, EvType, Id, Category) ->
    RKey = binary:list_to_bin(lists:join(<<".">>, [EvName, Db, kz_term:to_binary(EvType), Id])),
    BD = load_test_build_bd(Category, RKey),
    PBasic = load_test_build_pbasic(),
    Payload = load_test_build_amqp_payload(EvName, Db, EvType, Id, Category),
    CachePid ! {BD, #amqp_msg{'props'=PBasic, 'payload'=kz_json:encode(Payload)}},
    'ok'.

-spec load_test_build_bd(kz_term:ne_binary(), kz_term:ne_binary()) -> #'basic.deliver'{}.
load_test_build_bd(Category, RKey) ->
    #'basic.deliver'
    {'consumer_tag' = kz_binary:rand_hex(16)
    ,'delivery_tag' = rand:uniform(1000)
    ,'exchange' = Category
    ,'routing_key' = RKey
    }.

-spec load_test_build_pbasic() -> #'P_basic'{}.
load_test_build_pbasic() ->
    #'P_basic'
    {'content_type' = <<"application/json">>
    ,'content_encoding' = 'undefined'
    ,'headers' = 'undefined'
    ,'delivery_mode' = 'undefined'
    ,'priority' = 'undefined'
    ,'correlation_id' = 'undefined'
    ,'reply_to' = 'undefined'
    ,'expiration' = 'undefined'
    ,'message_id' = 'undefined'
    ,'timestamp' = kz_time:now_ms()
    ,'type' = 'undefined'
    ,'user_id' = 'undefined'
    ,'app_id' = 'undefined'
    ,'cluster_id' = 'undefined'
    }.

-spec load_test_build_amqp_payload(kz_term:ne_binary()
                             ,kz_term:ne_binary()
                             ,kz_term:ne_binary()
                             ,kz_term:ne_binary()
                             ,kz_term:ne_binary()) -> kz_json:object().
load_test_build_amqp_payload(EvName, Db, Type, Id, Category) ->
    Payload = [{<<"Database">>, Db}
              ,{<<"ID">>, Id}
              ,{<<"Server-ID">>, <<>>}
              ,{<<"Node">>, kz_term:to_binary(node())}
              ,{<<"Msg-ID">>, kz_binary:rand_hex(8)}
              ,{<<"Event-Name">>, EvName}
              ,{<<"Event-Category">>, Category}
              ,{<<"App-Version">>, <<"1.0.0">>}
              ,{<<"App-Name">>, <<"datamgr">>}
              ],
    %% Database events have less/different {key, value} pairs compared to document events.
    FinalPayload =
        case Type of
            <<"database">> ->
                [{<<"Type">>, Type} | Payload];
            'undefined' ->
                [{<<"Origin-Cache">>, <<"kazoo_data_cache">>}
                ,{<<"Revision">>, kz_binary:rand_hex(16)}
                ,{<<"Is-Soft-Deleted">>, 'false'}
                 | Payload]
        end,
    kz_json:from_list(FinalPayload).
