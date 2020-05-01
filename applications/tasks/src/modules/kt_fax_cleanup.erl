%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_fax_cleanup).

-export([init/0]).

-export([cleanup/0
        ,transform_doc/1
        ]).

-include("kt_fax_cleanup.hrl").

%%------------------------------------------------------------------------------
%% @doc cleanup the faxes db by purging invalid docs and migrating
%% any uncaught docs to the modb.
%% @end
%%------------------------------------------------------------------------------

-spec init() -> 'ok'.
init() ->
    case ?FAX_CLEANUP_ENABLED of
        'true' ->
            _ = tasks_bindings:bind(?TRIGGER_DAILY, ?MODULE, 'cleanup');
        'false' ->
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc starts a cleanup job
%% @end
%%------------------------------------------------------------------------------
-spec cleanup() -> 'ok'.
cleanup() ->
    lager:info("staring fax db cleanup"),
    State = #{<<"start_key">> => 'undefined'
             ,<<"counter">> => #{}
             ,<<"accounts">> => #{}
             },
    Report = process_chunks(State),
    log_report(Report).

%%------------------------------------------------------------------------------
%% @doc chunk the fetches so we do not use lots of resources for this
%% @end
%%------------------------------------------------------------------------------

-spec process_chunks(map()) -> map().
process_chunks(#{<<"start_key">> := 'done'}=State) -> State;
process_chunks(#{<<"start_key">> := 'undefined'}=State) ->
    process_chunks(process_chunk(State));
process_chunks(#{<<"start_key">> := _StartKey}=State) ->
    timer:sleep(?PER_PAGE_PAUSE),
    process_chunks(process_chunk(State)).

-spec process_chunk(map()) -> map().
process_chunk(State) ->
    {Docs, NewState} = fetch_fax_docs(State),
    move_docs(Docs, bump(NewState, <<"stale_documents">>, length(Docs))).

%%------------------------------------------------------------------------------
%% @doc move stale doc to modb or delete it if no account or modb found
%% @end
%%------------------------------------------------------------------------------
-spec move_docs(kz_json:objects(), map()) -> map().
move_docs([], State) -> State;
move_docs([Doc|Docs], State) ->
    move_docs(Docs, move_doc(Doc, State)).

-spec move_doc(kzd_fax:doc(), map()) -> map().
move_doc(Doc, State) ->
    AccountId = kz_doc:account_id(Doc),
    NewState = maybe_fetch_account(State, kz_doc:account_id(Doc)),
    handle_doc(Doc, account_exists(NewState, AccountId), NewState).

-spec handle_doc(kzd_fax:doc(), boolean(), map()) -> map().
handle_doc(Doc, 'true', State) ->
    {FromDB, FromId, ToDB, ToId} = format_modb(Doc),
    log_doc_moved(Doc, FromId, ToDB, ToId),
    case kazoo_modb:move_doc(FromDB, {<<"fax">>, FromId}, ToDB, ToId, ?MOVE_OPTIONS) of
        {'ok', _} ->
            bump(State, <<"move_success">>);
        {'error', 'not_found'} ->
            lager:debug("doc move failed for ~s because modb ~s not found, deleting doc", [FromId, ToDB]),
            handle_doc(Doc, 'false', bump(State, <<"modb_not_found">>));
        _Error ->
            bump(State, <<"move_failed">>)
    end;
handle_doc(Doc, 'false', State) ->
    log_doc_deleted(Doc),
    case kz_datamgr:del_doc(?KZ_FAXES_DB, Doc) of
        {'ok', _} ->
            bump(State, <<"delete_success">>);
        _Error ->
            bump(State, <<"delete_failed">>)
    end.

%%------------------------------------------------------------------------------
%% @doc view query and pagination over the doc chunks
%% @end
%%------------------------------------------------------------------------------
-spec fetch_fax_docs(map()) -> {kz_json:objects(), map()}.
fetch_fax_docs(#{<<"start_key">> := 'undefined'}=State) ->
    Options = [{'page_size', ?PAGE_SIZE}, include_docs],
    fetch_fax_docs(State, Options);
fetch_fax_docs(#{<<"start_key">> := StartKey}=State) ->
    Options = [{'page_size', ?PAGE_SIZE}
              ,{'startkey', StartKey}
              ,include_docs
              ],
    fetch_fax_docs(State, Options).

-spec fetch_fax_docs(map(), kz_term:proplist()) -> {kz_json:objects(), map()}.

fetch_fax_docs(State, Options) ->
    case kz_datamgr:paginate_results(?KZ_FAXES_DB, ?CROSSBAR_LISTING, Options) of
        {'ok', Page, 'undefined'} ->
            {filter_datetime(Page), bump(State#{<<"start_key">> => 'done'}, <<"total_docs">>, length(Page))};
        {'ok', Page, NextKey} ->
            {filter_datetime(Page), bump(State#{<<"start_key">> => NextKey}, <<"total_docs">>, length(Page))};
        {'error', Message} ->
            lager:info("failed to query faxes db with error ~p", [Message]),
            {[], State#{<<"start_key">> => 'done'}}
    end.

-spec filter_datetime(kz_json:objects()) -> kz_json:objects().
filter_datetime(Docs) ->
    filter_datetime(Docs, []).

-spec filter_datetime(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
filter_datetime([], Acc) -> Acc;
filter_datetime([Result|Results], Acc) ->
    Doc = kz_json:get_value(<<"doc">>, Result),
    case is_stale_doc(Doc) of
        'true' ->
            filter_datetime(Results, [Doc|Acc]);
        'false' ->
            filter_datetime(Results, Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc when moving, set the doc to an appropriate final state
%% @end
%%------------------------------------------------------------------------------
-spec transform_doc(kzd_fax:doc()) -> kzd_fax:doc().
transform_doc(Doc) ->
    maybe_update_status(Doc
                       ,kzd_fax:job_status(Doc)
                       ,kzd_fax:folder(Doc)
                       ,retries_exhausted(Doc)
                       ).

-spec maybe_update_status(kzd_fax:doc(), kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> kzd_fax:doc().
maybe_update_status(Doc, <<"complete">>, _Folder, _Retry) ->
    Doc;
maybe_update_status(Doc, <<"failed">>, _Folder, _Retry) ->
    Doc;
maybe_update_status(Doc, Status, <<"inbox">>, 'true') ->
    update_status(Doc, Status, <<"failed">>);
maybe_update_status(Doc, Status, <<"inbox">>, _Retry) ->
    case has_attachments(Doc) of
        'true' ->
            update_status(Doc, Status, <<"complete">>);
        'false' ->
            update_status(Doc, Status, <<"failed">>)
    end;
maybe_update_status(Doc, Status, <<"outbox">>, _Retry) ->
    update_status(Doc, Status, <<"failed">>);
maybe_update_status(Doc, Status, _Unknown, _Retry) ->
    NewDoc = kz_json:set_value(<<"folder">>, <<"outbox">>, Doc),
    update_status(NewDoc, Status, <<"failed">>).

-spec update_status(kzd_fax:doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> kzd_fax:doc().
update_status(Doc, OldStatus, NewStatus) ->
    kz_json:set_values([{<<"previous_status">>, OldStatus}
                       ,{<<"pvt_job_status">>, NewStatus}
                       ]
                      ,Doc
                      ).

%%------------------------------------------------------------------------------
%% @doc util functions
%% @end
%%------------------------------------------------------------------------------
-spec format_modb(kzd_fax:doc()) ->
          {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}.
format_modb(Doc) ->
    FromId = kz_doc:id(Doc),
    FromDB = kz_doc:account_db(Doc),
    AccountId = kz_doc:account_id(Doc),
    {Year, Month, _D} = kz_term:to_date(kz_doc:created(Doc)),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),
    ToDB = kzs_util:format_account_modb(AccountMODb, 'encoded'),
    ToId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), FromId),
    {FromDB, FromId, ToDB, ToId}.

-spec is_stale_doc(kzd_fax:doc()) -> boolean().
is_stale_doc(Doc) ->
    (kz_time:now_s() - ?STALE_AFTER) >= kz_doc:created(Doc).

-spec bump(map(), kz_term:ne_binary()) -> map().
bump(State, Key) ->
    bump(State, Key, 1).

-spec bump(map(), kz_term:ne_binary(), integer()) -> map().
bump(#{<<"counter">> := Counter}=State, Key, Value) ->
    State#{<<"counter">> => Counter#{Key => (maps:get(Key, Counter, 0) + Value)}}.

-spec has_attachments(kzd_fax:doc()) -> boolean().
has_attachments(Doc) ->
    case kz_doc:attachments(Doc) of
        'undefined' -> 'false';
        _Att -> 'true'
    end.

-spec list_attachments(kzd_fax:doc()) -> kz_term:ne_binaries().
list_attachments(Doc) ->
    case kz_doc:attachments(Doc) of
        'undefined' -> [];
        Att -> kz_json:get_keys(Att)
    end.

-spec maybe_fetch_account(map(), kz_term:ne_binary()) -> map().
maybe_fetch_account(State, 'undefined') -> State;
maybe_fetch_account(#{<<"accounts">> := Accounts}=State, AccountId) ->
    case maps:get(AccountId, Accounts, 'undefined') of
        'undefined' ->
            fetch_account(State, AccountId);
        _Found ->
            State
    end.

-spec fetch_account(map(), kz_term:ne_binary()) -> map().
fetch_account(#{<<"accounts">> := Accounts}=State, AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', _} ->
            State#{<<"accounts">> => Accounts#{AccountId => 'true'}};
        _Else ->
            State#{<<"accounts">> => Accounts#{AccountId => 'false'}}
    end.

-spec account_exists(map(), kz_term:ne_binary()) -> boolean().
account_exists(_State, 'undefined') -> 'false';
account_exists(#{<<"accounts">> := Accounts}, AccountId) ->
    maps:get(AccountId, Accounts, 'false').

-spec retries_exhausted(kzd_fax:doc()) -> boolean().
retries_exhausted(Doc) ->
    kzd_fax:retries(Doc, 0) =:= kzd_fax:attempts(Doc, 0).

%%------------------------------------------------------------------------------
%% @doc log stats so we can figure out what the task did and why
%% @end
%%------------------------------------------------------------------------------
-spec log_doc_moved(kzd_fax:doc(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
log_doc_moved(Doc, FromId, ToDB, ToId) ->
    Status = kzd_fax:job_status(Doc, <<"unknown">>),
    lager:debug("moving: ~s modified: ~s to_modb: ~s new_id ~s, job_status: ~s,  tx_result ~s folder: ~s attachments ~p"
               ,[FromId
                ,kz_date:to_iso8601_extended(kz_doc:created(Doc))
                ,ToDB
                ,ToId
                ,Status
                ,kz_json:get_ne_binary_value([<<"tx_result">>, <<"result_text">>], Doc)
                ,kzd_fax:folder(Doc, <<"unknown">>)
                ,list_attachments(Doc)
                ]
               ).

-spec log_doc_deleted(kzd_fax:doc()) -> 'ok'.
log_doc_deleted(Doc) ->
    Status = kzd_fax:job_status(Doc, <<"unknown">>),
    lager:debug("removing: ~s modified: ~s account: ~s job_status: ~s tx_result: ~s folder: ~s attachments: ~p"
               ,[kz_doc:id(Doc)
                ,kz_date:to_iso8601_extended(kz_doc:created(Doc))
                ,kz_doc:account_id(Doc)
                ,Status
                ,kz_json:get_ne_binary_value([<<"tx_result">>, <<"result_text">>], Doc)
                ,kzd_fax:folder(Doc, <<"unknown">>)
                ,list_attachments(Doc)
                ]
               ).

-spec log_report(map()) -> 'ok'.
log_report(#{<<"counter">> := Report}) ->
    NewMap = maps:remove(<<"accounts">>, Report),
    Message = maps:fold(fun (K, V, Acc) -> [<<K/binary, ": ", (kz_term:to_binary(V))/binary>>|Acc] end
                       ,[]
                       ,NewMap
                       ),
    lager:info("results: ~s", [kz_binary:join(lists:reverse(Message), <<", ">>)]).
