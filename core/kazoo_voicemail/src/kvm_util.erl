%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Voice mailbox utility functions
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_util).

-export([get_db/1, get_db/2, get_range_db/1, get_range_db/2
        ,open_modb_doc/3, open_accountdb_doc/3
        ,update_result/2, bulk_update_result/2, bulk_update_result/3
        ,retry_conflict/1, check_doc_type/3

        ,check_msg_belonging/2
        ,find_differences/3
        ,cleanup_moved_msgs/3

        ,retention_days/0

        ,publish_saved_notify/5, publish_voicemail_saved/5
        ,get_notify_completed_message/1
        ,get_caller_id_name/1, get_caller_id_number/1
        ]).

-include("kz_voicemail.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc Generate database name based on DocId
%% @end
%%--------------------------------------------------------------------
-spec get_db(ne_binary()) -> ne_binary().
-spec get_db(ne_binary(), kazoo_data:docid() | kz_json:object()) -> ne_binary().
-spec get_db(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
get_db(AccountId) ->
    kz_util:format_account_db(AccountId).

get_db(AccountId, {_, ?MATCH_MODB_PREFIX(Year, Month, _)}) ->
    get_db(AccountId, Year, Month);
get_db(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)) ->
    get_db(AccountId, Year, Month);
get_db(AccountId, ?NE_BINARY = _DocId) ->
    get_db(AccountId);
get_db(AccountId, Doc) ->
    get_db(AccountId, kz_doc:id(Doc)).

get_db(AccountId, Year, Month) ->
    kazoo_modb:get_modb(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)).

%%--------------------------------------------------------------------
%% @public
%% @doc Generate a range of database names
%% Options:
%%   'modb': only return modb range
%% @end
%%--------------------------------------------------------------------
-spec get_range_db(ne_binary()) -> ne_binaries().
-spec get_range_db(ne_binary(), atom()) -> ne_binaries().
get_range_db(AccountId) ->
    get_range_db(AccountId, 'modb') ++ [get_db(AccountId)].

get_range_db(AccountId, 'modb') ->
    To = kz_util:current_tstamp(),
    From = To - ?RETENTION_DAYS(?RETENTION_DURATION),
    lists:reverse([Db || Db <- kazoo_modb:get_range(AccountId, From, To)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec open_modb_doc(ne_binary(), kazoo_data:docid(), ne_binary()) -> db_ret().
open_modb_doc(AccountId, DocId, Type) ->
    case kazoo_modb:open_doc(AccountId, DocId) of
        {'ok', JObj}-> check_doc_type(JObj, Type, kz_doc:type(JObj));
        {'error', _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec open_accountdb_doc(ne_binary(), kazoo_data:docid(), ne_binary()) -> db_ret().
open_accountdb_doc(AccountId, DocId, Type) ->
    case kz_datamgr:open_doc(get_db(AccountId), DocId) of
        {'ok', JObj} -> check_doc_type(JObj, Type, kz_doc:type(JObj));
        {'error', _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Protect against returning wrong doc when expected type is not matched
%% (especially for requests from crossbar)
%% @end
%%--------------------------------------------------------------------
-spec check_doc_type(kz_json:object(), ne_binary(), ne_binary()) -> db_ret().
check_doc_type(Doc, Type, Type) ->
    {'ok', Doc};
check_doc_type(_Doc, _ExpectedType, _DocType) ->
    lager:debug("not expected type : ~s , ~s", [_ExpectedType, _DocType]),
    {'error', 'not_found'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_result(ne_binary(), db_ret()) -> db_ret().
update_result(?MATCH_MODB_PREFIX(_, _, _)=_Id, {'error', _R}=Error) ->
    lager:warning("failed to update voicemail message ~s: ~p", [_Id, _R]),
    Error;
update_result(?MATCH_MODB_PREFIX(_, _, _), {'ok', _}=Res) -> Res;
update_result(FromId, {'error', _R}=Error) ->
    lager:warning("failed move voicemail message ~s to modb: ~p", [FromId, _R]),
    Error;
update_result(_, {'ok', _}=Res) -> Res.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bulk_update_result(ne_binary(), db_ret(), kvm_messags:bulk_results()) ->
                                kvm_messags:bulk_results().
bulk_update_result(Id, {'error', R} = Error, #bulk_res{failed = Failed} = Blk) ->
    _ = update_result(Id, Error),
    Blk#bulk_res{failed = [kz_json:from_list([{Id, kz_util:to_binary(R)}]) | Failed]};
bulk_update_result(Id, {'ok', JObj} = OK, #bulk_res{succeeded = Succeeded
                                                   ,moved = Moved} = Blk) ->
    _ = update_result(Id, OK),
    NewId = kz_doc:id(JObj),
    case Id of
        ?MATCH_MODB_PREFIX(_, _, _) ->
            Blk#bulk_res{succeeded = [NewId | Succeeded]};
        _ ->
            Blk#bulk_res{succeeded = [NewId | Succeeded]
                        ,moved = [Id | Moved]
                        }
    end.

-spec bulk_update_result(kz_json:object(), kz_json:object()) -> kz_json:object().
bulk_update_result(Result, CurrentResults) ->
    Failed = lists:append([kz_json:get_value(<<"failed">>, CurrentResults, [])
                          ,kz_json:get_value(<<"failed">>, Result, [])
                          ]),
    Succeeded = lists:append([kz_json:get_value(<<"succeeded">>, CurrentResults, [])
                             ,kz_json:get_value(<<"succeeded">>, Result, [])
                             ]),

    kz_json:set_values(
      [{<<"succeeded">>, Succeeded}
      ,{<<"failed">>, Failed}
      ], CurrentResults
     ).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec retry_conflict(fun(() -> db_ret())) -> db_ret().
-spec retry_conflict(fun(() -> db_ret()), 0..3) -> db_ret().
retry_conflict(Fun) ->
    retry_conflict(Fun, 3).

retry_conflict(_, 0) -> {'error', 'conflict'};
retry_conflict(Fun, Tries) ->
    case Fun() of
        {'error', 'conflict'} -> retry_conflict(Fun, Tries - 1);
        Other -> Other
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_msg_belonging(api_ne_binary(), kz_json:object()) -> boolean().
check_msg_belonging(BoxId, JObj) ->
    BoxId =:= 'undefined'
        orelse BoxId =:= kzd_box_message:source_id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc Try to find changes made in messages list and return a tuple of
%% messages that changed and messages that are not changed and must go
%% into vmbox messages list.
%% @end
%%--------------------------------------------------------------------
-type diff_ret() :: {kz_json:objects(), kz_json:objects()}.

-spec find_differences(ne_binary(), ne_binary(), kz_json:objects()) -> diff_ret().
find_differences(AccountId, BoxId, DirtyJObj) ->
    Messages = kvm_messages:get(AccountId, BoxId),
    find_differences(DirtyJObj, Messages).

find_differences(DirtyJ, Messages) ->
    Fun = fun(MsgJ, Acc) -> find_differences_fold(DirtyJ, MsgJ, Acc) end,
    lists:foldl(Fun , {[], []}, Messages).

-spec find_differences_fold(kz_json:objects(),kz_json:object(), diff_ret()) -> diff_ret().
find_differences_fold(DirtyJ, MsgJ, {DiffAcc, VMMsgAcc}) ->
    MessageId = kzd_box_message:media_id(MsgJ),
    case kz_json:find_value(<<"media_id">>, MessageId, DirtyJ) of
        'undefined' ->
            {[MsgJ | DiffAcc], VMMsgAcc};
        J ->
            case kz_json:are_identical(MsgJ, J) of
                'true' -> {DiffAcc, maybe_add_to_vmbox(MsgJ, VMMsgAcc)};
                'false' -> {[J | DiffAcc], VMMsgAcc}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_moved_msgs(ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
cleanup_moved_msgs(_, _, []) -> 'ok';
cleanup_moved_msgs(AccountId, BoxId, OldIds) ->
    AccountDb = get_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDb, BoxId) of
        {'ok', VMBox} ->
            Messages = kz_json:get_value(?VM_KEY_MESSAGES, VMBox, []),
            FilterFun = fun(M) -> not lists:member(kzd_box_message:media_id(M), OldIds) end,
            NewMessages = lists:filter(FilterFun, Messages),
            NewBoxJObj = kz_json:set_value(?VM_KEY_MESSAGES, NewMessages, VMBox),
            case ?RETRY_CONFLICT(kz_datamgr:save_doc(AccountDb, NewBoxJObj)) of
                {'ok', _} -> 'ok';
                {'error', _R} ->
                    lager:error("could not update mailbox messages array after moving voicemail messages to MODb ~s: ~s", [BoxId, _R])
            end;
        {'error', _R} ->
            lager:error("unable to open mailbox for update messages array after moving voicemail messages to MODb ~s: ~s", [BoxId, _R])
    end.

-spec retention_days() -> integer().
retention_days() ->
    ?RETENTION_DAYS(?RETENTION_DURATION).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_caller_id_name(kapps_call:call()) -> ne_binary().
get_caller_id_name(Call) ->
    CallerIdName = kapps_call:caller_id_name(Call),
    case kapps_call:kvs_fetch('prepend_cid_name', Call) of
        'undefined' -> CallerIdName;
        Prepend ->
            Pre = <<(kz_util:to_binary(Prepend))/binary, CallerIdName/binary>>,
            kz_util:truncate_right_binary(Pre, kzd_schema_caller_id:external_name_max_length())
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_caller_id_number(kapps_call:call()) -> ne_binary().
get_caller_id_number(Call) ->
    CallerIdNumber = kapps_call:caller_id_number(Call),
    case kapps_call:kvs_fetch('prepend_cid_number', Call) of
        'undefined' -> CallerIdNumber;
        Prepend ->
            Pre = <<(kz_util:to_binary(Prepend))/binary, CallerIdNumber/binary>>,
            kz_util:truncate_right_binary(Pre, kzd_schema_caller_id:external_name_max_length())
    end.

%%%===================================================================
%%% Publish Notification
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_saved_notify(ne_binary(), ne_binary(), kapps_call:call(), pos_integer(), kz_proplist()) ->
                                  {'ok', kz_json:objects()} |
                                  {'timeout', kz_json:objects()} |
                                  {'error', any()}.
publish_saved_notify(MediaId, BoxId, Call, Length, Props) ->
    MaybeTranscribe = props:get_value(<<"Transcribe-Voicemail">>, Props),
    Transcription = maybe_transcribe(Call, MediaId, MaybeTranscribe),

    NotifyProp = [{<<"From-User">>, kapps_call:from_user(Call)}
                 ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
                 ,{<<"To-User">>, kapps_call:to_user(Call)}
                 ,{<<"To-Realm">>, kapps_call:to_realm(Call)}
                 ,{<<"Account-DB">>, kapps_call:account_db(Call)}
                 ,{<<"Account-ID">>, kapps_call:account_id(Call)}
                 ,{<<"Voicemail-Box">>, BoxId}
                 ,{<<"Voicemail-Name">>, MediaId}
                 ,{<<"Caller-ID-Number">>, get_caller_id_number(Call)}
                 ,{<<"Caller-ID-Name">>, get_caller_id_name(Call)}
                 ,{<<"Voicemail-Timestamp">>, kz_util:current_tstamp()}
                 ,{<<"Voicemail-Length">>, Length}
                 ,{<<"Voicemail-Transcription">>, Transcription}
                 ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                 ],

    lager:debug("notifying of voicemail saved"),
    kz_amqp_worker:call_collect(NotifyProp
                               ,fun kapi_notifications:publish_voicemail/1
                               ,fun collecting/1
                               ,30 * ?MILLISECONDS_IN_SECOND
                               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_voicemail_saved(pos_integer(), ne_binary(), kapps_call:call(), ne_binary(), gregorian_seconds()) -> 'ok'.
publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp) ->
    Prop = [{<<"From-User">>, kapps_call:from_user(Call)}
           ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
           ,{<<"To-User">>, kapps_call:to_user(Call)}
           ,{<<"To-Realm">>, kapps_call:to_realm(Call)}
           ,{<<"Account-DB">>, kapps_call:account_db(Call)}
           ,{<<"Account-ID">>, kapps_call:account_id(Call)}
           ,{<<"Voicemail-Box">>, BoxId}
           ,{<<"Voicemail-Name">>, MediaId}
           ,{<<"Caller-ID-Number">>, get_caller_id_number(Call)}
           ,{<<"Caller-ID-Name">>, get_caller_id_name(Call)}
           ,{<<"Voicemail-Timestamp">>, Timestamp}
           ,{<<"Voicemail-Length">>, Length}
           ,{<<"Call-ID">>, kapps_call:call_id(Call)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_notifications:publish_voicemail_saved(Prop),
    lager:debug("published voicemail_saved for ~s", [BoxId]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_notify_completed_message(kz_json:objects()) -> kz_json:object().
-spec get_notify_completed_message(kz_json:objects(), kz_json:object()) -> kz_json:object().
get_notify_completed_message(JObjs) ->
    get_notify_completed_message(JObjs, kz_json:new()).

get_notify_completed_message([], Acc) -> Acc;
get_notify_completed_message([JObj|JObjs], Acc) ->
    case kz_json:get_value(<<"Status">>, JObj) of
        <<"completed">> -> get_notify_completed_message([], JObj);
        _ -> get_notify_completed_message(JObjs, Acc)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_to_vmbox(kz_json:object(), kz_json:objects()) -> kz_json:objects().
-spec maybe_add_to_vmbox(kz_json:object(), ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_add_to_vmbox(M, Acc) ->
    maybe_add_to_vmbox(M, kzd_box_message:media_id(M), Acc).

maybe_add_to_vmbox(_M, ?MATCH_MODB_PREFIX(_Year, _Month, _), Acc) ->
    Acc;
maybe_add_to_vmbox(M, _Id, Acc) ->
    [M | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec collecting(kz_json:objects()) -> boolean().
collecting([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_transcribe(ne_binary(), ne_binary(), boolean()) -> api_object().
-spec maybe_transcribe(ne_binary(), kz_json:object(), binary(), api_binary()) -> api_object().
maybe_transcribe(AccountId, MediaId, 'true') ->
    Db = get_db(AccountId, MediaId),
    {'ok', MediaDoc} = kz_datamgr:open_doc(Db, MediaId),
    case kz_doc:attachment_names(MediaDoc) of
        [] ->
            lager:warning("no audio attachments on media doc ~s: ~p", [MediaId, MediaDoc]),
            'undefined';
        [AttachmentId|_] ->
            CT = kz_doc:attachment_content_type(MediaDoc, AttachmentId),
            case kz_datamgr:fetch_attachment(Db, MediaId, AttachmentId) of
                {'ok', Bin} ->
                    lager:info("transcribing first attachment ~s", [AttachmentId]),
                    maybe_transcribe(Db, MediaDoc, Bin, CT);
                {'error', _E} ->
                    lager:info("error fetching vm: ~p", [_E]),
                    'undefined'
            end
    end;
maybe_transcribe(_, _, 'false') -> 'undefined'.

maybe_transcribe(_, _, _, 'undefined') -> 'undefined';
maybe_transcribe(_, _, <<>>, _) -> 'undefined';
maybe_transcribe(Db, MediaDoc, Bin, ContentType) ->
    case kapps_speech:asr_freeform(Bin, ContentType) of
        {'ok', Resp} ->
            lager:info("transcription resp: ~p", [Resp]),
            MediaDoc1 = kz_json:set_value(<<"transcription">>, Resp, MediaDoc),
            _ = kz_datamgr:ensure_saved(Db, MediaDoc1),
            is_valid_transcription(kz_json:get_value(<<"result">>, Resp)
                                  ,kz_json:get_value(<<"text">>, Resp)
                                  ,Resp
                                  );
        {'error', _E} ->
            lager:info("error transcribing: ~p", [_E]),
            'undefined'
    end.

-spec is_valid_transcription(api_binary(), binary(), kz_json:object()) -> api_object().
is_valid_transcription(<<"success">>, ?NE_BINARY, Resp) -> Resp;
is_valid_transcription(_Res, _Txt, _) ->
    lager:info("not valid transcription: ~s: '~s'", [_Res, _Txt]),
    'undefined'.
