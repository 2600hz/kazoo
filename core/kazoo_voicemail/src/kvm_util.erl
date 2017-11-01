%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Voice mailbox utility functions
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_util).

-export([get_db/1, get_db/2
        ,get_range_db/1, get_range_db/2, split_to_modbs/2
        ,open_modb_doc/3, open_accountdb_doc/3
        ,check_doc_type/3

        ,check_msg_belonging/2
        ,get_change_vmbox_funs/4, get_change_vmbox_funs/5

        ,retention_days/1
        ,retention_seconds/0, retention_seconds/1
        ,enforce_retention/1, enforce_retention/2, is_prior_to_retention/2

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
    kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)).

%%--------------------------------------------------------------------
%% @public
%% @doc Generate a range of database names
%% @end
%%--------------------------------------------------------------------
-spec get_range_db(ne_binary()) -> {gregorian_seconds(), gregorian_seconds(), ne_binaries()}.
-spec get_range_db(ne_binary(), pos_integer()) -> {gregorian_seconds(), gregorian_seconds(), ne_binaries()}.
get_range_db(AccountId) ->
    get_range_db(AccountId, retention_days(AccountId)).

get_range_db(AccountId, Days) ->
    To = kz_time:current_tstamp(),
    From = To - retention_seconds(Days),
    {From, To, lists:reverse([Db || Db <- kazoo_modb:get_range(AccountId, From, To)])}.

-spec split_to_modbs(ne_binary(), ne_binaries()) -> map().
split_to_modbs(AccountId, MsgIds) ->
    lists:foldl(fun(Id, Map) ->
                        Db = get_db(AccountId, Id),
                        maps:update_with(Db, fun(List) -> [Id|List] end, [Id], Map)
                end, #{}, MsgIds).

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
%% @public
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
-spec check_msg_belonging(api_ne_binary(), kz_json:object()) -> boolean().
-spec check_msg_belonging(api_ne_binary(), kz_json:object(), api_ne_binary()) -> boolean().
check_msg_belonging(BoxId, JObj) ->
    check_msg_belonging(BoxId, JObj, kzd_box_message:source_id(JObj)).

check_msg_belonging(_BoxId, _JObj, 'undefined') -> 'true';
check_msg_belonging('undefined', _JObj, _SourceId) -> 'true';
check_msg_belonging(BoxId, _JObj, BoxId) -> 'true';
check_msg_belonging(_BoxId, _JObj, _SourceId) ->
    lager:debug("message ~s belongs to mailbox ~s but claims to belong to ~s"
               ,[kz_doc:id(_JObj), _SourceId, _BoxId]),
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec retention_seconds() -> gregorian_seconds().
retention_seconds() ->
    retention_seconds(?RETENTION_DAYS).

-spec retention_seconds(integer() | api_binary()) -> gregorian_seconds().
retention_seconds(Days) when is_integer(Days)
                             andalso Days > 0 ->
    ?SECONDS_IN_DAY * Days + ?SECONDS_IN_HOUR;
retention_seconds(?NE_BINARY=AccountId) ->
    retention_seconds(retention_days(AccountId));
retention_seconds(_) ->
    retention_seconds(?RETENTION_DAYS).

-spec retention_days(ne_binary()) -> integer().
retention_days(AccountId) ->
    case kapps_account_config:get_pos_integer(AccountId, ?CF_CONFIG_CAT, [?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]) of
        'undefined' -> ?RETENTION_DAYS;
        Days -> try kz_term:to_integer(Days) catch _:_ -> ?RETENTION_DAYS end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if message is older than retention duration, set folder to deleted
%% @end
%%--------------------------------------------------------------------
-spec enforce_retention(kz_json:object()) -> kz_json:object().
enforce_retention(JObj) ->
    enforce_retention(JObj, kz_time:current_tstamp() - retention_seconds(kz_doc:account_id(JObj))).

-spec enforce_retention(kz_json:object(), gregorian_seconds() | boolean()) -> kz_json:object().
enforce_retention(JObj, RetentionTimestamp)
  when is_integer(RetentionTimestamp) ->
    enforce_retention(JObj, is_prior_to_retention(JObj, RetentionTimestamp));
enforce_retention(JObj, 'false') ->
    JObj;
enforce_retention(JObj, 'true') ->
    case kzd_box_message:metadata(JObj) of
        'undefined' -> kzd_box_message:set_folder_deleted(JObj);
        Metadata ->
            kzd_box_message:set_metadata(kzd_box_message:set_folder_deleted(Metadata), JObj)
    end.

-spec is_prior_to_retention(kz_json:object(), api_seconds()) -> boolean().
is_prior_to_retention(_, 'undefined') ->
    'false';
is_prior_to_retention(JObj, RetentionTimestamp) ->
    TsTampPath = [<<"utc_seconds">>, <<"timestamp">>, [<<"metadata">>, <<"timestamp">>], <<"pvt_create">>],
    MsgTstamp = kz_term:to_integer(kz_json:get_first_defined(TsTampPath, JObj, 0)),
    MsgTstamp =/= 0
        andalso MsgTstamp < RetentionTimestamp.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_change_vmbox_funs(ne_binary(), ne_binary(), kz_json:object(), ne_binary()) ->
                                   {ne_binary(), update_funs()}.
get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId) ->
    get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId, 'undefined').

-spec get_change_vmbox_funs(ne_binary(), ne_binary(), kz_json:object(), ne_binary(), api_binary()) ->
                                   {ne_binary(), update_funs()}.
get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId, ToId) ->
    Timestamp = kz_time:current_tstamp(),
    {{Y, M, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    Year = kz_term:to_binary(Y),
    Month = kz_date:pad_month(M),

    NewId = case ToId of
                'undefined' -> kazoo_modb_util:modb_id(Year, Month, kz_binary:rand_hex(16));
                ?MATCH_MODB_PREFIX(Year, Month,  _Rest) -> ToId;
                _OldId -> kazoo_modb_util:modb_id(Year, Month, kz_binary:rand_hex(16))
            end,
    AccountDb = get_db(AccountId, NewId),

    {NewId
    ,[fun(DocJ) -> kzd_box_message:set_source_id(NewBoxId, DocJ) end
     ,fun(DocJ) -> kzd_box_message:apply_folder(?VM_FOLDER_NEW, DocJ) end
     ,fun(DocJ) -> kzd_box_message:change_message_name(NBoxJ, DocJ) end
     ,fun(DocJ) -> kzd_box_message:change_to_sip_field(AccountId, NBoxJ, DocJ) end
     ,fun(DocJ) -> kzd_box_message:add_message_history(OldBoxId, DocJ) end
     ,fun(DocJ) -> kzd_box_message:update_media_id(NewId, DocJ) end
     ,fun(DocJ) -> kz_json:set_value([<<"metadata">>, <<"timestamp">>], Timestamp, DocJ) end
     ,fun(DocJ) -> kz_json:set_value(<<"utc_seconds">>, Timestamp, DocJ) end
     ,fun(DocJ) -> kz_doc:set_account_db(DocJ, AccountDb) end
     ,fun(DocJ) -> kz_doc:set_modified(DocJ, Timestamp) end
     ]
    }.

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
            Pre = <<(kz_term:to_binary(Prepend))/binary, CallerIdName/binary>>,
            kz_binary:truncate_right(Pre, kzd_schema_caller_id:external_name_max_length())
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
            Pre = <<(kz_term:to_binary(Prepend))/binary, CallerIdNumber/binary>>,
            kz_binary:truncate_right(Pre, kzd_schema_caller_id:external_name_max_length())
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
                                  kz_amqp_worker:request_return().
publish_saved_notify(MediaId, BoxId, Call, Length, Props) ->
    MaybeTranscribe = props:get_value(<<"Transcribe-Voicemail">>, Props, 'false'),
    Transcription = maybe_transcribe(kapps_call:account_id(Call), MediaId, MaybeTranscribe),

    NotifyProp = [{<<"From-User">>, kapps_call:from_user(Call)}
                 ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
                 ,{<<"To-User">>, kapps_call:to_user(Call)}
                 ,{<<"To-Realm">>, kapps_call:to_realm(Call)}
                 ,{<<"Account-DB">>, kapps_call:account_db(Call)}
                 ,{<<"Account-ID">>, kapps_call:account_id(Call)}
                 ,{<<"Voicemail-Box">>, BoxId}
                 ,{<<"Voicemail-ID">>, MediaId}
                 ,{<<"Caller-ID-Number">>, get_caller_id_number(Call)}
                 ,{<<"Caller-ID-Name">>, get_caller_id_name(Call)}
                 ,{<<"Voicemail-Timestamp">>, kz_time:current_tstamp()}
                 ,{<<"Voicemail-Length">>, Length}
                 ,{<<"Voicemail-Transcription">>, Transcription}
                 ,{<<"Call-ID">>, kapps_call:call_id_direct(Call)}
                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                 ],

    lager:debug("notifying of voicemail saved"),
    kapps_notify_publisher:call_collect(NotifyProp, fun kapi_notifications:publish_voicemail_new/1).

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
           ,{<<"Voicemail-ID">>, MediaId}
           ,{<<"Caller-ID-Number">>, get_caller_id_number(Call)}
           ,{<<"Caller-ID-Name">>, get_caller_id_name(Call)}
           ,{<<"Voicemail-Timestamp">>, Timestamp}
           ,{<<"Voicemail-Length">>, Length}
           ,{<<"Call-ID">>, kapps_call:call_id_direct(Call)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapps_notify_publisher:cast(Prop, fun kapi_notifications:publish_voicemail_saved/1),
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
    case kazoo_asr:freeform(Bin, ContentType) of
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
