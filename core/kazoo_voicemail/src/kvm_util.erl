%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Voice mailbox utility functions.
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

        ,publish_saved_notify/5, publish_voicemail_saved/5, publish_voicemail_deleted/3
        ,get_caller_id_name/1, get_caller_id_number/1

        ,transcribe_default/0
        ]).

-include("kz_voicemail.hrl").

%%------------------------------------------------------------------------------
%% @doc Get formatted account's database name
%% @end
%%------------------------------------------------------------------------------
-spec get_db(kz_term:ne_binary()) -> kz_term:ne_binary().
get_db(AccountId) ->
    kzs_util:format_account_db(AccountId).

%%------------------------------------------------------------------------------
%% @doc Get formatted account's MODB database name.
%% If the MessageId is not a `MODB_PREFIX' account's database will be returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_db(kz_term:ne_binary(), kazoo_data:docid() | kz_json:object()) -> kz_term:ne_binary().
get_db(AccountId, {_, ?MATCH_MODB_PREFIX(Year, Month, _)}) ->
    get_db(AccountId, Year, Month);
get_db(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)) ->
    get_db(AccountId, Year, Month);
get_db(AccountId, ?NE_BINARY = _DocId) ->
    get_db(AccountId);
get_db(AccountId, Doc) ->
    get_db(AccountId, kz_doc:id(Doc)).

-spec get_db(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_db(AccountId, Year, Month) ->
    kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)).

%% @equiv get_range_db(AccountId, retention_days(AccountId))
-spec get_range_db(kz_term:ne_binary()) -> {kz_time:gregorian_seconds(), kz_time:gregorian_seconds(), kz_term:ne_binaries()}.
get_range_db(AccountId) ->
    get_range_db(AccountId, retention_days(AccountId)).

%%------------------------------------------------------------------------------
%% @doc Generate a range of MODB database names while considering retention days.
%% @end
%%------------------------------------------------------------------------------
-spec get_range_db(kz_term:ne_binary(), pos_integer()) -> {kz_time:gregorian_seconds(), kz_time:gregorian_seconds(), kz_term:ne_binaries()}.
get_range_db(AccountId, Days) ->
    To = kz_time:now_s(),
    From = To - retention_seconds(Days),
    {From, To, lists:reverse([Db || Db <- kazoo_modb:get_range(AccountId, From, To)])}.

%%------------------------------------------------------------------------------
%% @doc Split a list of messages into a map of database name and messages using the message ID.
%% @end
%%------------------------------------------------------------------------------
-spec split_to_modbs(kz_term:ne_binary(), kz_term:ne_binaries()) -> map().
split_to_modbs(AccountId, MsgIds) ->
    lists:foldl(fun(Id, Map) ->
                        Db = get_db(AccountId, Id),
                        maps:update_with(Db, fun(List) -> [Id|List] end, [Id], Map)
                end, #{}, MsgIds).

%%------------------------------------------------------------------------------
%% @doc Open `DocId' from MODB database and check the document type is matching expected type.
%% @end
%%------------------------------------------------------------------------------
-spec open_modb_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary()) -> db_ret().
open_modb_doc(AccountId, DocId, Type) ->
    case kazoo_modb:open_doc(AccountId, DocId) of
        {'ok', JObj}-> check_doc_type(JObj, Type, kz_doc:type(JObj));
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Open `DocId' from account database and check the document type is matching expected type.
%% @end
%%------------------------------------------------------------------------------
-spec open_accountdb_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary()) -> db_ret().
open_accountdb_doc(AccountId, DocId, Type) ->
    case kz_datamgr:open_doc(get_db(AccountId), DocId) of
        {'ok', JObj} -> check_doc_type(JObj, Type, kz_doc:type(JObj));
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Protect against returning wrong doc when expected type is not matched.
%% Especially useful for requests from crossbar
%% @end
%%------------------------------------------------------------------------------
-spec check_doc_type(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> db_ret().
check_doc_type(Doc, Type, Type) ->
    {'ok', Doc};
check_doc_type(_Doc, _ExpectedType, _DocType) ->
    lager:debug("not expected type : ~s , ~s", [_ExpectedType, _DocType]),
    {'error', 'not_found'}.

%%------------------------------------------------------------------------------
%% @doc If `BoxId' is not `undefined' check if message `source_id' matching the `BoxId'.
%% @end
%%------------------------------------------------------------------------------
-spec check_msg_belonging(kz_term:api_ne_binary(), kz_json:object()) -> boolean().
check_msg_belonging(BoxId, JObj) ->
    check_msg_belonging(BoxId, JObj, kzd_box_message:source_id(JObj)).

-spec check_msg_belonging(kz_term:api_ne_binary(), kz_json:object(), kz_term:api_ne_binary()) -> boolean().
check_msg_belonging(_BoxId, _JObj, 'undefined') -> 'true';
check_msg_belonging('undefined', _JObj, _SourceId) -> 'true';
check_msg_belonging(BoxId, _JObj, BoxId) -> 'true';
check_msg_belonging(_BoxId, _JObj, _SourceId) ->
    lager:debug("message ~s belongs to mailbox ~s but claims to belong to ~s"
               ,[kz_doc:id(_JObj), _SourceId, _BoxId]
               ),
    'false'.

%%------------------------------------------------------------------------------
%% @doc Get system's default retention duration.
%% @end
%%------------------------------------------------------------------------------
-spec retention_seconds() -> kz_time:gregorian_seconds().
retention_seconds() ->
    retention_seconds(?RETENTION_DAYS).

%%------------------------------------------------------------------------------
%% @doc Calculate Gregorian seconds of retention days.
%% @end
%%------------------------------------------------------------------------------
-spec retention_seconds(integer() | kz_term:api_binary()) -> kz_time:gregorian_seconds().
retention_seconds(Days) when is_integer(Days)
                             andalso Days > 0 ->
    ?SECONDS_IN_DAY * Days + ?SECONDS_IN_HOUR;
retention_seconds(?NE_BINARY=AccountId) ->
    retention_seconds(retention_days(AccountId));
retention_seconds(_) ->
    retention_seconds(?RETENTION_DAYS).

%%------------------------------------------------------------------------------
%% @doc Get account's configured of how many days a message should be retained.
%% If account is not configured will use system's default value.
%% @end
%%------------------------------------------------------------------------------
-spec retention_days(kz_term:ne_binary()) -> integer().
retention_days(AccountId) ->
    case kapps_account_config:get_pos_integer(AccountId, ?VM_CONFIG_CAT, [?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]) of
        'undefined' -> ?RETENTION_DAYS;
        Days -> try kz_term:to_integer(Days) catch _:_ -> ?RETENTION_DAYS end
    end.

%%------------------------------------------------------------------------------
%% @doc If message is older than retention duration, set folder to deleted
%% @end
%%------------------------------------------------------------------------------
-spec enforce_retention(kz_json:object()) -> kz_json:object().
enforce_retention(JObj) ->
    enforce_retention(JObj, kz_time:now_s() - retention_seconds(kz_doc:account_id(JObj))).

%%------------------------------------------------------------------------------
%% @doc If message is older than retention duration, set folder to deleted
%% @end
%%------------------------------------------------------------------------------
-spec enforce_retention(kz_json:object(), kz_time:gregorian_seconds() | boolean()) -> kz_json:object().
enforce_retention(JObj, RetentionTimestamp)
  when is_integer(RetentionTimestamp) ->
    enforce_retention(JObj, is_prior_to_retention(JObj, RetentionTimestamp));
enforce_retention(JObj, 'false') ->
    JObj;
enforce_retention(JObj, 'true') ->
    case kzd_box_message:metadata(JObj) of
        'undefined' ->
            kz_json:set_values([{<<"retention">>, <<"enforced">>}
                               ,{<<"retention_message">>
                                ,<<"this message is prior to retention policy, no update operation is permitted">>
                                }
                               ]
                              ,kzd_box_message:set_folder_deleted(JObj)
                              );
        Metadata ->
            kzd_box_message:set_metadata(
              kz_json:set_values([{<<"retention">>, <<"enforced">>}
                                 ,{<<"retention_message">>
                                  ,<<"this message is prior to retention policy, no update operation is permitted">>
                                  }
                                 ]
                                ,kzd_box_message:set_folder_deleted(Metadata)
                                )
             ,JObj
             )
    end.

%%------------------------------------------------------------------------------
%% @doc Checks if message is older than retention duration
%% @end
%%------------------------------------------------------------------------------
-spec is_prior_to_retention(kz_json:object(), kz_time:api_seconds()) -> boolean().
is_prior_to_retention(_, 'undefined') ->
    'false';
is_prior_to_retention(JObj, RetentionTimestamp) ->
    TsTampPath = [<<"utc_seconds">>, <<"timestamp">>, [<<"metadata">>, <<"timestamp">>], <<"pvt_create">>],
    MsgTstamp = kz_term:to_integer(kz_json:get_first_defined(TsTampPath, JObj, 0)),
    MsgTstamp =/= 0
        andalso MsgTstamp < RetentionTimestamp.

%% @equiv get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId, 'undefined')
-spec get_change_vmbox_funs(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) ->
          {kz_term:ne_binary(), update_funs()}.
get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId) ->
    get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId, 'undefined').

%%------------------------------------------------------------------------------
%% @doc List of function to pass to `update' functions when changing mailbox of a message.
%% @end
%%------------------------------------------------------------------------------
-spec get_change_vmbox_funs(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:api_binary()) ->
          {kz_term:ne_binary(), update_funs()}.
get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId, ToId) ->
    Timestamp = kz_time:now_s(),
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

%%------------------------------------------------------------------------------
%% @doc Get Caller ID Name.
%% @end
%%------------------------------------------------------------------------------
-spec get_caller_id_name(kapps_call:call()) -> kz_term:ne_binary().
get_caller_id_name(Call) ->
    CallerIdName = kapps_call:caller_id_name(Call),
    case kapps_call:kvs_fetch('prepend_cid_name', Call) of
        'undefined' -> CallerIdName;
        Prepend ->
            Pre = <<(kz_term:to_binary(Prepend))/binary, CallerIdName/binary>>,
            kz_binary:truncate_right(Pre, kzd_schema_caller_id:external_name_max_length())
    end.

%%------------------------------------------------------------------------------
%% @doc Get Caller ID Number.
%% @end
%%------------------------------------------------------------------------------
-spec get_caller_id_number(kapps_call:call()) -> kz_term:ne_binary().
get_caller_id_number(Call) ->
    CallerIdNumber = kapps_call:caller_id_number(Call),
    case kapps_call:kvs_fetch('prepend_cid_number', Call) of
        'undefined' -> CallerIdNumber;
        Prepend ->
            Pre = <<(kz_term:to_binary(Prepend))/binary, CallerIdNumber/binary>>,
            kz_binary:truncate_right(Pre, kzd_schema_caller_id:external_name_max_length())
    end.

%%------------------------------------------------------------------------------
%% @doc Get trascribe default.
%% @end
%%------------------------------------------------------------------------------
-spec transcribe_default() -> boolean().
transcribe_default() ->
    kapps_config:get_is_true(?VM_CONFIG_CAT, [?KEY_VOICEMAIL, <<"transcribe_default">>], 'false').

%%%=============================================================================
%%% Publish Notification
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Publishes `voicemail_new' notification to teletype.
%% @end
%%------------------------------------------------------------------------------
-spec publish_saved_notify(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call(), pos_integer(), kz_term:proplist()) ->
          kz_amqp_worker:request_return().
publish_saved_notify(MediaId, BoxId, Call, Length, Props) ->
    MaybeTranscribe = props:get_value(<<"Transcribe-Voicemail">>, Props, 'false'),
    Transcription = maybe_transcribe(Call, MediaId, MaybeTranscribe),

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
                 ,{<<"Voicemail-Timestamp">>, kz_time:now_s()}
                 ,{<<"Voicemail-Length">>, Length}
                 ,{<<"Voicemail-Transcription">>, Transcription}
                 ,{<<"Call-ID">>, kapps_call:call_id_direct(Call)}
                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                 ],

    lager:debug("sending voicemail_new notification"),
    kapps_notify_publisher:call_collect(NotifyProp, fun kapi_notifications:publish_voicemail_new/1).

%%------------------------------------------------------------------------------
%% @doc Publishes `voicemail_saved' notification.
%% @end
%%------------------------------------------------------------------------------
-spec publish_voicemail_saved(pos_integer(), kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binary(), kz_time:gregorian_seconds()) -> 'ok'.
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
    _ = kz_amqp_worker:cast(Prop, fun kapi_notifications:publish_voicemail_saved/1),
    lager:debug("published voicemail_saved for ~s", [BoxId]).

%%------------------------------------------------------------------------------
%% @doc Publishes `voicemail_deleted' notification.
%% @end
%%------------------------------------------------------------------------------
-spec publish_voicemail_deleted(kz_term:ne_binary(), kz_json:object(), vm_delete_reason()) -> 'ok'.
publish_voicemail_deleted(BoxId, Msg, Reason) ->
    Metadata = kzd_box_message:metadata(Msg),
    From = kz_json:get_ne_binary_value(<<"from">>, Metadata),
    To = kz_json:get_ne_binary_value(<<"to">>, Metadata),

    [FromUser, FromRealm] = binary:split(From, <<"@">>, [global]),
    [ToUser, ToRealm] = binary:split(To, <<"@">>, [global]),

    Prop = [{<<"From-User">>, FromUser}
           ,{<<"From-Realm">>, FromRealm}
           ,{<<"To-User">>, ToUser}
           ,{<<"To-Realm">>, ToRealm}
           ,{<<"Reason">>, Reason}
           ,{<<"Account-DB">>, kz_json:get_ne_binary_value(<<"pvt_account_db">>, Msg)}
           ,{<<"Account-ID">>, kz_json:get_ne_binary_value(<<"pvt_account_id">>, Msg)}
           ,{<<"Voicemail-Box">>, BoxId}
           ,{<<"Voicemail-ID">>, kz_json:get_ne_binary_value(<<"media_id">>, Metadata)}
           ,{<<"Caller-ID-Number">>, kz_json:get_ne_binary_value(<<"caller_id_number">>, Metadata)}
           ,{<<"Caller-ID-Name">>, kz_json:get_ne_binary_value(<<"caller_id_name">>, Metadata)}
           ,{<<"Voicemail-Timestamp">>, kz_json:get_ne_binary_value(<<"timestamp">>, Metadata)}
           ,{<<"Voicemail-Length">>, kz_json:get_ne_binary_value(<<"length">>, Metadata)}
           ,{<<"Call-ID">>, kz_json:get_ne_binary_value(<<"call_id">>, Metadata)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],

    case ?SEND_DELETE_NOTIFY_AMPQ of
        true ->
            _ = kz_amqp_worker:cast(Prop, fun kapi_notifications:publish_voicemail_deleted/1),
            lager:debug("published voicemail_deleted for ~s", [BoxId]);
        _ ->
            ok
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% generate and asr request to transcribe voicemail recording
%% @end
%%------------------------------------------------------------------------------
-spec maybe_transcribe(kapps_call:call(), kz_term:ne_binary(), boolean()) -> 'undefined' | kazoo_speech:asr_resp().
maybe_transcribe(_, _,'false') -> 'undefined';
maybe_transcribe(Call, MediaId, 'true') ->
    Req = asr_request:from_voicemail(Call, MediaId),
    Req0 = asr_request:transcribe(Req),
    case asr_request:error(Req0) of
        'undefined' -> asr_request:transcription(Req0);
        Error ->
            lager:notice("error transcribing ~p", [Error]),
            'undefined'
    end.
