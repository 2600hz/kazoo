%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(conf_pronounced_name).

%% API
-export([lookup_name/1, record/1]).

-include("conference.hrl").

-type name_pronounced_media() :: {atom(), kz_term:ne_binary(), kz_term:ne_binary()}.
-type name_pronounced_ids()   :: {atom(), kz_term:ne_binary(), kz_term:ne_binary()} |
                                 name_pronounced_media().
-type name_pronounced() :: name_pronounced_ids() |
                           'undefined'.
-export_type([name_pronounced/0]).

-define(PRONOUNCED_NAME_KEY, [<<"name_pronounced">>, <<"media_id">>]).

-spec get_user_id(kapps_call:call()) -> kz_term:api_binary().
get_user_id(Call) ->
    case kapps_call:authorizing_type(Call) of
        <<"user">> -> kapps_call:authorizing_id(Call);
        <<"device">> -> get_user_id_from_device(Call);
        <<"mobile">> -> get_user_id_from_device(Call);
        _Type -> 'undefined'
    end.

-spec get_user_id_from_device(kapps_call:call()) -> kz_term:api_binary().
get_user_id_from_device(Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call)
                                  ,kapps_call:authorizing_id(Call)
                                  )
    of
        {'ok', DeviceDoc} -> kz_json:get_value(<<"owner_id">>, DeviceDoc);
        _ -> 'undefined'
    end.

-spec lookup_name(kapps_call:call()) -> name_pronounced_media() | 'undefined'.
lookup_name(Call) ->
    case get_user_id(Call) of
        'undefined' -> 'undefined';
        UserId -> lookup_user_name(Call, UserId)
    end.

-spec lookup_user_name(kapps_call:call(), kz_term:ne_binary()) -> name_pronounced_media() | 'undefined'.
lookup_user_name(Call, UserId) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), UserId) of
        {'ok', UserDoc} ->
            case kz_json:get_value(?PRONOUNCED_NAME_KEY, UserDoc) of
                'undefined' -> 'undefined';
                DocId -> {'media_doc_id', kapps_call:account_db(Call), DocId}
            end;
        _ -> 'undefined'
    end.

-spec record(kapps_call:call()) -> name_pronounced().
record(Call) ->
    RecordName = list_to_binary(["conf_announce_",kz_datamgr:get_uuid(), ".mp3"]),

    Choice = while(fun user_discards_or_not_error/1
                  ,fun () -> record_name(RecordName, Call) end
                  ,'undefined'
                  ,'true'
                  ),

    case Choice of
        {'ok', <<"1">>} -> save_pronounced_name(RecordName, Call);
        _ -> 'undefined'
    end.

-type predicate() :: fun((any()) -> boolean()).
-type generator() :: fun(() -> any()).
-spec while(predicate(), generator(), any(), boolean()) -> any().
while(Predicate, GeneratorFun, _, 'true') ->
    Value = GeneratorFun(),
    while(Predicate, GeneratorFun, Value, Predicate(Value));
while(_, _, Value, 'false') ->
    Value.

-spec user_discards_or_not_error(kapps_call_command:collect_digits_return()) -> boolean().
user_discards_or_not_error({'ok', Digit}) ->
    Digit =/= <<"1">>;
user_discards_or_not_error(_) ->
    'false'.

-spec record_name(kz_term:ne_binary(), kapps_call:call()) -> kapps_call_command:collect_digits_return().
record_name(RecordName, Call) ->
    lager:debug("recording name"),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                             ,{<<"Duration-ON">>, <<"500">>}
                             ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _ = kapps_call_command:audio_macro([{'prompt', <<"conf-announce_your_name">>}
                                       ,{'tones', [Tone]}
                                       ], Call),
    _ = kapps_call_command:b_record(RecordName, ?ANY_DIGIT, <<"60">>, Call),
    Force = kapps_config:get_is_true(?CONFIG_CAT, <<"review_name">>, 'false'),
    case Force of
        'true' ->
            review(RecordName, Call);
        'false' ->
            {'ok', <<"1">>}
    end.

-spec prepare_media_doc(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
prepare_media_doc(RecordName, Call) ->
    UserId = get_user_id(Call),
    AccountDb = kapps_call:account_db(Call),
    Props = props:filter_undefined(
              [{<<"name">>, RecordName}
              ,{<<"description">>, <<"conference: user's pronounced name">>}
              ,{<<"source_type">>, <<"call to conference">>}
              ,{<<"source_id">>, kapps_call:fetch_id(Call)}
              ,{<<"owner_id">>, UserId}
              ,{<<"media_source">>, <<"recording">>}
              ,{<<"streamable">>, 'true'}
              ]),
    Doc = kz_doc:update_pvt_parameters(kz_json:from_list(Props), AccountDb, [{'type', <<"media">>}]),
    case kz_datamgr:save_doc(AccountDb, Doc) of
        {'ok', MediaJObj} when not is_list(MediaJObj) -> kz_doc:id(MediaJObj);
        _ -> 'undefined'
    end.

-spec save_recording(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> name_pronounced_ids().
save_recording(RecordName, MediaDocId, Call) ->
    UserId = get_user_id(Call),
    AccountDb = kapps_call:account_db(Call),
    AccountId = kapps_call:account_id(Call),
    _ = kapps_call_command:b_store(RecordName, get_new_attachment_url(RecordName, MediaDocId, Call), Call),
    case kz_datamgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} ->
            lager:debug("updating user's doc"),
            JObj1 = kz_json:set_value(?PRONOUNCED_NAME_KEY, MediaDocId, UserJObj),
            _ = kz_datamgr:save_doc(AccountDb, JObj1),
            {'media_doc_id', AccountId, MediaDocId};
        {'error', _Err} ->
            lager:info("can't update user's doc due to error ~p", [_Err]),
            {'temp_doc_id', AccountId, MediaDocId}
    end.

-spec save_pronounced_name(kz_term:ne_binary(), kapps_call:call()) -> name_pronounced().
save_pronounced_name(RecordName, Call) ->
    case prepare_media_doc(RecordName, Call) of
        'undefined' -> 'undefined';
        MediaDocId -> save_recording(RecordName, MediaDocId, Call)
    end.

-spec get_new_attachment_url(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
get_new_attachment_url(AttachmentName, MediaId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    _ = case kz_datamgr:open_doc(AccountDb, MediaId) of
            {'ok', JObj} -> maybe_remove_attachments(Call, JObj);
            {'error', _} -> 'ok'
        end,
    kz_media_url:store(AccountDb, MediaId, AttachmentName).

-spec maybe_remove_attachments(kapps_call:call(), kz_json:object()) -> 'ok'.
maybe_remove_attachments(Call, JObj) ->
    case kz_doc:maybe_remove_attachments(JObj) of
        {'false', _} -> 'ok';
        {'true', Removed} ->
            {'ok', _Saved} = kz_datamgr:save_doc(kapps_call:account_db(Call), Removed),
            lager:debug("removed attachments from media doc ~s (now ~s)"
                       ,[kz_doc:id(_Saved), kz_doc:revision(_Saved)]
                       )
    end.

-spec review(kz_term:ne_binary(), kapps_call:call()) -> kapps_call_command:collect_digits_return().
review(RecordName, Call) ->
    lager:debug("review record"),
    NoopId = kapps_call_command:audio_macro([{'prompt', <<"conf-your_announcement">>}
                                            ,{'play', RecordName}
                                            ,{'prompt', <<"conf-review">>}
                                            ], Call),

    kapps_call_command:collect_digits(1
                                     ,kapps_call_command:default_collect_timeout()
                                     ,kapps_call_command:default_interdigit_timeout()
                                     ,NoopId
                                     ,Call
                                     ).
