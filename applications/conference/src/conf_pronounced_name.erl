%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(conf_pronounced_name).

%% API
-export([lookup_name/1, record/1]).

-include("conference.hrl").

-type name_pronounced_media() :: {'media_doc_id', ne_binary(), ne_binary()}.
-type name_pronounced_ids()   :: {'temp_doc_id', ne_binary(), ne_binary()} |
                                 name_pronounced_media().
-type name_pronounced() :: name_pronounced_ids() |
                           'undefined'.
-export_type([name_pronounced/0]).

-define(PRONOUNCED_NAME_KEY, [<<"name_pronounced">>, <<"media_id">>]).

-spec get_user_id(whapps_call:call()) -> api_binary().
get_user_id(Call) ->
    case whapps_call:authorizing_type(Call) of
        <<"user">> -> whapps_call:authorizing_id(Call);
        <<"device">> -> get_user_id_from_device(Call);
        _Type -> 'undefined'
    end.

-spec get_user_id_from_device(whapps_call:call()) -> api_binary().
get_user_id_from_device(Call) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call)
                                  ,whapps_call:authorizing_id(Call)
                                 )
    of
        {'ok', DeviceDoc} -> wh_json:get_value(<<"owner_id">>, DeviceDoc);
        _ -> 'undefined'
    end.

-spec lookup_name(whapps_call:call()) -> name_pronounced_media() | 'undefined'.
lookup_name(Call) ->
    case get_user_id(Call) of
        'undefined' -> 'undefined';
        UserId -> lookup_user_name(Call, UserId)
    end.

-spec lookup_user_name(whapps_call:call(), ne_binary()) -> name_pronounced_media() | 'undefined'.
lookup_user_name(Call, UserId) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), UserId) of
        {'ok', UserDoc} ->
            case wh_json:get_value(?PRONOUNCED_NAME_KEY, UserDoc) of
                'undefined' -> 'undefined';
                DocId -> {'media_doc_id', whapps_call:account_db(Call), DocId}
            end;
        _ -> 'undefined'
    end.

-spec record(whapps_call:call()) -> name_pronounced().
record(Call) ->
    RecordName = list_to_binary(["conf_announce_",couch_mgr:get_uuid(), ".mp3"]),

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

-spec user_discards_or_not_error(whapps_call_command:collect_digits_return()) -> boolean().
user_discards_or_not_error({'ok', Digit}) ->
    Digit =/= <<"1">>;
user_discards_or_not_error(_) ->
    'false'.

-spec record_name(ne_binary(), whapps_call:call()) -> whapps_call_command:collect_digits_return().
record_name(RecordName, Call) ->
    lager:debug("recording name"),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _ = whapps_call_command:audio_macro([{'prompt', <<"conf-announce_your_name">>}
                                         ,{'tones', [Tone]}
                                        ], Call),
    whapps_call_command:b_record(RecordName, ?ANY_DIGIT, <<"60">>, Call),
    Force = whapps_config:get_is_true(?CONFIG_CAT, <<"review_name">>, 'false'),
    case Force of
        'true' ->
            review(RecordName, Call);
        'false' ->
            {'ok', <<"1">>}
    end.

-spec prepare_media_doc(ne_binary(), whapps_call:call()) -> api_binary().
prepare_media_doc(RecordName, Call) ->
    UserId = get_user_id(Call),
    AccountDb = whapps_call:account_db(Call),
    Props = props:filter_undefined(
              [{<<"name">>, RecordName}
               ,{<<"description">>, <<"conference: user's pronounced name">>}
               ,{<<"source_type">>, <<"call to conference">>}
               ,{<<"source_id">>, whapps_call:fetch_id(Call)}
               ,{<<"owner_id">>, UserId}
               ,{<<"media_source">>, <<"recording">>}
               ,{<<"streamable">>, 'true'}
              ]),
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb, [{'type', <<"media">>}]),
    case couch_mgr:save_doc(AccountDb, Doc) of
        {'ok', MediaJObj} when not is_list(MediaJObj) -> wh_doc:id(MediaJObj);
        _ -> 'undefined'
    end.

-spec save_recording(ne_binary(), ne_binary(), whapps_call:call()) -> name_pronounced_ids().
save_recording(RecordName, MediaDocId, Call) ->
    UserId = get_user_id(Call),
    AccountDb = whapps_call:account_db(Call),
    AccountId = whapps_call:account_id(Call),
    whapps_call_command:b_store(RecordName, get_new_attachment_url(RecordName, MediaDocId, Call), Call),
    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} ->
            lager:debug("Updating user's doc"),
            JObj1 = wh_json:set_value(?PRONOUNCED_NAME_KEY, MediaDocId, UserJObj),
            couch_mgr:save_doc(AccountDb, JObj1),
            {'media_doc_id', AccountId, MediaDocId};
        {'error', _Err} ->
            lager:info("Can't update user's doc due to error ~p", [_Err]),
            {'temp_doc_id', AccountId, MediaDocId}
    end.

-spec save_pronounced_name(ne_binary(), whapps_call:call()) -> name_pronounced().
save_pronounced_name(RecordName, Call) ->
    case prepare_media_doc(RecordName, Call) of
        'undefined' -> 'undefined';
        MediaDocId -> save_recording(RecordName, MediaDocId, Call)
    end.

-spec get_new_attachment_url(ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().
get_new_attachment_url(AttachmentName, MediaId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_doc(AccountDb, MediaId) of
            {'ok', JObj} -> maybe_remove_attachments(Call, JObj);
            {'error', _} -> 'ok'
        end,
    {'ok', URL} = wh_media_url:store(AccountDb, MediaId, AttachmentName),
    URL.

-spec maybe_remove_attachments(whapps_call:call(), wh_json:object()) -> 'ok'.
maybe_remove_attachments(Call, JObj) ->
    case wh_doc:maybe_remove_attachments(JObj) of
        {'false', _} -> 'ok';
        {'true', Removed} ->
            {'ok', _Saved} = couch_mgr:save_doc(whapps_call:account_db(Call), Removed),
            lager:debug("removed attachments from media doc ~s (now ~s)"
                        ,[wh_doc:id(_Saved), wh_doc:revision(_Saved)]
                       )
    end.

-spec review(ne_binary(), whapps_call:call()) -> whapps_call_command:collect_digits_return().
review(RecordName, Call) ->
    lager:debug("review record"),
    NoopId = whapps_call_command:audio_macro([{'prompt', <<"conf-your_announcment">>}
                                              ,{'play', RecordName}
                                              ,{'prompt', <<"conf-review">>}
                                             ], Call),

    whapps_call_command:collect_digits(1
                                       ,whapps_call_command:default_collect_timeout()
                                       ,whapps_call_command:default_interdigit_timeout()
                                       ,NoopId
                                       ,Call
                                      ).
