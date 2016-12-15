%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Mailbox maintenance
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kazoo_voicemail_maintenance).

-export([migrate/0
        ,migrate/1
        ,migrate/2
        ]).
-export([recover_messages_all/0
        ,recover_messages_month/2
        ,recover_messages_account/1
        ,recover_messages_account/3
        ]).
-export([renotify/2]).

-include("kz_voicemail.hrl").

-define(LOG(Format, Args),
        lager:debug(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-define(VIEW_MISSING_METADATA, <<"mailbox_messages/missing_metadata">>).

%%--------------------------------------------------------------------
%% @public
%% @doc Migrate all messages in vmbox into the new modb format
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> 'ok'.
migrate() ->
    _ = process_flag('trap_exit', 'true'),
    {'ok', Pid} = kvm_migrate_crawler:start(self()),
    link(Pid),
    receive
        'done' -> 'ok';
        {'EXIT', Pid, 'normal'} -> 'ok';
        {'EXIT', Pid, _Reason} ->
            io:format("~n********** migration process died with reason:~n~p~n", [_Reason])
    end.

-spec migrate(ne_binary()) -> 'ok'.
-spec migrate(ne_binary(), ne_binary() | ne_binaries() | kz_json:object()) -> 'ok'.
migrate(?NE_BINARY = AccountId) ->
    kvm_migrate_account:manual_migrate(AccountId);
migrate(AccountJObj) ->
    migrate(kz_doc:id(AccountJObj)).

migrate(AccountId, ?NE_BINARY = BoxId) ->
    migrate(AccountId, [BoxId]);
migrate(AccountId, BoxIds) when is_list(BoxIds) ->
    kvm_migrate_account:manual_migrate(AccountId, BoxIds);
migrate(AccountId, Box) ->
    migrate(AccountId, kz_doc:id(Box)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recover_messages_all() -> 'ok'.
recover_messages_all() ->
    MODbs = kapps_util:get_all_account_mods(),
    recover_messages(MODbs, length(MODbs)).

-spec recover_messages_month(ne_binary(), ne_binary()) -> 'ok'.
recover_messages_month(Year, Month) ->
    MODbs = [MODb || MODb <- kapps_util:get_all_account_mods()
                         ,modb_filter(MODb, Year, Month)
            ],
    recover_messages(MODbs, length(MODbs)).

-spec recover_messages_account(ne_binary()) -> 'ok'.
recover_messages_account(Account) ->
    MODbs = kapps_util:get_account_mods(Account),
    recover_messages(MODbs, length(MODbs)).

-spec recover_messages_account(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
recover_messages_account(Account, Year, Month) ->
    MODbs = [MODb || MODb <- kapps_util:get_account_mods(Account)
                         ,modb_filter(MODb, Year, Month)
            ],
    recover_messages(MODbs, length(MODbs)).

-spec modb_filter(ne_binary(), ne_binary(), ne_binary()) -> boolean().
modb_filter(?MATCH_MODB_SUFFIX_ENCODED(_A, _B, _C, Year, Month), Year, Month) -> 'true';
modb_filter(_, _, _) -> 'false'.

-spec recover_messages(ne_binaries(), non_neg_integer()) -> 'ok'.
recover_messages([], _) -> 'ok';
recover_messages([MODb|MODbs], Total) ->
    ?LOG("(~p/~p) attempting to recover voicemail messages from ~s", [length(MODbs)+1, Total, MODb]),
    _ = recover_missing_metadata(MODb),
    timer:sleep(50),
    recover_messages(MODbs, Total).

-spec recover_missing_metadata(ne_binary()) -> 'ok'.
recover_missing_metadata(MODb) ->
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(MODb, ?VIEW_MISSING_METADATA, ViewOptions) of
        {'ok', []} -> ?LOG("  no messages found with missing metadata", []);
        {'ok', Messages} ->
            JObjs = [kz_json:get_value(<<"doc">>, Message) || Message <- Messages],
            recover_missing_metadata(MODb, JObjs);
        {'error', 'not_found'} ->
            ?LOG("  adding view ~s", [?VIEW_MISSING_METADATA]),
            kapps_maintenance:refresh(MODb),
            recover_missing_metadata(MODb);
        {'error', 'timeout'} ->
            timer:sleep(1000),
            recover_missing_metadata(MODb);
        {'error', _R} ->
            ?LOG("  unable to query missing_metadata view: ~p", [_R])
    end.

-spec recover_missing_metadata(ne_binary(), kz_json:objects()) -> 'ok'.
recover_missing_metadata(MODb, JObjs) ->
    Messages = [maybe_rebuild_message_metadata(JObj) || JObj <- JObjs],
    _ = kz_datamgr:save_docs(MODb, Messages),
    'ok'.

-spec maybe_rebuild_message_metadata(kz_json:object()) -> kz_json:object().
maybe_rebuild_message_metadata(JObj) ->
    case kz_json:get_keys(<<"_attachments">>, JObj) of
        [AttachmentName|_] -> rebuild_message_metadata(JObj, AttachmentName);
        _Else ->
            ?LOG("  ~s missing attachment, skipping", [kz_json:get_value(<<"_id">>, JObj)]),
            JObj
    end.

-spec rebuild_message_metadata(kz_json:object(), ne_binary()) -> kz_json:object().
rebuild_message_metadata(JObj, AttachmentName) ->
    MediaId = kz_json:get_value(<<"_id">>, JObj),
    ?LOG("  rebuilding metadata for ~s", [MediaId]),
    Length = kz_json:get_value([<<"_attachments">>, AttachmentName, <<"length">>], JObj, 0),
    CIDNumber = kz_util:anonymous_caller_id_number(),
    CIDName = <<"Recovered Voicemail Message">>,
    Timestamp = kz_json:get_value(<<"pvt_created">>, JObj, kz_util:current_tstamp()),
    Routines = [{fun kapps_call:set_to/2, <<CIDNumber/binary, "@nodomain">>}
               ,{fun kapps_call:set_from/2, <<CIDNumber/binary, "@nodomain">>}
               ,{fun kapps_call:set_call_id/2, kz_util:rand_hex_binary(12)}
               ,{fun kapps_call:set_caller_id_number/2, CIDNumber}
               ,{fun kapps_call:set_caller_id_name/2, CIDName}
               ],
    Call = kapps_call:exec(Routines, kapps_call:new()),
    Metadata = kzd_box_message:build_metadata_object(Length, Call, MediaId, Timestamp),
    kzd_box_message:set_metadata(Metadata, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec renotify(text(), text()) -> 'ok'.
renotify(Account, MessageId) ->
    MODb = get_modb(Account, MessageId),
    AccountId = get_account_id(Account),
    case kz_datamgr:open_doc(MODb, MessageId) of
        {'error', _R} -> ?LOG("unable to find message ~s in ~s: ~p", [MODb, MessageId, _R]);
        {'ok', JObj} ->
            Call = rebuild_kapps_call(JObj, AccountId),
            BoxId = kzd_box_message:source_id(JObj),
            Metadata = kzd_box_message:metadata(JObj),
            Length = kz_json:get_value(<<"length">>, Metadata, 0),
            Props = [{<<"Transcribe-Voicemail">>, 'false'}],
            log_renotify_result(
              MessageId
                               ,BoxId
                               ,kvm_util:publish_saved_notify(MessageId, BoxId, Call, Length, Props)
             )
    end.

-spec log_renotify_result(ne_binary(), ne_binary(), any()) -> 'ok'.
log_renotify_result(MessageId, BoxId, {'ok', JObj}) ->
    ?LOG("re-notify sent message ~s from mailbox ~s: ~s"
        ,[MessageId, BoxId, kz_json:encode(JObj)]
        );
log_renotify_result(MessageId, BoxId, {'error', JObj}) ->
    ?LOG("re-notify failed to send message ~s from mailbox ~s: ~s"
        ,[MessageId, BoxId, kz_json:encode(JObj)]
        );
log_renotify_result(MessageId, BoxId, {'timeout', JObjs}) ->
    ?LOG("re-notify timed out sending message ~s from mailbox ~s: ~s"
        ,[MessageId, BoxId, kz_json:encode(JObjs)]
        );
log_renotify_result(MessageId, BoxId, Result) ->
    ?LOG("unexpected error in re-notify sending message ~s from mailbox ~s: ~p"
        ,[MessageId, BoxId, Result]
        ).

-spec get_modb(ne_binary(), ne_binary()) -> ne_binary().
get_modb(?MATCH_MODB_SUFFIX_ENCODED(_A, _B, _C, _Y, _M) = MODb, _) -> MODb;
get_modb(?MATCH_MODB_SUFFIX_encoded(_A, _B, _C, _Y, _M) = MODb, _) -> MODb;
get_modb(?MATCH_MODB_SUFFIX_RAW(_A, _B, _C, _Y, _M) = MODb, _) ->
    kz_util:format_account_modb(MODb, 'encoded');
get_modb(?MATCH_MODB_SUFFIX_UNENCODED(_A, _B, _C, _Y, _M) = MODb, _) ->
    kz_util:format_account_modb(MODb, 'encoded');
get_modb(Account, ?MATCH_MODB_PREFIX(Year, Month, _)) ->
    kz_util:format_account_mod_id(Account, Year, Month).

-spec get_account_id(ne_binary()) -> ne_binary().
get_account_id(?MATCH_ACCOUNT_RAW(AccountId)) -> AccountId;
get_account_id(?MATCH_ACCOUNT_UNENCODED(A, B, Rest)) -> ?MATCH_ACCOUNT_RAW(A, B, Rest);
get_account_id(?MATCH_ACCOUNT_ENCODED(A, B, Rest)) -> ?MATCH_ACCOUNT_RAW(A, B, Rest);
get_account_id(?MATCH_MODB_SUFFIX_RAW(AccountId, _, _)) -> AccountId;
get_account_id(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, _, _)) -> ?MATCH_ACCOUNT_RAW(A, B, Rest);
get_account_id(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, _, _)) -> ?MATCH_ACCOUNT_RAW(A, B, Rest).

-spec rebuild_kapps_call(kz_json:object(), ne_binary()) -> kapps_call:call().
rebuild_kapps_call(JObj, AccountId) ->
    Metadata = kzd_box_message:metadata(JObj),
    To = kz_json:get_value(<<"to">>, Metadata, <<"unknown@nodomain">>),
    CCVs = [{<<"Account-ID">>, AccountId}],
    Props = [{<<"Call-ID">>, kz_json:get_value(<<"call_id">>, Metadata, kz_util:rand_hex_binary(12))}
            ,{<<"From">>, kz_json:get_value(<<"from">>, Metadata, <<"unknown@nodomain">>)}
            ,{<<"Caller-ID-Name">>, kz_json:get_value(<<"caller_id_name">>, Metadata, kz_util:anonymous_caller_id_name())}
            ,{<<"Caller-ID-Number">>, kz_json:get_value(<<"caller_id_number">>, Metadata, kz_util:anonymous_caller_id_number())}
            ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
            ,{<<"Custom-SIP-Headers">>, kz_json:new()}
            ,{<<"Request">>, To}
            ,{<<"To">>, To}
            ],
    kapps_call:from_route_req(kz_json:from_list(Props), kapps_call:new()).
