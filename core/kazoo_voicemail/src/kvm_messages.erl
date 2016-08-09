%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_messages).


-export([get/2, get_from_vmbox/2, get_from_modb/2
        ,count/2, count_per_folder/2 ,count_by_owner/2
        ,count_by_modb/1, count_by_modb/3

        ,update/3
        ,change_box_id/4
        ,change_folder/4

        ,load_vmbox/2, load_vmbox/3
        ]).

-include("kz_voicemail.hrl").

-define(MODB_LISTING_BY_MAILBOX, <<"mailbox_messages/listing_by_mailbox">>).
-define(MODB_COUNT_VIEW, <<"mailbox_messages/count_per_folder">>).
-define(COUNT_BY_VMBOX, <<"mailbox_messages/count_by_vmbox">>).

%%--------------------------------------------------------------------
%% @public
%% @doc fetch all messages for a voicemail box
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary(), ne_binary()) -> kz_json:objects().
get(AccountId, BoxId) ->
    % first get messages metadata from vmbox for backward compatibility
    case get_from_vmbox(AccountId, BoxId) of
        {'ok', Msgs} -> Msgs ++ get_from_modb(AccountId, BoxId);
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_from_vmbox(ne_binary(), ne_binary()) -> db_ret().
get_from_vmbox(AccountId, BoxId) ->
    case kvm_util:open_accountdb_doc(AccountId, BoxId, kzd_voicemail_box:type()) of
        {'ok', BoxJObj} -> {'ok', kz_json:get_value(?VM_KEY_MESSAGES, BoxJObj, [])};
        {'error', _} = Error ->
            lager:debug("failed to fetch voicemail messages for vmbox ~s(~s)"
            	       ,[BoxId, AccountId]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_from_modb(ne_binary(), ne_binary()) -> kz_json:objects().
get_from_modb(AccountId, DocId) ->
    ViewOpts = [{'key', DocId}
                ,'include_docs'
               ],
    ViewOptsList = get_range_view(AccountId, ViewOpts),

    ModbResults = [kzd_box_message:metadata(kz_json:get_value(<<"doc">>, Msg))
                   || Msg <- modb_get_results(AccountId, ?MODB_LISTING_BY_MAILBOX, ViewOptsList, [])
                      ,Msg =/= []
                  ],
    ModbResults.

%%--------------------------------------------------------------------
%% @public
%% @doc Count non-deleted messages
%% @end
%%--------------------------------------------------------------------
-spec count(ne_binary(), ne_binary()) -> non_neg_integer().
count(AccountId, BoxId) ->
    {New, Saved} = count_per_folder(AccountId, BoxId),
    New + Saved.

count_by_owner(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, OwnerId) ->
    AccountId = kz_util:format_account_id(AccountDb),
    count_by_owner(AccountId, OwnerId);
count_by_owner(AccountId, OwnerId) ->
    ViewOpts = [{'key', [OwnerId, <<"vmbox">>]}],

    case kz_datamgr:get_results(kvm_util:get_db(AccountId), <<"attributes/owned">>, ViewOpts) of
        {'ok', []} ->
            lager:info("voicemail box owner is not found"),
            {0, 0};
        {'ok', [Owned|_]} ->
            VMBoxId = kz_json:get_value(<<"value">>, Owned),
            count_per_folder(AccountId, VMBoxId);
        {'error', _R} ->
            lager:info("unable to lookup vm counts by owner: ~p", [_R]),
            {0, 0}
    end.

-spec count_per_folder(ne_binary(), ne_binary()) -> {non_neg_integer(), non_neg_integer()}.
count_per_folder(AccountId, BoxId) ->
    % first count messages from vmbox for backward compatibility
    case get_from_vmbox(AccountId, BoxId) of
        {'ok', Msgs} ->
            New = kzd_box_message:count_folder(Msgs, [?VM_FOLDER_NEW]),
            Saved = kzd_box_message:count_folder(Msgs, [?VM_FOLDER_SAVED]),
            count_by_modb(AccountId, BoxId, {New, Saved});
        _ -> count_by_modb(AccountId, BoxId, {0, 0})
    end.

-spec count_by_modb(ne_binary()) -> kz_json:objects().
count_by_modb(AccountId) ->
    Opts = ['reduce', 'group'],
    ViewOptsList = get_range_view(AccountId, Opts),
    modb_get_results(AccountId, ?COUNT_BY_VMBOX, ViewOptsList, []).

-spec count_by_modb(ne_binary(), ne_binary(), {non_neg_integer(), non_neg_integer()}) -> {non_neg_integer(), non_neg_integer()}.
count_by_modb(AccountId, BoxId, {ANew, ASaved}=AccountDbCounts) ->
    Opts = ['reduce'
            ,'group'
            ,{'group_level', 2}
            ,{'startkey', [BoxId]}
            ,{'endkey', [BoxId, kz_json:new()]}
           ],
    ViewOptions = get_range_view(AccountId, Opts),

    case modb_get_results(AccountId, ?MODB_COUNT_VIEW, ViewOptions, []) of
        [] ->
            AccountDbCounts;
        Results ->
            {MNew, MSaved} = kzd_box_message:normalize_count(Results),
            {ANew + MNew, ASaved + MSaved}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-record(bulk_res, {succeeded = []  :: ne_binaries()
                   ,failed = [] :: kz_json:objects()
                   ,moved = [] :: kz_json:objects()
                  }).
-type bulk_results() :: #bulk_res{}.

-spec update(ne_binary(), ne_binary(), kz_json:objects()) ->
                                kz_json:object().
update(AccountId, BoxId, Msgs) ->
    update(AccountId, BoxId, Msgs, []).

-spec update(ne_binary(), ne_binary(), ne_binaries() | kz_json:objects(), update_funs()) ->
                                kz_json:object().
update(AccountId, BoxId, Things, Funs) ->
    #bulk_res{succeeded=Succeeded
              ,failed=Failed
              ,moved=Moved
             } = update_fold(AccountId, BoxId, Things, Funs, #bulk_res{}),
    kvm_util:cleanup_moved_msgs(AccountId, BoxId, Moved),
    kz_json:from_list([{<<"succeeded">>, Succeeded}
                       ,{<<"failed">>, Failed}
                      ]).

-spec update_fold(ne_binary(), ne_binary(), ne_binaries() | kz_json:objects(), update_funs(), bulk_results()) ->
                                bulk_results().
update_fold(_, _, [], _, Result) ->
    Result;
update_fold(AccountId, BoxId, [?JSON_WRAPPER(_)=Msg|Msgs], Funs, #bulk_res{failed=Failed}=Blk) ->
    NewFun = [fun(JObj) ->
                  kzd_box_message:set_metadata(Msg, JObj)
              end
              | Funs
             ],
    MsgId = kzd_box_message:media_id(Msg),
    case kvm_message:fetch(AccountId, MsgId) of
        {'ok', JObj} ->
            Result = do_update(AccountId, BoxId, MsgId, JObj, NewFun, Blk),
            update_fold(AccountId, BoxId, Msgs, Funs, Result);
        {'error', R} ->
            Result = Blk#bulk_res{failed=[kz_json:from_list([{MsgId, kz_util:to_binary(R)}]) | Failed]},
            update_fold(AccountId, BoxId, Msgs, Funs, Result)
    end;
update_fold(AccountId, BoxId, [MsgId|MsgIds], Funs, #bulk_res{failed=Failed}=Blk) ->
    case kvm_message:fetch(AccountId, MsgId) of
        {'ok', JObj} ->
            Result = do_update(AccountId, BoxId, MsgId, JObj, Funs, Blk),
            update_fold(AccountId, BoxId, MsgIds, Funs, Result);
        {'error', R} ->
            Result = Blk#bulk_res{failed=[kz_json:from_list([{MsgId, kz_util:to_binary(R)}]) | Failed]},
            update_fold(AccountId, BoxId,  MsgIds, Funs, Result)
    end.

-spec do_update(ne_binary(), ne_binary(), ne_binary(), kz_json:object(), update_funs(), bulk_results()) -> bulk_results().
do_update(AccountId, BoxId, ?MATCH_MODB_PREFIX(Year, Month, _)=Id, JObj, Funs, #bulk_res{succeeded=Succeeded
                                                                                         ,failed=Failed
                                                                                        }=Blk) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    case BoxId =:= kzd_box_message:source_id(JObj)
             andalso kvm_util:handle_update_result(Id, kazoo_modb:save_doc(AccountId, NewJObj, Year, Month))
    of
        {'ok', _} -> Blk#bulk_res{succeeded=[Id | Succeeded]};
        {'error', R} -> Blk#bulk_res{failed=[kz_json:from_list([{Id, kz_util:to_binary(R)}]) | Failed]};
        'false' -> Blk#bulk_res{failed=[kz_json:from_list([{Id, <<"not_found">>}]) | Failed]}
    end;
do_update(AccountId, BoxId, OldId, JObj, Funs, #bulk_res{succeeded=Succeeded
                                                         ,failed=Failed
                                                         ,moved=Moved
                                                        }=Blk) ->
    case BoxId =:= kzd_box_message:source_id(JObj)
             andalso kvm_util:handle_update_result(OldId, kvm_message:move_to_modb(AccountId, JObj, Funs, 'false'))
    of
        {'ok', NJObj} ->
            NewId = kz_doc:id(NJObj),
            Blk#bulk_res{succeeded=[NewId | Succeeded]
                         ,moved=[OldId | Moved]
                        };
        {'error', R} ->
            Blk#bulk_res{failed=[kz_json:from_list([{OldId, kz_util:to_binary(R)}]) | Failed]};
        'false' -> Blk#bulk_res{failed=[kz_json:from_list([{OldId, <<"not_found">>}]) | Failed]}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
 change_folder(Folder, MsgIds, AccountId, BoxId) ->
    Fun = [fun(JObj) ->
               kvm_util:apply_folder(Folder, JObj)
           end
          ],
    {'ok', update(AccountId, BoxId, MsgIds, Fun)}.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch vmbox doc from db. It will include metadata if IncludeMessages
%% set to true, otherwise would delete it from doc.
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox(ne_binary(), ne_binary()) -> db_ret().
load_vmbox(AccountId, BoxId) ->
    load_vmbox(AccountId, BoxId, 'true').

-spec load_vmbox(ne_binary(), ne_binary(), boolean()) -> db_ret().
load_vmbox(AccountId, BoxId, IncludeMessages) ->
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:open_cache_doc(Db, BoxId) of
        {'ok', J} ->
            case kvm_util:check_doc_type(J, kzd_voicemail_box:type(), kz_doc:type(J)) of
                {'ok', JObj} ->
                    maybe_include_messages(AccountId, BoxId, JObj, IncludeMessages);
                {'error', _}=E -> E
            end;
        {'error', _R}=E ->
            lager:debug("failed to open vmbox ~s: ~p", [BoxId, _R]),
            E
    end.

-spec maybe_include_messages(ne_binary(), ne_binary(), kz_json:object(), boolean()) -> {'ok', kz_json:object()}.
maybe_include_messages(AccountId, BoxId, JObj, 'true') ->
    BoxMessages = kz_json:get_value(?VM_KEY_MESSAGES, JObj, []),
    MODBMessages = get_from_modb(AccountId, BoxId),
    {'ok', kz_json:set_value(?VM_KEY_MESSAGES, BoxMessages ++ MODBMessages, JObj)};
maybe_include_messages(_AccountId, _BoxId, JObj, _) ->
    {'ok', kz_json:delete_key(?VM_KEY_MESSAGES, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc Move messages to another vmbox
%% @end
%%--------------------------------------------------------------------
-spec change_box_id(ne_binary(), ne_binary() | ne_binaries(), ne_binary(), ne_binary()) -> kz_json:objects().
change_box_id(AccountId, MsgIds, OldBoxId, NewBoxId) when is_list(MsgIds) ->
    AccountDb = kvm_util:get_db(AccountId),
    {'ok', NBoxJ} = kz_datamgr:open_cache_doc(AccountDb, NewBoxId),

    Funs = [fun(JObj) -> kzd_box_message:set_source_id(NewBoxId, JObj) end
            ,fun(JObj) -> kvm_util:apply_folder(?VM_FOLDER_NEW, JObj) end
            ,fun(JObj) -> change_message_name(NBoxJ, JObj) end
            ,fun(JObj) -> change_to_sip_field(AccountId, NBoxJ, JObj) end
            ,fun(JObj) -> kzd_box_message:add_message_history(OldBoxId, JObj) end
           ],
    update(AccountId, OldBoxId, MsgIds, Funs);
change_box_id(AccountId, MsgId, OldBoxId, NewBoxId) ->
    change_box_id(AccountId, [MsgId], OldBoxId, NewBoxId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec modb_get_results(ne_binary(), ne_binary(), kz_proplist(), kz_json:objects()) -> kz_json:objects().
modb_get_results(_AccountId, _View, [], ViewResults) ->
    ViewResults;
modb_get_results(AccountId, View, [ViewOpts|ViewOptsList], Acc) ->
    case kazoo_modb:get_results(AccountId, View, ViewOpts) of
        {'ok', []} -> modb_get_results(AccountId, View, ViewOptsList, Acc);
        {'ok', Msgs} -> modb_get_results(AccountId, View, ViewOptsList, Msgs ++ Acc);
        {'error', _}=_E ->
            lager:debug("error when fetching voicemail message for ~s from modb ~s"
                        ,[props:get_value('key', ViewOpts), props:get_value('modb', ViewOpts)]
                       ),
            modb_get_results(AccountId, View, ViewOptsList, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_range_view(ne_binary(), kz_proplist()) -> kz_proplists().
get_range_view(AccountId, ViewOpts) ->
    To = kz_util:current_tstamp(),
    From = To - ?RETENTION_DAYS(?RETENTION_DURATION),

    Fun = fun(MODB) ->
              {AccountId, Year, Month} = kazoo_modb_util:split_account_mod(MODB),
              [{'year', Year}
              ,{'month', Month}
              ,{'modb', MODB}
               | ViewOpts
              ]
          end,
    [Fun(Db) || Db <- kazoo_modb:get_range(AccountId, From, To)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_message_name(kz_json:object(), kz_json:object()) -> kz_json:object().
change_message_name(NBoxJ, MsgJObj) ->
    BoxNum = kzd_voicemail_box:mailbox_number(NBoxJ),
    Timezone = kzd_voicemail_box:timezone(NBoxJ),
    UtcSeconds = kzd_box_message:utc_seconds(MsgJObj),

    NewName = kzd_box_message:create_message_name(BoxNum, Timezone, UtcSeconds),
    kzd_box_message:set_message_name(NewName, MsgJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_to_sip_field(ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
change_to_sip_field(AccountId, NBoxJ, MsgJObj) ->
    Realm = kz_util:get_account_realm(AccountId),
    BoxNum = kzd_voicemail_box:mailbox_number(NBoxJ),

    Metadata = kzd_box_message:metadata(MsgJObj),
    To = <<BoxNum/binary, "@", Realm/binary>>,
    kzd_box_message:set_metadata(kzd_box_message:set_to_sip(To, Metadata), MsgJObj).
