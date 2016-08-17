%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_messages).


-export([get/1, get_from_vmbox/1, get_from_modb/1
        ,get/2, get_from_vmbox/2, get_from_modb/2

        ,count/1, count_per_folder/1
        ,count/2, count_per_folder/2
        ,count_by_owner/2
        ,count_from_vmbox/1, count_from_vmbox/2
        ,count_from_modb/1, count_from_modb/2, count_from_modb/3

        ,update/3
        ,move_to_vmbox/4
        ,copy_to_vmboxes/4
        ,change_folder/4

        ,load_vmbox/2, load_vmbox/3
        ]).

-include("kz_voicemail.hrl").

-define(MSG_LISTING_BY_MAILBOX, <<"mailbox_messages/listing_by_mailbox">>).
-define(MSG_COUNT_VIEW, <<"mailbox_messages/count_per_folder">>).

-type bulk_results() :: #bulk_res{}.
-type count_result() :: {non_neg_integer(), non_neg_integer()}.

-export_type([bulk_results/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc fetch all messages for a voicemail box or on an account
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary(), ne_binary() | kz_json:object()) -> kz_json:objects().
get(AccountId) ->
    %% first get messages metadata from vmbox for backward compatibility
    get_from_vmbox(AccountId) ++ get_from_modb(AccountId).

get(AccountId, Box) ->
    %% first get messages metadata from vmbox for backward compatibility
    get_from_vmbox(AccountId, Box) ++ get_from_modb(AccountId, Box).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_from_vmbox(ne_binary()) -> kz_json:objects().
-spec get_from_vmbox(ne_binary(), ne_binary() | kz_json:object()) ->
                            kz_json:objects().
get_from_vmbox(AccountId) ->
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?MSG_LISTING_BY_MAILBOX, []) of
        {'ok', []} -> [];
        {'ok', Result} ->
            [kz_json:from_list([{kz_json:get_value([<<"key">>, 1], Msg)
                                ,kz_json:get_value(<<"value">>, Msg)
                                }
                               ])
             || Msg <- Result
            ];
        {'error', _Reason} ->
            lager:debug("failed to fetch voicemail messages for account ~s: ~p"
                       ,[AccountId, _Reason]),
            []
    end.

get_from_vmbox(AccountId, ?NE_BINARY = BoxId) ->
    ViewOpts = [{'startkey', [BoxId]}
               ,{'endkey', [BoxId, kz_json:new()]}
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?MSG_LISTING_BY_MAILBOX, ViewOpts) of
        {'ok', []} -> [];
        {'ok', Result} ->
            [kz_json:get_value(<<"value">>, Msg) || Msg <- Result];
        {'error', _Reason} ->
            lager:debug("failed to fetch voicemail messages for vmbox ~s(~s): ~p"
                       ,[BoxId, AccountId, _Reason]),
            []
    end;
get_from_vmbox(_AccountId, BoxJObj) ->
    {'ok', kz_json:get_value(?VM_KEY_MESSAGES, BoxJObj, [])}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_from_modb(ne_binary()) -> kz_json:objects().
-spec get_from_modb(ne_binary(), ne_binary() | kz_json:object()) ->
                           kz_json:objects().
get_from_modb(AccountId) ->
    ViewOpts = get_range_view(AccountId, []),
    ModbResults = [kz_json:get_value(<<"value">>, Msg)
                   || Msg <- modb_get_results(AccountId, ?MSG_LISTING_BY_MAILBOX, ViewOpts, [])
                          ,Msg =/= []
                  ],
    ModbResults.

get_from_modb(AccountId, ?NE_BINARY = BoxId) ->
    ViewOpts = [{'startkey', [BoxId]}
               ,{'endkey', [BoxId, kz_json:new()]}
               ],
    ViewOptsList = get_range_view(AccountId, ViewOpts),

    ModbResults = [kz_json:get_value(<<"value">>, Msg)
                   || Msg <- modb_get_results(AccountId, ?MSG_LISTING_BY_MAILBOX, ViewOptsList, [])
                          ,Msg =/= []
                  ],
    ModbResults;
get_from_modb(AccountId, Box) ->
    get_from_modb(AccountId, kz_doc:id(Box)).

%%--------------------------------------------------------------------
%% @public
%% @doc Count non-deleted messages
%% @end
%%--------------------------------------------------------------------
-spec count(ne_binary()) -> kz_proplist().
-spec count(ne_binary(), ne_binary()) -> non_neg_integer().
count(AccountId) ->
    count_per_folder(AccountId).

count(AccountId, BoxId) ->
    {New, Saved} = count_per_folder(AccountId, BoxId),
    New + Saved.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec count_by_owner(ne_binary(), ne_binary()) -> count_result().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec count_per_folder(ne_binary()) -> kz_proplist().
-spec count_per_folder(ne_binary(), ne_binary()) -> count_result().
count_per_folder(AccountId) ->
    %% first count messages from vmbox for backward compatibility
    VMCount = count_from_vmbox(AccountId),
    MODBCount = count_from_modb(AccountId),
    [{<<"accountdb_counts">>, VMCount}
    ,{<<"modb_counts">>, MODBCount}
    ].

count_per_folder(AccountId, BoxId) ->
    %% first count messages from vmbox for backward compatibility
    {New, Saved} = count_from_vmbox(AccountId, BoxId),
    count_from_modb(AccountId, BoxId, {New, Saved}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec count_from_vmbox(ne_binary()) -> kz_json:object().
-spec count_from_vmbox(ne_binary(), ne_binary()) -> count_result().
count_from_vmbox(AccountId) ->
    ViewOpts = ['reduce'
               ,'group'
               ,{'group_level', 2}
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?MSG_COUNT_VIEW, ViewOpts) of
        {'ok', Msgs} -> normalize_count(Msgs);
        _ ->
            lager:debug("failed to count accountdb messages for account ~s"
                       ,[AccountId]),
            kz_json:new()
    end.

count_from_vmbox(AccountId, BoxId) ->
    ViewOpts = ['reduce'
               ,'group'
               ,{'group_level', 2}
               ,{'startkey', [BoxId]}
               ,{'endkey', [BoxId, kz_json:new()]}
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?MSG_COUNT_VIEW, ViewOpts) of
        {'ok', ViewRes} ->
            Results = normalize_count(ViewRes),
            normalize_count_non_deleted(BoxId, Results);
        {'error', _R} ->
            lager:debug("failed to count accountdb messages for vmbox ~s(~s): ~p"
                       ,[BoxId, AccountId, _R]),
            {0, 0}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec count_from_modb(ne_binary()) -> kz_json:object().
-spec count_from_modb(ne_binary(), ne_binary()) -> count_result().
-spec count_from_modb(ne_binary(), ne_binary(), count_result()) -> count_result().
count_from_modb(AccountId) ->
    Opts = ['reduce'
           ,'group'
           ,{'group_level', 2}
           ],
    ViewOptsList = get_range_view(AccountId, Opts),
    Result = modb_get_results(AccountId, ?MSG_COUNT_VIEW, ViewOptsList, []),
    normalize_count(Result).

count_from_modb(AccountId, BoxId) ->
    count_from_modb(AccountId, BoxId, {0, 0}).

count_from_modb(AccountId, BoxId, {ANew, ASaved}=AccountDbCounts) ->
    Opts = ['reduce'
           ,'group'
           ,{'group_level', 2}
           ,{'startkey', [BoxId]}
           ,{'endkey', [BoxId, kz_json:new()]}
           ],
    ViewOptions = get_range_view(AccountId, Opts),

    case modb_get_results(AccountId, ?MSG_COUNT_VIEW, ViewOptions, []) of
        [] ->
            AccountDbCounts;
        ViewRes ->
            Results = normalize_count(ViewRes),
            {MNew, MSaved} = normalize_count_non_deleted(BoxId, Results),
            {ANew + MNew, ASaved + MSaved}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), ne_binary(), ne_binaries() | kz_json:objects()) ->
                    kz_json:object().
-spec update(ne_binary(), ne_binary(), ne_binaries() | kz_json:objects(), update_funs()) ->
                    kz_json:object().
update(AccountId, BoxId, Msgs) ->
    update(AccountId, BoxId, Msgs, []).

update(AccountId, BoxId, Things, Funs) ->
    #bulk_res{succeeded = Succeeded
             ,failed = Failed
             ,moved = Moved
             } = update_fold(AccountId, BoxId, Things, Funs, #bulk_res{}),
    kvm_util:cleanup_moved_msgs(AccountId, BoxId, Moved),
    kz_json:from_list([{<<"succeeded">>, Succeeded}
                      ,{<<"failed">>, Failed}
                      ]).

-spec update_fold(ne_binary(), ne_binary(), ne_binaries() | kz_json:objects(), update_funs(), bulk_results()) ->
                         bulk_results().
update_fold(_AccountId, _BoxId, [], _Funs, Result) ->
    Result;
update_fold(AccountId, BoxId, [Msg | Msgs], Funs, #bulk_res{failed = Failed} = Blk) ->
    {MsgId, NewFuns} = maybe_add_update_fun(Msg, Funs),
    case kvm_message:fetch(AccountId, MsgId, BoxId) of
        {'ok', JObj} ->
            Result = do_update(AccountId, MsgId, JObj, NewFuns, Blk),
            update_fold(AccountId, BoxId, Msgs, Funs, Result);
        {'error', R} ->
            Result = Blk#bulk_res{failed = [kz_json:from_list([{MsgId, kz_util:to_binary(R)}]) | Failed]},
            update_fold(AccountId, BoxId, Msgs, Funs, Result)
    end.

-spec do_update(ne_binary(), ne_binary(), kz_json:object(), update_funs(), bulk_results()) -> bulk_results().
do_update(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)=Id, JObj, Funs, Blk) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    kvm_util:bulk_update_result(Id, kazoo_modb:save_doc(AccountId, NewJObj, Year, Month), Blk);
do_update(AccountId, OldId, JObj, Funs, Blk) ->
    kvm_util:bulk_update_result(OldId, kvm_message:move_to_modb(AccountId, JObj, Funs, 'false'), Blk).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
change_folder(Folder, MsgIds, AccountId, BoxId) ->
    Fun = [fun(JObj) ->
                   kzd_box_message:apply_folder(Folder, JObj)
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
-spec load_vmbox(ne_binary(), ne_binary(), boolean()) -> db_ret().
load_vmbox(AccountId, BoxId) ->
    load_vmbox(AccountId, BoxId, 'true').

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

%%--------------------------------------------------------------------
%% @public
%% @doc Move messages to another vmbox
%% @end
%%--------------------------------------------------------------------
-spec move_to_vmbox(ne_binary(), ne_binary() | ne_binaries(), ne_binary(), ne_binary()) ->
                           kz_json:object().
move_to_vmbox(AccountId, ?NE_BINARY = MsgId, OldBoxId, NewBoxId) ->
    move_to_vmbox(AccountId, [MsgId], OldBoxId, NewBoxId);
move_to_vmbox(AccountId, MsgIds, OldBoxId, NewBoxId) ->
    AccountDb = kvm_util:get_db(AccountId),
    {'ok', NBoxJ} = kz_datamgr:open_cache_doc(AccountDb, NewBoxId),
    Funs = ?CHANGE_VMBOX_FUNS(AccountId, NewBoxId, NBoxJ, OldBoxId),
    update(AccountId, OldBoxId, MsgIds, Funs).

%%--------------------------------------------------------------------
%% @public
%% @doc copy messages to other vmbox(es)
%% @end
%%--------------------------------------------------------------------
-spec copy_to_vmboxes(ne_binary(), ne_binaries(), ne_binary(), ne_binary() | ne_binaries()) ->
                             kz_json:object().
copy_to_vmboxes(AccountId, Ids, OldBoxId, ?NE_BINARY = NewBoxId) ->
    copy_to_vmboxes(AccountId, Ids, OldBoxId, [NewBoxId]);
copy_to_vmboxes(AccountId, Ids, OldBoxId, NewBoxIds) ->
    copy_to_vmboxes_fold(AccountId, Ids, OldBoxId, NewBoxIds, kz_json:new()).

copy_to_vmboxes_fold(_, [], _, _, Copied) -> Copied;
copy_to_vmboxes_fold(AccountId, [Id | Ids], OldBoxId, NewBoxIds, Copied) ->
    CopyRes = kvm_message:copy_to_vmboxes(AccountId, Id, OldBoxId, NewBoxIds),

    NewCopied = kvm_util:bulk_update_result(CopyRes, Copied),
    copy_to_vmboxes_fold(AccountId, Ids, OldBoxId, NewBoxIds, NewCopied).

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
            lager:debug("failed to count voicemail messages for ~s from modb ~s"
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
%% @doc Normalize count views results
%% @end
%%--------------------------------------------------------------------
-spec normalize_count(kz_json:objects()) -> kz_json:object().
normalize_count([]) -> kz_json:new();
normalize_count(ViewRes) ->
    Fun = fun(M, Acc) ->
                  VMBox = kz_json:get_value([<<"key">>, 1], M),
                  Folder = kz_json:get_value([<<"key">>, 2], M),
                  Value = kz_json:get_integer_value(<<"value">>, M),
                  kz_json:insert_value([VMBox, Folder], Value, Acc)
          end,
    lists:foldl(Fun, kz_json:new(), ViewRes).

-spec normalize_count_non_deleted(ne_binary(), kz_json:object()) -> count_result().
normalize_count_non_deleted(BoxId, ViewRes) ->
    {kz_json:get_integer_value([BoxId, ?VM_FOLDER_NEW], ViewRes, 0)
    ,kz_json:get_integer_value([BoxId, ?VM_FOLDER_SAVED], ViewRes, 0)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_update_fun(ne_binary() | kz_json:object(), update_funs()) ->
                                  {ne_binary(), update_funs()}.
maybe_add_update_fun(?NE_BINARY = Id, Funs) -> {Id, Funs};
maybe_add_update_fun(Msg, Funs) ->
    MsgId = kzd_box_message:media_id(Msg),
    NewFuns = [fun(JObj) ->
                       kzd_box_message:set_metadata(Msg, JObj)
               end
               | Funs
              ],
    {MsgId, NewFuns}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_include_messages(ne_binary(), ne_binary(), kz_json:object(), boolean()) -> {'ok', kz_json:object()}.
maybe_include_messages(AccountId, BoxId, JObj, 'true') ->
    BoxMessages = kz_json:get_value(?VM_KEY_MESSAGES, JObj, []),
    MODBMessages = get_from_modb(AccountId, BoxId),
    {'ok', kz_json:set_value(?VM_KEY_MESSAGES, BoxMessages ++ MODBMessages, JObj)};
maybe_include_messages(_AccountId, _BoxId, JObj, _) ->
    {'ok', kz_json:delete_key(?VM_KEY_MESSAGES, JObj)}.
