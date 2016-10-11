%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_messages).


-export([get/1, get/2
        ,fetch/2, fetch/3
        ]).

-export([count/1, count_per_folder/1, count_per_folder/2
         %% functions below are returning counts in JObj format
        ,count/2, count_by_owner/2, count_none_deleted/2
        ]).

-export([update/3, update/4, change_folder/4
        ,move_to_vmbox/4
        ,copy_to_vmboxes/4
        ]).

-include("kz_voicemail.hrl").

-define(MSG_LISTING_BY_MAILBOX, <<"mailbox_messages/listing_by_mailbox">>).
-define(MSG_COUNT_VIEW, <<"mailbox_messages/count_per_folder">>).

-define(BOX_ID_KEY_INDEX, 1).
-define(FOLDER_KEY_INDEX, 2).

-type norm_fun() :: 'undefined' |
                    fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

%%--------------------------------------------------------------------
%% @public
%% @doc fetch all messages for a voicemail box or on an account
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> kz_json:objects().
-spec get(ne_binary(), message() | kz_proplist()) -> kz_json:objects().
get(AccountId) ->
    get(AccountId, []).

get(AccountId, ?NE_BINARY = BoxId) ->
    get(AccountId, [{'startkey', [BoxId]}
                   ,{'endkey', [BoxId, kz_json:new()]}
                   ]);
get(AccountId, ViewOpts) when is_list(ViewOpts) ->
    NormFun = fun normalize_view_results/2,
    get_view_results(AccountId, ?MSG_LISTING_BY_MAILBOX, ViewOpts, NormFun);
get(AccountId, Box) ->
    get(AccountId, kz_doc:id(Box)).

%%--------------------------------------------------------------------
%% @public
%% @doc Sum of non-deleted messages
%% @end
%%--------------------------------------------------------------------
-spec count(ne_binary()) -> kz_json:object().
-spec count(ne_binary(), ne_binary()) -> non_neg_integer().
-spec count_none_deleted(ne_binary(), ne_binary()) -> count_result().
%% Note: returns counts per folders in JObj
count(AccountId) ->
    count_per_folder(AccountId).

%% Note: returns total non-deleted count
count(AccountId, BoxId) ->
    {New, Saved} = count_none_deleted(AccountId, BoxId),
    New + Saved.

%% Note: returns counts in {new, saved} format
count_none_deleted(AccountId, BoxId) ->
    FoldersCounts = count_per_folder(AccountId, BoxId),
    normalize_count_none_deleted(BoxId, FoldersCounts).

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
            FoldersCounts = count_per_folder(AccountId, VMBoxId),
            normalize_count_none_deleted(VMBoxId, FoldersCounts);
        {'error', _R} ->
            lager:info("unable to lookup vm counts by owner: ~p", [_R]),
            {0, 0}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec count_per_folder(ne_binary()) -> kz_json:object().
-spec count_per_folder(ne_binary(), ne_binary() | kz_proplist()) -> kz_json:object().
count_per_folder(AccountId) ->
    count_per_folder(AccountId, []).

count_per_folder(AccountId, ?NE_BINARY = BoxId) ->
    ViewOpts = [{'startkey', [BoxId]}
               ,{'endkey', [BoxId, kz_json:new()]}
               ],
    count_per_folder(AccountId, ViewOpts);
count_per_folder(AccountId, ViewOpts0) ->
    ViewOpts = ['reduce'
               ,'group'
               ,{'group_level', 2}
                | ViewOpts0
               ],
    case get_view_results(AccountId, ?MSG_COUNT_VIEW, ViewOpts, 'undefined') of
        [] -> kz_json:new();
        Results -> normalize_count(Results)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), ne_binary(), messages()) -> kz_json:object().
-spec update(ne_binary(), ne_binary(), messages(), update_funs()) -> kz_json:object().
update(AccountId, BoxId, Msgs) ->
    update(AccountId, BoxId, Msgs, []).

update(AccountId, BoxId, [?NE_BINARY = _Msg | _] = MsgIds, Funs) ->
    JObjs = fetch(AccountId, MsgIds, BoxId),
    SucceededJObjs = kz_json:get_value(<<"succeeded">>, JObjs, []),
    FailedJObjs = kz_json:get_value(<<"failed">>, JObjs, []),
    Results = do_update(AccountId, SucceededJObjs, Funs, FailedJObjs),
    kz_json:from_list(dict:to_list(Results));
update(AccountId, _BoxId, JObjs, Funs) ->
    Results = do_update(AccountId, JObjs, Funs, []),
    kz_json:from_list(dict:to_list(Results)).

-spec do_update(ne_binary(), kz_json:objects(), update_funs(), kz_json:objects()) -> dict:dict().
do_update(AccountId, SucceededJObjs, Funs, FailedJObjs) ->
    ToUpdateDict = apply_fun_and_map_msgs_to_modb(AccountId, SucceededJObjs, Funs, dict:new()),
    ResultsDict = dict:from_list([{<<"failed">>, FailedJObjs}]),
    dict:fold(fun update_fun/3, ResultsDict, ToUpdateDict).

-spec update_fun(ne_binary(), kz_json:objects(), dict:dict()) -> dict:dict().
update_fun(Db, JObj, ResDict) ->
    case kz_datamgr:save_docs(Db, JObj) of
        {'ok', Saved} ->
            normalize_bulk_results(<<"update">>, 'undefined', 'undefined', Saved, ResDict);
        {'error', R} ->
            lager:warning("failed to bulk update voicemail messages for db ~s: ~p"
                         ,[Db, R]),
            Failed = kz_json:from_list([{kz_doc:id(D), kz_util:to_binary(R)}
                                        || D <- JObj
                                       ]),
            dict:append(<<"failed">>, Failed, ResDict)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message docs for a voicemail box
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), ne_binaries()) -> kz_json:object().
-spec fetch(ne_binary(), ne_binaries(), api_ne_binary()) -> kz_json:object().
fetch(AccountId, MsgIds) ->
    fetch(AccountId, MsgIds, 'undefined').

fetch(AccountId, MsgIds, BoxId) ->
    DbsRange = kvm_util:create_range_dbs(AccountId, MsgIds),
    RetenTimestamp = kz_util:current_tstamp() - kvm_util:retention_seconds(),
    Fun = fun(Db, Ids, ResDict) ->
                  fetch_fun(Db, BoxId, Ids, ResDict, RetenTimestamp)
          end,
    kz_json:from_list(
      dict:to_list(
        dict:fold(Fun, dict:new(), DbsRange)
       )
     ).
-spec fetch_fun(ne_binary(), ne_binary(), ne_binaries(), dict:dict(), gregorian_seconds()) -> dict:dict().
fetch_fun(Db, BoxId, Ids, ResDict, RetenTimestamp) ->
    ViewOpts = [{'keys', Ids}
               ,'include_docs'
               ],
    case kz_datamgr:db_exists(Db)
        andalso kz_datamgr:all_docs(Db, ViewOpts)
    of
        'false' ->
            fetch_faild_with_reason("not_found", Db, Ids, ResDict);
        {'ok', JObjs} ->
            normalize_bulk_results(<<"fetch">>, BoxId, RetenTimestamp, JObjs, ResDict);
        {'error', R} ->
            fetch_faild_with_reason(R, Db, Ids, ResDict)
    end.

-spec fetch_faild_with_reason(any(), ne_binary(), ne_binaries(), dict:dict()) -> dict:dict().
fetch_faild_with_reason(Reason, Db, Ids, ResDict) ->
    lager:warning("failed to bulk fetch voicemail messages from db ~s: ~p"
                 ,[Db, Reason]),
    Failed = kz_json:from_list([{Id, kz_util:to_binary(Reason)}
                                || Id <- Ids
                               ]),
    dict:append(<<"failed">>, Failed, ResDict).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_folder(ne_binary(), messages(), ne_binary(), ne_binary()) -> kz_json:object().
change_folder(Folder, Msgs, AccountId, BoxId) ->
    Fun = [fun(JObj) -> kzd_box_message:apply_folder(Folder, JObj) end
          ],
    update(AccountId, BoxId, Msgs, Fun).

%%--------------------------------------------------------------------
%% @public
%% @doc Move messages to another vmbox
%% @end
%%--------------------------------------------------------------------
-spec move_to_vmbox(ne_binary(), messages(), ne_binary(), ne_binary()) ->
                           kz_json:object().
move_to_vmbox(AccountId, Msgs, OldBoxId, NewBoxId) ->
    AccountDb = kvm_util:get_db(AccountId),
    {'ok', NBoxJ} = kz_datamgr:open_cache_doc(AccountDb, NewBoxId),
    Funs = kvm_util:get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId),
    update(AccountId, OldBoxId, Msgs, Funs).

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
    copy_to_vmboxes_fold(AccountId, Ids, OldBoxId, NewBoxIds, dict:new()).

-spec copy_to_vmboxes_fold(ne_binary(), ne_binaries(), ne_binary(), ne_binaries(), dict:dict()) ->
                                  dict:dict().
copy_to_vmboxes_fold(_, [], _, _, Copied) -> Copied;
copy_to_vmboxes_fold(AccountId, [Id | Ids], OldBoxId, NewBoxIds, Copied) ->
    CopyRes = kvm_message:copy_to_vmboxes(AccountId, Id, OldBoxId, NewBoxIds, Copied),
    copy_to_vmboxes_fold(AccountId, Ids, OldBoxId, NewBoxIds, CopyRes).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_view_results(ne_binary(), ne_binary(), kz_proplist(), norm_fun()) -> kz_json:object().
-spec get_view_results(ne_binaries(), ne_binary(), kz_proplist(), norm_fun(), kz_json:objects()) ->
                              kz_json:objects().
get_view_results(AccountId, View, ViewOpts, NormFun) ->
    Dbs = kvm_util:get_range_db(AccountId),
    get_view_results(Dbs, View, ViewOpts, NormFun, []).

get_view_results([], _View, _ViewOpts, 'undefined', ViewResults) ->
    ViewResults;
get_view_results([], _View, _ViewOpts, NormFun, ViewResults) ->
    [JObj
     || JObj <- lists:foldl(NormFun, [], ViewResults),
        not kz_util:is_empty(JObj)
    ];
get_view_results([Db | Dbs], View, ViewOpts, NormFun, Acc) ->
    case kz_datamgr:get_results(Db, View, ViewOpts) of
        {'ok', []} -> get_view_results(Dbs, View, ViewOpts, NormFun, Acc);
        {'ok', Msgs} -> get_view_results(Dbs, View, ViewOpts, NormFun, Msgs ++ Acc);
        {'error', _}=_E ->
            lager:debug("failed to get voicemail message ~s view results from db ~s with ViewOpts ~s"
                       ,[View, Db, ViewOpts]
                       ),
            get_view_results(Dbs, View, ViewOpts, NormFun, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize listing view results
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize fetch/update bulk result
%% Note: Optional checks message belonging to the BoxId
%% Note: It always returns a dict which contains secceeded and failed
%%       messages operation
%% @end
%%--------------------------------------------------------------------
-spec normalize_bulk_results(ne_binary(), api_ne_binary(), api_seconds(), kz_json:objects(), dict:dict()) ->
                                    dict:dict().
normalize_bulk_results(Method, BoxId, RetenTimestamp, JObjs, Dict) ->
    DefaultDict = dict:from_list([{<<"succeeded">>, []}
                                 ,{<<"failed">>, []}
                                 ]),
    MergeFun = fun(_K, _V1, V2) -> V2 end,
    normalize_bulk_results1(Method, BoxId, RetenTimestamp, JObjs, dict:merge(MergeFun, DefaultDict, Dict)).

normalize_bulk_results1(_Method, _BoxId, _, [], Dict) ->
    lager:debug("voicemail ~s bulk for mailbox ~s resulted in ~b succeeded and ~b failed docs"
               ,[_Method
                ,_BoxId
                ,length(dict:fetch(<<"succeeded">>, Dict))
                ,length(dict:fetch(<<"failed">>, Dict))
                ]),
    Dict;
normalize_bulk_results1(Method, BoxId, RetenTimestamp, [JObj | JObjs], Dict) ->
    Id = kz_json:get_first_defined([<<"key">>, <<"id">>], JObj),
    IsPrior = is_prior_to_retention(
                kzd_box_message:utc_seconds(kz_json:get_value(<<"doc">>, JObj))
                                   ,RetenTimestamp
              ),
    NewDict = case kvm_util:check_msg_belonging(BoxId, JObj)
                  andalso kz_json:get_value(<<"error">>, JObj)
              of
                  'false' ->
                      Failed = kz_json:from_list([{Id, <<"not_found">>}]),
                      dict:append(<<"failed">>, Failed, Dict);
                  'undefined' when not IsPrior ->
                      dict:append(<<"succeeded">>, kz_json:get_value(<<"doc">>, JObj, Id), Dict);
                  'undefined' ->
                      Failed = kz_json:from_list([{Id, <<"prior_to_retention_duration">>}]),
                      dict:append(<<"failed">>, Failed, Dict);
                  Error ->
                      Failed = kz_json:from_list([{Id, kz_util:to_binary(Error)}]),
                      dict:append(<<"failed">>, Failed, Dict)
              end,
    normalize_bulk_results1(Method, BoxId, RetenTimestamp, JObjs, NewDict).

-spec is_prior_to_retention(pos_integer(), api_seconds()) -> boolean().
is_prior_to_retention(0, _RetenTimestamp) -> 'false';
is_prior_to_retention(_UtcSeconds, 'undefined') -> 'false';
is_prior_to_retention(UtcSeconds, RetenTimestamp) when UtcSeconds > RetenTimestamp -> 'false';
is_prior_to_retention(_UtcSeconds, _RetenTimestamp) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize count view results
%% @end
%%--------------------------------------------------------------------
-spec normalize_count(kz_json:objects()) -> kz_json:object().
-spec normalize_count_fold(kz_json:object(), kz_json:object()) -> kz_json:object().
normalize_count(ViewRes) ->
    lists:foldl(fun normalize_count_fold/2, kz_json:new(), ViewRes).

normalize_count_fold(M, Acc) ->
    VMBoxId = kz_json:get_value([<<"key">>, ?BOX_ID_KEY_INDEX], M),
    Folder = kz_json:get_value([<<"key">>, ?FOLDER_KEY_INDEX], M),
    Value = kz_json:get_integer_value(<<"value">>, M),

    Total = kz_json:get_integer_value([VMBoxId, <<"total">>], Acc, 0),
    PreviousNonDeleted = kz_json:get_integer_value([VMBoxId, <<"non_deleted">>], Acc, 0),
    NonDeleted = case Folder of
                     ?VM_FOLDER_NEW -> PreviousNonDeleted + Value;
                     ?VM_FOLDER_SAVED -> PreviousNonDeleted + Value;
                     _ -> PreviousNonDeleted
                 end,
    PreviousValue = kz_json:get_integer_value([VMBoxId, Folder], Acc, 0),
    kz_json:set_values([{[VMBoxId, Folder], PreviousValue + Value}
                       ,{[VMBoxId, <<"non_deleted">>], NonDeleted}
                       ,{[VMBoxId, <<"total">>], Total + Value}
                       ]
                      ,Acc
                      ).

-spec normalize_count_none_deleted(ne_binary(), kz_json:object()) -> count_result().
normalize_count_none_deleted(BoxId, ViewRes) ->
    {kz_json:get_integer_value([BoxId, ?VM_FOLDER_NEW], ViewRes, 0)
    ,kz_json:get_integer_value([BoxId, ?VM_FOLDER_SAVED], ViewRes, 0)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Apply update functions and map all messages to their MODBs
%% @end
%%--------------------------------------------------------------------
-spec apply_fun_and_map_msgs_to_modb(ne_binary(), kz_json:objects(), update_funs(), dict:dict()) ->
                                            dict:dict().
apply_fun_and_map_msgs_to_modb(_AccountId, [], _Funs, Dict) -> Dict;
apply_fun_and_map_msgs_to_modb(AccountId, [JObj | JObjs], Funs, Dict) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    Db = kz_doc:account_db(JObj, kvm_util:get_db(AccountId, kz_doc:id(NewJObj))),
    NewDict = dict:append(Db, NewJObj, Dict),
    apply_fun_and_map_msgs_to_modb(AccountId, JObjs, Funs, NewDict).
