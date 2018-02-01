%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
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
        ,count/2, count_by_owner/2, count_non_deleted/2
        ]).

-export([update/3, update/4, change_folder/4, change_folder/5
        ,move_to_vmbox/4, move_to_vmbox/5
        ,copy_to_vmboxes/4, copy_to_vmboxes/5
        ]).

-include("kz_voicemail.hrl").

-define(COUNT_ALL, [<<"new">>, <<"saved">>, <<"deleted">>]).
-define(COUNT_NON_DELETED, [<<"new">>, <<"saved">>]).

-define(MSG_LISTING_BY_MAILBOX, <<"mailbox_messages/listing_by_mailbox">>).
-define(MSG_LISTING_BY_TIMESTAMP, <<"mailbox_messages/listing_by_timestamp">>).
-define(MSG_COUNT_PER_FOLDER, <<"mailbox_messages/count_per_folder">>).
-define(MSG_COUNT_PER_BOX_FOLDER, <<"mailbox_messages/count_per_box_folder">>).

-type norm_fun() :: 'undefined' |
                    fun((kz_json:object()) -> kz_json:object()).


%% keys are: <<"new">>, <<"saved">>, <<"deleted">>, <<"non_deleted">>, <<"total">>
-type per_folder_count() :: #{kz_term:ne_binary() => non_neg_integer()}.

-type count_map() :: #{kz_term:ne_binary() => per_folder_count()}.
-type non_deleted_tuple() :: {non_neg_integer(), non_neg_integer()}.

-type get_map() :: #{kz_term:ne_binary() => kz_json:objects()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get all voicemail messages (metadata only) for an account which
%% are in retention duration range in whatever folder they're in.
%%
%% returns a map with box_id as key and a list of messages metadata
%% as key's value
%% @end
%%--------------------------------------------------------------------
-spec get(kz_term:ne_binary()) -> get_map().
get(AccountId) ->
    ViewOpts = [{'startkey', []}
               ,{'endkey', []}
               ],
    normalize_account_listing(get_view_results(AccountId, ?MSG_LISTING_BY_TIMESTAMP, ViewOpts, 'undefined')).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get all voicemail messages (metadata only) for a specific box which
%% are in retention duration range and in whatever folder they're in.
%% @end
%%--------------------------------------------------------------------
-spec get(kz_term:ne_binary(), message() | kz_term:proplist()) -> kz_json:objects().
get(AccountId, ?NE_BINARY=BoxId) ->
    ViewOpts = [{'startkey', [BoxId]}
               ,{'endkey', [BoxId]}
               ],
    NormFun = fun(JObj) -> kz_json:get_value(<<"value">>, JObj) end,
    get_view_results(AccountId, ?MSG_LISTING_BY_MAILBOX, ViewOpts, NormFun);
get(AccountId, Box) ->
    get(AccountId, kz_doc:id(Box)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns count of messages in an account which are in retention
%% duration in whatever folder they are in.
%%
%% Note: returns a map with box_id as key, and a map of count of
%% each folder as box_id key's value
%%
%% @equiv Same as calling count_per_folder/1
%%
%% Example:
%% #{<<"5ee08e5e8d4cac3d0fbc2f23870e5dbc">> =>
%%       #{<<"new">> => 7,<<"non_deleted">> => 8,<<"saved">> => 1,
%%         <<"total">> => 8},
%%   <<"79371edb968bf5086d61a41b1849ab8e">> =>
%%       #{<<"new">> => 7,<<"non_deleted">> => 7,<<"total">> => 7}}
%% @end
%%--------------------------------------------------------------------
-spec count(kz_term:ne_binary()) -> count_map().
count(AccountId) ->
    count_per_folder(AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns total count of non-deleted messages in a specific box which
%% are in retention duration and are in 'new' or 'saved' folder only.
%% @end
%%--------------------------------------------------------------------
-spec count(kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
count(AccountId, BoxId) ->
    {New, Saved} = count_non_deleted(AccountId, BoxId),
    New + Saved.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns a tuple of count of non-deleted messages in a specific box which
%% are in retention duration and are in 'new' or 'saved' folder only.
%%
%% Note: returns counts in {new, saved} format (both values are integer)
%% @end
%%--------------------------------------------------------------------
-spec count_non_deleted(kz_term:ne_binary(), kz_term:ne_binary()) -> non_deleted_tuple().
count_non_deleted(AccountId, BoxId) ->
    CountMap = count_per_folder(AccountId, [BoxId], ?MSG_COUNT_PER_BOX_FOLDER, ?COUNT_NON_DELETED),
    VMBoxCount = maps:get(BoxId, CountMap, maps:new()),
    {maps:get(<<"new">>, VMBoxCount, 0)
    ,maps:get(<<"saved">>, VMBoxCount, 0)
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get a list of voicemail boxes assign to a user first and then
%% return total count of non-deleted messages of those boxes.
%%
%% returns same format as count_non_deleted/2
%% @end
%%--------------------------------------------------------------------
-spec count_by_owner(kz_term:ne_binary(), kz_term:ne_binary()) -> non_deleted_tuple().
count_by_owner(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, OwnerId) ->
    AccountId = kz_util:format_account_id(AccountDb),
    count_by_owner(AccountId, OwnerId);
count_by_owner(AccountId, OwnerId) ->
    ViewOptions = [{'key', [OwnerId, <<"vmbox">>]}],
    case kz_datamgr:get_results(kvm_util:get_db(AccountId), <<"attributes/owned">>, ViewOptions) of
        {'ok', []} ->
            lager:info("no voicemail boxes belonging to user ~s found", [OwnerId]),
            {0, 0};
        {'ok', Boxes} ->
            BoxIds = [kz_json:get_value(<<"value">>, Box) || Box <- Boxes],
            lager:debug("found ~p vociemail boxes belonging to user ~s", [length(BoxIds), OwnerId]),
            sum_owner_mailboxes(AccountId, BoxIds, {0, 0});
        {'error', _R} ->
            lager:info("unable to lookup vm counts by owner: ~p", [_R]),
            {0, 0}
    end.

-spec sum_owner_mailboxes(kz_term:ne_binary(), kz_term:ne_binaries(), non_deleted_tuple()) -> non_deleted_tuple().
sum_owner_mailboxes(_, [], Results) -> Results;
sum_owner_mailboxes(AccountId, [BoxId|BoxIds], {New, Saved}) ->
    {BoxNew, BoxSaved} = count_non_deleted(AccountId, BoxId),
    lager:debug("adding mailbox ~s with ~p new and ~p saved messages to user's quantities"
               ,[BoxId, BoxNew, BoxSaved]),
    sum_owner_mailboxes(AccountId,  BoxIds, {New + BoxNew, Saved + BoxSaved}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns count of messages per box per folder in an account which are in
%% retention duration.
%%
%% Note: returns a map with box_id as key, and a map of count of
%% each folder as box_id key's value
%%
%% Example:
%% #{<<"5ee08e5e8d4cac3d0fbc2f23870e5dbc">> =>
%%       #{<<"new">> => 7,<<"non_deleted">> => 8,<<"saved">> => 1,
%%         <<"total">> => 8},
%%   <<"79371edb968bf5086d61a41b1849ab8e">> =>
%%       #{<<"new">> => 7,<<"non_deleted">> => 7,<<"total">> => 7}}
%% @end
%%--------------------------------------------------------------------
-spec count_per_folder(kz_term:ne_binary()) -> count_map().
count_per_folder(AccountId) ->
    count_per_folder(AccountId, [], ?MSG_COUNT_PER_FOLDER, ?COUNT_ALL).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Same as count_per_folder/1 but only for a specific box
%% @end
%%--------------------------------------------------------------------
-spec count_per_folder(kz_term:ne_binary(), kz_term:ne_binary()) -> count_map().
count_per_folder(AccountId, BoxId) ->
    count_per_folder(AccountId, [BoxId], ?MSG_COUNT_PER_BOX_FOLDER, ?COUNT_ALL).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loop over each folder, get view result and do the sum with
%% previous result.
%% @end
%%--------------------------------------------------------------------
-spec count_per_folder(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binaries()) -> count_map().
count_per_folder(AccountId, Keys, View, Folders) ->
    lists:foldl(fun(Folder, ResultMap) ->
                        count_per_folder(AccountId, Keys, View, Folder, ResultMap)
                end
               ,#{}
               ,Folders
               ).

-spec count_per_folder(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), count_map()) -> count_map().
count_per_folder(AccountId, Keys, View, Folder, ResultMap) ->
    ViewOpts = [{'startkey', Keys ++ [Folder]}
               ,{'endkey', Keys ++ [Folder]}
               ],
    case get_view_results(AccountId, View, ViewOpts, 'undefined') of
        [] -> ResultMap;
        Results -> normalize_count(View, Results, ResultMap)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec update(kz_term:ne_binary(), kz_term:ne_binary(), messages()) -> kz_json:object().
update(AccountId, BoxId, Msgs) ->
    update(AccountId, BoxId, Msgs, []).

-spec update(kz_term:ne_binary(), kz_term:ne_binary(), messages(), update_funs()) -> kz_json:object().
update(AccountId, BoxId, [?NE_BINARY = _Msg | _] = MsgIds, Funs) ->
    RetenTimestamp = kz_time:now_s() - kvm_util:retention_seconds(AccountId),
    FetchMap = fetch(AccountId, MsgIds, BoxId, RetenTimestamp),
    do_update(AccountId, FetchMap, Funs, RetenTimestamp);
update(AccountId, _BoxId, JObjs, Funs) ->
    RetenTimestamp = kz_time:now_s() - kvm_util:retention_seconds(AccountId),
    do_update(AccountId, #{succeeded => JObjs}, Funs, RetenTimestamp).

-spec do_update(kz_term:ne_binary(), bulk_map(), update_funs(), kz_time:gregorian_seconds()) -> kz_json:object().
do_update(AccountId, FetchMap, Funs, RetenTimestamp) ->
    #{to_update_map := ToUpdateMap
     ,failed := Failed
     ,enforce_set := EnforceSet
     } = split_to_modbs_and_apply_funs(FetchMap, AccountId, Funs, RetenTimestamp),
    bulk_result(
      maps:fold(fun update_fun/3
               ,#{succeeded => []
                 ,failed => Failed
                 ,enforce_set => EnforceSet
                 }
               ,ToUpdateMap
               )
     ).

-spec bulk_result(bulk_map()) -> kz_json:object().
bulk_result(Map) ->
    kz_json:from_list_recursive(
      props:filter_empty(
        [{<<"succeeded">>, maps:get(succeeded, Map, [])}
        ,{<<"failed">>, maps:get(failed, Map, [])}
        ])
     ).

-spec update_fun(kz_term:ne_binary(), kz_json:objects(), bulk_map()) -> bulk_map().
update_fun(Db, JObjs, #{failed := Failed}=ResultMap) ->
    case kz_datamgr:save_docs(Db, JObjs) of
        {'ok', Saved} ->
            normalize_bulk_results(ResultMap, Saved, <<"update">>, 'undefined', 'undefined');
        {'error', R} ->
            lager:warning("failed to bulk update voicemail messages for db ~s: ~p", [Db, R]),
            IdReasons = [{kz_doc:id(D), kz_term:to_binary(R)} || D <- JObjs],
            ResultMap#{failed => Failed ++ IdReasons}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message docs for a voicemail box
%% @end
%%--------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:object().
fetch(AccountId, MsgIds) ->
    fetch(AccountId, MsgIds, 'undefined').

-spec fetch(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:api_ne_binary()) -> kz_json:object().
fetch(AccountId, MsgIds, BoxId) ->
    RetenTimestamp = kz_time:now_s() - kvm_util:retention_seconds(AccountId),
    bulk_result(fetch(AccountId, MsgIds, BoxId, RetenTimestamp)).

-spec fetch(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:api_ne_binary(), kz_time:gregorian_seconds()) -> bulk_map().
fetch(AccountId, MsgIds, BoxId, RetenTimestamp) ->
    DbsRange = kvm_util:split_to_modbs(AccountId, MsgIds),
    Fun = fun(Db, Ids, ResultMap) ->
                  fetch_fun(Db, BoxId, Ids, ResultMap, RetenTimestamp)
          end,
    maps:fold(Fun, #{succeeded => [], failed => []}, DbsRange).

-spec fetch_fun(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries(), bulk_map(), kz_time:gregorian_seconds()) -> bulk_map().
fetch_fun(Db, BoxId, Ids, ResultMap, RetenTimestamp) ->
    case kz_datamgr:db_exists(Db)
        andalso kz_datamgr:open_docs(Db, Ids)
    of
        'false' ->
            fetch_faild_with_reason("not_found", Db, Ids, ResultMap);
        {'ok', JObjs} ->
            normalize_bulk_results(ResultMap, JObjs, <<"fetch">>, BoxId, RetenTimestamp);
        {'error', R} ->
            fetch_faild_with_reason(R, Db, Ids, ResultMap)
    end.

-spec fetch_faild_with_reason(any(), kz_term:ne_binary(), kz_term:ne_binaries(), bulk_map()) -> bulk_map().
fetch_faild_with_reason(Reason, Db, Ids, #{failed := Failed}=ResultMap) ->
    lager:warning("failed to bulk fetch voicemail messages from db ~s: ~p", [Db, Reason]),
    IdReasons = [{Id, kz_term:to_binary(Reason)} || Id <- Ids],
    ResultMap#{failed => IdReasons ++ Failed}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_folder(kvm_message:vm_folder(), messages(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                           kz_json:object().
change_folder(Folder, Msgs, AccountId, BoxId) ->
    change_folder(Folder, Msgs, AccountId, BoxId, []).

-spec change_folder(kvm_message:vm_folder(), messages(), kz_term:ne_binary(), kz_term:ne_binary(), update_funs()) ->
                           kz_json:object().
change_folder(Folder, Msgs, AccountId, BoxId, Funs) ->
    Fun = [fun(JObj) -> kzd_box_message:apply_folder(Folder, JObj) end
           | Funs
          ],
    update(AccountId, BoxId, Msgs, Fun).

%%--------------------------------------------------------------------
%% @public
%% @doc Move messages to another vmbox
%% @end
%%--------------------------------------------------------------------
-spec move_to_vmbox(kz_term:ne_binary(), messages(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                           kz_json:object().
move_to_vmbox(AccountId, MsgThings, OldBoxId, NewBoxId) ->
    move_to_vmbox(AccountId, MsgThings, OldBoxId, NewBoxId, []).

-spec move_to_vmbox(kz_term:ne_binary(), messages(), kz_term:ne_binary(), kz_term:ne_binary(), update_funs()) ->
                           kz_json:object().
move_to_vmbox(AccountId, [?NE_BINARY = _Msg | _] = MsgIds, OldBoxId, NewBoxId, Funs) ->
    AccountDb = kvm_util:get_db(AccountId),
    {'ok', NBoxJ} = kz_datamgr:open_cache_doc(AccountDb, NewBoxId),
    RetenTimestamp = kz_time:now_s() - kvm_util:retention_seconds(AccountId),
    bulk_result(do_move(AccountId, MsgIds, OldBoxId, NewBoxId, NBoxJ, #{}, Funs, RetenTimestamp));
move_to_vmbox(AccountId, MsgJObjs, OldBoxId, NewBoxId, Funs) ->
    MsgIds = [kzd_box_message:get_msg_id(J) || J <- MsgJObjs],
    move_to_vmbox(AccountId, MsgIds, OldBoxId, NewBoxId, Funs).

-spec do_move(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), bulk_map(), update_funs(), kz_time:gregorian_seconds()) -> bulk_map().
do_move(_AccountId, [], _OldboxId, _NewBoxId, _NBoxJ, ResultMap, _, _) ->
    ResultMap;
do_move(AccountId, [FromId | FromIds], OldboxId, NewBoxId, NBoxJ, ResultMap, Funs, RetenTimestamp) ->
    case kvm_message:maybe_do_move(AccountId, FromId, OldboxId, NewBoxId, NBoxJ, Funs, RetenTimestamp) of
        {'ok', Moved} ->
            NewMap = maps:update_with(succeeded, fun(List) -> [kz_doc:id(Moved)|List] end, [kz_doc:id(Moved)], ResultMap),
            do_move(AccountId, FromIds, OldboxId, NewBoxId, NBoxJ, NewMap, Funs, RetenTimestamp);
        {'error', Reason} ->
            IdReason = {FromId, kz_term:to_binary(Reason)},
            NewMap = maps:update_with(failed, fun(List) -> [IdReason|List] end, [IdReason], ResultMap),
            do_move(AccountId, FromIds, OldboxId, NewBoxId, NBoxJ, NewMap, Funs, RetenTimestamp)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc copy messages to other vmbox(es)
%% @end
%%--------------------------------------------------------------------
-spec copy_to_vmboxes(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) ->
                             kz_json:object().
copy_to_vmboxes(AccountId, Ids, OldBoxId, NewBoxIds) ->
    copy_to_vmboxes(AccountId, Ids, OldBoxId, NewBoxIds, []).

-spec copy_to_vmboxes(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries(), update_funs()) ->
                             kz_json:object().
copy_to_vmboxes(AccountId, Ids, OldBoxId, ?NE_BINARY = NewBoxId, Funs) ->
    copy_to_vmboxes(AccountId, Ids, OldBoxId, [NewBoxId], Funs);
copy_to_vmboxes(AccountId, Ids, OldBoxId, NewBoxIds, Funs) ->
    RetenTimestamp = kz_time:now_s() - kvm_util:retention_seconds(AccountId),
    bulk_result(
      lists:foldl(fun(Id, AccMap) ->
                          kvm_message:maybe_copy_to_vmboxes(AccountId, Id, OldBoxId, NewBoxIds, AccMap, Funs, RetenTimestamp)
                  end
                 ,#{}
                 ,Ids
                 )
     ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_view_results(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), norm_fun()) -> kz_json:objects().
get_view_results(?NE_BINARY = AccountId, View, ViewOpts, NormFun) ->
    {From, To, Dbs} = kvm_util:get_range_db(AccountId),
    get_view_results(Dbs, View, maybe_add_range_to_keys(From, To, ViewOpts), NormFun, []).

-spec get_view_results(kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:proplist(), norm_fun(), kz_json:objects()) -> kz_json:objects().
get_view_results([], _View, _ViewOpts, 'undefined', ViewResults) ->
    ViewResults;
get_view_results([], _View, _ViewOpts, NormFun, ViewResults) ->
    [JObj
     || J <- ViewResults,
        JObj <- [NormFun(J)],
        not kz_term:is_empty(JObj)
    ];
get_view_results([Db | Dbs], View, ViewOpts, NormFun, Acc) ->
    case kazoo_modb:get_results(Db, View, ViewOpts) of
        {'ok', []} -> get_view_results(Dbs, View, ViewOpts, NormFun, Acc);
        {'ok', Msgs} -> get_view_results(Dbs, View, ViewOpts, NormFun, Acc ++ Msgs);
        {'error', _}=_E ->
            lager:debug("failed to get voicemail message ~s view results from db ~s with ViewOpts ~s"
                       ,[View, Db, ViewOpts]
                       ),
            get_view_results(Dbs, View, ViewOpts, NormFun, Acc)
    end.

-spec maybe_add_range_to_keys(kz_time:gregorian_seconds(), kz_time:gregorian_seconds(), kz_term:proplist()) -> kz_term:proplist().
maybe_add_range_to_keys(From, To, ViewOpts) ->
    [{'startkey', add_timestamp_if_defined('startkey', To, ViewOpts)}
    ,{'endkey', add_timestamp_if_defined('startkey', From, ViewOpts)}
    ,'descending'
     | props:delete_keys(['startkey', 'endkey'], ViewOpts)
    ].

-spec add_timestamp_if_defined('startkey' | 'endkey', kz_time:gregorian_seconds(), kz_term:proplist()) -> kz_json:path().
add_timestamp_if_defined(Key, Timestamp, ViewOpts) ->
    case props:get_value(Key, ViewOpts) of
        [] -> [Timestamp];
        [Thing] -> [Thing, Timestamp];
        [Thing1, Thing2] -> [Thing1, Thing2, Timestamp]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize listing view results
%% @end
%%--------------------------------------------------------------------
-define(LISTING_BY_TIMESTAMP_BOX_ID_KEY_INDEX, 2).

-spec normalize_account_listing(kz_json:objects()) -> get_map().
normalize_account_listing(JObjs) ->
    lists:foldl(fun normalize_account_listing/2, #{}, JObjs).

-spec normalize_account_listing(kz_json:object(), get_map()) -> get_map().
normalize_account_listing(JObj, Map) ->
    Value = kz_json:get_value(<<"value">>, JObj),
    case kz_json:get_value([<<"key">>, ?LISTING_BY_TIMESTAMP_BOX_ID_KEY_INDEX], JObj) of
        'undefined' -> Map;
        BoxId ->
            maps:update_with(BoxId, fun(List) -> List ++ [Value] end, [Value], Map)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize fetch/update bulk result
%% Note: Optional checks message belonging to the BoxId and message
%%       retention
%% @end
%%--------------------------------------------------------------------
-spec normalize_bulk_results(bulk_map(), kz_json:objects(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_time:api_seconds()) -> bulk_map().
normalize_bulk_results(ResultMap, [], _Method, _BoxId, _) ->
    lager:debug("voicemail ~s bulk for mailbox ~s resulted in ~b succeeded and ~b failed docs"
               ,[_Method
                ,_BoxId
                ,erlang:length(maps:get(succeeded, ResultMap, []))
                ,erlang:length(maps:get(failed, ResultMap, []))
                ]),
    ResultMap;
normalize_bulk_results(#{succeeded := Succeeded
                        ,failed := Failed
                        }=ResultMap, [JObj | JObjs], Method, BoxId, RetenTimestamp) ->
    Id = kz_json:get_first_defined([<<"key">>, <<"id">>], JObj),

    IsPrior = is_prior_to_retention(JObj, RetenTimestamp, ResultMap),

    NewMap = case kvm_util:check_msg_belonging(BoxId, JObj)
                 andalso kz_json:get_value(<<"error">>, JObj)
             of
                 'false' ->
                     ResultMap#{failed => [{Id, <<"not_found">>}|Failed]};
                 'undefined' when not IsPrior ->
                     ResultMap#{succeeded => [kz_json:get_value(<<"doc">>, JObj, Id)|Succeeded]};
                 'undefined' ->
                     case Method of
                         <<"fetch">> ->
                             ResultMap#{succeeded => [kvm_util:enforce_retention(kz_json:get_value(<<"doc">>, JObj), 'true')
                                                      | Succeeded
                                                     ]
                                       };
                         <<"update">> ->
                             ResultMap#{failed => [{Id, <<"prior_to_retention_duration">>}|Failed]}
                     end;
                 Error ->
                     ResultMap#{failed => [{Id, kz_term:to_binary(Error)}|Failed]}
             end,
    normalize_bulk_results(NewMap, JObjs, Method, BoxId, RetenTimestamp).

%% check message retention, also if the operation is update, checks if the message is in retention enforce set
%% so the message won't be added to succeeded list
-spec is_prior_to_retention(kz_json:object(), kz_time:api_seconds(), bulk_map()) -> boolean().
is_prior_to_retention(JObj, _, #{enforce_set := EnforceSet}) ->
    sets:is_element(kz_doc:id(JObj), EnforceSet);
is_prior_to_retention(JObj, RetenTimestamp, _) ->
    kvm_util:is_prior_to_retention(kz_json:get_value(<<"doc">>, JObj), RetenTimestamp).

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize count view results
%% @end
%%--------------------------------------------------------------------
-define(PER_FOLDER_VIEW_FOLDER_KEY_INDEX, 1).
-define(PER_FOLDER_VIEW_BOX_ID_KEY_INDEX, 3).

-define(PER_BOX_FOLDER_VIEW_BOX_ID_KEY_INDEX, 1).
-define(PER_BOX_FOLDER_VIEW_FOLDER_KEY_INDEX, 2).

-spec normalize_count(kz_term:ne_binary(), kz_json:objects(), count_map()) -> count_map().
normalize_count(?MSG_COUNT_PER_BOX_FOLDER, ViewResults, ResultMap) ->
    lists:foldl(fun(JObj, Map) ->
                        sum_per_folder(JObj, ?PER_BOX_FOLDER_VIEW_FOLDER_KEY_INDEX, ?PER_BOX_FOLDER_VIEW_BOX_ID_KEY_INDEX, Map)
                end
               ,ResultMap
               ,ViewResults
               );
normalize_count(?MSG_COUNT_PER_FOLDER, ViewResults, ResultMap) ->
    lists:foldl(fun(JObj, Map) ->
                        sum_per_folder(JObj, ?PER_FOLDER_VIEW_FOLDER_KEY_INDEX, ?PER_FOLDER_VIEW_BOX_ID_KEY_INDEX, Map)
                end
               ,ResultMap
               ,ViewResults
               ).


-spec sum_per_folder(kz_json:object(), non_neg_integer(), non_neg_integer(), count_map()) -> count_map().
sum_per_folder(JObj, FolderKeyIndex, BoxIdKeyIndex, ResultMap) ->
    Folder = kz_json:get_binary_value([<<"key">>, FolderKeyIndex], JObj),
    BoxId = kz_json:get_binary_value([<<"key">>, BoxIdKeyIndex], JObj),

    VMBoxCount = maps:get(BoxId, ResultMap, maps:new()),
    OldNonDeleted = maps:get(<<"non_deleted">>, VMBoxCount, 0),
    NonDeleted = case Folder of
                     ?VM_FOLDER_NEW -> OldNonDeleted + 1;
                     ?VM_FOLDER_SAVED -> OldNonDeleted + 1;
                     _ -> OldNonDeleted
                 end,
    OldFolderCount = maps:get(Folder, VMBoxCount, 0),
    ResultMap#{BoxId => VMBoxCount#{Folder => OldFolderCount + 1
                                   ,<<"non_deleted">> => NonDeleted
                                   ,<<"total">> => maps:get(<<"total">>, VMBoxCount, 0) + 1
                                   }
              }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply update functions and map all messages to their MODBs
%%
%% If message is prior to retention, do not apply functions, just
%% set the folder to 'deleted' and set it to_update_map to update the
%% document in database then add it to enforce_set to return it to caller
%% as an error.
%% @end
%%--------------------------------------------------------------------
-spec split_to_modbs_and_apply_funs(bulk_map(), kz_term:ne_binary(), update_funs(), kz_time:gregorian_seconds()) -> bulk_map().
split_to_modbs_and_apply_funs(FetchMap, AccountId, Funs, RetenTimestamp) ->
    Succeeded = maps:get(succeeded, FetchMap, []),
    Failed = maps:get(failed, FetchMap, []),
    Map = #{to_update_map => #{}
           ,failed => Failed
           ,enforce_set => sets:new()
           },
    split_to_modbs_and_apply_funs(Map, AccountId, Succeeded, Funs, RetenTimestamp).

-spec split_to_modbs_and_apply_funs(bulk_map(), kz_term:ne_binary(), kz_json:objects(), update_funs(), kz_time:gregorian_seconds()) -> bulk_map().
split_to_modbs_and_apply_funs(SplitMap, _AccountId, [], _Funs, _RetenTimestamp) ->
    SplitMap;
split_to_modbs_and_apply_funs(#{to_update_map := ToUpdateMap
                               ,enforce_set := EnforceSet
                               }=SplitMap, AccountId, [JObj | JObjs], Funs, RetenTimestamp) ->
    case kvm_util:is_prior_to_retention(JObj, RetenTimestamp) of
        'true' ->
            Id = kz_doc:id(JObj),
            Db = kvm_util:get_db(AccountId, JObj),
            NewJObj = kvm_util:enforce_retention(JObj, 'true'),
            NewMap = SplitMap#{to_update_map => maps:update_with(Db, fun(List) -> [NewJObj|List] end, [NewJObj], ToUpdateMap)
                              ,enforce_set => sets:add_element(Id, EnforceSet)
                              },
            split_to_modbs_and_apply_funs(NewMap, AccountId, JObjs, Funs, RetenTimestamp);
        'false' ->
            NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
            Db = kvm_util:get_db(AccountId, NewJObj),
            NewToUpdateMap = maps:update_with(Db, fun(List) -> [NewJObj|List] end, [NewJObj], ToUpdateMap),
            split_to_modbs_and_apply_funs(SplitMap#{to_update_map => NewToUpdateMap}, AccountId, JObjs, Funs, RetenTimestamp)
    end.
