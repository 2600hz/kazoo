%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc The Great Kazoo Migration (TM)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_modb_migrate_maintenance).

-export([migrate/0
        ,migrate_account/1

        ,migrate_voicemails/1
        ,migrate_cdrs/1
        ]).

-include("kazoo_modb.hrl").

-type update_funs() :: [fun((kz_json:object()) -> kz_json:object())].

-spec migrate() -> 'ok'.
migrate() ->
    AccountIds = kz_term:shuffle_list(kapps_util:get_all_accounts('raw')),
    Total = length(AccountIds),
    io:format("Start migrating items to MODB from ~b databases~n~n", [Total]),
    lists:foldl(fun(A, C) -> migrate_account_fold(A, C, Total) end, 1, AccountIds),
    'ok'.

-spec migrate_account_fold(kz_term:ne_binary(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
migrate_account_fold(AccountId, Current, Total) ->
    io:format("(~p/~p) migrating items within account '~s'~n", [Current, Total, AccountId]),
    _ = migrate_account(AccountId),
    io:format("[~s] finished migrating items~n~n", [AccountId]),
    Current + 1.

-spec migrate_account(kz_term:ne_binary()) -> 'ok'.
migrate_account(Account) ->
    lists:foreach(fun(Fun) -> Fun(Account) end
                 ,[fun migrate_voicemails/1
                  ,fun migrate_cdrs/1
                  ]
                 ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_view_count(kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
get_view_count(AccountId, View) ->
    get_view_count(AccountId, View, 2).

-spec get_view_count(kz_term:ne_binary(), kz_term:ne_binary(), integer()) -> non_neg_integer().
get_view_count(_AccountId, _View, Retry) when Retry < 0 ->
    io:format("[~s] failed to fetch view ~s count~n", [_AccountId, _View]),
    0;
get_view_count(AccountId, View, Retry) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_datamgr:get_results_count(AccountDb, View, []) of
        {'ok', Total} -> Total;
        {'error', 'not_found'} ->
            _ = kapps_maintenance:refresh(AccountDb),
            get_view_count(AccountDb, View, Retry-1);
        {'error', _} ->
            get_view_count(AccountDb, View, Retry-1)
    end.

-spec next_skip(non_neg_integer(), non_neg_integer(), kz_term:proplist()) -> kz_term:proplist().
next_skip(0, Skip, ViewOptions) -> props:set_value('skip', Skip, ViewOptions);
next_skip(Remaining, Skip, ViewOptions) ->
    case Remaining < props:get_value(limit, ViewOptions) of
        true -> props:set_value('skip', Skip, props:delete(limit, ViewOptions));
        false -> props:set_value('skip', Skip, ViewOptions)
    end.

-spec maps_update_with(kz_term:ne_binary(), fun((any()) -> any()), any(), map()) -> map().
maps_update_with(Key, UpdateFun, Init, Map) ->
    try maps:get(Key, Map) of
        OldValue -> maps:put(Key, UpdateFun(OldValue), Map)
    catch
        error:{badkey, _} -> maps:put(Key, Init, Map)
    end.

%%%=============================================================================
%%% Voicemail Migration
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_voicemails(kz_term:ne_binary()) -> 'ok'.
migrate_voicemails(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    Total = get_view_count(AccountId, <<"vmboxes/legacy_msg_by_timestamp">>),
    migrate_voicemails(AccountId, Total).

-spec migrate_voicemails(kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
migrate_voicemails(_AccountId, 0) ->
    io:format("[~s] no voicemail messages found~n", [_AccountId]);
migrate_voicemails(AccountId, Total) ->
    Limit = kz_datamgr:max_bulk_read(),
    io:format("[~s] start migrating total ~b voicemail messages with batch size ~b~n", [AccountId, Total, Limit]),
    Stats = #{total => Total
             ,processed => 0
             ,moved => 0
             ,result => #{}
             ,skip => 0
             },
    migrate_voicemails(AccountId, Stats, [{'limit', Limit}]).

-spec migrate_voicemails(kz_term:ne_binary(), map(), kz_term:proplist()) -> 'ok'.
migrate_voicemails(AccountId, #{total := Total, processed := Processed, moved := Moved, skip := Skip}=Stats, ViewOptions) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_datamgr:get_results(AccountDb, <<"vmboxes/legacy_msg_by_timestamp">>, ViewOptions) of
        {'ok', []} ->
            io:format("[~s] voicemail message migration finished, (~b/~b) messages has been moved~n"
                     ,[AccountId, Moved, Total]
                     );
        {'ok', ViewResults} ->
            Length = length(ViewResults),
            io:format("[~s] processing ~b messages~n", [AccountId, Length]),
            MoveFun = fun(JObj, Map) ->
                              move_vm_to_modb(AccountId, kz_json:get_value(<<"value">>, JObj), Map)
                      end,
            ProcessMap = lists:foldl(MoveFun, Stats, ViewResults),
            update_mailboxes(AccountId, maps:get(result, ProcessMap)),

            NewProcessed = Processed + Length,
            io:format("[~s] finished processing ~b messages, ~b remain~n", [AccountId, Length, Total - NewProcessed]),
            timer:sleep(1000),

            NewMoved = maps:get(moved, ProcessMap),
            NewSkip = Skip + maps:get(skip, ProcessMap),
            Remaining = Total - NewMoved,
            migrate_voicemails(AccountId
                              ,Stats#{processed := NewProcessed
                                     ,moved := NewMoved
                                     ,skip := NewSkip
                                     ,result := #{}
                                     }
                              ,next_skip(Remaining, NewSkip, ViewOptions)
                              );
        {'error', _R} ->
            io:format("[~s] failed to fetch voicemail messages: ~p~n~n", [AccountId, _R])
    end.

-spec move_vm_to_modb(kz_term:ne_binary(), kz_json:object(), map()) -> map().
move_vm_to_modb(AccountId, LegacyVMJObj, #{total := Total
                                          ,processed := Processed
                                          ,moved := Moved
                                          ,skip := Skip
                                          ,result := Result
                                          }=Map) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    BoxId = kz_json:get_value(<<"source_id">>, LegacyVMJObj),
    {ToDb, ToId, TransformFuns} = transform_vm_doc_funs(AccountDb, LegacyVMJObj),

    FromId = kz_json:get_value([<<"metadata">>, <<"media_id">>], LegacyVMJObj),

    io:format("[~s] (~b/~b) moving voicemail ~s to ~s => "
             ,[AccountId, Processed + 1, Total, FromId, ToId]
             ),

    Opts = [{'transform', fun(_, B) -> lists:foldl(fun(F, J) -> F(J) end, B, TransformFuns) end}
           ,{'create_db', 'true'}
           ,{'allow_old_modb_creation', 'true'}
           ,{'max_retries', 3}
           ],

    case kazoo_modb:move_doc(AccountDb, FromId, ToDb, ToId, Opts) of
        {'ok', _} ->
            io:format("done~n"),
            Map#{result := maps_update_with(BoxId
                                           ,fun(Old) -> sets:add_element(FromId, Old) end
                                           ,sets:from_list([FromId])
                                           ,Result
                                           )
                ,moved := Moved + 1
                ,processed := Processed + 1
                };
        {'error', 'conflict'} ->
            io:format("done~n"),
            Map#{result := maps_update_with(BoxId
                                           ,fun(Old) -> sets:add_element(FromId, Old) end
                                           ,sets:from_list([FromId])
                                           ,Result
                                           )
                ,moved := Moved + 1
                ,processed := Processed + 1
                };
        {'error', _Reason} ->
            io:format("failed: ~p~n", [_Reason]),
            Map#{processed := Processed + 1
                ,skip := Skip + 1
                }
    end.

-spec transform_vm_doc_funs(kz_term:ne_binary(), kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary(), update_funs()}.
transform_vm_doc_funs(AccountDb, LegacyVMJObj) ->
    Metadata = kz_json:get_value(<<"metadata">>, LegacyVMJObj),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, Metadata),
    OldId = kz_json:get_value(<<"media_id">>, Metadata),
    OldCallId = kz_json:get_value(<<"call_id">>, Metadata),

    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),

    NewId = kazoo_modb_util:modb_id(Year, Month, OldId),
    NewCallId = kazoo_modb_util:modb_id(Year, Month, OldCallId),
    NewDb = kazoo_modb:get_modb(AccountDb, Year, Month),

    NewMetadata = kz_json:set_values(
                    [{<<"media_id">>, NewId}
                    ,{<<"call_id">>, NewCallId}
                    ], Metadata),
    {NewDb
    ,NewId
    ,[fun(J) -> kz_json:set_value(<<"metadata">>, NewMetadata, J) end
     ,fun(J) -> kz_doc:set_type(J, kzd_box_message:type()) end
     ,fun(J) -> kz_doc:set_account_db(J, NewDb) end
     ]
    }.

-spec update_mailboxes(kz_term:ne_binary(), map()) -> 'ok'.
update_mailboxes(AccountId, Map) ->
    BoxIds = maps:keys(Map),

    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_term:is_not_empty(BoxIds)
        andalso kz_datamgr:open_docs(AccountDb, BoxIds) of
        false -> ok;
        {'ok', BoxJObjs} ->
            NewBoxJObjs = update_mailbox_jobjs(BoxJObjs, Map),
            case kz_datamgr:save_docs(AccountDb, NewBoxJObjs) of
                {'ok', _} -> 'ok';
                {'error', _Reason} ->
                    io:format("[~s] failed to update mailbox message arrays: ~p~n", [AccountId, _Reason])
            end;
        {'error', _Reason} ->
            io:format("[~s] failed to open mailboxes for updating message arrays: ~p~n", [AccountId, _Reason])
    end.

-spec update_mailbox_jobjs(kz_json:objects(), map()) -> kz_json:objects().
update_mailbox_jobjs(BoxJObjs, Map) ->
    [update_message_array(kz_json:get_value(<<"doc">>, J), maps:get(kz_doc:id(J), Map))
     || J <- BoxJObjs
    ].

-spec update_message_array(kz_json:object(), sets:set()) -> kz_json:object().
update_message_array(BoxJObj, ResultSet) ->
    Messages = kz_json:get_value(<<"messages">>, BoxJObj),
    Fun = fun(Msg, Acc) ->
                  case sets:is_element(kz_json:get_value(<<"media_id">>, Msg), ResultSet) of
                      true -> Acc;
                      false -> [Msg|Acc]
                  end
          end,
    NewMessages = lists:foldl(Fun, [], Messages),
    kz_json:set_value(<<"messages">>, NewMessages, BoxJObj).

%%%=============================================================================
%%% Voicemail Migration
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_cdrs(kz_term:ne_binary()) -> 'ok'.
migrate_cdrs(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    Total = get_view_count(AccountId, <<"cdrs/crossbar_listing">>),
    migrate_cdrs(AccountId, Total).

-spec migrate_cdrs(kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
migrate_cdrs(_AccountId, 0) ->
    io:format("[~s] no cdrs found~n", [_AccountId]);
migrate_cdrs(AccountId, Total) ->
    Limit = kz_datamgr:max_bulk_read(),
    io:format("[~s] Start migrating total ~b cdrs with batch size ~b~n", [AccountId, Total, Limit]),
    Stats = #{total => Total
             ,processed => 0
             ,moved => 0
             ,skip => 0
             },
    migrate_cdrs(AccountId, Stats, [{'limit', Limit}, include_docs]).

-spec migrate_cdrs(kz_term:ne_binary(), map(), kz_term:proplist()) -> 'ok'.
migrate_cdrs(AccountId, #{total := Total, processed := Processed, moved := Moved, skip := Skip}=Stats, ViewOptions) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_datamgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} ->
            io:format("[~s] cdrs migration finished, (~b/~b) doc has been moved~n"
                     ,[AccountId, Moved, Total]
                     );
        {'ok', ViewResults} ->
            Length = length(ViewResults),
            io:format("[~s] processing ~b cdrs~n", [AccountId, Length]),

            MovedIds = move_cdrs_to_modb(AccountId, ViewResults),
            case kz_term:is_not_empty(MovedIds)
                andalso kz_datamgr:del_docs(AccountDb, MovedIds) of
                false -> ok;
                {'ok', _} -> 'ok';
                {'error', _Reason} ->
                    io:format("[~s] failed to remove moved cdrs from account db: ~p~n", [AccountId, _Reason])
            end,

            NewProcessed = Processed + Length,
            io:format("[~s] finished processing ~b cdrs, ~b remain~n", [AccountId, Length, Total - NewProcessed]),

            timer:sleep(1000),

            MovedLength = length(MovedIds),
            NewSkip = Skip + Length - MovedLength,
            Remaining = Total - MovedLength,
            migrate_cdrs(AccountId
                        ,Stats#{processed := NewProcessed
                               ,moved := Moved + MovedLength
                               ,skip := NewSkip
                               }
                        ,next_skip(Remaining, NewSkip, ViewOptions)
                        );
        {'error', _R} ->
            io:format("[~s] failed to fetch cdrs: ~p~n", [AccountId, _R])
    end.

-spec move_cdrs_to_modb(kz_term:ne_binary(), kz_json:objects()) -> kz_term:ne_binaries().
move_cdrs_to_modb(AccountId, ViewResults) ->
    Mapped = map_tranform_cdrs(AccountId, ViewResults, maps:new()),
    maps:fold(fun do_move_cdrs/3, [], Mapped).

-spec map_tranform_cdrs(kz_term:ne_binary(), kz_json:objects(), map()) -> map().
map_tranform_cdrs(_AccountId, [], Map) -> Map;
map_tranform_cdrs(AccountId, [VR|VRs], Map) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    Doc = kz_json:get_value(<<"doc">>, VR),
    OldId = kz_doc:id(Doc),
    Created = kz_doc:created(Doc),

    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Created),

    NewId = kazoo_modb_util:modb_id(Year, Month, OldId),
    NewDb = kazoo_modb:get_modb(AccountDb, Year, Month),

    TransformFuns = [fun(J) -> kz_doc:set_id(J, NewId) end
                    ,fun(J) -> kz_doc:delete_revision(J) end
                    ,fun(J) -> kz_doc:set_account_db(J, NewDb) end
                    ],
    NewDoc = lists:foldl(fun(F, J) -> F(J) end, Doc, TransformFuns),

    NewMap = maps_update_with(NewDb, fun(Old) -> [NewDoc|Old] end, [NewDoc], Map),
    map_tranform_cdrs(AccountId, VRs, NewMap).

-spec do_move_cdrs(kz_term:ne_binary(), kz_json:objects(), kz_term:ne_binaries()) ->
                          kz_term:ne_binaries().
do_move_cdrs(MODB, Docs, MovedAcc) ->
    _AccountId = kzs_util:format_account_id(MODB),
    case kz_datamgr:save_docs(MODB, Docs) of
        {'ok', SavedJObj} -> check_for_failure(SavedJObj, MovedAcc);
        {'error', _Reason} ->
            io:format("[~s] failed to move cdrs in batch: ~p~n", [_AccountId, _Reason]),
            MovedAcc
    end.

-spec check_for_failure(kz_json:objects(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
check_for_failure(JObjs, MovedAcc) ->
    Fun = fun(J, Acc) ->
                  case kz_json:get_value(<<"error">>, J) of
                      'undefined' ->
                          ?MATCH_MODB_PREFIX(_Year, _Month, OldId) = kz_doc:id(J),
                          [OldId|Acc];
                      <<"conflict">> ->
                          ?MATCH_MODB_PREFIX(_Year, _Month, OldId) = kz_doc:id(J),
                          [OldId|Acc];
                      _ ->
                          Acc
                  end
          end,
    lists:foldl(Fun, MovedAcc, JObjs).
