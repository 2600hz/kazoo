%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_migrate_account).
-behaviour(gen_server).

-export([start/1, start_link/1
        ,stop/0, stop/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3

        % ,migrate/1, migrate/2
        ]).

-include("kz_voicemail.hrl").

-define(SERVER, ?MODULE).

-define(TIMEOUT, 2 * ?MILLISECONDS_IN_SECOND).

-define(DEFAULT_VM_EXTENSION,
        kapps_config:get(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"extension">>], <<"mp3">>)).

-define(MAX_BULK_INSERT,
        kapps_config:get(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_bulk_insert">>], kz_datamgr:max_bulk_insert())).

-define(LEGACY_MSG_LISTING, <<"vmboxes/legacy_msg_by_timestamp">>).

-record(state, {timer_ref = cleanup_timer() :: reference()
               ,account_id :: ne_binary()
               ,first_of_month :: gregorian_seconds()
               ,last_of_month :: gregorian_seconds()
               }).
-type state() :: #state{}.

-define(TOTAL_MESSAGES, 'total_messages').
-define(TOTAL_SUCCEEDED, 'total_succeeded').
-define(TOTAL_FAILED, 'total_failed').
-define(FAILED, 'moved_failed').
-define(FAILED_MODB, 'no_modb').
-define(SUCCEEDED, 'succeeded').

-define(DEBUG(Format, Args),
        lager:debug(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-define(WARNING(Format, Args),
        lager:warning(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-define(ERROR(Format, Args),
        lager:error(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-type migrate_stats() :: non_neg_integer() |
                         ne_binary() |
                         ne_binaries() |
                         {ne_binary(), atom()} |
                         [{ne_binary(), atom()}] |
                         'undefined'.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start(next_account()) -> startlink_ret().
start(Account) ->
    gen_server:start(?SERVER, Account, []).

-spec start_link(next_account()) -> startlink_ret().
start_link(Account) ->
    gen_server:start_link(?SERVER, Account, []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?SERVER, 'stop').

-spec stop(server_ref()) -> 'ok'.
stop(Pid) ->
    gen_server:cast(Pid, 'stop').

%%--------------------------------------------------------------------
%% @public
%% @doc Triggers migration for an Account or a list of mailboxes
%% @end
%%--------------------------------------------------------------------
% -spec migrate(ne_binary()) -> 'ok'.
% -spec migrate(ne_binary(), ne_binary() | ne_binaries()) -> 'ok'.
% migrate(AccountId) when is_binary(AccountId) ->
%     migrate(AccountId, []).

% migrate(AccountId, ?NE_BINARY = BoxId) ->
%     migrate(AccountId, [BoxId]);
% migrate(AccountId, BoxIds) ->
%     case get_account_vmboxes(AccountId, BoxIds) of
%         {'ok', []} ->
%             ?ERROR("=== no voicemail box with messages found in account ~s ===", [AccountId]),
%         {'ok', BoxIds} ->
%             do_migrate(AccountId, BoxIds, 'undefined');
%         _ ->
%             'ok'
%     end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%--------------------------------------------------------------------
-spec init(next_account()) -> {'ok', state()} |
                              {'stop', state()}.
init({AccountId, FirstOfMonth, LastOfMonth}) ->
    kz_util:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),

    {'ok', #state{timer_ref = cleanup_timer()
                 ,account_id = AccountId
                 ,first_of_month = FirstOfMonth
                 ,last_of_month = LastOfMonth
                 }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("~p has been stopped", [?MODULE]),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'timeout', Ref, _Msg}, #state{timer_ref = Ref
                                          ,account_id = AccountId
                                          ,first_of_month = FirstOfMonth
                                          ,last_of_month = LastOfMonth
                                          }=State) ->
    ?WARNING("=== start migrating vm messages in account ~s ===", [AccountId]),

    ViewOpts = props:filter_empty(
                 [{'limit', ?MAX_BULK_INSERT}
                 ,{'startkey', LastOfMonth}
                 ,{'endkey', FirstOfMonth}
                 ,'descending'
                 ]),
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?LEGACY_MSG_LISTING, ViewOpts) of
        {'ok', []} ->
            ?WARNING("no legacy voicemail messages left in account ~s (last timestamp proccessed: ~p), going down", [AccountId, FirstOfMonth]),
            kvm_migrate_crawler:account_is_done(AccountId, FirstOfMonth, LastOfMonth),
            {'stop', 'normal', State};
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults),
            migration_result(AccountId, FirstOfMonth, LastOfMonth),
            {'stop', 'normal', State};
        {'error', R} ->
            lager:warning("failed to fetch legacy voicemail message view result for ~s: ~p"
                         ,[AccountId, R]),
            kvm_migrate_crawler:account_maybe_failed(AccountId, FirstOfMonth, LastOfMonth, R),
            {'stop', 'normal', State}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?SERVER, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec migration_result(ne_binary(), gregorian_seconds(), gregorian_seconds()) -> 'ok'.
migration_result(AccountId, FirstOfMonth, LastOfMonth) ->
    TotalMsgs = get_stats(?TOTAL_MESSAGES),
    TotalSucceeded = get_stats(?TOTAL_SUCCEEDED),
    TotalFailed = get_stats(?TOTAL_FAILED),
    MODbFailed = length(get_stats(?FAILED_MODB)),

    Props = [{<<"total_messages">>, TotalMsgs}
            ,{<<"total_succeeded">>, TotalSucceeded}
            ,{<<"total_failed">>, TotalFailed}
            ],
    case TotalMsgs == MODbFailed of
        'true' ->
            kvm_migrate_crawler:worker_finished(AccountId, Props),
            kvm_migrate_crawler:account_is_done(AccountId, FirstOfMonth, LastOfMonth),
            ?WARNING("reached to the latest avialable modb for account ~s", [AccountId]);
        'false' ->
            kvm_migrate_crawler:worker_finished(AccountId, Props),
            ?WARNING("finished a migrate cycle for account ~s: succeeded ~b failed ~b no_modb ~b"
                    ,[AccountId, TotalSucceeded, TotalFailed, MODbFailed])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Process messages and do migrate
%% @end
%%--------------------------------------------------------------------
-spec migrate_messages(ne_binary(), kz_json:objects()) -> 'ok'.
migrate_messages(AccountId, ViewResults) ->
    MsgCount = length(ViewResults),
    _ = update_process_key(?TOTAL_MESSAGES, MsgCount),

    ?WARNING("processing ~b voicemail messages in account ~s", [MsgCount, AccountId]),
    MsgsDict = process_messages(AccountId, ViewResults),
    maybe_migrate(AccountId, ViewResults, MsgsDict, dict:fetch_keys(MsgsDict)).

%%--------------------------------------------------------------------
%% @private
%% @doc Check Db existence and process with migration
%% @end
%%--------------------------------------------------------------------
-spec maybe_migrate(ne_binary(), kz_json:objects(), kz_json:objects(), ne_binaries() | non_neg_integer()) -> 'ok'.
maybe_migrate(AccountId, ViewResults, MsgsDict, Dbs) when is_list(Dbs) ->
    NewMsgsDict = check_dbs_existence(Dbs, MsgsDict),
    maybe_migrate(AccountId, ViewResults, NewMsgsDict, dict:size(NewMsgsDict));
maybe_migrate(_AccountId, _ViewResults, _MsgsDict, 0) ->
    ?WARNING("none of modbs for proccessed messages in account ~s is exists", [_AccountId]);
maybe_migrate(AccountId, ViewResults, MsgsDict, _DbCount) ->
    do_migrate(AccountId, MsgsDict),
    update_mailboxes(AccountId, ViewResults).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec do_migrate(ne_binary(), dict:dict()) -> 'ok'.
do_migrate(AccountId, MsgsDict) ->
    ?WARNING("migrating voicemail messages to ~b modbs for account_id ~s"
            ,[dict:size(MsgsDict), AccountId]),
    dict:fold(fun bulk_save_modb/3, [], MsgsDict).

-spec bulk_save_modb(ne_binary(), kz_json:objects(), list()) -> 'ok'.
bulk_save_modb(Db, Js, _Acc) ->
    case kz_datamgr:save_docs(Db, Js) of
        {'ok', Saved} ->
            {Success, Failed} = normalize_bulk_result(Db, Saved),
            ?WARNING("bulk save vm messages resulted in ~b success and ~b failed in db ~s"
                    ,[Success, Failed, Db]);
        {'error', R} ->
            update_stats(?FAILED, Js, R),
            ?ERROR("failed to migrate voicemail messages to db ~s: ~p"
                  ,[Db, R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_mailboxes(kz_json:object(), kz_json:objects()) -> 'ok'.
update_mailboxes(AccountId, ViewResults) ->
    Succeeded = dict:from_list(get_stats(?SUCCEEDED)),
    MODbFailed = dict:from_list(get_stats(?FAILED_MODB)),
    Failed = dict:from_list(get_stats(?FAILED)),
    BoxIds = lists:usort([kz_doc:id(B) || B <- ViewResults]),

    ViewOpts = [{'keys', BoxIds}
               ,'include_docs'
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:all_docs(Db, ViewOpts) of
        {'ok', BoxJObjs} ->
            NewBoxJObjs = [update_message_array(Box, Succeeded, MODbFailed, Failed)
                           || Box <- BoxJObjs
                          ],
            case kz_datamgr:save_docs(Db, NewBoxJObjs) of
                {'ok', _} -> 'ok';
                {'error', R} ->
                    failed_to_update_mailbox(ViewResults, R)
            end;
        {'error', R} ->
            failed_to_update_mailbox(ViewResults, R)
    end.

failed_to_update_mailbox(ViewResults, Reason) ->
    %% nuke process stats
    _ = erlang:erase(?SUCCEEDED),
    _ = update_process_key(?TOTAL_SUCCEEDED, 0),
    Failed = [{kz_json:get_value([<<"value">>, <<"metadata">>, <<"media_id">>], M), Reason}
              || M <- ViewResults
             ],
    _ = erlang:put(?FAILED, Failed),
    _ = erlang:put(?TOTAL_FAILED, length(Failed)),
    'ok'.

-spec update_message_array(kz_json:object(), dict:dict(), dict:dict(), dict:dict()) -> kz_json:object().
update_message_array(BoxJObj, Succeeded, MODbFailed, Failed) ->
    Messages = kz_json:get_value(<<"messages">>, BoxJObj),
    Fun = fun(Msg, Acc) ->
              MsgId = kzd_box_message:media_id(Msg),
              S = dict:is_key(MsgId, Succeeded),
              M = dict:is_key(MsgId, MODbFailed),
              F = dict:is_key(MsgId, Failed),

              case {S, M, F} of
                  {'true', _, _} -> Acc;
                  {_, 'true', _} ->
                      Error = dict:fetch(MsgId, MODbFailed),
                      [kz_json:set_value(<<"migration_error">>, kz_util:to_binary(Error)) | Acc];
                  {_, _, 'true'} ->
                      Error = dict:fetch(MsgId, Failed),
                      [kz_json:set_value(<<"migration_error">>, kz_util:to_binary(Error)) | Acc]
              end
          end,
    NewMessages = lists:foldl(Fun, [], Messages),
    kz_json:set_value(?VM_KEY_MESSAGES, NewMessages, BoxJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
% -spec get_account_vmboxes(ne_binary(), api_ne_binaries()) -> db_ret().
% get_account_vmboxes(AccountId, 'undefined') ->
%     get_account_vmboxes(AccountId, []);
% get_account_vmboxes(AccountId, ExpectedBoxIds) ->
%     Db = kz_util:format_account_db(AccountId),
%     case kz_datamgr:get_results(Db, ?VMBOX_CB_LIST) of
%         {'ok', JObjs} -> normalize_mailbox_results(JObjs, ExpectedBoxIds);
%         {'error', _E} = Error ->
%             ?ERROR("failed to get voicemail boxes in ~s: ~p", [AccountId, _E]),
%             Error
%     end.

% -spec normalize_mailbox_results(kz_json:objects(), ne_binaries()) -> ne_binaries().
% normalize_mailbox_results(JObjs, ExpectedBoxIds) ->
%     [kz_doc:id(J)
%      || J <- JObjs,
%         lists:member(kz_doc:id(J), ExpectedBoxIds),
%         has_messages(J)
%     ].

% -spec has_messages(kz_json:object()) -> boolean().
% has_messages(JObj) ->
%     Count = kz_json:get_integer_value([<<"value">>, ?VM_KEY_MESSAGES], JObj),
%     _ = update_process_key('total_messages', Count),
%     Count > 0.

%%--------------------------------------------------------------------
%% @private
%% @doc Check Db existence and remove messages that non exists dbs
%% @end
%%--------------------------------------------------------------------
-spec check_dbs_existence(ne_binaries(), dict:dict()) -> dict:dict().
check_dbs_existence([], MsgsDict) -> MsgsDict;
check_dbs_existence([Db | Dbs], MsgsDict) ->
    case kz_datamgr:db_exists(Db) of
        'true' ->
            check_dbs_existence(Dbs, MsgsDict);
        'false' ->
            ?WARNING("modb ~s is not exists", [Db]),
            update_stats(?FAILED_MODB, dict:fetch(Db, MsgsDict), <<"modb_not_exists">>),
            dict:erase(Db, MsgsDict)
    end.

%--------------------------------------------------------------------
%% @private
%% @doc Normalize bulk save results and update stats accordingly
%% @end
%%--------------------------------------------------------------------
-spec normalize_bulk_result(ne_binary(), kz_json:objects()) ->
                                    {non_neg_integer(), non_neg_integer()}.
-spec normalize_bulk_result(ne_binary(), kz_json:objects(), dict:dict()) ->
                                    {non_neg_integer(), non_neg_integer()}.
normalize_bulk_result(Db, Saved) ->
    DefaultDict = dict:from_list([{<<"succeeded">>, []}
                                 ,{<<"failed">>, []}
                                 ]),
    normalize_bulk_result(Db, Saved, DefaultDict).

normalize_bulk_result(_Db, [], Dict) ->
    Succeeded = dict:fetch(<<"succeeded">>, Dict),
    Failed = dict:fetch(<<"failed">>, Dict),
    update_stats(?FAILED, Failed),
    update_stats(?SUCCEEDED, Succeeded),
    {length(Succeeded), length(Failed)};
normalize_bulk_result(Db, [S | Saved], Dict) ->
    Id = kz_json:get_first_defined([<<"key">>, <<"id">>, <<"_id">>], S),
    case kz_json:get_value(<<"error">>, S) of
        'undefined' ->
            %% successful
            normalize_bulk_result(Db, Saved, dict:append(<<"succeeded">>, Id, Dict));
        'conflict' ->
            %% successful
            normalize_bulk_result(Db, Saved, dict:append(<<"succeeded">>, Id, Dict));
        Reason ->
            ?ERROR("failed to save voicemail message ~s in db ~s: ~p", [Id, Db, Reason]),
            normalize_bulk_result(Db, Saved, dict:append(<<"failed">>, {Id, Reason}, Dict))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc process legacy message view result, generate a new message doc
%% for them and map them to proper modb.
%%
%% Note: We are moving metadata only, attachment still remains in AccountDb,
%% This is so much faster than moving with attachments which probably
%% takes a couple of days for huge systems. We are creating message docs
%% from scratch based on message metadata from each mailbox, and we use
%% kazoo_data bulk operation for faster db writes.
%% @end
%%--------------------------------------------------------------------
-spec process_messages(ne_binary(), kz_json:objects()) -> dict:dict().
process_messages(AccountId, JObjs) ->
    DefaultExt = ?DEFAULT_VM_EXTENSION,
    Fun = fun(J, Acc) ->
              Doc = create_message(AccountId, J, DefaultExt),
              dict:append(kz_doc:account_db(Doc), Doc, Acc)
          end,
    lists:foldl(Fun, dict:new(), JObjs).

-spec create_message(ne_binary(), kz_json:object(), ne_binary()) -> kz_json:object().
create_message(AccountId, FakeBoxJObj, DefaultExt) ->
    BoxJObj = kz_json:get_value(<<"value">>, FakeBoxJObj),

    BoxNum = kzd_voicemail_box:mailbox_number(BoxJObj),
    TimeZone = kzd_voicemail_box:timezone(BoxJObj),

    Metadata = kzd_box_message:metadata(BoxJObj),
    Timestamp = kz_json:get_value(<<"timestamp">>, Metadata),

    %% setting a db_link as attachment
    AttName = <<(kz_util:rand_hex_binary(16))/binary, ".", DefaultExt/binary>>,
    AttHandlerProps = [{<<"att_dbname">>, kz_util:format_account_db(AccountId)}
                      ,{<<"att_docid">>, kzd_box_message:media_id(Metadata)}
                      ],
    AttHandler = kz_json:from_list([{<<"kz_att_link">>, kz_json:from_list(AttHandlerProps)}]),
    AttProps = [{<<"content_type">>, kz_mime:from_extension(DefaultExt)}
               ,{<<"length">>, 0}
               ,{<<"stub">>, 'false'}
               ,{<<"handler">>, AttHandler}
               ],
    Att = kz_json:from_list([{AttName, kz_json:from_list(AttProps)}]),

    Props = props:filter_undefined(
              [{<<"Box-Id">>, kz_doc:id(BoxJObj)}
              ,{<<"Media-ID">>, kzd_box_message:media_id(Metadata)}
              ,{<<"Box-Num">>, BoxNum}
              ,{<<"Timezone">>, TimeZone}
              ,{<<"Message-Timestamp">>, Timestamp}
              ,{<<"Document-Timestamp">>, Timestamp}
              ,{<<"Attachment-Name">>, AttName}
              ]),
    Msg = kzd_box_message:new(AccountId, Props),
    UpdateProps =
      [{<<"metadata">>, kzd_box_message:set_media_id(kz_doc:id(Msg), Metadata)}
      ,{<<"pvt_moved_to_modb">>, <<"true">>}
      ,{<<"pvt_previous_id">>, kzd_box_message:media_id(Metadata)}
      ,{<<"pvt_attachments">>, Att}
      ],
    kz_json:set_values(UpdateProps, Msg).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_timer() -> reference().
cleanup_timer() ->
    erlang:start_timer(?TIMEOUT, self(), 'ok').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_stats(atom()) -> migrate_stats().
get_stats(Key) ->
    case erlang:get(Key) of
        'undefined' ->
            return_default_value(Key);
        Value -> Value
    end.

-spec return_default_value(atom()) -> migrate_stats().
return_default_value(?TOTAL_MESSAGES) -> 0;
return_default_value(?TOTAL_FAILED) -> 0;
return_default_value(?TOTAL_SUCCEEDED) -> 0;
return_default_value(_) -> [].

-spec update_stats(atom(), migrate_stats()) -> 'ok'.
-spec update_stats(atom(), kz_json:objects(), any()) -> 'ok'.
update_stats(Key, Values) ->
    _ = update_process_total_key(Key, length(Values)),
    _ = update_process_key(Key, Values),
    'ok'.

update_stats(Key, Msgs, Reason) ->
    NewStats = [{kz_doc:id(M), Reason}
                || M <- Msgs
               ],
    _ = update_process_total_key(Key, length(NewStats)),
    _ = update_process_key(Key, NewStats),
    'ok'.

-spec update_process_key(atom(), migrate_stats()) -> migrate_stats().
update_process_key(Key, Value) ->
    case erlang:get(Key) of
        'undefined' ->
            erlang:put(Key, Value);
        OldVal ->
            case is_list(Value) of
                'true' ->
                    erlang:put(Key, lists:flatten(OldVal, Value));
                'false' when is_integer(OldVal) ->
                    erlang:put(Key, OldVal + Value);
                'false' when is_binary(OldVal) ->
                    erlang:put(Key, [Value, OldVal]);
                'false' ->
                    erlang:put(Key, [Value | OldVal])
            end
    end.

-spec update_process_total_key(atom(), non_neg_integer()) -> migrate_stats().
update_process_total_key(?SUCCEEDED, Count) ->
    update_process_key(?TOTAL_SUCCEEDED, Count);
update_process_total_key(?FAILED, Count) ->
    update_process_key(?TOTAL_FAILED, Count);
update_process_total_key(?FAILED_MODB, Count) ->
    update_process_key(?TOTAL_FAILED, Count).
