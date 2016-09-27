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

        ,migrate/1, migrate/2
        ]).

-include("kz_voicemail.hrl").

-define(SERVER, ?MODULE).

-define(MAX_BOX_PROCESS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_mailbox_process">>], 10)).

-define(TIME_BETWEEN_BOX_CRAWLS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interbox_delay_ms">>], 2 * ?MILLISECONDS_IN_SECOND)).

-define(DEFAULT_VM_EXTENSION,
        kapps_config:get(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"extension">>], <<"mp3">>)).

-record(state, {timer_ref = cleanup_mailbox_timer() :: reference()
               ,account_id :: ne_binary()
               ,start_key = 'undefined' :: api_seconds()
               ,read_offset = 0 :: integer()
               }).
-type state() :: #state{}.

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
-spec migrate(ne_binary()) -> 'ok'.
-spec migrate(ne_binary(), ne_binary() | ne_binaries()) -> 'ok'.
migrate(AccountId) when is_binary(AccountId) ->
    migrate(AccountId, []).

migrate(AccountId, ?NE_BINARY = BoxId) ->
    migrate(AccountId, [BoxId]);
migrate(AccountId, BoxIds) ->
    ViewOpts = props:filter_empty(
                 [{'keys', BoxIds}
                 ,'include_docs'
                 ]),
    case get_account_vmboxes(AccountId, ViewOpts) of
        {'ok', Results} ->
            do_migrate(AccountId, Results, 'undefined');
        _ ->
            'ok'
    end.


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
init(Account) ->
    kz_util:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),

    State = get_next_account(Account),
    {'ok', State#state{timer_ref = cleanup_mailbox_timer()
                      }}.

-spec get_next_account(next_account()) -> #state{}.
get_next_account({AccountId, StartKey}) ->
    #state{account_id = AccountId
          ,start_key = StartKey
          };
get_next_account(AccountId) ->
    #state{account_id = AccountId}.


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
                                          ,start_key = StartKey
                                          ,read_offset = Offset
                                          }=State) ->
    case crawl_account(AccountId, Offset) of
        {'ok', []} ->
            ?WARNING("=== voicemail migration for account ~s is finished, going down ===", [AccountId]),
            {'stop', 'normal', State};
        {'ok', BoxViewResults} ->
            do_migrate(AccountId, BoxViewResults, StartKey),
            {'noreply', State#state{timer_ref = cleanup_mailbox_timer()
                                   ,read_offset = length(BoxViewResults) - Offset
                                   }};
        {'error', _R} ->
            ?ERROR("=== failed to migrate voicemail messages for account ~s, going down: err ~p ==="
                  ,[AccountId, _R]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec crawl_account(ne_binary(), api_integer()) -> 'done' | db_ret().
crawl_account(_AccountId, Offset) when Offset < 0 -> 'done';
crawl_account(AccountId, Offset) ->
    ?WARNING("=== crawling account ~s with offset ~p ===", [AccountId, Offset]),

    ViewOpts = props:filter_empty(
                 [{'limit', ?MAX_BOX_PROCESS}
                 ,{'skip', Offset}
                 ,'include_docs'
                 ]),
    get_account_vmboxes(AccountId, ViewOpts).

%%--------------------------------------------------------------------
%% @private
%% @doc Create message docs and insert them into database in bulk
%% @end
%%--------------------------------------------------------------------
-spec do_migrate(ne_binary(), kz_json:objects(), api_seconds()) -> 'ok'.
do_migrate(AccountId, BoxViewResults, StartKey) ->
    ?WARNING("processing ~b voicemail boxes in account ~s", [length(BoxViewResults), AccountId]),

    Msgs = process_mailboxes(AccountId, BoxViewResults, StartKey),
    migrate_messages(AccountId, Msgs),
    update_mailboxes(AccountId, BoxViewResults, StartKey).

-spec migrate_messages(ne_binary(), dict:dict()) -> 'ok'.
migrate_messages(AccountId, MsgsDict) ->
    ?WARNING("migrating voicemail messages to ~b modbs for account_id ~s"
            ,[dict:size(MsgsDict), AccountId]),
    dict:fold(fun bulk_save_modb/3, [], MsgsDict).

-spec bulk_save_modb(ne_binary(), kz_json:objects(), list()) -> 'ok'.
bulk_save_modb(Db, Js, _Acc) ->
    bulk_save_modb(Db, Js).

-spec bulk_save_modb(ne_binary(), kz_json:objects()) -> 'ok'.
bulk_save_modb(Db, Js) ->
    case kz_datamgr:db_exists(Db)
        andalso kz_datamgr:save_docs(Db, Js)
    of
        {'ok', Saved} ->
            Failed = length(normalize_bulk_result(Db, Saved, [])),
            Success = length(Saved) - Failed,
            ?WARNING("bulk save vm messages resulted in ~b success and ~b failed in db ~s"
                    ,[Success, Failed, Db]);
        {'error', _R} ->
            ?ERROR("failed to migrate voicemail messages to db ~s: ~p"
                  ,[Db, _R]);
        'false' ->
            ?WARNING("modb ~s is not existed, saving voicemail metadata to private_media", [Db]),
            save_metadata_to_private_media(Db, Js)
    end.

-spec save_metadata_to_private_media(ne_binary(), kz_json:objects()) -> 'ok'.
save_metadata_to_private_media(MODb, MessageDocs) ->
    {MsgIds, Metadatas} = remove_modb_id_from_metadata(MessageDocs),
    ViewOpts = [{'keys', MsgIds}
               ,'include_docs'
               ],
    Db = kz_util:format_account_db(MODb),
    case kz_datamgr:all_docs(Db, ViewOpts) of
        {'ok', JObjs} ->
            Failed = normalize_private_media_results(Db, JObjs, []),
            bulk_save_private_media(Db, JObjs, Metadatas, Failed);
        {'error', R} ->
            ?ERROR("failed to bulk fetch voicemail messages from db ~s: ~p"
                  ,[Db, R])
    end.

-spec bulk_save_private_media(ne_binary(), kz_json:objects(), kz_json:objects(), ne_binaries()) -> 'ok'.
bulk_save_private_media(Db, JObjs, Metadatas, Failed) ->
    Fun = fun(M, Acc) ->
                  Id = kzd_box_message:media_id(M),
                  MDoc = kz_json:find_value(<<"_id">>, Id, JObjs),
                  [kzd_box_message:set_metadata(M, MDoc) | Acc]
          end,
    ToSave = lists:foldl(Fun, [], Metadatas),
    case kz_datamgr:save_docs(Db, ToSave) of
        {'ok', Saved} ->
            Norm = normalize_bulk_result(Db, Saved, []),
            Failed = length([Norm]),
            Success = length(Saved) - Failed,
            ?WARNING("bulk save vm private_media resulted in ~b success and ~b failed in db ~s"
                    ,[Success, Failed, Db]);
        {'error', R} ->
            ?ERROR("failed to save voicemail messages metadata to their private media in db ~s: ~p"
                  ,[Db, R])
    end.

-spec remove_modb_id_from_metadata(kz_json:objects()) -> {ne_binaries(), kz_json:objects()}.
remove_modb_id_from_metadata(MsgDocs) ->
    Fun = fun(J, {Ids, Metas}) ->
                  M = kzd_box_message:metadata(J),
                  <<_:7/binary, Id/binary>> = kzd_box_message:media_id(M),
                  {[Id | Ids], [kzd_box_message:set_media_id(Id, M) | Metas]}
          end,
    lists:foldl(Fun, {[], []}, MsgDocs).

-spec normalize_private_media_results(ne_binary(), kz_json:objects(), ne_binaries()) -> ne_binaries().
normalize_private_media_results(_AccountDb, [], Acc) -> Acc;
normalize_private_media_results(AccountDb, [JObj | JObjs], Acc) ->
    case normalize_bulk_result(JObj) of
        {_Id, 'undefined'} -> normalize_private_media_results(AccountDb, JObjs, Acc);
        {Id, _Error} ->
            ?ERROR("failed to get private_media doc for message ~s in db ~s: ~p", [Id, AccountDb, _Error]),
            normalize_private_media_results(AccountDb, JObjs, [Id | Acc])
    end.

-spec normalize_bulk_result(ne_binary(), kz_json:objects(), ne_binaries()) -> ne_binaries().
normalize_bulk_result(_Db, [], Acc) -> Acc;
normalize_bulk_result(Db, [JObj | JObjs], Acc) ->
    %% just logging error messages
    case normalize_bulk_result(JObj) of
        {Id, 'undefined'} ->
            normalize_bulk_result(Db, JObjs, [Id | Acc]);
        {Id, _Error} ->
            ?ERROR("failed to voicemail message ~s in db ~s: ~p", [Id, Db, _Error]),
            normalize_bulk_result(Db, JObjs, [Id | Acc])
    end.

-spec normalize_bulk_result(kz_json:object()) -> {ne_binary(), any()}.
normalize_bulk_result(JObj) ->
    Id = kz_json:get_first_defined([<<"key">>, <<"id">>, <<"_id">>], JObj),
    {Id, kz_json:get_value(<<"error">>, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_mailboxes(kz_json:object(), kz_json:objects(), api_seconds()) -> 'ok'.
update_mailboxes(AccountId, BoxViewResults, StartKey) ->
    BoxJObjs = [maybe_clear_message_array(J, StartKey)
                || J <- BoxViewResults
               ],
    _ = kz_datamgr:save_docs(kvm_util:get_db(AccountId), BoxJObjs),
    'ok'.

-spec maybe_clear_message_array(kz_json:object(), api_seconds()) -> kz_json:object().
maybe_clear_message_array(Box, 'undefined') ->
    BoxJObj = kz_json:get_value(<<"doc">>, Box),
    kz_json:set_value(<<"messages">>, [], BoxJObj);
maybe_clear_message_array(Box, StartKey) ->
    BoxJObj = kz_json:get_value(<<"doc">>, Box),
    Msgs = [Message
            || Message <- kz_json:get_value(<<"messages">>, BoxJObj, []),
               not check_message(Message, StartKey)
           ],
    kz_json:set_value(<<"messages">>, Msgs, BoxJObj).

-spec get_account_vmboxes(ne_binary(), kz_proplist()) -> db_ret().
get_account_vmboxes(AccountId, ViewOpts) ->
    Db = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(Db, ?VMBOX_CB_LIST, ViewOpts) of
        {'ok', []} = OK ->
            ?WARNING("no more voicemail box left to process in ~s", [AccountId]),
            OK;
        {'ok', _} = OK -> OK;
        {'error', _E} = Error ->
            ?ERROR("failed to get voicemail boxes in ~s: ~p", [AccountId, _E]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc get messages array from each mailbox and generate a new message doc
%% for them, then map it to the proper MODB
%%
%% Note: We are moving metadata only, attachment still remains in AccountDb,
%% This is so much faster than moving with attachments which probably takes
%% a couple of days for huge systems. Because if that we are just creating
%% message docs from scratch based on message metadata from each mailbox,
%% and we use kazoo_data bulk operation for faster db writes.
%% @end
%%--------------------------------------------------------------------
-spec process_mailboxes(ne_binary(), kz_json:objects(), api_seconds()) -> kz_json:objects().
process_mailboxes(AccountId, JObjs, StartKey) ->
    DefaultExt = ?DEFAULT_VM_EXTENSION,
    Fun = fun(J, Acc) -> process_mailbox(AccountId, J, DefaultExt, Acc, StartKey) end,
    lists:foldl(Fun, dict:new(), JObjs).

-spec process_mailbox(ne_binary(), kz_json:objects(), ne_binary(), dict:dict(), api_seconds()) -> kz_json:objects().
process_mailbox(AccountId, JObj, DefaultExt, Dict, StartKey) ->
    BoxJObj = kz_json:get_value(<<"doc">>, JObj),
    Metadatas = kz_json:get_value(<<"messages">>, BoxJObj, []),

    Fun = fun(M, D) ->
                  case check_message(M, StartKey) of
                      'false' -> D;
                      'true' ->
                          Msg = process_message(AccountId, BoxJObj, M, DefaultExt),
                          dict:append(kz_doc:account_db(Msg), Msg, D)
                  end
          end,
    lists:foldl(Fun, Dict, Metadatas).

-spec check_message(kz_json:object(), api_seconds()) -> boolean().
check_message(M, StartKey) ->
    not kz_util:is_empty(M)
        andalso maybe_check_timestamp(kz_json:get_value(<<"timestamp">>, M), StartKey).

-spec maybe_check_timestamp(gregorian_seconds(), api_seconds()) -> boolean().
maybe_check_timestamp(_Timestamp, 'undefined') -> 'true';
maybe_check_timestamp(Timestamp, StartKey) when Timestamp >= StartKey -> 'true';
maybe_check_timestamp(_Timestamp, _StartKey) -> 'false'.

-spec process_message(ne_binary(), kz_json:object(), kz_json:object(), ne_binary()) -> kz_json:object().
process_message(AccountId, BoxJObj, Message, DefaultExt) ->
    BoxNum = kzd_voicemail_box:mailbox_number(BoxJObj),
    TimeZone = kzd_voicemail_box:timezone(BoxJObj),
    Timestamp = kz_json:get_value(<<"timestamp">>, Message),

    %% setting a db_link as attachment
    AttName = <<(kz_util:rand_hex_binary(16))/binary, ".", DefaultExt/binary>>,
    AttHandlerProps = [{<<"att_dbname">>, kz_util:format_account_db(AccountId)}
                      ,{<<"att_docid">>, kzd_box_message:media_id(Message)}
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
              ,{<<"Media-ID">>, kzd_box_message:media_id(Message)}
              ,{<<"Box-Num">>, BoxNum}
              ,{<<"Timezone">>, TimeZone}
              ,{<<"Message-Timestamp">>, Timestamp}
              ,{<<"Document-Timestamp">>, Timestamp}
              ,{<<"Attachment-Name">>, AttName}
              ]),
    Msg = kzd_box_message:new(AccountId, Props),
    UpdateProps =
        [{<<"metadata">>, kzd_box_message:set_media_id(kz_doc:id(Msg), Message)}
        ,{<<"pvt_moved_to_modb">>, <<"true">>}
        ,{<<"pvt_previous_id">>, kzd_box_message:media_id(Message)}
        ,{<<"pvt_attachments">>, Att}
        ],
    kz_json:set_values(UpdateProps, Msg).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_mailbox_timer() -> reference().
cleanup_mailbox_timer() ->
    erlang:start_timer(?TIME_BETWEEN_BOX_CRAWLS, self(), 'ok').
