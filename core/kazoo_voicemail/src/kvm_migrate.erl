%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_migrate).
-behaviour(gen_server).

-export([start/0, stop/1]).
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

-define(TIME_BETWEEN_ACCOUNT_CRAWLS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interaccount_delay_ms">>], 10 * ?MILLISECONDS_IN_SECOND)).

-define(MAX_BULK_INSERT,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_bulk_insert">>], kz_datamgr:max_bulk_insert())).

%% -define(SHOULD_MIGRATE_TO_CURRENT_MODB,
%%         kapps_config:get_is_true(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"should_migrate_to_current_modb">>], 'true')).

%% -define(SHOULD_MIGRATE_VM_MEDIA,
%%         kapps_config:get_is_true(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"should_migrate_vm_media">>], 'false')).

-record(state, {timer_ref = cleanup_account_timer() :: reference()
               ,account_ids = [] :: ne_binaries()
               ,read_offset = 0 :: api_integer()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start() -> startlink_ret().
start() ->
    gen_server:start(?SERVER, [], []).

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
            do_migrate(AccountId, Results);
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
-spec init([]) -> {'ok', state()} |
                  {'stop', state()}.
init([]) ->
    kz_util:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),
    case kapps_util:get_all_accounts('raw') of
        [] ->
            lager:info("no account found, going down", []),
            {'stop', #state{}};
        AccountIds ->
            lager:info("beginning crawling ~b accounts to migrate voicemail messages"
                      ,[length(AccountIds)]),
            {'ok', #state{timer_ref = cleanup_account_timer()
                         ,account_ids = kz_util:shuffle_list(AccountIds)
                         }}
    end.

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
                                          ,account_ids = []
                                          }=State) ->
    lager:info("voicemail migration finished, going down"),
    {'stop', 'normal', State};
handle_info({'timeout', Ref, _Msg}, #state{timer_ref = Ref
                                          ,account_ids = [AccountId | AccountIds]
                                          ,read_offset = Offset
                                          }=State) ->
    case crawl_account(AccountId, Offset) of
        {'ok', NewOffset} ->
            {'noreply', State#state{timer_ref = cleanup_mailbox_timer()
                                   ,read_offset = NewOffset
                                   }};
        _ ->
            lager:info("voicemail migration for account ~s is finished, ~b accounts remain to process"
                      ,[AccountId, length(AccountIds)]),
            {'noreply', State#state{timer_ref = cleanup_account_timer()
                                   ,account_ids = AccountIds
                                   ,read_offset = 0
                                   }}
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
    lager:info("crawling account ~s with offset ~p", [AccountId, Offset]),

    ViewOpts = props:filter_undefined(
                 [{'limit', ?MAX_BOX_PROCESS}
                 ,{'skip', Offset}
                 ,'include_docs'
                 ]),
    case get_account_vmboxes(AccountId, ViewOpts) of
        {'ok', Results} ->
            do_migrate(AccountId, Results),
            {'ok', length(Results) - Offset};
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
do_migrate(AccountId, BoxViewResults) ->
    lager:info("processing ~b voicemail boxes in account ~s", [length(BoxViewResults), AccountId]),

    Msgs = process_mailboxes(AccountId, BoxViewResults),
    maybe_migrate_messages(AccountId, Msgs),
    update_mailboxes(AccountId, BoxViewResults).

-spec maybe_migrate_messages(ne_binary(), kz_json:objects()) -> 'ok'.
maybe_migrate_messages(_AccountId, []) -> 'ok';
maybe_migrate_messages(AccountId, Messages) ->
    lager:info("migrating ~b voicemail messages from account ~s", [length(Messages), AccountId]),
    migrate_messages(AccountId, Messages).

-spec migrate_messages(ne_binary(), kz_json:objects()) -> 'ok'.
migrate_messages(_AccountId, []) -> 'ok';
migrate_messages(AccountId, Messages) ->
    Db = kazoo_modb:get_modb(AccountId),
    try lists:split(?MAX_BULK_INSERT, Messages) of
        {Batch, NextBatch} ->
            lager:info("inserting ~b voicemail messages to ~s", [length(Batch), Db]),
            save_messages(Db, Batch),
            migrate_messages(AccountId, NextBatch)
    catch
        'error':'badarg' ->
            lager:info("inserting ~b voicemail messages to ~s", [length(Messages), Db]),
            save_messages(Db, Messages)
    end.

update_mailboxes(AccountId, BoxViewResults) ->
    BoxJObjs = [kz_json:set_value(<<"messages">>
                                 ,[]
                                 ,kz_json:get_value(<<"doc">>, J))
                || J <- BoxViewResults
               ],
    _ = kz_datamgr:save_docs(kvm_util:get_db(AccountId), BoxJObjs),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_messages(ne_binary(), kz_json:objects()) -> 'ok'.
save_messages(Db, Messages) ->
    case kz_datamgr:save_docs(Db, Messages) of
        {'ok', _S} ->
            lager:info("successfully saved ~b vm messages", [length(_S)]);
        {'error', _E} ->
            lager:info("failed to save vm messages: ~p", [_E])
    end.

-spec get_account_vmboxes(ne_binary(), kz_proplist()) -> db_ret().
get_account_vmboxes(AccountId, ViewOpts) ->
    Db = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(Db, ?VMBOX_CB_LIST, ViewOpts) of
        {'ok', []} ->
            lager:info("no more voicemail box left to process in ~s", [AccountId]),
            {'error', 'not_found'};
        {'ok', _} = OK ->
            OK;
        {'error', _E} = Error ->
            lager:info("failed to get voicemail boxes in ~s: ~p", [AccountId, _E]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec process_mailboxes(ne_binary(), kz_json:objects()) -> kz_json:objects().
-spec process_mailboxes(ne_binary(), kz_json:objects(), kz_json:objects()) -> kz_json:objects().
process_mailboxes(AccountId, JObjs) ->
    Fun = fun(J, Acc) -> process_mailboxes(AccountId, J, Acc) end,
    lists:foldl(Fun, [], JObjs).

process_mailboxes(AccountId, JObj, Acc) ->
    BoxJObj = kz_json:get_value(<<"doc">>, JObj),
    Metadatas = kz_json:get_value(<<"messages">>, BoxJObj, []),

    Msgs = [process_message(AccountId, BoxJObj, M)
            || M <- Metadatas,
               not kz_util:is_empty(M)
           ],
    case Msgs of
        [] -> Acc;
        _ -> lists:flatten([Msgs | Acc])
    end.

-spec process_message(ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
process_message(AccountId, BoxJObj, Message) ->
    BoxNum = kzd_voicemail_box:mailbox_number(BoxJObj),
    TimeZone = kzd_voicemail_box:timezone(BoxJObj),
    Timestamp = kz_json:get_value(<<"timestamp">>, Message),
    AttName = <<(kz_util:rand_hex_binary(16))/binary, ".lnk">>,
    AttHandlerProps = [{<<"att_dbname">>, kz_util:format_account_db(AccountId)}
                      ,{<<"att_docid">>, kzd_box_message:media_id(Message)}
                      ],
    AttHandler = kz_json:from_list([{<<"kz_att_link">>, kz_json:from_list(AttHandlerProps)}]),
    AttProps = [{<<"content_type">>, <<"kazoo/dblink">>}
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
               %% TODO: support moving to multiple modbs(although it would be so slower operation)
               %% @lazedo comment:
               %%   I'm debating if we should move all existing vm messages to current modb
               %%   that would be great for bulk create of modb docs
               %%   since they would all go into same db
               %%   if that is the option, then change Timestamp below
               %%   with kz_util:current_tstamp()
              ,{<<"Document-Timestamp">>, kz_util:current_tstamp()}
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
-spec cleanup_account_timer() -> reference().
cleanup_account_timer() ->
    erlang:start_timer(?TIME_BETWEEN_ACCOUNT_CRAWLS, self(), 'ok').

-spec cleanup_mailbox_timer() -> reference().
cleanup_mailbox_timer() ->
    erlang:start_timer(?TIME_BETWEEN_BOX_CRAWLS, self(), 'ok').
