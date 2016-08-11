%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Mailbox maintenance
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_maintenance).

-export([migrate/0, migrate/1, migrate/2
        ,cleanup_heard_voicemail/1
        ]).

-include("kz_voicemail.hrl").

-define(LOG(Format, Args),
        lager:debug(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc Migrate all messages in vmbox into the new modb format
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> 'ok'.
migrate() ->
    _ = [migrate(Id)|| Id <- kapps_util:get_all_accounts('raw')],
    'ok'.

-spec migrate(ne_binary()) -> 'ok'.
migrate(AccountId) ->
    AccountDb = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(AccountDb, ?VMBOX_CB_LIST, []) of
        {'ok', []} -> ?LOG("no voicemail boxes in ~s", [AccountDb]);
        {'ok', View} ->
            _ = [migrate(AccountId, kz_json:get_value(<<"value">>, V)) || V <- View],
            ?LOG("migrated all messages of ~b mail boxes in ~s to modbs", [length(View), AccountDb]);
        {'error', _E} ->
            ?LOG("failed to get voicemail boxes in ~s: ~p", [AccountDb, _E])
    end.

-spec migrate(ne_binary(), ne_binary() | kz_json:object()) -> 'ok'.
migrate(AccountId, ?JSON_WRAPPER(_)=Box) ->
    migrate(AccountId, kz_doc:id(Box));
migrate(AccountId, BoxId) ->
    Msgs = kvm_messages:get(AccountId, BoxId),
    Ids = [M || M <- Msgs, maybe_migrate_to_modb(kzd_box_message:media_id(M))],
    _ = kvm_messages:update(AccountId, BoxId, Ids),
    'ok'.

-spec maybe_migrate_to_modb(ne_binary()) -> boolean().
maybe_migrate_to_modb(?MATCH_MODB_PREFIX(_, _, _)) -> 'false';
maybe_migrate_to_modb(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc Clean old heard voice messages from db
%% @end
%%--------------------------------------------------------------------
-spec cleanup_heard_voicemail(ne_binary()) -> 'ok'.
cleanup_heard_voicemail(AccountId) ->
    Today = kz_util:current_tstamp(),
    Duration = ?RETENTION_DURATION,
    DurationS = ?RETENTION_DAYS(Duration),
    ?LOG("retaining messages for ~p days, delete those older for ~s", [Duration, AccountId]),

    AccountDb = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(AccountDb, ?VMBOX_CB_LIST, []) of
        {'ok', []} -> ?LOG("no voicemail boxes in ~s", [AccountDb]);
        {'ok', View} ->
            cleanup_heard_voicemail(AccountId
                                   ,Today - DurationS
                                   ,[kz_json:get_value(<<"value">>, V) || V <- View]
                                   ),
            ?LOG("cleaned up ~b voicemail boxes in ~s", [length(View), AccountDb]);
        {'error', _E} ->
            ?LOG("failed to get voicemail boxes in ~s: ~p", [AccountDb, _E])
    end.

-spec cleanup_heard_voicemail(ne_binary(), pos_integer(), kz_proplist()) -> 'ok'.
cleanup_heard_voicemail(AccountId, Timestamp, Boxes) ->
    _ = [cleanup_voicemail_box(AccountId, Timestamp, Box) || Box <- Boxes],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc Filter out old messages in vmbox and soft delete them
%% @end
%%--------------------------------------------------------------------
-spec cleanup_voicemail_box(ne_binary(), pos_integer(), kz_json:object()) -> 'ok'.
cleanup_voicemail_box(AccountId, Timestamp, Box) ->
    BoxId = kz_doc:id(Box),
    Msgs = kvm_messages:get(AccountId, BoxId),
    case
        lists:partition(
            fun(Msg) ->
                %% must be old enough, and not in the NEW folder
                kz_json:get_integer_value(<<"timestamp">>, Msg) < Timestamp
                    andalso kz_json:get_value(<<"folder">>, Msg) =/= <<"new">>
            end
            ,Msgs
        )
    of
        {[], _} ->
            ?LOG("there are no old messages to remove from ~s", [BoxId]);
        {Older, _} ->
            ?LOG("there are ~b old messages to remove", [length(Older)]),

            _ = kvm_messages:change_folder(?VM_FOLDER_DELETED, Older, AccountId, BoxId),
            ?LOG("soft-deleted old messages", []),
            ?LOG("updated messages in voicemail box ~s", [BoxId])
    end.
