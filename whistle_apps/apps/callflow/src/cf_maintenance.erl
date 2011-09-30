%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_maintenance).

-define(CB_VIEW, {<<"vmboxes">>, <<"crossbar_listing">>}).

-export([migrate_voicemail/0, migrate_voicemail/1]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will migrate all the voicemail mailbox documents to
%% the latest version.
%% @end
%%--------------------------------------------------------------------
-spec migrate_voicemail/0:: () -> [done | error,...].
-spec migrate_voicemail/1 :: (Account) -> done | error when
      Account :: binary().

migrate_voicemail() ->
    [ migrate_voicemail(Account) || Account <- whapps_util:get_all_accounts(raw) ].

migrate_voicemail(Account) ->
    Db = whapps_util:get_db_name(Account, encoded),
    log("migrating all vmboxes in ~s", [Db]),
    case couch_mgr:get_all_results(Db, ?CB_VIEW) of
        {ok, []} ->
            done;
        {ok, VMBoxes} ->
            VMIds = [Id || VMBox <- VMBoxes, (Id = wh_json:get_value(<<"id">>, VMBox)) =/= undefined],
            migrate_vmboxes(VMIds, Db);
        {error, _E} ->
            log("unable to get a list of vmboxes: ~p", [_E]),
            error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the voicemail boxes found in an account and migrates
%% each to the latest schema
%% @end
%%--------------------------------------------------------------------
-spec migrate_vmboxes/2 :: (VMIds, Db) -> done when
      VMIds :: list(),
      Db :: binary().
migrate_vmboxes([Id|T], Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            {ok, _} = migrate_vmbox(JObj, Db), % better save, or we crash to alert the caller
            migrate_vmboxes(T, Db);
        {error, _E} ->
            migrate_vmboxes(T, Db)
    end;
migrate_vmboxes([], _) ->
    done.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes the migration routines on a voicemail box required to
%% migrate it to the latest version
%% @end
%%--------------------------------------------------------------------
-spec migrate_vmbox/2 :: (JObj, Db) -> tuple(ok, json_object()) | tuple(error, atom) when
      JObj :: json_object(),
      Db :: binary().
migrate_vmbox(JObj, Db) ->
    Messages = wh_json:get_value(<<"messages">>, JObj, []),
    Vsn = wh_json:get_value(<<"pvt_vsn">>, JObj, <<"1">>),
    log("attempting to migrate messages on vmbox ~s", [wh_json:get_value(<<"_id">>, JObj)]),
    Updaters = [migrate_vm_vsn(Vsn)
                ,migrate_vm_messages(Messages, JObj, Db, [], false)
                ,migrate_vm_unavailable_greeting(wh_json:get_value([<<"_attachments">>, <<"unavailable_greeting.mp3">>], JObj)
                                                 ,JObj, Db
                                                 ,wh_json:get_value([<<"media">>, <<"unavailable">>], JObj))],
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Updaters),
    couch_mgr:ensure_saved(Db, NewJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used to migrate the private version number on the voicemail box
%% document to the correct version
%% @end
%%--------------------------------------------------------------------
-spec migrate_vm_vsn/1 :: (Vsn) -> fun() when
      Vsn :: binary() | atom().
migrate_vm_vsn(<<"2">>) ->
    fun (J) -> J end;
migrate_vm_vsn(_) ->
    fun (J) ->
            log("updating vmbox vsn", []),
            wh_json:set_value(<<"pvt_vsn">>, <<"2">>, J)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used to scan the messages and correct the metadata schema if it
%% requires an update
%% @end
%%--------------------------------------------------------------------
-spec migrate_vm_messages/5 :: (CurrentMsgs, Box, Db, Messages, Updated) -> fun() when
      CurrentMsgs :: list(),
      Box :: json_object(),
      Db :: binary(),
      Messages :: json_objects(),
      Updated :: boolean().
migrate_vm_messages([], _, _, _, false) ->
    fun (J) -> J end;
migrate_vm_messages([], _, _, Messages, true) ->
    fun (J) -> wh_json:set_value(<<"messages">>, Messages, J) end;
migrate_vm_messages([Msg|T], Box, Db, Messages, Updated) ->
    case {wh_json:get_value(<<"media_id">>, Msg), wh_json:get_value(<<"attachment">>, Msg)} of
        {undefined, undefined} ->
            log("message does not have an attachment or media id", []),
            migrate_vm_messages(T, Box, Db, Messages, Updated);
        {undefined, Attachment} ->
            try
                {ok, MediaId} = try_move_vm_attachment(Attachment, Msg, Box, Db),
                log("moving message ~s to media id ~s", [Attachment, MediaId]),
                M = wh_json:set_value(<<"media_id">>, MediaId, Msg),
                migrate_vm_messages(T, Box, Db, [M|Messages], true)
            catch
                _:R ->
                    Id = wh_json:get_value(<<"_id">>, Box),
                    log("failed to migrate ~s on ~s in ~s: ~p", [Attachment, Id, Db, R]),
                    migrate_vm_messages(T, Box, Db, [Msg|Messages], Updated)
            end;
        _ ->
            migrate_vm_messages(T, Box, Db, [Msg|Messages], Updated)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used to update the unavailable greeting to the new vmbox schema
%% version
%% @end
%%--------------------------------------------------------------------
-spec migrate_vm_unavailable_greeting/4 :: (Attach, Box, Db, MediaId) -> fun() when
      Attach :: json_object(),
      Box :: json_object(),
      Db :: binary(),
      MediaId :: undefined | binary().
migrate_vm_unavailable_greeting(undefined, _, _, _) ->
    fun (J) ->
            log("vmbox has no unavaliable greeting", []), J
    end;
migrate_vm_unavailable_greeting(_, _, _, MediaId) when MediaId =/= undefined ->
    fun (J) ->
            log("vmbox has unavaliable greeting migrated already", []), J
    end;
migrate_vm_unavailable_greeting(_, Box, Db, _) ->
    case wh_json:get_value(<<"unavailable_media_id">>, Box) of
        undefined ->
            Id = wh_json:get_value(<<"_id">>, Box),
            BoxNum = wh_json:get_value(<<"mailbox">>, Box, <<"">>),
            Name = <<"mailbox ", BoxNum/binary, " unavailable greeting">>,
            Props = [{<<"name">>, Name}
                     ,{<<"description">>, <<"voicemail recorded/prompt media">>}
                     ,{<<"source_type">>, <<"voicemail">>}
                     ,{<<"source_id">>, Id}
                     ,{<<"media_type">>, <<"mp3">>}
                     ,{<<"content_type">>, <<"audio/mpeg">>}
                     ,{<<"streamable">>, true}],
            Doc = wh_doc:update_pvt_parameters({struct, Props}, Db, [{type, <<"media">>}]),
            try
                {ok, Media} = couch_mgr:save_doc(Db, Doc),
                MediaId = wh_json:get_value(<<"_id">>, Media),
                {ok, Bin} = couch_mgr:fetch_attachment(Db, Id, <<"unavailable_greeting.mp3">>),
                {ok, _} = couch_mgr:put_attachment(Db, MediaId, MediaId, Bin),
                fun(J) ->
                        wh_json:set_value([<<"media">>, <<"unavailable">>], MediaId, J)
                end
            catch
                _:R ->
                    fun(J) ->
                            log("failed to migrate unavailable greeting for ~s in ~s: ~p", [Id, Db, R]), J
                    end
            end;
        MediaId ->
            fun(J) ->
                    J1 = wh_json:delete_key(<<"unavailable_media_id">>, J),
                    wh_json:set_value([<<"media">>, <<"unavailable_greeting">>], MediaId, J1)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is specific for the vmbox document change from version
%% 1 to greater.  It will extract the message audio files and place them
%% in their own document.
%% @end
%%--------------------------------------------------------------------
-spec try_move_vm_attachment/4 :: (Attachment, Msg, Box, Db) -> tuple(ok, binary()) when
      Attachment :: binary(),
      Msg :: json_object(),
      Box :: json_object(),
      Db :: binary().
try_move_vm_attachment(Attachment, Msg, Box, Db) ->
    Id = wh_json:get_value(<<"_id">>, Box),
    Timestamp = wh_json:get_integer_value(<<"timestamp">>, Msg, wh_util:current_tstamp()),
    Timezone = wh_json:get_value(<<"timezone">>, Box, <<"America/Los_Angeles">>),
    BoxNum = wh_json:get_value(<<"mailbox">>, Box, <<"">>),
    UtcDateTime = calendar:gregorian_seconds_to_datetime(Timestamp),
    {{Y,M,D},{H,I,S}} = localtime:utc_to_local(UtcDateTime, wh_util:to_list(Timezone)),
    Name = <<"mailbox ", BoxNum/binary, " message "
             ,(wh_util:to_binary(M))/binary, $-, (wh_util:to_binary(D))/binary, $-, (wh_util:to_binary(Y))/binary
             ,$ , (wh_util:to_binary(H))/binary, $:, (wh_util:to_binary(I))/binary, $:, (wh_util:to_binary(S))/binary>>,
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"voicemail message media">>}
             ,{<<"source_type">>, <<"voicemail">>}
             ,{<<"source_id">>, Id}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"streamable">>, true}],
    Doc = wh_doc:update_pvt_parameters({struct, Props}, Db, [{type, <<"private_media">>}]),
    {ok, Media} = couch_mgr:save_doc(Db, Doc),
    MediaId = wh_json:get_value(<<"_id">>, Media),
    {ok, Bin} = couch_mgr:fetch_attachment(Db, Id, Attachment),
    {ok, _} = couch_mgr:put_attachment(Db, MediaId, MediaId, Bin),
    {ok, MediaId}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Simple log wrapper.
%% @end
%%--------------------------------------------------------------------
-spec log/2 :: (Format, Args) -> ok when
      Format :: string(),
      Args :: list().
log(Format, Args) ->
    io:format(Format ++ "~n", Args),
    ?LOG(Format, Args).
