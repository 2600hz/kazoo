%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 6 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_menu).

-include("../callflow.hrl").

-export([handle/2]).

-record(keys, {
           %% Record Review
           save = <<"1">>
          ,listen = <<"2">>
          ,record = <<"3">>
         }).

-record(prompts, {
           generic_prompt = <<"/system_media/menu-no_prompt">>
          ,hunt_transfer = <<"/system_media/menu-transferring_call">>
          ,invalid_entry = <<"/system_media/menu-invalid_entry">>
          ,exit = <<"/system_media/menu-exit">>
          ,goodbye = <<"/system_media/vm-goodbye">>

          ,record_prompt= <<"/system_media/vm-record_greeting">>
          ,review_recording = <<"/system_media/vm-review_recording">>

          ,message_saved = <<"/system_media/vm-saved">>
          ,message_deleted = <<"/system_media/vm-deleted">>
          ,return_to_ivr = <<"/system_media/menu-return">>

          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
         }).

-record(menu, {
           menu_id = undefined :: binary() | undefined
          ,name = <<>>
          ,retries = 3 :: pos_integer()
          ,timeout = <<"2000">> :: binary()
          ,max_length = <<"4">> :: binary()
          ,hunt = false :: boolean()
          ,hunt_deny = <<>> :: binary()
          ,hunt_allow = <<>> :: binary()
          ,record_pin = <<>> :: binary()
          ,greeting_id = 'undefined' :: 'undefined' | ne_binary()
          ,prompts = #prompts{} :: #prompts{}
          ,keys = #keys{} :: #keys{}
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> ok.
handle(Data, Call) ->
    Menu = get_menu_profile(Data, Call),
    cf_call_command:answer(Call),
    menu_loop(Menu, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The main auto-attendant loop, will execute for the number
%% of retries playing the greeting and collecting digits till the
%% digits are routable
%% @end
%%--------------------------------------------------------------------
-spec menu_loop/2 :: (#menu{}, #cf_call{}) -> ok.
menu_loop(#menu{retries=Retries, prompts=Prompts}, Call) when Retries =< 0 ->
    ?LOG("maxium number of retries reached"),
    cf_call_command:flush_dtmf(Call),
    cf_call_command:b_play(Prompts#prompts.exit, Call),
    case cf_exe:attempt(<<"max_retries">>, Call) of
        {attempt_resp, ok} ->
            ok;
        {attempt_resp, {error, _}} ->
            {branch_keys, Keys} = cf_exe:get_all_branch_keys(Call), 
            case lists:member(<<"_">>, Keys) of
                false ->
                    cf_call_command:b_play(Prompts#prompts.goodbye, Call);
                true ->
                    cf_call_command:b_play(Prompts#prompts.hunt_transfer, Call)
            end,
            cf_exe:continue(Call)
    end;
menu_loop(#menu{retries=Retries, max_length=MaxLength, timeout=Timeout, record_pin=RecordPin, prompts=Prompts}=Menu, Call) ->
    case cf_call_command:b_play_and_collect_digits(<<"1">>, MaxLength, get_prompt(Menu, Call), <<"1">>, Timeout, Call) of
        {ok, <<>>} ->
            ?LOG("menu entry timeout"),
            case cf_exe:attempt(<<"timeout">>, Call) of
                {attempt_resp, ok} ->
                    ok;
                {attempt_resp, {error, _}} ->
                    menu_loop(Menu#menu{retries=Retries - 1}, Call)
            end;
        {ok, RecordPin} ->
            ?LOG("selection matches recording pin"),
            M = record_greeting(tmp_file(), Menu, Call),
            ?LOG("returning caller to menu"),
            cf_call_command:b_play(Prompts#prompts.return_to_ivr, Call),
            menu_loop(M, Call);
        {ok, Digits} ->
            %% this try_match_digits calls hunt_for_callflow() based on the digits dialed
            %% if it finds a callflow, the main CFPid will move on to it and try_match_digits
            %% will return true, matching here, and causing menu_loop to exit; this is
            %% expected behaviour as CFPid has moved on from this invocation
            case try_match_digits(Digits, Menu, Call) of
                true -> 
                    ok;
                false -> 
                    ?LOG("invalid selection ~w", [Digits]),
                    play_invalid_prompt(Menu, Call),
                    menu_loop(Menu#menu{retries=Retries - 1}, Call)
            end;
        {error, _} ->
            ?LOG("caller hungup while in the menu"),
            cf_exe:stop(Call)  
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The primary sequence logic to route the collected digits
%% @end
%%--------------------------------------------------------------------
-spec(try_match_digits/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
try_match_digits(Digits, Menu, Call) ->
    ?LOG("trying to match digits ~s", [Digits]),
    is_callflow_child(Digits, Menu, Call)
        orelse (is_hunt_enabled(Digits, Menu, Call)
                andalso is_hunt_allowed(Digits, Menu, Call)
                andalso not is_hunt_denied(Digits, Menu, Call)
                andalso hunt_for_callflow(Digits, Menu, Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the digits are a exact match for the auto-attendant children
%% @end
%%--------------------------------------------------------------------
-spec is_callflow_child/3 :: (ne_binary(), #menu{}, #cf_call{}) -> boolean().
is_callflow_child(Digits, _, Call) ->
    case cf_exe:attempt(Digits, Call) of
        {attempt_resp, ok} ->
            ?LOG("selection is a callflow child"),
            true;
        {attempt_resp, {error, _}} ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if hunting is enabled
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_enabled/3 :: (ne_binary(), #menu{}, #cf_call{}) -> boolean().
is_hunt_enabled(_, #menu{hunt=Hunt}, _) ->
    Hunt.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check whitelist hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_allowed/3 :: (ne_binary(), #menu{}, #cf_call{}) -> boolean().
is_hunt_allowed(_, #menu{hunt_allow = <<>>}, _) ->
    ?LOG("hunt_allow implicitly accepted digits"),
    true;
is_hunt_allowed(Digits, #menu{hunt_allow=RegEx}, _) ->
    try
        {match, _} = re:run(Digits, RegEx),
        ?LOG("hunt_allow accepted digits"),
        true
    catch
        _:_ ->
            ?LOG("hunt_allow denied digits"),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check blacklisted hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_denied/3 :: (ne_binary(), #menu{}, #cf_call{}) -> boolean().
is_hunt_denied(_, #menu{hunt_deny = <<>>}, _) ->
    ?LOG("hunt_deny implicitly accepted digits"),
    false;
is_hunt_denied(Digits, #menu{hunt_deny=RegEx}, _) ->
    try
        {match, _} = re:run(Digits, RegEx),
        ?LOG("hunt_deny denied digits"),
        true
    catch
        _:_ ->
            ?LOG("hunt_deny accepted digits"),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Hunt for a callflow with these numbers
%% @end
%%--------------------------------------------------------------------
-spec hunt_for_callflow/3 :: (ne_binary(), #menu{}, #cf_call{}) -> boolean().
hunt_for_callflow(Digits, #menu{prompts=Prompts}, #cf_call{account_id=AccountId}=Call) ->
    ?LOG("hunting for ~s in account ~s", [Digits, AccountId]),
    case cf_util:lookup_callflow(Digits, AccountId) of
        {ok, Flow, false} ->
            ?LOG("callflow hunt succeeded, branching"),
            cf_call_command:flush_dtmf(Call),
            cf_call_command:b_play(Prompts#prompts.hunt_transfer, Call),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow, ?EMPTY_JSON_OBJECT), Call),
            true;
        _ ->
            ?LOG("callflow hunt failed"),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(play_invalid_prompt/2 :: (Menu :: #menu{}, Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
play_invalid_prompt(#menu{prompts=Prompts}, Call) ->
    Prompts#prompts.invalid_entry =/= <<>> 
        andalso cf_call_command:b_play(Prompts#prompts.invalid_entry, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_greeting/3 :: (binary(), #menu{}, #cf_call{}) -> #menu{}.
record_greeting(AttachmentName, #menu{greeting_id=undefined}=Menu, Call) ->
    MediaId = recording_media_doc(<<"greeting">>, Menu, Call),
    ok = update_doc([<<"media">>, <<"greeting">>], MediaId, Menu, Call),
    record_greeting(AttachmentName, Menu#menu{greeting_id=MediaId}, Call);
record_greeting(AttachmentName, #menu{prompts=#prompts{record_prompt=RecordGreeting, tone_spec=ToneSpec
                                                      ,message_saved=Saved, message_deleted=Deleted}
                                     ,greeting_id=MediaId}=Menu, Call) ->
    ?LOG("recording new menu greeting"),
    cf_call_command:audio_macro([{play, RecordGreeting}
                                 ,{tones, ToneSpec}]
                                ,Call),
    {ok, _} = cf_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, Menu, Call) of
        {ok, record} ->
            record_greeting(tmp_file(), Menu, Call);
        {ok, save} ->
            {ok, _} = store_recording(AttachmentName, MediaId, Call),
            cf_call_command:b_play(Saved, Call),
            Menu;
        {ok, no_selection} ->
            ?LOG("abandoning recorded greeting"),
            cf_call_command:b_play(Deleted, Call),
            Menu
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_prompt/2 :: (#menu{}, #cf_call{}) -> ne_binary().
get_prompt(#menu{greeting_id=undefined, prompts=Prompts}, _) ->
    Prompts#prompts.generic_prompt;
get_prompt(#menu{greeting_id = <<"local_stream://", _/binary>> = ID}, _) ->
    ID;
get_prompt(#menu{greeting_id=Id}, #cf_call{account_db=Db}) ->
    <<$/, Db/binary, $/, Id/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording/3 :: (binary(), binary(), #cf_call{}) -> {ok, json_object()} | {error, json_object()}.
store_recording(AttachmentName, MediaId, Call) ->
    ?LOG("storing recording ~s as media ~s", [AttachmentName, MediaId]),
    ok = update_doc(<<"content_type">>, <<"audio/mpeg">>, MediaId, Call),
    cf_call_command:b_store(AttachmentName, get_new_attachment_url(AttachmentName, MediaId, Call), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_attachment_url/3 :: (binary(), binary(), #cf_call{}) -> binary().
get_new_attachment_url(AttachmentName, MediaId, #cf_call{account_db=Db}) ->
    case couch_mgr:open_doc(Db, MediaId) of
        {ok, JObj} ->
            case wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())) of
                [] ->
                    ok;
                Existing ->
                    [couch_mgr:delete_attachment(Db, MediaId, Attach) || Attach <- Existing]
            end;
        {error, _} ->
            ok
    end,
    Rev = case couch_mgr:lookup_doc_rev(Db, MediaId) of
              {ok, R} ->
                  <<"?rev=", R/binary>>;
              _ ->
                  <<>>
          end,
    <<(couch_mgr:get_url())/binary
      ,Db/binary
      ,$/, MediaId/binary
      ,$/, AttachmentName/binary
      ,Rev/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(tmp_file/0 :: () -> binary()).
tmp_file() ->
     <<(list_to_binary(wh_util:to_hex(crypto:rand_bytes(16))))/binary, ".mp3">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec review_recording/3 :: (ne_binary(), #menu{}, #cf_call{}) -> {ok, record | save | no_selection}.
review_recording(MediaName, #menu{prompts=Prompts, keys=#keys{listen=ListenKey, record=RecordKey, save=SaveKey}}=Menu, Call) ->
    ?LOG("playing menu greeting review options"),
    cf_call_command:flush_dtmf(Call),
    case cf_call_command:b_play_and_collect_digit(Prompts#prompts.review_recording, Call) of
        {ok, ListenKey} ->
            cf_call_command:b_play(MediaName, Call),
            review_recording(MediaName, Menu, Call);
        {ok, RecordKey} ->
            {ok, record};
        {ok, SaveKey} ->
            {ok, save};
        {ok, _} ->
            review_recording(MediaName, Menu, Call);
        {error, _} ->
            {ok, no_selection}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recording_media_doc/3 :: (binary(), #menu{}, #cf_call{}) -> binary().
recording_media_doc(Type, #menu{name=MenuName, menu_id=Id}, #cf_call{account_db=Db}) ->
    Name = <<MenuName/binary, " menu ", Type/binary >>,
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"menu recorded/prompt media">>}
             ,{<<"source_type">>, <<"menu">>}
             ,{<<"source_id">>, Id}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, true}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), Db, [{type, <<"media">>}]),
    {ok, JObj} = couch_mgr:save_doc(Db, Doc),
    wh_json:get_value(<<"_id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc/4 :: (list() | binary(), json_term(), #menu{} | binary(),  #cf_call{} | binary()) -> ok | {error, atom()}.
update_doc(Key, Value, #menu{menu_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, #cf_call{account_db=Db}) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {error, conflict} ->
                    update_doc(Key, Value, Id, Db);
                {ok, _} ->
                    ok;
                {error, _}=E ->
                    ?LOG("unable to update ~s in ~s, ~p", [Id, Db, E])
            end;
        {error, _}=E ->
            ?LOG("unable to update ~s in ~s, ~p", [Id, Db, E])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_menu_profile/2 :: (json_object(), #cf_call{}) -> #menu{}.
get_menu_profile(Data, #cf_call{account_id=AccountId}) ->
    Id = wh_json:get_value(<<"id">>, Data),
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded menu route ~s", [Id]),
            Default=#menu{},
            #menu{menu_id = Id
                  ,name =
                      wh_json:get_ne_value(<<"name">>, JObj, Id)
                  ,retries =
                      wh_json:get_integer_value(<<"retries">>, JObj, Default#menu.retries)
                  ,timeout =
                      wh_json:get_binary_value(<<"timeout">>, JObj, Default#menu.timeout)
                  ,max_length =
                      wh_json:get_binary_value(<<"max_extension_length">>, JObj, Default#menu.max_length)
                  ,hunt =
                      wh_json:is_true(<<"hunt">>, JObj, Default#menu.hunt)
                  ,hunt_deny =
                      wh_json:get_value(<<"hunt_deny">>, JObj, Default#menu.hunt_deny)
                  ,hunt_allow =
                      wh_json:get_value(<<"hunt_allow">>, JObj, Default#menu.hunt_allow)
                  ,record_pin =
                      wh_json:get_value(<<"record_pin">>, JObj, Default#menu.record_pin)
                  ,greeting_id =
                      wh_json:get_ne_value([<<"media">>, <<"greeting">>], JObj)
                 };
        {error, R} ->
            ?LOG("failed to load menu route ~s, ~w", [Id, R]),
            #menu{}
    end.
