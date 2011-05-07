%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 6 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_menu).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [
                           answer/1, b_play_and_collect_digits/6, b_play/2
                          ,store/3, b_record/2, audio_macro/2, wait_for_dtmf/1
                          ,flush/1
                         ]).

-define(MEDIA_PROMPT, <<"prompt.mp3">>).

-record(keys, {
           %% Record Review
           listen = <<"1">>
          ,save = <<"2">>
          ,record = <<"3">>
         }).

-record(prompts, {
           generic_prompt = <<"/system_media/ivr-no_menu_items">>
          ,hunt_transfer = <<"/system_media/ivr-hold_connect_call">>
          ,invalid_entry = <<"/system_media/ivr-that_was_an_invalid_entry">>
          ,exit = <<"/system_media/ivr-call_being_transferred">>

          ,record_prompt= <<"/system_media/vm-record_greeting">>

          ,record_instructions = <<"/system_media/vm-record_message">>
          ,press = <<"/system_media/vm-press">>

          ,to_listen = <<"/system_media/vm-listen_to_recording">>
          ,to_save = <<"/system_media/vm-save_recording">>
          ,to_rerecord = <<"/system_media/vm-rerecord">>

          ,message_saved = <<"/system_media/ivr-recording_saved">>
          ,message_deleted = <<"/system_media/vm-deleted">>
          ,return_to_ivr = <<"shout://translate.google.com/translate_tts?tl=en&q=One+moment+while+I+transfer+you+back+to+the+auto+attendant.">>

          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
         }).

-record(menu, {
           menu_id = undefined :: binary() | undefined
          ,s_prompt = false :: boolean()
          ,retries = 3 :: pos_integer()
          ,timeout = <<"4000">> :: binary()
          ,max_length = <<"1">> :: binary()
          ,hunt = <<"false">> :: binary()
          ,hunt_deny = <<>> :: binary()
          ,hunt_allow = <<>> :: binary()
          ,record_pin = <<>> :: binary()
          ,has_prompt_media = false :: boolean()
          ,prompts = #prompts{} :: #prompts{}
          ,keys = #keys{} :: #keys{}
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, Call) ->
    Menu = get_menu_profile(Data, Call#cf_call.account_db),
    answer(Call),
    menu_loop(Menu, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The main auto-attendant loop, will execute for the number
%% of retries playing the greeting and collecting digits till the
%% digits are routable
%% @end
%%--------------------------------------------------------------------
-spec(menu_loop/2 :: (Menu :: #menu{}, Call :: #cf_call{}) -> no_return()).
menu_loop(#menu{retries=Retries, prompts=Prompts}, #cf_call{cf_pid=CFPid} = Call) when Retries =< 0 ->
    Prompts#prompts.exit =/= <<>> andalso b_play(Prompts#prompts.exit, Call),
    CFPid ! {continue}; %% too many retries, we're out
menu_loop(#menu{retries=Retries, max_length=MaxLength, timeout=Timeout, record_pin=RecordPin, prompts=Prompts}=Menu, Call) ->
    try
	case b_play_and_collect_digits(<<"1">>, MaxLength, get_prompt(Menu, Call), <<"1">>, Timeout, Call) of
	    {ok, <<>>} ->
		throw(no_digits_collected);
	    {ok, RecordPin} ->
                M = record_prompt(tmp_file(), Menu, Call),
                _ = b_play(Prompts#prompts.return_to_ivr, Call),
                menu_loop(M, Call);
            {ok, Digits} ->
		%% this try_match_digits calls hunt_for_callflow() based on the digits dialed
		%% if it finds a callflow, the main CFPid will move on to it and try_match_digits
		%% will return true, matching here, and causing menu_loop to exit; this is
		%% expected behaviour as CFPid has moved on from this invocation
                true = try_match_digits(Digits, Menu, Call)
        end
    catch
        _:_ ->
            _ = play_invalid_prompt(Menu, Call),
            menu_loop(Menu#menu{retries=Retries-1}, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The primary sequence logic to route the collected digits
%% @end
%%--------------------------------------------------------------------
-spec(try_match_digits/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
try_match_digits(Digits, Menu, Call) ->
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
-spec(is_callflow_child/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
is_callflow_child(Digits, _, #cf_call{cf_pid=CFPid}) ->
    CFPid ! {attempt, Digits},
    ok =:= receive
               {attempt_resp, Resp} -> Resp
           after 1000 ->
                   {error, timeout}
           end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if hunting is enabled
%% @end
%%--------------------------------------------------------------------
-spec(is_hunt_enabled/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
is_hunt_enabled(_, #menu{hunt=Hunt}, _) ->
    whistle_util:is_true(Hunt).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check whitelist hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec(is_hunt_allowed/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
is_hunt_allowed(_, #menu{hunt_allow = <<>>}, _) ->
    true;
is_hunt_allowed(Digits, #menu{hunt_allow=RegEx}, _) ->
    try
        {match, _} = re:run(Digits, RegEx),
        true
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check blacklisted hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec(is_hunt_denied/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
is_hunt_denied(_, #menu{hunt_deny = <<>>}, _) ->
    false;
is_hunt_denied(Digits, #menu{hunt_deny=RegEx}, _) ->
    try
        {match, _} = re:run(Digits, RegEx),
        true
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Hunt for a callflow with these numbers
%% @end
%%--------------------------------------------------------------------
-spec(hunt_for_callflow/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> boolean()).
hunt_for_callflow(Digits, #menu{prompts=Prompts}, #cf_call{from_realm=To, cf_pid=CFPid, cf_responder=CFRPid}=Call) ->
    case gen_server:call(CFRPid, {find_flow, <<Digits/binary, $@, To/binary>>}, 2000) of
        {ok, Flow} ->
            _ = cf_call_command:flush_dtmf(Call),
            _ = b_play(Prompts#prompts.hunt_transfer, Call),
            CFPid ! {branch, Flow},
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(play_invalid_prompt/2 :: (Menu :: #menu{}, Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
play_invalid_prompt(#menu{s_prompt=true, retries=1}, Call) ->
    cf_call_command:hangup(Call);
play_invalid_prompt(#menu{s_prompt=true, retries=2}, Call) ->
    b_play(<<"/system_media/ivr-one_more_mistake">>, Call);
play_invalid_prompt(#menu{s_prompt=true, retries=3}, Call) ->
    b_play(<<"/system_media/ivr-seriously_mean_to_press_key">>, Call);
play_invalid_prompt(#menu{s_prompt=true}, Call) ->
    b_play(<<"/system_media/ivr-did_you_mean_to_press_key">>, Call);
play_invalid_prompt(#menu{prompts=Prompts}, Call) ->
    Prompts#prompts.invalid_entry =/= <<>> andalso b_play(Prompts#prompts.invalid_entry, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(record_prompt/3 :: (MediaName :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> #menu{}).
record_prompt(MediaName, #menu{prompts=Prompts}=Menu, Call) ->
    audio_macro([
                  {play,  Prompts#prompts.record_prompt}
                 ,{tones, Prompts#prompts.tone_spec}
                ], Call),
    case b_record(MediaName, Call) of
        {ok, _Msg} ->
            case review_recording(MediaName, Menu, Call) of
                {ok, record} ->
                    record_prompt(MediaName, Menu, Call);
                {ok, save} ->
                    _ = b_play(Prompts#prompts.message_saved, Call),
                    store_recording(MediaName, ?MEDIA_PROMPT, Menu, Call),
                    Menu#menu{has_prompt_media=true};
                _Else ->
                    _ = b_play(Prompts#prompts.message_deleted, Call),
                    Menu
            end;
        _Else ->
            Menu
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(review_recording/3 :: (MediaName :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> tuple(ok, record | save) | tuple(error, term())).
review_recording(MediaName, #menu{prompts=Prompts, keys=#keys{listen=ListenKey, record=RecordKey, save=SaveKey}}=Menu, Call) ->
    audio_macro([
                  {play, Prompts#prompts.press}
                 ,{say,  ListenKey}
                 ,{play, Prompts#prompts.to_listen}

                 ,{play, Prompts#prompts.press}
                 ,{say,  SaveKey}
                 ,{play, Prompts#prompts.to_save}

                 ,{play, Prompts#prompts.press}
                 ,{say,  RecordKey}
                 ,{play, Prompts#prompts.to_rerecord}
                ], Call),
    case wait_for_dtmf(5000) of
        {error, _}=E ->
            E;
        {ok, ListenKey} ->
            _ = flush(Call),
	    _ = b_play(MediaName, Call),
	    review_recording(MediaName, Menu, Call);
	{ok, RecordKey} ->
	    _ = flush(Call),
	    {ok, record};
	{ok, SaveKey} ->
	    _ = flush(Call),
	    {ok, save};
	_ ->
	    _ = flush(Call),
	    review_recording(MediaName, Menu, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(store_recording/4 :: (MediaName :: binary(), DestName :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> ok).
store_recording(MediaName, DestName, Menu, Call) ->
    store(MediaName, get_attachment_path(DestName, Menu, Call), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_attachment_path/3 :: (MediaName :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> binary()).
get_attachment_path(MediaName, #menu{menu_id=Id}, #cf_call{account_db=Db}) ->
    {ok, Rev} = couch_mgr:lookup_doc_rev(Db, Id),
	<<(couch_mgr:get_url())/binary
	  ,Db/binary
	  ,$/, Id/binary
	  ,$/, MediaName/binary
	  ,"?rev=", Rev/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(tmp_file/0 :: () -> binary()).
tmp_file() ->
     <<(list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16))))/binary, ".mp3">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_prompt/2 :: (Menu :: #menu{}, Call :: #cf_call{}) -> binary()).
get_prompt(#menu{has_prompt_media=false, prompts=Prompts}, _) ->
    Prompts#prompts.generic_prompt;
get_prompt(#menu{menu_id=Id}, #cf_call{account_db=Db}) ->
    Prompt = ?MEDIA_PROMPT,
    <<$/, Db/binary, $/, Id/binary, $/, Prompt/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_menu_profile/2 :: (Data :: json_object(), Db :: binary()) -> #menu{}).
get_menu_profile(Data, Db) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            Default=#menu{},
            #menu{
		   menu_id = Id
                   ,retries = wh_json:get_value(<<"retries">>, JObj, Default#menu.retries)
                   ,timeout = wh_json:get_value(<<"timeout">>, JObj, Default#menu.timeout)
                   ,max_length = wh_json:get_value(<<"max_extension_length">>, JObj, Default#menu.max_length)
                   ,hunt = wh_json:get_value(<<"hunt">>, JObj, Default#menu.hunt)
                   ,hunt_deny = wh_json:get_value(<<"hunt_deny">>, JObj, Default#menu.hunt_deny)
                   ,hunt_allow = wh_json:get_value(<<"hunt_allow">>, JObj, Default#menu.hunt_allow)
                   ,record_pin = wh_json:get_value(<<"record_pin">>, JObj, Default#menu.record_pin)
                   ,has_prompt_media = wh_json:get_value([<<"_attachments">>, ?MEDIA_PROMPT], JObj) =/= undefined
                   ,s_prompt = wh_json:get_value(<<115,97,115,115,121,95,109,111,100,101>>, JObj) =/= undefined
                 };
        _ ->
            #menu{}
    end.
