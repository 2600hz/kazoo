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

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).
-import(cf_call_command, [
                           answer/1, b_play_and_collect_digits/6, b_play/2
                          ,store/3, b_record/2, audio_macro/2, wait_for_dtmf/1
                          ,flush/1
                         ]).

-define(MEDIA_PROMPT, <<"prompt.wav">>).

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
           database = undefined :: binary() | undefined
          ,menu_id = undefined :: binary() | undefined
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
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, Call) ->
    Menu = get_menu_profile(Data),
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
-spec(menu_loop/2 :: (Menu :: #menu{}, Call :: #cf_call{}) -> ok).
menu_loop(#menu{retries=Retries, prompts=Prompts}, #cf_call{cf_pid=CFPid} = Call) when Retries =< 0 ->
    if 
        Prompts#prompts.exit =/= <<>> ->
            b_play(Prompts#prompts.exit, Call);
        true ->
            ok
    end,
    CFPid ! {continue};
menu_loop(#menu{max_length=MaxLength, timeout=Timeout, record_pin=RecordPin, prompts=Prompts}=Menu, Call) ->
    try 
        {ok, Digits} = 
            b_play_and_collect_digits(<<"1">>, MaxLength, get_prompt(Menu), <<"1">>, Timeout, Call),
        if
            Digits =:= <<>> ->
                throw(no_digits_collected);
            Digits =:= RecordPin ->
                M = record_prompt(tmp_file(), Menu, Call),
                b_play(Prompts#prompts.return_to_ivr, Call),
                menu_loop(M, Call);
            true ->
                true = try_match_digits(Digits, Menu, Call)
        end
    catch
        _:_ ->
            play_invalid_prompt(Menu, Call),
            menu_loop(Menu#menu{retries=Menu#menu.retries-1}, Call)
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
    Hunt.

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
        {match, _} = re:run(
                       whistle_util:to_list(Digits),
                       whistle_util:to_list(RegEx)
                      ), 
        true
    catch
        _:_ ->
            false
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
        {match, _} = re:run(
                       whistle_util:to_list(Digits),
                       whistle_util:to_list(RegEx)
                      ), 
        true
    catch
        _:_ ->
            false
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
        {ok, JObj} ->
            cf_call_command:flush_dtmf(Call),
            b_play(Prompts#prompts.hunt_transfer, Call),
            CFPid ! {branch, whapps_json:get_value(<<"flow">>, JObj)},
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
-spec(play_invalid_prompt/2 :: (Menu :: #menu{}, Call :: #cf_call{}) -> ok|tuple(error, atom())).
play_invalid_prompt(#menu{s_prompt=true, retries=1}, Call) ->
    cf_call_command:hangup(Call);   
play_invalid_prompt(#menu{s_prompt=true, retries=2}, Call) ->
    b_play(<<"/system_media/ivr-one_more_mistake">>, Call);
play_invalid_prompt(#menu{s_prompt=true, retries=3}, Call) ->
    b_play(<<"/system_media/ivr-seriously_mean_to_press_key">>, Call);
play_invalid_prompt(#menu{s_prompt=true}, Call) ->
    b_play(<<"/system_media/ivr-did_you_mean_to_press_key">>, Call);
play_invalid_prompt(#menu{prompts=Prompts}, Call) ->
    if 
        Prompts#prompts.invalid_entry =/= <<>> ->
            b_play(Prompts#prompts.invalid_entry, Call);
        true ->
            ok
    end.

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
                    b_play(Prompts#prompts.message_saved, Call),
                    store_recording(MediaName, ?MEDIA_PROMPT, Menu, Call),
                    Menu#menu{has_prompt_media=true};                
                _Else ->
                    b_play(Prompts#prompts.message_deleted, Call),
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
-spec(review_recording/3 :: (MediaName :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> stop | continue).
review_recording(MediaName, #menu{prompts=Prompts, keys=Keys}=Menu, Call) ->
    audio_macro([
                  {play, Prompts#prompts.press}
                 ,{say,  Keys#keys.listen}
                 ,{play, Prompts#prompts.to_listen}

                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.save}
                 ,{play, Prompts#prompts.to_save}

                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.record}
                 ,{play, Prompts#prompts.to_rerecord}
                ], Call),
    case wait_for_dtmf(5000) of
        {error, _}=E ->
            E;
        {ok, Digit} ->
            flush(Call),
            if 
                Digit == Keys#keys.listen ->
                    b_play(MediaName, Call),
                    review_recording(MediaName, Menu, Call);
                Digit == Keys#keys.record -> 
                    {ok, record};
                Digit == Keys#keys.save ->                   
                    {ok, save};
                true ->
                    review_recording(MediaName, Menu, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(store_recording/4 :: (MediaName :: binary(), DestName :: binary(), Menu :: #menu{}, Call :: #cf_call{}) -> ok | tuple(error, atom())).
store_recording(MediaName, DestName, Menu, Call) ->
    store(MediaName, get_attachment_path(DestName, Menu), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_attachment_path/2 :: (MediaName :: binary(), Menu :: #menu{}) -> binary()).
get_attachment_path(MediaName, #menu{database=Db, menu_id=Id}) ->
    <<(couch_mgr:get_url())/binary
      ,Db/binary
      ,$/, Id/binary
      ,$/, MediaName/binary
      ,"?rev=", (couch_mgr:lookup_doc_rev(Db, Id))/binary>>.   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(tmp_file/0 :: () -> binary()).
tmp_file() ->
     <<(list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16))))/binary, ".wav">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(get_prompt/1 :: (Menu :: #menu{}) -> binary()).
get_prompt(#menu{has_prompt_media=false, prompts=Prompts}) ->
    Prompts#prompts.generic_prompt;
get_prompt(#menu{database=Db, menu_id=Id}) ->                            
    Prompt = ?MEDIA_PROMPT,
    <<$/, Db/binary, $/, Id/binary, $/, Prompt/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(get_menu_profile/1 :: (Data :: json_object()) -> #menu{}).                                  
get_menu_profile(Data) ->
    Db = whapps_json:get_value(<<"database">>, Data),
    Id = whapps_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            Default=#menu{},
            #menu{         
                    database = Db
                   ,menu_id = Id
                   ,retries = whapps_json:get_value([<<"base">>, <<"retries">>], JObj, Default#menu.retries)
                   ,timeout = whapps_json:get_value([<<"base">>, <<"timeout">>], JObj, Default#menu.timeout)
                   ,max_length = whapps_json:get_value([<<"base">>, <<"max-extension-length">>], JObj, Default#menu.max_length)
                   ,hunt = whapps_json:get_value([<<"base">>, <<"hunt">>], JObj, Default#menu.hunt)
                   ,hunt_deny = whapps_json:get_value([<<"base">>, <<"hunt-deny">>], JObj, Default#menu.hunt_deny)
                   ,hunt_allow = whapps_json:get_value([<<"base">>, <<"hunt-allow">>], JObj, Default#menu.hunt_allow)
                   ,record_pin = whapps_json:get_value([<<"base">>, <<"record_pin">>], JObj, Default#menu.record_pin)
                   ,has_prompt_media = whapps_json:get_value([<<"_attachments">>, ?MEDIA_PROMPT], JObj) =/= undefined
                   ,s_prompt = whapps_json:get_value([<<98,97,115,101>>, <<115,97,115,115,121,45,109,111,100,101>>], JObj) =/= undefined
                 };
        _ -> 
            #menu{}
    end.
    
