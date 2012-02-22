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

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".menu">>).

-record(keys, {
           %% Record Review
           save = <<"1">>
          ,listen = <<"2">>
          ,record = <<"3">>
         }).

-record(menu, {menu_id = undefined :: binary() | undefined
               ,name = <<>>
               ,retries = 3 :: pos_integer()
               ,timeout = <<"2000">> :: binary()
               ,max_length = <<"4">> :: binary()
               ,hunt = false :: boolean()
               ,hunt_deny = <<>> :: binary()
               ,hunt_allow = <<>> :: binary()
               ,record_pin = <<>> :: binary()
               ,record_from_offnet = false :: boolean()   
               ,greeting_id = 'undefined' :: 'undefined' | ne_binary()
               ,exit_media = true :: boolean() | ne_binary()
               ,transfer_media = true :: boolean() | ne_binary()
               ,invalid_media = true :: boolean() | ne_binary()
               ,keys = #keys{} :: #keys{}
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data :: wh_json:json_object(), Call :: whapps_call:call()) -> ok.
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
-spec menu_loop/2 :: (#menu{}, whapps_call:call()) -> ok.
menu_loop(#menu{retries=Retries}=Menu, Call) when Retries =< 0 ->
    ?LOG("maxium number of retries reached"),
    cf_call_command:flush_dtmf(Call),
    play_exit_prompt(Menu, Call),
    case cf_exe:attempt(<<"max_retries">>, Call) of
        {attempt_resp, ok} ->
            ok;
        {attempt_resp, {error, _}} ->
            case cf_exe:wildcard_is_empty(Call) of
                true -> cf_call_command:b_prompt(<<"vm-goodbye">>, Call);
                false -> play_transferring_prompt(Menu, Call)
            end,
            cf_exe:continue(Call)
    end;
menu_loop(#menu{retries=Retries, max_length=MaxLength, timeout=Timeout
                ,record_pin=RecordPin, record_from_offnet=RecOffnet}=Menu, Call) ->
    case cf_call_command:b_play_and_collect_digits(<<"1">>, MaxLength, get_prompt(Menu, Call), <<"1">>, Timeout, Call) of
        {ok, <<>>} ->
            ?LOG("menu entry timeout"),
            case cf_exe:attempt(<<"timeout">>, Call) of
                {attempt_resp, ok} ->
                    ok;
                {attempt_resp, {error, _}} ->
                    menu_loop(Menu#menu{retries=Retries - 1}, Call)
            end;
        {ok, Digits} ->
            %% this try_match_digits calls hunt_for_callflow() based on the digits dialed
            %% if it finds a callflow, the main CFPid will move on to it and try_match_digits
            %% will return true, matching here, and causing menu_loop to exit; this is
            %% expected behaviour as CFPid has moved on from this invocation
            AllowRecord = RecOffnet orelse whapps_call:inception(Call) =:= <<"on-net">>,
            case try_match_digits(Digits, Menu, Call) of
                true -> 
                    ok;
                false when Digits =:= RecordPin, AllowRecord -> 
                    ?LOG("selection matches recording pin"),
                    case record_greeting(tmp_file(), Menu, Call) of
                        {ok, M} ->
                            ?LOG("returning caller to menu"),
                            cf_call_command:b_prompt(<<"menu-return">>, Call),
                            menu_loop(M, Call);
                        {error, _} -> 
                            cf_exe:stop(Call)
                    end;                            
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
-spec(try_match_digits/3 :: (Digits :: binary(), Menu :: #menu{}, Call :: whapps_call:call()) -> boolean()).
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
-spec is_callflow_child/3 :: (ne_binary(), #menu{}, whapps_call:call()) -> boolean().
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
-spec is_hunt_enabled/3 :: (ne_binary(), #menu{}, whapps_call:call()) -> boolean().
is_hunt_enabled(_, #menu{hunt=Hunt}, _) ->
    Hunt.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check whitelist hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_allowed/3 :: (ne_binary(), #menu{}, whapps_call:call()) -> boolean().
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
-spec is_hunt_denied/3 :: (ne_binary(), #menu{}, whapps_call:call()) -> boolean().
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
-spec hunt_for_callflow/3 :: (ne_binary(), #menu{}, whapps_call:call()) -> boolean().
hunt_for_callflow(Digits, Menu, Call) ->
    AccountId = whapps_call:account_id(Call),
    ?LOG("hunting for ~s in account ~s", [Digits, AccountId]),
    case cf_util:lookup_callflow(Digits, AccountId) of
        {ok, Flow, false} ->
            ?LOG("callflow hunt succeeded, branching"),
            cf_call_command:flush_dtmf(Call),
            play_transferring_prompt(Menu, Call),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow, wh_json:new()), Call),
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
-spec record_greeting/3 :: (binary(), #menu{}, whapps_call:call()) -> {ok, #menu{}} | {error, wh_json:json_object()}.
record_greeting(AttachmentName, #menu{greeting_id=undefined}=Menu, Call) ->
    MediaId = recording_media_doc(<<"greeting">>, Menu, Call),
    record_greeting(AttachmentName, Menu#menu{greeting_id=MediaId}, Call);
record_greeting(AttachmentName, #menu{greeting_id=MediaId}=Menu, Call) ->
    ?LOG("recording new menu greeting"),
    cf_call_command:audio_macro([{prompt, <<"vm-record_greeting">>}
                                 ,{tones, [wh_json:from_list([{<<"Frequencies">>, [440]}
                                                              ,{<<"Duration-ON">>, 500}
                                                              ,{<<"Duration-OFF">>, 100}
                                                             ])
                                          ]}
                                ], Call),
    case cf_call_command:b_record(AttachmentName, Call) of
        {error, _}=E -> E;
        {ok, JObj} ->
            NoRec = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_greeting_length">>, 1500) 
                > wh_json:get_integer_value(<<"Length">>, JObj),
            case review_recording(AttachmentName, Menu, Call) of
                {ok, record} ->
                    record_greeting(tmp_file(), Menu, Call);
                {ok, save} when NoRec ->
                    cf_call_command:b_prompt(<<"vm-recording_to_short">>, Call),
                    record_greeting(tmp_file(), Menu, Call);
                {ok, save} ->
                    {ok, _} = store_recording(AttachmentName, MediaId, Call),
                    ok = update_doc([<<"media">>, <<"greeting">>], MediaId, Menu, Call),
                    cf_call_command:b_prompt(<<"vm-saved">>, Call),
                    {ok, Menu};
                {ok, no_selection} ->
                    ?LOG("abandoning recorded greeting"),
                    cf_call_command:b_prompt(<<"vm-deleted">>, Call),
                    {ok, Menu};
                {error, _}=E -> E
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_invalid_prompt/2 :: (Menu :: #menu{}, Call :: whapps_call:call()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
play_invalid_prompt(#menu{invalid_media=false}, _) ->
    {ok, wh_json:new()};
play_invalid_prompt(#menu{invalid_media=true}, Call) ->
    cf_call_command:b_prompt(<<"menu-invalid_entry">>, Call);
play_invalid_prompt(#menu{invalid_media=Id}, Call) ->
    cf_call_command:b_play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_transferring_prompt/2 :: (Menu :: #menu{}, Call :: whapps_call:call()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
play_transferring_prompt(#menu{transfer_media=false}, _) ->
    {ok, wh_json:new()};
play_transferring_prompt(#menu{transfer_media=true}, Call) ->
    cf_call_command:b_prompt(<<"menu-transferring_call">>, Call);
play_transferring_prompt(#menu{transfer_media=Id}, Call) ->
    cf_call_command:b_play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_exit_prompt/2 :: (Menu :: #menu{}, Call :: whapps_call:call()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
play_exit_prompt(#menu{exit_media=false}, _) ->
    {ok, wh_json:new()};
play_exit_prompt(#menu{exit_media=true}, Call) ->
    cf_call_command:b_prompt(<<"menu-exit">>, Call);
play_exit_prompt(#menu{exit_media=Id}, Call) ->
    cf_call_command:b_play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_prompt/2 :: (#menu{}, whapps_call:call()) -> ne_binary().
get_prompt(#menu{greeting_id=undefined}, _) ->
    cf_util:get_prompt(<<"menu-no_prompt">>);
get_prompt(#menu{greeting_id = <<"local_stream://", _/binary>> = ID}, _) ->
    ID;
get_prompt(#menu{greeting_id=Id}, Call) ->
    <<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording/3 :: (binary(), binary(), whapps_call:call()) -> {ok, wh_json:json_object()} | {error, wh_json:json_object()}.
store_recording(AttachmentName, MediaId, Call) ->
    ?LOG("storing recording ~s as media ~s", [AttachmentName, MediaId]),
    ok = update_doc(<<"content_type">>, <<"audio/mpeg">>, MediaId, Call),
    cf_call_command:b_store(AttachmentName, get_new_attachment_url(AttachmentName, MediaId, Call), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_attachment_url/3 :: (binary(), binary(), whapps_call:call()) -> binary().
get_new_attachment_url(AttachmentName, MediaId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, MediaId) of
        {ok, JObj} ->
            case wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())) of
                [] ->
                    ok;
                Existing ->
                    [couch_mgr:delete_attachment(AccountDb, MediaId, Attach) || Attach <- Existing]
            end;
        {error, _} ->
            ok
    end,
    Rev = case couch_mgr:lookup_doc_rev(AccountDb, MediaId) of
              {ok, R} ->
                  <<"?rev=", R/binary>>;
              _ ->
                  <<>>
          end,
    <<(couch_mgr:get_url())/binary
      ,AccountDb/binary
      ,$/, MediaId/binary
      ,$/, AttachmentName/binary
      ,Rev/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_file/0 :: () -> ne_binary().
tmp_file() ->
     <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".mp3">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec review_recording/3 :: (ne_binary(), #menu{}, whapps_call:call()) -> {ok, record | save | no_selection}.
review_recording(MediaName, #menu{keys=#keys{listen=ListenKey, record=RecordKey, save=SaveKey}}=Menu, Call) ->
    ?LOG("playing menu greeting review options"),
    cf_call_command:flush_dtmf(Call),
    Prompt = cf_util:get_prompt(<<"vm-review_recording">>),
    case cf_call_command:b_play_and_collect_digit(Prompt, Call) of
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
-spec recording_media_doc/3 :: (binary(), #menu{}, whapps_call:call()) -> binary().
recording_media_doc(Type, #menu{name=MenuName, menu_id=Id}, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Name = <<MenuName/binary, " menu ", Type/binary >>,
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"menu recorded/prompt media">>}
             ,{<<"source_type">>, <<"menu">>}
             ,{<<"source_id">>, Id}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, true}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb, [{type, <<"media">>}]),
    {ok, JObj} = couch_mgr:save_doc(AccountDb, Doc),
    wh_json:get_value(<<"_id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc/4 :: (list() | binary(), wh_json:json_term(), #menu{} | binary(),  whapps_call:call() | binary()) -> ok | {error, atom()}.
update_doc(Key, Value, #menu{menu_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, Call) when is_tuple(Call) ->
    update_doc(Key, Value, Id, whapps_call:account_db(Call));
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
-spec get_menu_profile/2 :: (wh_json:json_object(), whapps_call:call()) -> #menu{}.
get_menu_profile(Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, Id) of
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
                  ,record_from_offnet =
                      wh_json:is_true(<<"allow_record_from_offnet">>, JObj, Default#menu.record_from_offnet)
                  ,greeting_id =
                      wh_json:get_ne_value([<<"media">>, <<"greeting">>], JObj)
                  ,exit_media =
                      (not wh_json:is_false([<<"media">>, <<"exit_media">>], JObj))
                  andalso wh_json:get_ne_value([<<"media">>, <<"exit_media">>], JObj, true)
                  ,transfer_media = 
                      (not wh_json:is_false([<<"media">>, <<"transfer_media">>], JObj))
                  andalso wh_json:get_ne_value([<<"media">>, <<"transfer_media">>], JObj, true)
                  ,invalid_media =
                      (not wh_json:is_false([<<"media">>, <<"invalid_media">>], JObj)) 
                  andalso wh_json:get_ne_value([<<"media">>, <<"invalid_media">>], JObj, true)
                 };
        {error, R} ->
            ?LOG("failed to load menu route ~s, ~w", [Id, R]),
            #menu{}
    end.
