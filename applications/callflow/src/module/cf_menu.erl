%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "id":"menu_id"
%%%   // optional after here
%%%   "interdigit_timeout":2000
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_menu).

-export([handle/2]).

-include("../callflow.hrl").

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".menu">>).

-record(menu_keys, {
           %% Record Review
           save = <<"1">> :: ne_binary()
          ,listen = <<"2">> :: ne_binary()
          ,record = <<"3">> :: ne_binary()
         }).
-type menu_keys() :: #menu_keys{}.
-define(MENU_KEY_LENGTH, 1).

-record(cf_menu_data, {
          menu_id :: api_binary()
         ,name = <<>> :: binary()
         ,retries = 3 :: pos_integer()
         ,timeout = 10000 :: pos_integer()
         ,max_length = 4 :: pos_integer()
         ,hunt = 'false' :: boolean()
         ,hunt_deny = <<>> :: binary()
         ,hunt_allow = <<>> :: binary()
         ,record_pin = <<>> :: binary()
         ,record_from_offnet = 'false' :: boolean()
         ,greeting_id :: api_binary()
         ,exit_media = 'true' :: boolean() | ne_binary()
         ,transfer_media = 'true' :: boolean() | ne_binary()
         ,invalid_media = 'true' :: boolean() | ne_binary()
         ,keys = #menu_keys{} :: menu_keys()
         ,interdigit_timeout = whapps_call_command:default_interdigit_timeout() :: pos_integer()
         }).
-type menu() :: #cf_menu_data{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Menu = get_menu_profile(Data, Call),
    whapps_call_command:answer(Call),
    menu_loop(Menu, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The main auto-attendant loop, will execute for the number
%% of retries playing the greeting and collecting digits till the
%% digits are routable
%% @end
%%--------------------------------------------------------------------
-spec menu_loop(menu(), whapps_call:call()) -> 'ok'.
menu_loop(#cf_menu_data{retries=Retries}=Menu, Call) when Retries =< 0 ->
    lager:info("maxium number of retries reached"),
    _ = whapps_call_command:flush_dtmf(Call),
    _ = play_exit_prompt(Menu, Call),
    case cf_exe:attempt(<<"max_retries">>, Call) of
        {'attempt_resp', 'ok'} -> 'ok';
        {'attempt_resp', {'error', _}} ->
            _ = case cf_exe:wildcard_is_empty(Call) of
                    'true' -> whapps_call_command:b_prompt(<<"vm-goodbye">>, Call);
                    'false' -> play_transferring_prompt(Menu, Call)
                end,
            cf_exe:continue(Call)
    end;
menu_loop(#cf_menu_data{max_length=MaxLength
                        ,timeout=Timeout
                        ,interdigit_timeout=Interdigit
                       }=Menu, Call) ->
    NoopId = whapps_call_command:play(get_prompt(Menu, Call), Call),

    case whapps_call_command:collect_digits(MaxLength, Timeout, Interdigit, NoopId, Call) of
        {'ok', <<>>} ->
            menu_handle_no_digits(Menu, Call);
        {'ok', Digits} ->
            menu_handle_digits(Menu, Call, Digits);
        {'error', _} ->
            lager:info("caller hungup while in the menu"),
            cf_exe:stop(Call)
    end.

menu_handle_digits(#cf_menu_data{retries=Retries
                                 ,record_from_offnet=RecOffnet
                                 ,record_pin=RecordPin
                                }=Menu, Call, Digits) ->
    %% this try_match_digits calls hunt_for_callflow() based on the digits dialed
    %% if it finds a callflow, the main CFPid will move on to it and try_match_digits
    %% will return true, matching here, and causing menu_loop to exit; this is
    %% expected behaviour as CFPid has moved on from this invocation
    AllowRecord = (RecOffnet orelse whapps_call:inception(Call) =:= 'undefined'),
    case try_match_digits(Digits, Menu, Call) of
        'true' -> lager:debug("hunt callflow found");
        'false' when Digits =:= RecordPin, AllowRecord ->
            menu_handle_record(Menu, Call);
        'false' ->
            lager:info("invalid selection ~w", [Digits]),
            _ = play_invalid_prompt(Menu, Call),
            menu_loop(Menu#cf_menu_data{retries=Retries - 1}, Call)
    end.

-spec menu_handle_record(menu(), whapps_call:call()) -> 'ok'.
menu_handle_record(Menu, Call) ->
    lager:info("selection matches recording pin"),
    case record_greeting(tmp_file(), Menu, Call) of
        {'ok', M} ->
            lager:info("returning caller to menu"),
            _ = whapps_call_command:b_prompt(<<"menu-return">>, Call),
            menu_loop(M, Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec menu_handle_no_digits(menu(), whapps_call:call()) -> 'ok'.
menu_handle_no_digits(#cf_menu_data{retries=Retries}=Menu, Call) ->
    lager:info("menu entry timeout"),
    case try_match_digits(<<"timeout">>, Menu, Call) of
        'true' -> lager:debug("timeout hunt callflow found");
        'false' ->
            case cf_exe:attempt(<<"timeout">>, Call) of
                {'attempt_resp', 'ok'} -> 'ok';
                {'attempt_resp', {'error', _}} ->
                    menu_loop(Menu#cf_menu_data{retries=Retries - 1}, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The primary sequence logic to route the collected digits
%% @end
%%--------------------------------------------------------------------
-spec try_match_digits(ne_binary(), menu(), whapps_call:call()) -> boolean().
try_match_digits(Digits, Menu, Call) ->
    lager:info("trying to match digits ~s", [Digits]),
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
-spec is_callflow_child(ne_binary(), menu(), whapps_call:call()) -> boolean().
is_callflow_child(Digits, _, Call) ->
    case cf_exe:attempt(Digits, Call) of
        {'attempt_resp', 'ok'} ->
            lager:info("selection is a callflow child"),
            'true';
        {'attempt_resp', {'error', _}} -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if hunting is enabled
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_enabled(ne_binary(), menu(), whapps_call:call()) -> boolean().
is_hunt_enabled(_, #cf_menu_data{hunt=Hunt}, _) ->
    Hunt.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check whitelist hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_allowed(ne_binary(), menu(), whapps_call:call()) -> boolean().
is_hunt_allowed(_, #cf_menu_data{hunt_allow = <<>>}, _) ->
    lager:info("hunt_allow implicitly accepted digits"),
    'true';
is_hunt_allowed(Digits, #cf_menu_data{hunt_allow=RegEx}, _) ->
    try re:run(Digits, RegEx) of
        {'match', _} ->
            lager:info("hunt_allow accepted digits"),
            'true';
        'nomatch' ->
            lager:info("hunt_allow denied digits"),
            'false'
    catch
        _E:_R ->
            lager:info("failed to run regex ~s: ~s: ~p", [RegEx, _E, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check blacklisted hunt digit patterns
%% @end
%%--------------------------------------------------------------------
-spec is_hunt_denied(ne_binary(), menu(), whapps_call:call()) -> boolean().
is_hunt_denied(_, #cf_menu_data{hunt_deny = <<>>}, _) ->
    lager:info("hunt_deny implicitly accepted digits"),
    'false';
is_hunt_denied(Digits, #cf_menu_data{hunt_deny=RegEx}, _) ->
    try re:run(Digits, RegEx) of
        {'match', _} ->
            lager:info("hunt_deny denied digits"),
            'true';
        'nomatch' ->
            lager:info("hunt_deny accepted digits"),
            'false'
    catch
        _E:_R ->
            lager:info("failed to run regex ~s: ~s: ~p", [RegEx, _E, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Hunt for a callflow with these numbers
%% @end
%%--------------------------------------------------------------------
-spec hunt_for_callflow(ne_binary(), menu(), whapps_call:call()) -> boolean().
hunt_for_callflow(Digits, Menu, Call) ->
    AccountId = whapps_call:account_id(Call),
    lager:info("hunting for ~s in account ~s", [Digits, AccountId]),
    case cf_util:lookup_callflow(Digits, AccountId) of
        {'ok', Flow, 'false'} ->
            lager:info("callflow hunt succeeded, branching"),
            _ = whapps_call_command:flush_dtmf(Call),
            _ = play_transferring_prompt(Menu, Call),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow, wh_json:new()), Call),
            'true';
        _ ->
            lager:info("callflow hunt failed"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_greeting(binary(), menu(), whapps_call:call()) ->
                             {'ok', menu()} |
                             {'error', wh_json:object()}.
record_greeting(AttachmentName, #cf_menu_data{greeting_id='undefined'}=Menu, Call) ->
    MediaId = recording_media_doc(<<"greeting">>, Menu, Call),
    record_greeting(AttachmentName, Menu#cf_menu_data{greeting_id=MediaId}, Call);
record_greeting(AttachmentName, #cf_menu_data{greeting_id=MediaId}=Menu, Call) ->
    lager:info("recording new menu greeting"),
    _ = whapps_call_command:audio_macro([{'prompt', <<"vm-record_greeting">>}
                                         ,{'tones', [wh_json:from_list([{<<"Frequencies">>, [440]}
                                                                        ,{<<"Duration-ON">>, 500}
                                                                        ,{<<"Duration-OFF">>, 100}
                                                                       ])
                                                    ]}
                                        ], Call),
    case whapps_call_command:b_record(AttachmentName, Call) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            NoRec = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_greeting_length">>, 1500)
                > wh_json:get_integer_value(<<"Length">>, JObj),
            case review_recording(AttachmentName, Menu, Call) of
                {'ok', 'record'} ->
                    record_greeting(tmp_file(), Menu, Call);
                {'ok', 'save'} when NoRec ->
                    _ = whapps_call_command:b_prompt(<<"vm-recording_to_short">>, Call),
                    record_greeting(tmp_file(), Menu, Call);
                {'ok', 'save'} ->
                    {'ok', _} = store_recording(AttachmentName, MediaId, Call),
                    'ok' = update_doc([<<"media">>, <<"greeting">>], MediaId, Menu, Call),
                    _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
                    {'ok', Menu};
                {'ok', 'no_selection'} ->
                    lager:info("abandoning recorded greeting"),
                    _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
                    {'ok', Menu}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_invalid_prompt(menu(), whapps_call:call()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', atom()}.
play_invalid_prompt(#cf_menu_data{invalid_media='false'}, _) ->
    {'ok', wh_json:new()};
play_invalid_prompt(#cf_menu_data{invalid_media='true'}, Call) ->
    whapps_call_command:b_prompt(<<"menu-invalid_entry">>, Call);
play_invalid_prompt(#cf_menu_data{invalid_media=Id}, Call) ->
    whapps_call_command:b_play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_transferring_prompt(menu(), whapps_call:call()) ->
                                      {'ok', wh_json:object()} |
                                      {'error', atom()}.
play_transferring_prompt(#cf_menu_data{transfer_media='false'}, _) ->
    {'ok', wh_json:new()};
play_transferring_prompt(#cf_menu_data{transfer_media='true'}, Call) ->
    whapps_call_command:b_prompt(<<"menu-transferring_call">>, Call);
play_transferring_prompt(#cf_menu_data{transfer_media=Id}, Call) ->
    whapps_call_command:b_play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_exit_prompt(menu(), whapps_call:call()) ->
                              {'ok', wh_json:object()} |
                              {'error', atom()}.
play_exit_prompt(#cf_menu_data{exit_media='false'}, _) ->
    {'ok', wh_json:new()};
play_exit_prompt(#cf_menu_data{exit_media='true'}, Call) ->
    whapps_call_command:b_prompt(<<"menu-exit">>, Call);
play_exit_prompt(#cf_menu_data{exit_media=Id}, Call) ->
    whapps_call_command:b_play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_prompt(menu(), whapps_call:call()) -> ne_binary().
get_prompt(#cf_menu_data{greeting_id='undefined'}, Call) ->
    wh_media_util:get_prompt(<<"menu-no_prompt">>, Call);
get_prompt(#cf_menu_data{greeting_id = <<"local_stream://", _/binary>> = ID}, _) ->
    ID;
get_prompt(#cf_menu_data{greeting_id=Id}, Call) ->
    <<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary(), whapps_call:call()) ->
                             {'ok', wh_json:object()} |
                             {'error', wh_json:object()}.
store_recording(AttachmentName, MediaId, Call) ->
    lager:info("storing recording ~s as media ~s", [AttachmentName, MediaId]),
    CallerIdName = whapps_call:caller_id_name(Call),
    Description = <<"recorded by ", CallerIdName/binary>>,
    Updates = [{<<"content_type">>, <<"audio/mpeg">>}
               ,{<<"media_source">>, <<"recording">>}
               ,{<<"description">>, Description}
              ],
    'ok' = update_doc(Updates, MediaId, Call),
    whapps_call_command:b_store(AttachmentName, get_new_attachment_url(AttachmentName, MediaId, Call), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_attachment_url(binary(), binary(), whapps_call:call()) -> ne_binary().
get_new_attachment_url(AttachmentName, MediaId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_cache_doc(AccountDb, MediaId) of
            {'ok', JObj} ->
                maybe_delete_attachments(AccountDb, MediaId, JObj);
            {'error', _} -> 'ok'
        end,
    {'ok', URL} = wh_media_url:store(AccountDb, MediaId, AttachmentName),
    URL.

-spec maybe_delete_attachments(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_delete_attachments(AccountDb, _MediaId, JObj) ->
    case wh_doc:maybe_remove_attachments(JObj) of
        {'false', _} -> 'ok';
        {'true', Removed} ->
            couch_mgr:save_doc(AccountDb, Removed),
            lager:debug("removing attachments from ~s", [_MediaId])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_file() -> ne_binary().
tmp_file() ->
     <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".mp3">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec review_recording(ne_binary(), menu(), whapps_call:call()) ->
                              {'ok', 'record' | 'save' | 'no_selection'}.
review_recording(MediaName, #cf_menu_data{keys=#menu_keys{listen=ListenKey
                                                          ,record=RecordKey
                                                          ,save=SaveKey
                                                         }
                                          ,timeout=Timeout
                                          ,interdigit_timeout=Interdigit
                                         }=Menu, Call) ->
    lager:info("playing menu greeting review options"),
    _ = whapps_call_command:flush_dtmf(Call),

    NoopId = whapps_call_command:prompt(<<"vm-review_recording">>, Call),

    case whapps_call_command:collect_digits(?MENU_KEY_LENGTH, Timeout, Interdigit, NoopId, Call) of
        {'ok', ListenKey} ->
            _ = whapps_call_command:b_play(MediaName, Call),
            review_recording(MediaName, Menu, Call);
        {'ok', RecordKey} -> {'ok', 'record'};
        {'ok', SaveKey} -> {'ok', 'save'};
        {'ok', _} -> review_recording(MediaName, Menu, Call);
        {'error', _} -> {'ok', 'no_selection'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recording_media_doc(ne_binary(), menu(), whapps_call:call()) -> ne_binary().
recording_media_doc(Type, #cf_menu_data{name=MenuName
                                        ,menu_id=Id
                                       }, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Name = <<MenuName/binary, " menu ", Type/binary >>,
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"menu recorded/prompt media">>}
             ,{<<"source_type">>, <<"menu">>}
             ,{<<"source_id">>, Id}
             ,{<<"media_source">>, <<"recording">>}
             ,{<<"streamable">>, 'true'}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb, [{'type', <<"media">>}]),
    {'ok', JObj} = couch_mgr:save_doc(AccountDb, Doc),
    wh_doc:id(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc(text(), wh_json:json_term(), menu() | ne_binary(),  whapps_call:call() | ne_binary()) ->
                        'ok' | {'error', atom()}.
-spec update_doc(wh_proplist(), ne_binary(), whapps_call:call() | ne_binary()) ->
                        'ok' | {'error', atom()}.
update_doc(Key, Value, #cf_menu_data{menu_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, <<_/binary>> = Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {'error', _}=E -> lager:info("unable to update ~s in ~s, ~p", [Id, Db, E]);
        {'ok', JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {'error', 'conflict'} -> update_doc(Key, Value, Id, Db);
                {'ok', _} -> 'ok';
                {'error', _}=E -> lager:info("unable to update ~s in ~s, ~p", [Id, Db, E])
            end
    end;
update_doc(Key, Value, Id, Call) ->
    update_doc(Key, Value, Id, whapps_call:account_db(Call)).

update_doc(Updates, Id, <<_/binary>> = Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {'error', _}=E -> lager:info("unable to update ~s in ~s, ~p", [Id, Db, E]);
        {'ok', JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_values(Updates, JObj)) of
                {'error', 'conflict'} -> update_doc(Updates, Id, Db);
                {'ok', _} -> 'ok';
                {'error', _}=E -> lager:info("unable to update ~s in ~s, ~p", [Id, Db, E])
            end
    end;
update_doc(Updates, Id, Call) ->
    update_doc(Updates, Id, whapps_call:account_db(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_menu_profile(wh_json:object(), whapps_call:call()) -> menu().
get_menu_profile(Data, Call) ->
    Id = wh_doc:id(Data),
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, Id) of
        {'ok', JObj} ->
            lager:info("loaded menu route ~s", [Id]),
            Default = #cf_menu_data{},
            #cf_menu_data{menu_id = Id
                          ,name =
                              wh_json:get_ne_value(<<"name">>, JObj, Id)
                          ,retries =
                              wh_json:get_integer_value(<<"retries">>, JObj, Default#cf_menu_data.retries)
                          ,timeout =
                              wh_json:get_integer_value(<<"timeout">>, JObj, Default#cf_menu_data.timeout)
                          ,max_length =
                              wh_json:get_integer_value(<<"max_extension_length">>, JObj, Default#cf_menu_data.max_length)
                          ,hunt =
                              wh_json:is_true(<<"hunt">>, JObj, Default#cf_menu_data.hunt)
                          ,hunt_deny =
                              wh_json:get_value(<<"hunt_deny">>, JObj, Default#cf_menu_data.hunt_deny)
                          ,hunt_allow =
                              wh_json:get_value(<<"hunt_allow">>, JObj, Default#cf_menu_data.hunt_allow)
                          ,record_pin =
                              wh_json:get_value(<<"record_pin">>, JObj, Default#cf_menu_data.record_pin)
                          ,record_from_offnet =
                              wh_json:is_true(<<"allow_record_from_offnet">>, JObj, Default#cf_menu_data.record_from_offnet)
                          ,greeting_id =
                              wh_json:get_ne_value([<<"media">>, <<"greeting">>], JObj)
                          ,exit_media =
                              (not wh_json:is_false([<<"media">>, <<"exit_media">>], JObj))
                          andalso wh_json:get_ne_value([<<"media">>, <<"exit_media">>], JObj, 'true')
                          ,transfer_media =
                              (not wh_json:is_false([<<"media">>, <<"transfer_media">>], JObj))
                          andalso wh_json:get_ne_value([<<"media">>, <<"transfer_media">>], JObj, 'true')
                          ,invalid_media =
                              (not wh_json:is_false([<<"media">>, <<"invalid_media">>], JObj))
                          andalso wh_json:get_ne_value([<<"media">>, <<"invalid_media">>], JObj, 'true')
                          ,interdigit_timeout =
                              wh_util:to_integer(
                                wh_json:find(<<"interdigit_timeout">>
                                             ,[JObj, Data]
                                             ,whapps_call_command:default_interdigit_timeout()
                                            ))
                         };
        {'error', R} ->
            lager:info("failed to load menu route ~s, ~w", [Id, R]),
            #cf_menu_data{}
    end.
