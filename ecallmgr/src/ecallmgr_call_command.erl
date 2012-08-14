%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012 VoIP INC
%%% @doc
%%% Execute call commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_call_command).

-export([exec_cmd/4]).

-include("ecallmgr.hrl").

-spec exec_cmd/4 :: (atom(), ne_binary(), wh_json:json_object(), pid()) ->
                            'ok' |
                            'error' |
                            ecallmgr_util:send_cmd_ret() |
                            [ecallmgr_util:send_cmd_ret(),...].
exec_cmd(Node, UUID, JObj, ControlPID) ->
    DestID = wh_json:get_value(<<"Call-ID">>, JObj),
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    case DestID =:= UUID of
        true ->
            case get_fs_app(Node, UUID, JObj, App) of
                {'error', Msg} ->
                    _ = ecallmgr_util:fs_log(Node, "whistle error while building command ~s: ~s", [App, Msg]),
                    throw({msg, Msg});
                {return, Result} ->
                    Result;
                {AppName, noop} ->
                    ecallmgr_call_control:event_execute_complete(ControlPID, UUID, AppName);
                {AppName, AppData} ->
                    ecallmgr_util:send_cmd(Node, UUID, AppName, AppData);
                Apps when is_list(Apps) ->
                    [ecallmgr_util:send_cmd(Node, UUID, AppName, AppData) || {AppName, AppData} <- Apps]
            end;
        false ->
            lager:debug("command ~s not meant for us but for ~s", [wh_json:get_value(<<"Application-Name">>, JObj), DestID]),
            throw(<<"call command provided with a command for a different call id">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% return the app name and data (as a binary string) to send to
%% the FS ESL via mod_erlang_event
%% @end
%%--------------------------------------------------------------------
-type fs_app() :: {ne_binary(), ne_binary() | 'noop'}.
-spec get_fs_app/4 :: (atom(), ne_binary(), wh_json:json_object(), ne_binary()) ->
                              fs_app() |
                              {'return', 'error'} |
                              {'error', ne_binary()} |
                              [fs_app(),...].
get_fs_app(_Node, _UUID, JObj, <<"noop">>) ->
    case wapi_dialplan:noop_v(JObj) of
        false ->
            {'error', <<"noop failed to execute as JObj did not validate">>};
        true ->
            Args = case wh_json:get_value(<<"Msg-ID">>, JObj) of
                       undefined ->
                           <<"Event-Subclass=whistle::noop,Event-Name=CUSTOM"
                             ,",whistle_event_name=CHANNEL_EXECUTE_COMPLETE"
                             ,",whistle_application_name=noop">>;
                       NoopId ->
                           <<"Event-Subclass=whistle::noop,Event-Name=CUSTOM"
                             ,",whistle_event_name=CHANNEL_EXECUTE_COMPLETE"
                             ,",whistle_application_name=noop"
                             ,",whistle_application_response=", (wh_util:to_binary(NoopId))/binary>>
                   end,
            {<<"event">>, Args}
    end;

get_fs_app(Node, UUID, JObj, <<"play">>) ->
    case wapi_dialplan:play_v(JObj) of
        false -> {'error', <<"play failed to execute as JObj did not validate">>};
        true ->
            F = ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), new, UUID, JObj, ?ECALLMGR_CALL_CACHE),
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),

            _ = case wh_json:get_value(<<"Group-ID">>, JObj) of
                    undefined -> ok;
                    GID -> set(Node, UUID, <<"media_group_id=", (GID)/binary>>)
                end,

            %% if Leg is set, use uuid_broadcast; otherwise use playback
            case wh_json:get_value(<<"Leg">>, JObj) of
                <<"A">> -> {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" aleg">>])};
                <<"B">> -> {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" bleg">>])};
                <<"Both">> -> {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" both">>])};
                _ -> {<<"playback">>, F}
            end
    end;

get_fs_app(_Node, _UUID, JObj, <<"playstop">>) ->
    case wapi_dialplan:playstop_v(JObj) of
        false -> {'error', <<"playstop failed to execute as JObj did not validate">>};
        true -> {<<"playstop">>, <<>>}
    end;

get_fs_app(_Node, _UUID, JObj, <<"hangup">>) ->
    case wh_json:is_true(<<"Other-Leg-Only">>, JObj, false) of
        false -> {<<"hangup">>, <<>>};
        true ->  {<<"unbridge">>, <<>>}
    end;

get_fs_app(_Node, UUID, JObj, <<"play_and_collect_digits">>) ->
    case wapi_dialplan:play_and_collect_digits_v(JObj) of
        false -> {'error', <<"play_and_collect_digits failed to execute as JObj did not validate">>};
        true ->
            Min = wh_json:get_value(<<"Minimum-Digits">>, JObj),
            Max = wh_json:get_value(<<"Maximum-Digits">>, JObj),
            Timeout = wh_json:get_value(<<"Timeout">>, JObj),
            Terminators = wh_json:get_value(<<"Terminators">>, JObj),
            Media = <<$', (ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), new, UUID, JObj, ?ECALLMGR_CALL_CACHE))/binary, $'>>,
            InvalidMedia = <<$', (ecallmgr_util:media_path(wh_json:get_value(<<"Failed-Media-Name">>, JObj), new, UUID, JObj, ?ECALLMGR_CALL_CACHE))/binary, $'>>,
            Tries = wh_json:get_value(<<"Media-Tries">>, JObj),
            Regex = wh_json:get_value(<<"Digits-Regex">>, JObj),
            Storage = <<"collected_digits">>,
            Data = list_to_binary([Min, " ", Max, " ", Tries, " ", Timeout, " ", Terminators, " "
                                   ,Media, " ", InvalidMedia, " ", Storage, " ", Regex]),
            {<<"play_and_get_digits">>, Data}
    end;

get_fs_app(Node, UUID, JObj, <<"record">>) ->
    case wapi_dialplan:record_v(JObj) of
        false -> {'error', <<"record failed to execute as JObj did not validate">>};
        true ->
            MediaName = ecallmgr_util:recording_filename(wh_json:get_value(<<"Media-Name">>, JObj)),

            %% disable buffering to see if we get all the media
            %%'ok' = set(Node, UUID, <<"enable_file_write_buffering=false">>),
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),

            %% some carriers kill the channel during long recordings since there is no
            %% reverse RTP stream
            _ = case wh_util:is_true(ecallmgr_config:get(<<"record_waste_resources">>, false)) of
                    false -> ok;
                    true -> set(Node, UUID, <<"record_waste_resources=true">>)
                end,

            RecArg = list_to_binary([MediaName, " "
                                     ,wh_json:get_string_value(<<"Time-Limit">>, JObj, "20"), " "
                                     ,wh_json:get_string_value(<<"Silence-Threshold">>, JObj, "500"), " "
                                     ,wh_json:get_string_value(<<"Silence-Hits">>, JObj, "5")
                                    ]),
            _ = wh_cache:store_local(?ECALLMGR_CALL_CACHE, ?ECALLMGR_RECORDED_MEDIA_KEY(MediaName), MediaName),

            {<<"record">>, RecArg}
    end;

get_fs_app(Node, UUID, JObj, <<"record_call">>) ->
    case wapi_dialplan:record_call_v(JObj) of
        false -> {'error', <<"record_call failed to execute as JObj did not validate">>};
        true ->
            MediaName = ecallmgr_util:recording_filename(wh_json:get_value(<<"Media-Name">>, JObj)),

            case wh_json:get_value(<<"Record-Action">>, JObj) of
                <<"start">> ->
                    _ = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),

                    _ = set(Node, UUID, <<"RECORD_APPEND=true">>), % allow recording to be appended to
                    _ = set(Node, UUID, <<"enable_file_write_buffering=false">>), % disable buffering to see if we get all the media

                    %% some carriers kill the channel during long recordings since there is no
                    %% reverse RTP stream
                    _ = case wh_util:is_true(ecallmgr_config:get(<<"record_waste_resources">>, false)) of
                            false -> ok;
                            true -> set(Node, UUID, <<"record_waste_resources=true">>)
                        end,

                    %% UUID start path/to/media limit
                    RecArg = binary_to_list(list_to_binary([
                                                            UUID, <<" start ">>
                                                           ,MediaName, <<" ">>
                                                           ,wh_json:get_string_value(<<"Time-Limit">>, JObj, "3600") % one hour
                                                           ])),
                    _ = wh_cache:store_local(?ECALLMGR_CALL_CACHE, ?ECALLMGR_RECORDED_MEDIA_KEY(MediaName), MediaName),
                    {<<"record_call">>, RecArg};
                <<"stop">> ->
                    %% UUID stop path/to/media
                    RecArg = binary_to_list(list_to_binary([UUID, <<" stop ">>, MediaName])),
                    {<<"record_call">>, RecArg}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"store">>) ->
    case wapi_dialplan:store_v(JObj) of
        false -> {'error', <<"store failed to execute as JObj did not validate">>};
        true ->
            MediaName = ecallmgr_util:recording_filename(wh_json:get_value(<<"Media-Name">>, JObj)),
            lager:debug("streaming media ~s", [MediaName]),
            case wh_json:get_value(<<"Media-Transfer-Method">>, JObj) of
                <<"put">> ->
                    %% stream file over HTTP PUT
                    lager:debug("stream ~s via HTTP PUT", [MediaName]),
                    stream_over_http(Node, UUID, MediaName, put, store, JObj),
                    {<<"store">>, noop};
                <<"post">> ->
                    %% stream file over HTTP POST
                    lager:debug("stream ~s via HTTP POST", [MediaName]),
                    stream_over_http(Node, UUID, MediaName, post, store, JObj),
                    {<<"store">>, noop};
                _Method ->
                    %% unhandled method
                    lager:debug("unhandled stream method ~s", [_Method]),
                    {return, error}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"store_fax">> = App) ->
    case wapi_dialplan:store_fax_v(JObj) of
        false -> {'error', <<"store_fax failed to execute as JObj did not validate">>};
        true ->
            File = ecallmgr_util:fax_filename(UUID),
            lager:debug("attempting to store fax on ~s: ~s", [Node, File]),
            case wh_json:get_value(<<"Media-Transfer-Method">>, JObj) of
                <<"put">> ->
                    stream_over_http(Node, UUID, File, put, fax, JObj),
                    {App, noop};
                _Method ->
                    lager:debug("invalid media transfer method for storing fax: ~s", [_Method]),
                    {error, <<"invalid media transfer method">>}
            end
    end;

get_fs_app(_Node, _UUID, JObj, <<"send_dtmf">>) ->
    case wapi_dialplan:send_dtmf_v(JObj) of
        false -> {'error', <<"send_dtmf failed to execute as JObj did not validate">>};
        true ->
            DTMFs = wh_json:get_value(<<"DTMFs">>, JObj),
            Duration = case wh_json:get_binary_value(<<"Duration">>, JObj) of
                           undefined -> <<>>;
                           D -> [<<"@">>, D]
                       end,
            {<<"send_dtmf">>, iolist_to_binary([DTMFs, Duration])}
    end;

get_fs_app(Node, UUID, JObj, <<"tones">>) ->
    case wapi_dialplan:tones_v(JObj) of
        false -> {'error', <<"tones failed to execute as JObj did not validate">>};
        true ->
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
            Tones = wh_json:get_value(<<"Tones">>, JObj, []),
            FSTones = [begin
                           Vol = case wh_json:get_value(<<"Volume">>, Tone) of
                                     undefined -> [];
                                     %% need to map V (0-100) to FS values
                                     V -> list_to_binary(["v=", wh_util:to_list(V), ";"])
                                 end,
                           Repeat = case wh_json:get_value(<<"Repeat">>, Tone) of
                                        undefined -> [];
                                        R -> list_to_binary(["l=", wh_util:to_list(R), ";"])
                                    end,
                           Freqs = string:join([ wh_util:to_list(V) || V <- wh_json:get_value(<<"Frequencies">>, Tone) ], ","),
                           On = wh_util:to_list(wh_json:get_value(<<"Duration-ON">>, Tone)),
                           Off = wh_util:to_list(wh_json:get_value(<<"Duration-OFF">>, Tone)),
                           wh_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
                       end || Tone <- Tones],
            Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
            {<<"playback">>, Arg}
    end;

get_fs_app(_Node, _UUID, _JObj, <<"answer">>) ->
    {<<"answer">>, <<>>};

get_fs_app(_Node, _UUID, _JObj, <<"progress">>) ->
    {<<"pre_answer">>, <<>>};

get_fs_app(Node, UUID, JObj, <<"ring">>) ->
    _ = case wh_json:get_value(<<"Ringback">>, JObj) of
            undefined -> ok;
            Ringback ->
                Stream = ecallmgr_util:media_path(Ringback, extant, UUID, JObj),
                lager:debug("custom ringback: ~s", [Stream]),
                _ = ecallmgr_util:send_cmd(Node, UUID, <<"set">>, <<"ringback=", Stream/binary>>)
        end,
    {<<"ring_ready">>, <<>>};

%% receive a fax from the caller
get_fs_app(Node, UUID, _JObj, <<"receive_fax">>) ->
    _ = set(Node, UUID, <<"fax_enable_t38_request=true">>),
    _ = set(Node, UUID, <<"fax_enable_t38=true">>),

    [{<<"playback">>, <<"silence_stream://2000">>}
     ,{<<"rxfax">>, ecallmgr_util:fax_filename(UUID)}
    ];

get_fs_app(_Node, UUID, JObj, <<"hold">>) ->
    case wh_json:get_value(<<"Hold-Media">>, JObj) of
        undefined -> {<<"endless_playback">>, <<"${hold_music}">>};
        Media ->
            Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
            lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
            [{"application", <<"set hold_music=", Stream/binary>>}
             ,{<<"endless_playback">>, <<"${hold_music}">>}
            ]
    end;

get_fs_app(_Node, _UUID, _JObj, <<"park">>) ->
    {<<"park">>, <<>>};

get_fs_app(_Node, _UUID, JObj, <<"sleep">>) ->
    case wapi_dialplan:sleep_v(JObj) of
        false -> {'error', <<"sleep failed to execute as JObj did not validate">>};
        true -> {<<"sleep">>, wh_json:get_binary_value(<<"Time">>, JObj, <<"50">>)}
    end;

get_fs_app(_Node, _UUID, JObj, <<"say">>) ->
    case wapi_dialplan:say_v(JObj) of
        false -> {'error', <<"say failed to execute as JObj did not validate">>};
        true ->
            Lang = wh_json:get_value(<<"Language">>, JObj),
            Type = wh_json:get_value(<<"Type">>, JObj),
            Method = wh_json:get_value(<<"Method">>, JObj),
            Txt = wh_json:get_value(<<"Say-Text">>, JObj),
            Arg = list_to_binary([Lang, " ", Type, " ", Method, " ", Txt]),
            lager:debug("say command ~s", [Arg]),
            {<<"say">>, Arg}
    end;

get_fs_app(Node, UUID, JObj, <<"bridge">>) ->
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case wapi_dialplan:bridge_v(JObj) of
        false -> {'error', <<"bridge failed to execute as JObj did not validate">>};
        true when Endpoints =:= [] -> {'error', <<"bridge request had no endpoints">>};
        true ->
            %% if we are intending to ring multiple device simultaneously then
            %% execute ring_ready so we dont leave the caller hanging with dead air.
            %% this does not test how many are ACTUALLY dialed (registered)
            %% since that is one of the things we want to be ringing during
            DialSeparator = case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
                                <<"simultaneous">> when length(Endpoints) > 1 ->
                                    lager:debug("bridge is simultaneous to multiple endpoints, starting local ringing"),
                                    %% we don't really care if this succeeds, the call will fail later on
                                    _ = ecallmgr_util:send_cmd(Node, UUID, <<"ring_ready">>, ""),
                                    <<",">>;
                                _Else ->
                                    <<"|">>
                            end,

            DialStrings = ecallmgr_util:build_bridge_string(Endpoints, DialSeparator),

            Generators = [fun(DP) ->
                                  case wh_json:get_integer_value(<<"Timeout">>, JObj) of
                                      undefined ->
                                          DP;
                                      TO when TO > 2 ->
                                          lager:debug("bridge will be attempted for ~p seconds", [TO]),
                                          [{"application", "set call_timeout=" ++ wh_util:to_list(TO)}|DP];
                                      _ ->
                                          lager:debug("bridge timeout invalid overwritting with 20 seconds", []),
                                          [{"application", "set call_timeout=20"}|DP]
                                  end
                          end
                          ,fun(DP) ->
                                   case wh_json:get_value(<<"Ringback">>, JObj) of
                                       undefined ->
                                           {ok, RBSetting} = ecallmgr_util:get_setting(<<"default_ringback">>, <<"%(2000,4000,440,480)">>),
                                           [{"application", "set ringback=" ++ wh_util:to_list(RBSetting)}|DP];
                                       Ringback ->
                                           Stream = ecallmgr_util:media_path(Ringback, extant, UUID, JObj),
                                           lager:debug("bridge has custom ringback: ~s", [Stream]),
                                           [{"application", <<"set ringback=", Stream/binary>>},
                                            {"application", "set instant_ringback=true"}
                                            |DP
                                           ]
                                   end
                           end
                          ,fun(DP) ->
                                   case wh_json:get_value(<<"Hold-Media">>, JObj) of
                                       undefined ->
                                           case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Hold-Media">>], JObj) of
                                               undefined ->
                                                   case ecallmgr_fs_nodes:channel_import_moh(UUID) of
                                                       true -> [{"application", "set import=hold_music"}|DP];
                                                       false -> DP
                                                   end;
                                               Media ->
                                                   Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
                                                   lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
                                                   [{"application", <<"set hold_music=", Stream/binary>>}|DP]
                                           end;
                                       Media ->
                                           Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
                                           lager:debug("bridge has custom music-on-hold: ~s", [Stream]),
                                           [{"application", <<"set hold_music=", Stream/binary>>}|DP]
                                   end
                           end
                          ,fun(DP) ->
                                   case wh_json:is_true(<<"Secure-RTP">>, JObj, false) of
                                       true -> [{"application", "set sip_secure_media=true"}|DP];
                                       false -> DP
                                   end
                           end
                          ,fun(DP) ->
                                   case wh_json:get_value(<<"Media">>, JObj) of
                                       <<"process">> ->
                                           lager:debug("bridge will process media through host switch"),
                                           [{"application", "set bypass_media=false"}|DP];
                                       <<"bypass">> ->
                                           lager:debug("bridge will connect the media peer-to-peer"),
                                           [{"application", "set bypass_media=true"}|DP];
                                       _ ->
                                           DP
                                   end
                           end
                          ,fun(DP) ->
                                   CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
                                   case wh_json:is_json_object(CCVs) of
                                       true ->
                                           [{"application", <<"set ", Var/binary, "=", (wh_util:to_binary(V))/binary>>}
                                            || {K, V} <- wh_json:to_proplist(CCVs),
                                               (Var = props:get_value(K, ?SPECIAL_CHANNEL_VARS)) =/= undefined
                                           ] ++ DP;
                                       _ ->
                                           DP
                                   end
                           end
                          ,fun(DP) ->
                                   [{"application", "set failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH"}
                                    ,{"application", "set continue_on_fail=true"}
                                    |DP
                                   ]
                           end
                          ,fun(DP) ->
                                   J = wh_json:set_value([<<"Custom-Channel-Vars">>, <<"Bridge-ID">>], wh_util:rand_hex_binary(16), JObj),
                                   BridgeCmd = list_to_binary(["bridge ", ecallmgr_fs_xml:get_channel_vars(J), DialStrings]),
                                   [{"application", BridgeCmd}|DP]
                           end
                          ,fun(DP) ->
                                   [{"application", ecallmgr_util:create_masquerade_event(<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>)}
                                    ,{"application", "park "}
                                    |DP
                                   ]
                           end
                         ],
            case DialStrings of
                <<>> ->
                    {error, <<"registrar returned no endpoints">>};
                _ ->
                    lager:debug("creating bridge dialplan"),
                    {<<"xferext">>, lists:foldr(fun(F, DP) -> F(DP) end, [], Generators)}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"call_pickup">>) ->
    case wapi_dialplan:call_pickup_v(JObj) of
        false -> {'error', <<"intercept failed to execute as JObj did not validate">>};
        true ->
            Target = wh_json:get_value(<<"Target-Call-ID">>, JObj),

            case ecallmgr_fs_nodes:fetch_channel(Target) of
                {ok, Channel} ->
                    case wh_json:get_binary_value(<<"node">>, Channel) of
                        Node ->
                            lager:debug("target ~s is on same node(~s) as us", [Target, Node]),
                            get_call_pickup_app(Node, UUID, JObj, Target);
                        OtherNode ->
                            lager:debug("target ~s is on ~s, not ~s...moving", [Target, OtherNode, Node]),
                            true = ecallmgr_fs_nodes:channel_move(Target, OtherNode, Node),
                            get_call_pickup_app(Node, UUID, JObj, Target)
                    end;
                {error, not_found} ->
                    lager:debug("failed to find target callid ~s", [Target]),
                    {error, <<"failed to find target callid ", Target/binary>>}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"execute_extension">>) ->
    case wapi_dialplan:execute_extension_v(JObj) of
        false -> {'error', <<"execute extension failed to execute as JObj did not validate">>};
        true ->
            Generators = [fun(DP) ->
                                  case wh_json:is_true(<<"Reset">>, JObj) of
                                      false -> ok;
                                      true ->
                                          create_dialplan_move_ccvs(<<"Execute-Extension-Original-">>, Node, UUID, DP)
                                  end
                          end
                          ,fun(DP) ->
                                   CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
                                   case wh_json:is_empty(CCVs) of
                                       true -> DP;
                                       false ->
                                           ChannelVars = wh_json:to_proplist(CCVs),
                                           [{"application", <<"set ", (get_fs_kv(K, V, UUID))/binary>>}
                                            || {K, V} <- ChannelVars] ++ DP
                                   end
                           end
                          ,fun(DP) ->
                                   [{"application", <<"set ", ?CHANNEL_VAR_PREFIX, "Executing-Extension=true">>}
                                    ,{"application", <<"execute_extension ", (wh_json:get_value(<<"Extension">>, JObj))/binary>>}
                                    |DP
                                   ]
                           end
                          ,fun(DP) ->
                                   [{"application", <<"unset ", ?CHANNEL_VAR_PREFIX, "Executing-Extension">>}
                                    ,{"application", ecallmgr_util:create_masquerade_event(<<"execute_extension">>
                                                                                               ,<<"CHANNEL_EXECUTE_COMPLETE">>)}
                                    ,{"application", "park "}
                                    |DP
                                   ]
                           end
                          ],
            {<<"xferext">>, lists:foldr(fun(F, DP) -> F(DP) end, [], Generators)}
    end;

get_fs_app(Node, UUID, JObj, <<"tone_detect">>) ->
    case wapi_dialplan:tone_detect_v(JObj) of
        false -> {'error', <<"tone detect failed to execute as JObj did not validate">>};
        true ->
            Key = wh_json:get_value(<<"Tone-Detect-Name">>, JObj),
            Freqs = [ wh_util:to_list(V) || V <- wh_json:get_value(<<"Frequencies">>, JObj) ],
            FreqsStr = string:join(Freqs, ","),
            Flags = case wh_json:get_value(<<"Sniff-Direction">>, JObj, <<"read">>) of
                        <<"read">> -> <<"r">>;
                        <<"write">> -> <<"w">>
                    end,
            Timeout = wh_json:get_value(<<"Timeout">>, JObj, <<"+1000">>),
            HitsNeeded = wh_json:get_value(<<"Hits-Needed">>, JObj, <<"1">>),

            SuccessJObj = case wh_json:get_value(<<"On-Success">>, JObj, []) of
                              %% default to parking the call
                              [] ->
                                  [{<<"Application-Name">>, <<"park">>} | wh_api:extract_defaults(JObj)];
                              AppJObj ->
                                  wh_json:from_list(AppJObj ++ wh_api:extract_defaults(JObj))
                          end,

            {SuccessApp, SuccessData} = case get_fs_app(Node, UUID, SuccessJObj
                                                        ,wh_json:get_value(<<"Application-Name">>, SuccessJObj)) of
                                            %% default to park if passed app isn't right
                                            {'error', _Str} ->
                                                {<<"park">>, <<>>};
                                            {_, _}=Success ->
                                                Success
                                        end,

            Data = list_to_binary([Key, " ", FreqsStr, " ", Flags, " ", Timeout
                                   ," ", SuccessApp, " ", SuccessData, " ", HitsNeeded
                                  ]),

            {<<"tone_detect">>, Data}
    end;

get_fs_app(Node, UUID, JObj, <<"set_terminators">>) ->
    case wapi_dialplan:set_terminators_v(JObj) of
        false -> {'error', <<"set_terminators failed to execute as JObj did not validate">>};
        true ->
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
            {<<"set">>, noop}
    end;

get_fs_app(Node, UUID, JObj, <<"set">>) ->
    case wapi_dialplan:set_v(JObj) of
        false -> {'error', <<"set failed to execute as JObj did not validate">>};
        true ->
            ChannelVars = wh_json:to_proplist(wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
            _ = multiset(Node, UUID, ChannelVars),

            CallVars = wh_json:to_proplist(wh_json:get_value(<<"Custom-Call-Vars">>, JObj, wh_json:new())),
            _ = [ export(Node, UUID, get_fs_kv(K, V, UUID)) || {K, V} <- CallVars],

            {<<"set">>, noop}
    end;

get_fs_app(_Node, _UUID, JObj, <<"respond">>) ->
    case wapi_dialplan:respond_v(JObj) of
        false -> {'error', <<"respond failed to execute as JObj did not validate">>};
        true ->
            Code = wh_json:get_value(<<"Response-Code">>, JObj, ?DEFAULT_RESPONSE_CODE),
            Response = <<Code/binary ," "
                         ,(wh_json:get_value(<<"Response-Message">>, JObj, <<>>))/binary>>,
            {<<"respond">>, Response}
    end;

get_fs_app(Node, UUID, JObj, <<"redirect">>) ->
    case wapi_dialplan:redirect_v(JObj) of
        false -> {'error', <<"redirect failed to execute as JObj did not validate">>};
        true ->
            _ = case wh_json:get_value(<<"Redirect-Server">>, JObj) of
                    undefined ->
                        ok;
                    Server ->
                        set(Node, UUID, <<"sip_rh_X-Redirect-Server=", Server/binary>>)
                end,
            {<<"redirect">>, wh_json:get_value(<<"Redirect-Contact">>, JObj, <<>>)}
    end;

get_fs_app(Node, UUID, JObj, <<"fetch">>) ->
    spawn(fun() ->
                  send_fetch_call_event(Node, UUID, JObj)
          end),
    {<<"fetch">>, noop};

get_fs_app(_Node, _UUID, JObj, <<"conference">>) ->
    case wapi_dialplan:conference_v(JObj) of
        false -> {'error', <<"conference failed to execute as JObj did not validate">>};
        true ->
            ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
            {<<"conference">>, list_to_binary([ConfName, "@default"])}
    end;

get_fs_app(_Node, _UUID, _JObj, _App) ->
    lager:debug("unknown application ~s", [_App]),
    {'error', <<"application unknown">>}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% set channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec get_fs_kv/3 :: (ne_binary(), ne_binary(), ne_binary()) -> binary().
get_fs_kv(<<"Hold-Media">>, Media, UUID) ->
    list_to_binary(["hold_music="
                    ,wh_util:to_list(ecallmgr_util:media_path(Media, extant, UUID, wh_json:new()))
                   ]);
get_fs_kv(Key, Val, _) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        false ->
            list_to_binary([?CHANNEL_VAR_PREFIX, wh_util:to_list(Key), "=", wh_util:to_list(Val)]);
        {_, Prefix} ->
            list_to_binary([Prefix, "=", wh_util:to_list(Val)])
    end.

-spec get_call_pickup_app/4 :: (atom(), ne_binary(), wh_json:json_object(), ne_binary()) ->
                                       {ne_binary(), ne_binary()}.
get_call_pickup_app(Node, UUID, JObj, Target) ->
    _ = case wh_json:is_true(<<"Park-After-Pickup">>, JObj, false) of
            false -> export(Node, UUID, <<"park_after_bridge=false">>);
            true ->
                _ = set(Node, Target, <<"park_after_bridge=true">>),
                set(Node, UUID, <<"park_after_bridge=true">>)
        end,

    _ = case wh_json:is_true(<<"Continue-On-Fail">>, JObj, true) of
            false -> export(Node, UUID, <<"continue_on_fail=false">>);
            true -> export(Node, UUID, <<"continue_on_fail=true">>)
        end,

    _ = case wh_json:is_true(<<"Continue-On-Cancel">>, JObj, true) of
            false -> export(Node, UUID, <<"continue_on_cancel=false">>);
            true -> export(Node, UUID, <<"continue_on_cancel=true">>)
        end,

    _ = export(Node, UUID, <<"failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH">>),

    {<<"call_pickup">>, <<UUID/binary, " ", Target/binary>>}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stream_over_http/6 :: (atom(), ne_binary(), ne_binary(), 'put' | 'post', 'store' | 'fax', wh_json:json_object()) -> any().
stream_over_http(Node, UUID, File, Method, Type, JObj) ->
    Url = wh_util:to_list(wh_json:get_value(<<"Media-Transfer-Destination">>, JObj)),
    lager:debug("streaming via HTTP(~s) to ~s", [Method, Url]),

    Args = list_to_binary([Url, <<" ">>, File]),
    lager:debug("execute on node ~s: http_put(~s)", [Node, Args]),
    Result = case send_fs_store(Node, Args, Method) of
                 {ok, <<"+OK", _/binary>>} ->
                     lager:debug("successfully stored media"),
                     <<"success">>;
                 {ok, Err} ->
                     lager:debug("store media failed: ~s", [Err]),
                     wh_notify:system_alert("Failed to store media file ~s for call ~s on ~s "
                                            ,[File, UUID, Node]
                                            ,[{<<"Details">>, Err}]
                                           ),
                     <<"failure">>;
                 {error, E} ->
                     lager:debug("error executing http_put: ~p", [E]),
                     wh_notify:system_alert("Failed to store media file ~s for call ~s on ~s "
                                            ,[File, UUID, Node]
                                            ,[{<<"Details">>, E}]
                                           ),
                     <<"failure">>;
                 timeout ->
                     lager:debug("timeout waiting for http_put"),
                     wh_notify:system_alert("Failed to store media file ~s for call ~s on ~s "
                                            ,[File, UUID, Node]
                                            ,[{<<"Details">>, <<"Timeout sending http_put to node">>}]
                                           ),
                     <<"timeout">>
             end,
    case Type of
        store -> send_store_call_event(Node, UUID, Result);
        fax -> send_store_fax_call_event(UUID, Result)
    end.
            
-spec send_fs_store/3 :: (atom(), ne_binary(), 'put' | 'post') -> fs_api_ret().
send_fs_store(Node, Args, put) ->
    freeswitch:api(Node, http_put, wh_util:to_list(Args));
send_fs_store(Node, Args, post) ->
    freeswitch:api(Node, http_post, wh_util:to_list(Args)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_terminators/3 :: (atom(), ne_binary(), 'undefined' | binary() | list()) -> 'ok' |
                                                                                     fs_api_ret().
set_terminators(_Node, _UUID, undefined) -> 'ok';
set_terminators(Node, UUID, <<>>) -> set(Node, UUID, <<"playback_terminators=none">>);
set_terminators(Node, UUID, <<"none">>) -> set(Node, UUID, <<"playback_terminators=none">>);
set_terminators(Node, UUID, []) -> set(Node, UUID, <<"playback_terminators=none">>);
set_terminators(Node, UUID, Ts) ->
    Terms = list_to_binary(["playback_terminators=", Ts]),
    set(Node, UUID, Terms).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec multiset/3 :: (atom(), ne_binary(), proplist()) -> ecallmgr_util:send_cmd_ret().
multiset(Node, UUID, Props) ->
    Multiset = lists:foldl(fun({K, V}, Acc) ->
                                   <<"|", (get_fs_kv(K, V, UUID))/binary, Acc/binary>>
                           end, <<>>, Props),
    ecallmgr_util:send_cmd(Node, UUID, "multiset", <<"^^", Multiset/binary>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set/3 :: (atom(), ne_binary(), ne_binary()) -> ecallmgr_util:send_cmd_ret().
set(Node, UUID, Arg) ->
    case wh_util:to_binary(Arg) of
        <<"hold_music=", _/binary>> -> 
            ecallmgr_fs_nodes:channel_set_import_moh(Node, UUID, false);
        _Else -> ok
    end,
    ecallmgr_util:send_cmd(Node, UUID, "set", Arg).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec export/3 :: (atom(), ne_binary(), binary()) -> ecallmgr_util:send_cmd_ret().
export(Node, UUID, Arg) ->
    case wh_util:to_binary(Arg) of
        <<"hold_music=", _/binary>> -> 
            ecallmgr_fs_nodes:channel_set_import_moh(Node, UUID, false);
        _Else -> ok
    end,
    ecallmgr_util:send_cmd(Node, UUID, "export", wh_util:to_list(Arg)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_fetch_call_event/3 :: (atom(), ne_binary(), wh_json:json_object()) -> 'ok'.
send_fetch_call_event(Node, UUID, JObj) ->
    try
        Prop = case wh_util:is_true(wh_json:get_value(<<"From-Other-Leg">>, JObj)) of
                   true ->
                       {ok, OtherUUID} = freeswitch:api(Node, uuid_getvar, wh_util:to_list(<<UUID/binary, " bridge_uuid">>)),
                       {ok, Dump} = freeswitch:api(Node, uuid_dump, wh_util:to_list(OtherUUID)),
                       ecallmgr_util:eventstr_to_proplist(Dump);
                   false ->
                       {ok, Dump} = freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)),
                       ecallmgr_util:eventstr_to_proplist(Dump)

               end,
        EvtProp1 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                    ,{<<"Call-ID">>, UUID}
                    ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop)}
                    ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop)}
                    ,{<<"Application-Name">>, <<"fetch">>}
                    ,{<<"Application-Response">>, <<>>}
                    | wh_api:default_headers(<<>>, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
                   ],
        EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
                       [] -> EvtProp1;
                       CustomProp -> [{<<"Custom-Channel-Vars">>, wh_json:from_list(CustomProp)} | EvtProp1]
                   end,
        wapi_call:publish_event(UUID, EvtProp2)
    catch
        Type:_ ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                     ,{<<"Error-Message">>, "failed to construct or publish fetch call event"}
                     ,{<<"Call-ID">>, UUID}
                     ,{<<"Application-Name">>, <<"fetch">>}
                     ,{<<"Application-Response">>, <<>>}
                     | wh_api:default_headers(<<>>, <<"error">>, wh_util:to_binary(Type), ?APP_NAME, ?APP_VERSION)
                    ],
            wapi_dialplan:publish_error(UUID, Error)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_store_call_event/3 :: (atom(), ne_binary(), wh_json:json_object() | ne_binary()) -> 'ok'.
send_store_call_event(Node, UUID, MediaTransResults) ->
    Timestamp = wh_util:to_binary(wh_util:current_tstamp()),
    Prop = try freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)) of
               {ok, Dump} ->
                   ecallmgr_util:eventstr_to_proplist(Dump);
               {error, _Err} ->
                   lager:debug("failed to query channel: ~s", [_Err]),
                   [];
               timeout ->
                   lager:debug("timed out waiting for uuid_dump"),
                   []
           catch
               _E:_R ->
                   lager:debug("failed get params from uuid_dump"),
                   lager:debug("~p : ~p", [_E, _R]),
                   lager:debug("sending less interesting call_event message"),
                   []
           end,
    EvtProp1 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop, Timestamp)}
                ,{<<"Call-ID">>, UUID}
                ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop, <<>>)}
                ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop, <<"HANGUP">>)}
                ,{<<"Application-Name">>, <<"store">>}
                ,{<<"Application-Response">>, MediaTransResults}
                | wh_api:default_headers(<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
               ],
    EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
                   [] -> EvtProp1;
                   CustomProp -> [{<<"Custom-Channel-Vars">>, wh_json:from_list(CustomProp)} | EvtProp1]
               end,
    wapi_call:publish_event(UUID, EvtProp2).

-spec send_store_fax_call_event/2 :: (ne_binary(), ne_binary()) -> 'ok'.
send_store_fax_call_event(UUID, Results) ->
    Timestamp = wh_util:to_binary(wh_util:current_tstamp()),
    Prop = [{<<"Msg-ID">>, Timestamp}
            ,{<<"Call-ID">>, UUID}
            ,{<<"Application-Name">>, <<"store_fax">>}
            ,{<<"Application-Response">>, Results}
            | wh_api:default_headers(<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_call:publish_event(UUID, Prop).

-spec create_dialplan_move_ccvs/4 :: (ne_binary(), atom(), ne_binary(), proplist()) -> proplist().
create_dialplan_move_ccvs(Root, Node, UUID, DP) ->
    case freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)) of
        {'ok', Result} ->
            Props = ecallmgr_util:eventstr_to_proplist(Result),
            lists:foldr(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, Val}, Acc) ->
                                [{"application"
                                  ,<<"unset ", ?CHANNEL_VAR_PREFIX, Key/binary>>}
                                 ,{"application"
                                   ,<<"set ",?CHANNEL_VAR_PREFIX, Root/binary ,Key/binary, "=", Val/binary>>}
                                 |Acc
                                ];
                           ({<<?CHANNEL_VAR_PREFIX, K/binary>> = Key, Val}, Acc) ->
                                [{"application", <<"unset ", Key/binary>>}
                                 ,{"application", <<"set ", ?CHANNEL_VAR_PREFIX, Root/binary, K/binary, "=", Val/binary>>}
                                 |Acc];
                           (_, Acc) ->
                                Acc
                        end, DP, Props);
        _Error ->
            lager:debug("failed to get result from uuid_dump for ~s", [UUID]),
            DP
    end.
