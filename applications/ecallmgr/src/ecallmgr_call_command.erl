%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013 2600Hz INC
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

-spec exec_cmd(atom(), ne_binary(), wh_json:object(), pid()) ->
                      'ok' |
                      'error' |
                      ecallmgr_util:send_cmd_ret() |
                      [ecallmgr_util:send_cmd_ret(),...].
exec_cmd(Node, UUID, JObj, ControlPID) ->
    DestID = wh_json:get_value(<<"Call-ID">>, JObj),
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    case DestID =:= UUID of
        'true' ->
            case get_fs_app(Node, UUID, JObj, App) of
                {'error', Msg} -> throw({'msg', Msg});
                {'return', Result} -> Result;
                {AppName, 'noop'} ->
                    ecallmgr_call_control:event_execute_complete(ControlPID, UUID, AppName);
                {AppName, AppData} ->
                    ecallmgr_util:send_cmd(Node, UUID, AppName, AppData);
                {AppName, AppData, NewNode} ->
                    ecallmgr_util:send_cmd(NewNode, UUID, AppName, AppData);
                [_|_]=Apps ->
                    [ecallmgr_util:send_cmd(Node, UUID, AppName, AppData) || {AppName, AppData} <- Apps]
            end;
        'false' ->
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
-type fs_app() :: {ne_binary(), ne_binary() | 'noop'} |
                  {ne_binary(), ne_binary(), atom()}.
-spec get_fs_app(atom(), ne_binary(), wh_json:object(), ne_binary()) ->
                        fs_app() |
                        {'return', 'error'} |
                        {'error', ne_binary()} |
                        [fs_app(),...].
get_fs_app(_Node, _UUID, JObj, <<"noop">>) ->
    case wapi_dialplan:noop_v(JObj) of
        'false' ->
            {'error', <<"noop failed to execute as JObj did not validate">>};
        'true' ->
            Args = case wh_json:get_value(<<"Msg-ID">>, JObj) of
                       'undefined' ->
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

get_fs_app(Node, UUID, JObj, <<"tts">>) ->
    case wapi_dialplan:tts_v(JObj) of
        'false' -> {'error', <<"tts failed to execute as JObj didn't validate">>};
        'true' ->
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),

            case wh_json:get_value(<<"Engine">>, JObj, <<"flite">>) of
                <<"flite">> -> ecallmgr_fs_flite:call_command(Node, UUID, JObj);
                _Engine ->
                    SayMe = wh_json:get_value(<<"Text">>, JObj),
                    lager:debug("using engine ~s to say: ~s", [_Engine, SayMe]),
                    play(Node, UUID, wh_json:set_value(<<"Media-Name">>, <<"tts://", SayMe/binary>>, JObj))
            end
    end;

get_fs_app(Node, UUID, JObj, <<"play">>) ->
    case wapi_dialplan:play_v(JObj) of
        'false' -> {'error', <<"play failed to execute as JObj did not validate">>};
        'true' -> play(Node, UUID, JObj)
    end;

get_fs_app(_Node, _UUID, JObj, <<"playstop">>) ->
    case wapi_dialplan:playstop_v(JObj) of
        'false' -> {'error', <<"playstop failed to execute as JObj did not validate">>};
        'true' -> {<<"playstop">>, <<>>}
    end;

get_fs_app(_Node, _UUID, JObj, <<"hangup">>) ->
    case wh_json:is_true(<<"Other-Leg-Only">>, JObj, 'false') of
        'false' -> {<<"hangup">>, <<>>};
        'true' ->  {<<"unbridge">>, <<>>}
    end;

get_fs_app(_Node, UUID, JObj, <<"play_and_collect_digits">>) ->
    case wapi_dialplan:play_and_collect_digits_v(JObj) of
        'false' -> {'error', <<"play_and_collect_digits failed to execute as JObj did not validate">>};
        'true' ->
            Min = wh_json:get_value(<<"Minimum-Digits">>, JObj),
            Max = wh_json:get_value(<<"Maximum-Digits">>, JObj),
            Timeout = wh_json:get_value(<<"Timeout">>, JObj),
            Terminators = wh_json:get_value(<<"Terminators">>, JObj),
            Media = <<$', (ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), 'new', UUID, JObj))/binary, $'>>,
            InvalidMedia = <<$', (ecallmgr_util:media_path(wh_json:get_value(<<"Failed-Media-Name">>, JObj), 'new', UUID, JObj))/binary, $'>>,
            Tries = wh_json:get_value(<<"Media-Tries">>, JObj),
            Regex = wh_json:get_value(<<"Digits-Regex">>, JObj),
            Storage = <<"collected_digits">>,
            Data = list_to_binary([Min, " ", Max, " ", Tries, " ", Timeout, " ", Terminators, " "
                                   ,Media, " ", InvalidMedia, " ", Storage, " ", Regex]),
            {<<"play_and_get_digits">>, Data}
    end;

get_fs_app(Node, UUID, JObj, <<"record">>) ->
    case wapi_dialplan:record_v(JObj) of
        'false' -> {'error', <<"record failed to execute as JObj did not validate">>};
        'true' ->
            %% some carriers kill the channel during long recordings since there is no
            %% reverse RTP stream
            Routines = [fun(V) ->
                                case wh_util:is_true(ecallmgr_config:get(<<"record_waste_resources">>, 'false')) of
                                    'false' -> V;
                                    'true' -> [{<<"record_waste_resources">>, <<"true">>}|V]
                                end
                        end
                        ,fun(V) ->
                                 case get_terminators(JObj) of
                                     'undefined' -> V;
                                     Terminators -> [Terminators|V]
                                 end
                         end
                       ],
            Vars = lists:foldl(fun(F, V) -> F(V) end, [], Routines),
            _ = ecallmgr_util:set(Node, UUID, Vars),

            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            RecordingName = ecallmgr_util:recording_filename(MediaName),
            RecArg = list_to_binary([RecordingName, " "
                                     ,wh_json:get_string_value(<<"Time-Limit">>, JObj, "20"), " "
                                     ,wh_json:get_string_value(<<"Silence-Threshold">>, JObj, "500"), " "
                                     ,wh_json:get_string_value(<<"Silence-Hits">>, JObj, "5")
                                    ]),
            {<<"record">>, RecArg}
    end;

get_fs_app(Node, UUID, JObj, <<"record_call">>) ->
    case wapi_dialplan:record_call_v(JObj) of
        'false' -> {'error', <<"record_call failed to execute as JObj did not validate">>};
        'true' ->
            Routines = [fun(V) ->
                                case wh_util:is_true(ecallmgr_config:get(<<"record_waste_resources">>, 'false')) of
                                    'false' -> V;
                                    'true' -> [{<<"record_waste_resources">>, <<"true">>}|V]
                                end
                        end
                        ,fun(V) ->
                                 case get_terminators(JObj) of
                                     'undefined' -> V;
                                     Terminators -> [Terminators|V]
                                 end
                         end
                        ,fun(V) -> [{<<"RECORD_APPEND">>, <<"true">>}
                                    ,{<<"enable_file_write_buffering">>, <<"false">>}
                                    | V
                                   ]
                         end
                       ],
            Vars = lists:foldl(fun(F, V) -> F(V) end, [], Routines),
            _ = ecallmgr_util:set(Node, UUID, Vars),

            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            RecordingName = ecallmgr_util:recording_filename(MediaName),
            case wh_json:get_value(<<"Record-Action">>, JObj) of
                <<"start">> ->
                    %% UUID start path/to/media limit
                    RecArg = binary_to_list(list_to_binary([UUID, <<" start ">>
                                                            ,RecordingName, <<" ">>
                                                            ,wh_json:get_string_value(<<"Time-Limit">>, JObj, "3600") % one hour
                                                           ])),
                    {<<"record_call">>, RecArg};
                <<"stop">> ->
                    %% UUID stop path/to/media
                    RecArg = binary_to_list(list_to_binary([UUID, <<" stop ">>, RecordingName])),
                    {<<"record_call">>, RecArg}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"store">>) ->
    case wapi_dialplan:store_v(JObj) of
        'false' -> {'error', <<"store failed to execute as JObj did not validate">>};
        'true' ->
            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            RecordingName = ecallmgr_util:recording_filename(MediaName),
            lager:debug("streaming media ~s", [RecordingName]),
            case wh_json:get_value(<<"Media-Transfer-Method">>, JObj) of
                <<"put">> ->
                    %% stream file over HTTP PUT
                    lager:debug("stream ~s via HTTP PUT", [RecordingName]),
                    stream_over_http(Node, UUID, RecordingName, 'put', 'store', JObj),
                    {<<"store">>, 'noop'};
                <<"post">> ->
                    %% stream file over HTTP POST
                    lager:debug("stream ~s via HTTP POST", [RecordingName]),
                    stream_over_http(Node, UUID, RecordingName, 'post', 'store', JObj),
                    {<<"store">>, 'noop'};
                _Method ->
                    %% unhandled method
                    lager:debug("unhandled stream method ~s", [_Method]),
                    {'return', 'error'}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"store_fax">> = App) ->
    case wapi_dialplan:store_fax_v(JObj) of
        'false' -> {'error', <<"store_fax failed to execute as JObj did not validate">>};
        'true' ->
            File = ecallmgr_util:fax_filename(UUID),
            lager:debug("attempting to store fax on ~s: ~s", [Node, File]),
            case wh_json:get_value(<<"Media-Transfer-Method">>, JObj) of
                <<"put">> ->
                    stream_over_http(Node, UUID, File, 'put', 'fax', JObj),
                    {App, 'noop'};
                _Method ->
                    lager:debug("invalid media transfer method for storing fax: ~s", [_Method]),
                    {'error', <<"invalid media transfer method">>}
            end
    end;

get_fs_app(_Node, _UUID, JObj, <<"send_dtmf">>) ->
    case wapi_dialplan:send_dtmf_v(JObj) of
        'false' -> {'error', <<"send_dtmf failed to execute as JObj did not validate">>};
        'true' ->
            DTMFs = wh_json:get_value(<<"DTMFs">>, JObj),
            Duration = case wh_json:get_binary_value(<<"Duration">>, JObj) of
                           'undefined' -> <<>>;
                           D -> [<<"@">>, D]
                       end,
            {<<"send_dtmf">>, iolist_to_binary([DTMFs, Duration])}
    end;

get_fs_app(Node, UUID, JObj, <<"tones">>) ->
    case wapi_dialplan:tones_v(JObj) of
        'false' -> {'error', <<"tones failed to execute as JObj did not validate">>};
        'true' ->
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
            Tones = wh_json:get_value(<<"Tones">>, JObj, []),
            FSTones = [begin
                           Vol = case wh_json:get_value(<<"Volume">>, Tone) of
                                     'undefined' -> [];
                                     %% need to map V (0-100) to FS values
                                     V -> list_to_binary(["v=", wh_util:to_list(V), ";"])
                                 end,
                           Repeat = case wh_json:get_value(<<"Repeat">>, Tone) of
                                        'undefined' -> [];
                                        R -> list_to_binary(["l=", wh_util:to_list(R), ";"])
                                    end,
                           Freqs = string:join([wh_util:to_list(V) || V <- wh_json:get_value(<<"Frequencies">>, Tone)], ","),
                           On = wh_util:to_list(wh_json:get_value(<<"Duration-ON">>, Tone)),
                           Off = wh_util:to_list(wh_json:get_value(<<"Duration-OFF">>, Tone)),
                           wh_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
                       end || Tone <- Tones
                      ],
            Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
            {<<"playback">>, Arg}
    end;

get_fs_app(_Node, _UUID, _JObj, <<"answer">>) ->
    {<<"answer">>, <<>>};

get_fs_app(_Node, _UUID, _JObj, <<"progress">>) ->
    {<<"pre_answer">>, <<>>};

get_fs_app(_Node, _UUID, JObj, <<"privacy">>) ->
    case wapi_dialplan:privacy_v(JObj) of
        'false' -> {'error', <<"privacy failed to execute as JObj did not validate">>};
        'true' ->
            Mode = wh_json:get_value(<<"Privacy-Mode">>, JObj),
            {<<"privacy">>, Mode}
    end;

get_fs_app(Node, UUID, JObj, <<"ring">>) ->
    _ = case wh_json:get_value(<<"Ringback">>, JObj) of
            'undefined' -> 'ok';
            Ringback ->
                Stream = ecallmgr_util:media_path(Ringback, 'extant', UUID, JObj),
                lager:debug("custom ringback: ~s", [Stream]),
                _ = ecallmgr_util:set(Node, UUID, [{<<"ringback">>, Stream}])
        end,
    {<<"ring_ready">>, <<>>};

%% receive a fax from the caller
get_fs_app(Node, UUID, _JObj, <<"receive_fax">>) ->
    Vars = [{<<"fax_enable_t38_request">>, <<"true">>}
            ,{<<"fax_enable_t38">>, <<"true">>}
           ],
    _ = ecallmgr_util:set(Node, UUID, Vars),

    [{<<"playback">>, <<"silence_stream://2000">>}
     ,{<<"rxfax">>, ecallmgr_util:fax_filename(UUID)}
    ];

get_fs_app(_Node, UUID, JObj, <<"hold">>) ->
    case wh_json:get_value(<<"Hold-Media">>, JObj) of
        'undefined' -> {<<"endless_playback">>, <<"${hold_music}">>};
        Media ->
            Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
            lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
            {<<"endless_playback">>, Stream}
    end;

get_fs_app(_Node, _UUID, JObj, <<"page">>) ->
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case wapi_dialplan:page_v(JObj) of
        'false' -> {'error', <<"page failed to execute as JObj did not validate">>};
        'true' when Endpoints =:= [] -> {'error', <<"page request had no endpoints">>};
        'true' ->
            PageId = <<"page_", (wh_util:rand_hex_binary(8))/binary>>,
            Routines = [fun(DP) ->
                                [{"application", <<"set api_hangup_hook=conference ", PageId/binary, " kick all">>}
                                 ,{"application", <<"export sip_invite_params=intercom=true">>}
                                 ,{"application", <<"export sip_auto_answer=true">>}
                                 ,{"application", <<"export alert_info=intercom">>}
                                 ,{"application", <<"set conference_auto_outcall_flags=mute">>}
                                 |DP
                                ]
                        end
                        ,fun(DP) ->
                                 CIDName = wh_json:get_ne_value(<<"Caller-ID-Name">>, JObj, <<"$${effective_caller_id_name}">>),
                                 [{"application", <<"set conference_auto_outcall_caller_id_name=", CIDName/binary>>}|DP]
                         end
                        ,fun(DP) ->
                                 CIDNumber = wh_json:get_ne_value(<<"Caller-ID-Number">>, JObj, <<"$${effective_caller_id_number}">>),
                                 [{"application", <<"set conference_auto_outcall_caller_id_number=", CIDNumber/binary>>}|DP]
                         end
                        ,fun(DP) ->
                                 Timeout = wh_json:get_binary_value(<<"Timeout">>, JObj, <<"5">>),
                                 [{"application", <<"set conference_auto_outcall_timeout=", Timeout/binary>>}|DP]
                         end
                        ,fun(DP) ->
                                 lists:foldl(fun(Channel, D) ->
                                                     [{"application", <<"conference_set_auto_outcall "
                                                                        ,"{sip_auto_answer=true}"
                                                                        ,Channel/binary>>}
                                                      |D
                                                     ]
                                             end, DP, ecallmgr_util:build_simple_channels(Endpoints))
                         end
                        ,fun(DP) ->
                                 [{"application", <<"conference ", PageId/binary, "@default">>}
                                  ,{"application", <<"park">>}
                                  |DP
                                 ]
                         end
                       ],
            {<<"xferext">>, lists:foldr(fun(F, DP) -> F(DP) end, [], Routines)}
    end;

get_fs_app(_Node, _UUID, JObj, <<"park">>) ->
    case wapi_dialplan:park_v(JObj) of
        'false' -> {'error', <<"park failed to execute as JObj did not validate">>};
        'true' -> {<<"park">>, <<>>}
    end;

get_fs_app(_Node, _UUID, JObj, <<"echo">>) ->
    case wapi_dialplan:echo_v(JObj) of
        'false' -> {'error', <<"echo failed to execute as JObj did not validate">>};
        'true' -> {<<"echo">>, <<>>}
    end;

get_fs_app(_Node, _UUID, JObj, <<"sleep">>) ->
    case wapi_dialplan:sleep_v(JObj) of
        'false' -> {'error', <<"sleep failed to execute as JObj did not validate">>};
        'true' -> {<<"sleep">>, wh_json:get_binary_value(<<"Time">>, JObj, <<"50">>)}
    end;

get_fs_app(_Node, _UUID, JObj, <<"say">>) ->
    case wapi_dialplan:say_v(JObj) of
        'false' -> {'error', <<"say failed to execute as JObj did not validate">>};
        'true' ->
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
        'false' -> {'error', <<"bridge failed to execute as JObj did not validate">>};
        'true' when Endpoints =:= [] -> {'error', <<"bridge request had no endpoints">>};
        'true' ->
            %% if we are intending to ring multiple device simultaneously then
            %% execute ring_ready so we dont leave the caller hanging with dead air.
            %% this does not test how many are ACTUALLY dialed (registered)
            %% since that is one of the things we want to be ringing during
            _ = bridge_handle_ringback(Node, UUID, JObj),
            _ = bridge_maybe_early_media(Node, UUID, JObj),
            lager:debug("creating bridge dialplan"),
            Routines = [fun bridge_handle_hold_media/4
                        ,fun bridge_handle_secure_rtp/4
                        ,fun bridge_handle_bypass_media/4
                        ,fun bridge_handle_ccvs/4
                        ,fun bridge_pre_exec/4
                        ,fun bridge_create_command/4
                        ,fun bridge_post_exec/4
                       ],
            lager:debug("creating bridge dialplan"),
            XferExt = lists:foldr(fun(F, DP) ->
                                          F(DP, Node, UUID, JObj) end
                                  ,[], Routines),
            {<<"xferext">>, XferExt}
    end;

get_fs_app(Node, UUID, JObj, <<"call_pickup">>) ->
    case wapi_dialplan:call_pickup_v(JObj) of
        'false' -> {'error', <<"intercept failed to execute as JObj did not validate">>};
        'true' -> call_pickup(Node, UUID, JObj)
    end;

get_fs_app(Node, UUID, JObj, <<"execute_extension">>) ->
    case wapi_dialplan:execute_extension_v(JObj) of
        'false' -> {'error', <<"execute extension failed to execute as JObj did not validate">>};
        'true' ->
            Routines = [fun execute_exten_handle_reset/4
                        ,fun execute_exten_handle_ccvs/4
                        ,fun execute_exten_pre_exec/4
                        ,fun execute_exten_create_command/4
                        ,fun execute_exten_post_exec/4
                       ],
            Extension = lists:foldr(fun(F, DP) ->
                                            F(DP, Node, UUID, JObj)
                                    end, [], Routines),
            {<<"xferext">>, Extension}
    end;

get_fs_app(Node, UUID, JObj, <<"tone_detect">>) ->
    case wapi_dialplan:tone_detect_v(JObj) of
        'false' -> {'error', <<"tone detect failed to execute as JObj did not validate">>};
        'true' ->
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
        'false' -> {'error', <<"set_terminators failed to execute as JObj did not validate">>};
        'true' ->
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
            {<<"set">>, 'noop'}
    end;

get_fs_app(Node, UUID, JObj, <<"set">>) ->
    case wapi_dialplan:set_v(JObj) of
        'false' -> {'error', <<"set failed to execute as JObj did not validate">>};
        'true' ->
            ChannelVars = wh_json:to_proplist(wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
            _ = ecallmgr_util:set(Node, UUID, ChannelVars),

            CallVars = wh_json:to_proplist(wh_json:get_value(<<"Custom-Call-Vars">>, JObj, wh_json:new())),
            _ = ecallmgr_util:export(Node, UUID, CallVars),

            {<<"set">>, 'noop'}
    end;

get_fs_app(_Node, _UUID, JObj, <<"respond">>) ->
    case wapi_dialplan:respond_v(JObj) of
        'false' -> {'error', <<"respond failed to execute as JObj did not validate">>};
        'true' ->
            Code = wh_json:get_value(<<"Response-Code">>, JObj, ?DEFAULT_RESPONSE_CODE),
            Response = <<Code/binary ," "
                         ,(wh_json:get_value(<<"Response-Message">>, JObj, <<>>))/binary>>,
            {<<"respond">>, Response}
    end;

get_fs_app(Node, UUID, JObj, <<"redirect">>) ->
    case wapi_dialplan:redirect_v(JObj) of
        'false' -> {'error', <<"redirect failed to execute as JObj did not validate">>};
        'true' ->
            _ = case wh_json:get_value(<<"Redirect-Server">>, JObj) of
                    'undefined' -> 'ok';
                    Server -> ecallmgr_util:set(Node, UUID, [{<<"sip_rh_X-Redirect-Server">>, Server}])
                end,
            {<<"redirect">>, wh_json:get_value(<<"Redirect-Contact">>, JObj, <<>>)}
    end;

%% TODO: can we depreciate this command? It was prior to ecallmgr_fs_query....dont think anything is using it.
get_fs_app(Node, UUID, JObj, <<"fetch">>) ->
    spawn(fun() ->
                  send_fetch_call_event(Node, UUID, JObj)
          end),
    {<<"fetch">>, 'noop'};

get_fs_app(Node, UUID, JObj, <<"conference">>) ->
    case wapi_dialplan:conference_v(JObj) of
        'false' -> {'error', <<"conference failed to execute as JObj did not validate">>};
        'true' -> get_conference_app(Node, UUID, JObj, wh_json:is_true(<<"Reinvite">>, JObj, 'false'))
    end;

get_fs_app(_Node, _UUID, _JObj, _App) ->
    lager:debug("unknown application ~s", [_App]),
    {'error', <<"application unknown">>}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call pickup command helpers
%% @end
%%--------------------------------------------------------------------
-spec call_pickup(atom(), ne_binary(), wh_json:object()) ->
                         {ne_binary(), ne_binary()} |
                         {'error', ne_binary()}.
call_pickup(Node, UUID, JObj) ->
    Target = wh_json:get_value(<<"Target-Call-ID">>, JObj),

    case ecallmgr_fs_channel:fetch(Target, 'record') of
        {'ok', #channel{node=Node
                        ,answered=IsAnswered
                       }} ->
            lager:debug("target ~s is on same node(~s) as us", [Target, Node]),
            maybe_answer(Node, UUID, IsAnswered),
            get_call_pickup_app(Node, UUID, JObj, Target);
        {'ok', #channel{node=OtherNode}} ->
            lager:debug("target ~s is on other node (~s), not ~s", [Target, OtherNode, Node]),
            call_pickup_maybe_move(Node, UUID, JObj, Target, OtherNode);
        {'error', 'not_found'} ->
            lager:debug("failed to find target callid ~s", [Target]),
            {'error', <<"failed to find target callid ", Target/binary>>}
    end.

maybe_answer(_Node, _UUID, 'true') -> 'ok';
maybe_answer(Node, UUID, 'false') ->
    ecallmgr_util:send_cmd(Node, UUID, <<"answer">>, <<>>).

-spec call_pickup_maybe_move(atom(), ne_binary(), wh_json:object(), ne_binary(), atom()) ->
                                    {ne_binary(), ne_binary()}.
call_pickup_maybe_move(Node, UUID, JObj, Target, OtherNode) ->
    case wh_json:is_true(<<"Move-Channel-If-Necessary">>, JObj, 'false') of
        'true' ->
            lager:debug("target ~s is on ~s, not ~s...moving", [Target, OtherNode, Node]),
            'true' = ecallmgr_channel_move:move(Target, OtherNode, Node),
            get_call_pickup_app(Node, UUID, JObj, Target);
        'false' ->
            lager:debug("target ~s is on ~s, not ~s, need to redirect", [Target, OtherNode, Node]),
            lager:debug("gotta usurp some fools first"),

            ControlUsurp = [{<<"Call-ID">>, UUID}
                            ,{<<"Reason">>, <<"redirect">>}
                            ,{<<"Fetch-ID">>, wh_util:rand_hex_binary(4)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ],
            PublishUsurp = [{<<"Call-ID">>, UUID}
                            ,{<<"Reference">>, wh_util:rand_hex_binary(4)}
                            ,{<<"Media-Node">>, wh_util:to_binary(Node)}
                            ,{<<"Reason">>, <<"redirect">>}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ],

            wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                                ,ControlUsurp
                                ,fun(C) -> wapi_call:publish_usurp_control(UUID, C) end
                               ),
            wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                                ,PublishUsurp
                                ,fun(C) -> wapi_call:publish_usurp_publisher(UUID, C) end
                               ),

            lager:debug("now issue the redirect to ~s", [OtherNode]),
            _ = ecallmgr_channel_redirect:redirect(UUID, OtherNode),
            {'return', <<"target is on different media server: ", (wh_util:to_binary(OtherNode))/binary>>}
    end.

-spec get_call_pickup_app(atom(), ne_binary(), wh_json:object(), ne_binary()) ->
                                 {ne_binary(), ne_binary()}.
get_call_pickup_app(Node, UUID, JObj, Target) ->
    ExportsApi = [{<<"Park-After-Pickup">>, <<"false">>}
                  ,{<<"Continue-On-Fail">>, <<"true">>}
                  ,{<<"Continue-On-Cancel">>, <<"true">>}
                 ],

    SetApi = [{<<"Unbridged-Only">>, 'undefined', <<"intercept_unbridged_only">>}
              ,{<<"Unanswered-Only">>, 'undefined', <<"intercept_unanswered_only">>}
             ],

    Exports = [{<<"failure_causes">>, <<"NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH">>}
               | build_set_args(ExportsApi, JObj)
              ],

    ControlUsurp = [{<<"Call-ID">>, Target}
                    ,{<<"Reason">>, <<"redirect">>}
                    ,{<<"Fetch-ID">>, wh_util:rand_hex_binary(4)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,ControlUsurp
                        ,fun(C) -> wapi_call:publish_usurp_control(Target, C) end
                       ),
    io:format("published ~p for ~s~n", [ControlUsurp, Target]),

    ecallmgr_util:set(Node, UUID, build_set_args(SetApi, JObj)),
    ecallmgr_util:export(Node, UUID, Exports),
    {<<"intercept">>, Target}.

-type set_headers() :: wh_proplist() | [{ne_binary(), api_binary(), ne_binary()},...].
-spec build_set_args(set_headers(), wh_json:object()) ->
                            wh_proplist().
-spec build_set_args(set_headers(), wh_json:object(), wh_proplist()) ->
                            wh_proplist().
build_set_args(Headers, JObj) ->
    build_set_args(Headers, JObj, []).

build_set_args([], _, Args) ->
    lists:reverse(props:filter_undefined(Args));
build_set_args([{ApiHeader, Default}|Headers], JObj, Args) ->
    build_set_args(Headers, JObj, [{wh_json:normalize_key(ApiHeader)
                                    ,wh_json:get_binary_boolean(ApiHeader, JObj, Default)
                                   } | Args
                                  ]);
build_set_args([{ApiHeader, Default, FSHeader}|Headers], JObj, Args) ->
    build_set_args(Headers, JObj, [{FSHeader
                                    ,wh_json:get_binary_boolean(ApiHeader, JObj, Default)
                                   } | Args
                                   ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Conference command helpers
%% @end
%%--------------------------------------------------------------------
get_conference_app(ChanNode, UUID, JObj, 'true') ->
    ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
    ConferenceConfig = wh_json:get_value(<<"Profile">>, JObj, <<"default">>),
    Cmd = list_to_binary([ConfName, "@", ConferenceConfig, get_conference_flags(JObj)]),
    ecallmgr_util:export(ChanNode, UUID, [{<<"Hold-Media">>, <<"silence">>}]),
    case ecallmgr_fs_conferences:node(ConfName) of
        {'error', 'not_found'} ->
            lager:debug("conference ~s hasn't been started yet", [ConfName]),
            {'ok', _} = ecallmgr_util:send_cmd(ChanNode, UUID, "conference", Cmd),

            case wait_for_conference(ConfName) of
                {'ok', ChanNode} ->
                    lager:debug("conference has started on ~s", [ChanNode]),
                    maybe_set_nospeak_flags(ChanNode, UUID, JObj),
                    {<<"conference">>, 'noop'};
                {'ok', OtherNode} ->
                    lager:debug("conference has started on other node ~s, lets move", [OtherNode]),
                    get_conference_app(ChanNode, UUID, JObj, 'true')
            end;
        {'ok', ChanNode} ->
            lager:debug("channel is on same node as conference"),
            maybe_set_nospeak_flags(ChanNode, UUID, JObj),
            {<<"conference">>, Cmd};
        {'ok', ConfNode} ->
            lager:debug("channel is on node ~s, conference is on ~s, moving channel", [ChanNode, ConfNode]),
            'true' = ecallmgr_channel_move:move(UUID, ChanNode, ConfNode),
            lager:debug("channel has moved to ~s", [ConfNode]),
            maybe_set_nospeak_flags(ConfNode, UUID, JObj),
            ecallmgr_util:export(ConfNode, UUID, [{<<"Hold-Media">>, <<"silence">>}]),
            {<<"conference">>, Cmd, ConfNode}
    end;
get_conference_app(ChanNode, UUID, JObj, 'false') ->
    ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
    ConferenceConfig = wh_json:get_value(<<"Profile">>, JObj, <<"default">>),
    maybe_set_nospeak_flags(ChanNode, UUID, JObj),
    ecallmgr_util:export(ChanNode, UUID, [{<<"Hold-Media">>, <<"silence">>}]),
    {<<"conference">>, list_to_binary([ConfName, "@", ConferenceConfig, get_conference_flags(JObj)])}.

maybe_set_nospeak_flags(Node, UUID, JObj) ->
    case wh_json:is_true(<<"Member-Nospeak">>, JObj) of
        'false' -> 'ok';
        'true' ->
            ecallmgr_util:set(Node, UUID, [{<<"conference_member_nospeak_relational">>, <<"true">>}])
    end,
    case wh_json:is_true(<<"Nospeak-Check">>, JObj) of
        'false' -> 'ok';
        'true' ->
            ecallmgr_util:set(Node, UUID, [{<<"conference_member_nospeak_check">>, <<"true">>}])
    end.

%% [{FreeSWITCH-Flag-Name, Kazoo-Flag-Name}]
%% Conference-related entry flags
%% convert from FS conference flags to Kazoo conference flags
-define(CONFERENCE_FLAGS, [{<<"mute">>, <<"Mute">>}
                           ,{<<"deaf">>, <<"Deaf">>}
                           ,{<<"moderator">>, <<"Moderator">>}
                           ]).

-spec get_conference_flags(wh_json:object()) -> binary().
get_conference_flags(JObj) ->
    case wh_json:to_proplist(JObj) of
        [] -> <<>>;
        [{_Key,_Val}=KV|L] ->
            Flags = lists:foldl(fun maybe_add_conference_flag/2, [<<>>], L),
            All = case maybe_add_conference_flag(KV, []) of
                [] -> tl(Flags);
                [<<",">> | T] -> T ++ Flags;
                Fs -> Fs ++ Flags
            end,
            <<"+flags{", (iolist_to_binary(All))/binary, "}">>
    end.

maybe_add_conference_flag({K, V}, Acc) ->
    case lists:keyfind(K, 2, ?CONFERENCE_FLAGS) of
        'false' -> Acc;
        {FSFlag, _} when V =:= 'true' -> [<<",">>, FSFlag | Acc];
        _ -> Acc
    end.

-spec wait_for_conference(ne_binary()) -> {'ok', atom()}.
wait_for_conference(ConfName) ->
    case ecallmgr_fs_conferences:node(ConfName) of
        {'ok', _N}=OK -> OK;
        {'error', 'not_found'} ->
            timer:sleep(100),
            wait_for_conference(ConfName)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Bridge command helpers
%% @end
%%--------------------------------------------------------------------
bridge_handle_ringback(Node, UUID, JObj) ->
    case wh_json:get_value(<<"Ringback">>, JObj) of
        'undefined' ->
            case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Ringback">>], JObj) of
                'undefined' -> 'ok';
                Media ->
                    Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
                    lager:debug("bridge has custom ringback in channel vars: ~s", [Stream]),
                    ecallmgr_util:set(Node, UUID, [{<<"ringback">>, Stream}])
            end;
        Media ->
            Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
            lager:debug("bridge has custom ringback: ~s", [Stream]),
            ecallmgr_util:set(Node, UUID, [{<<"ringback">>, Stream}])
    end.

bridge_maybe_early_media(Node, UUID, JObj) ->
    case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
        <<"simultaneous">> ->
            case length(wh_json:get_ne_value(<<"Endpoints">>, JObj, [])) > 1 of
                'false' -> 'ok';
                'true' ->
                    lager:debug("bridge is simultaneous to multiple endpoints, starting local ringing"),
                    %% we don't really care if this succeeds, the call will fail later on
                    ecallmgr_util:send_cmd(Node, UUID, <<"ring_ready">>, "")
            end;
        _Else -> 'ok'
    end.

bridge_handle_hold_media(DP, _Node, UUID, JObj) ->
    case wh_json:get_value(<<"Hold-Media">>, JObj) of
        'undefined' ->
            case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Hold-Media">>], JObj) of
                'undefined' -> DP;
                Media ->
                    Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
                    lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
                    [{"application", <<"set hold_music=", Stream/binary>>}
                     ,{"application", <<"set transfer_ringback=", Stream/binary>>}
                     |DP
                    ]
            end;
        Media ->
            Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
            lager:debug("bridge has custom music-on-hold: ~s", [Stream]),
            [{"application", <<"set hold_music=", Stream/binary>>}
             ,{"application", <<"set transfer_ringback=", Stream/binary>>}
             |DP
            ]
    end.

bridge_handle_secure_rtp(DP, _Node, _UUID, JObj) ->
    case wh_json:is_true(<<"Secure-RTP">>, JObj, 'false') of
        'true' -> [{"application", "set zrtp_secure_media=true"}|DP];
        'false' -> DP
    end.

bridge_handle_bypass_media(DP, _Node, _UUID, JObj) ->
    case wh_json:get_value(<<"Media">>, JObj) of
        <<"process">> ->
            lager:debug("bridge will process media through host switch"),
            [{"application", "set bypass_media=false"}|DP];
        <<"bypass">> ->
            lager:debug("bridge will connect the media peer-to-peer"),
            [{"application", "set bypass_media=true"}|DP];
        _ ->
            Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
            maybe_bypass_endpoint_media(Endpoints, DP)
    end.

-spec maybe_bypass_endpoint_media(wh_json:objects(), wh_proplist()) -> wh_proplist().
maybe_bypass_endpoint_media([Endpoint], DP) ->
    case wh_json:is_true(<<"Bypass-Media">>, Endpoint) of
        'true' ->
            [{"application", "set bypass_media=true"}|DP];
        'false' ->
            DP
    end;
maybe_bypass_endpoint_media(_, DP) ->
    DP.

bridge_handle_ccvs(DP, _Node, _UUID, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case wh_json:is_json_object(CCVs) of
        'true' ->
            [{"application", <<"set ", Var/binary, "=", (wh_util:to_binary(V))/binary>>}
             || {K, V} <- wh_json:to_proplist(CCVs),
                (Var = props:get_value(K, ?SPECIAL_CHANNEL_VARS)) =/= 'undefined'
            ] ++ DP;
        _ ->
            DP
    end.

bridge_pre_exec(DP, _, _, _) ->
    [{"application", "set continue_on_fail=true"}
     ,{"application", "export sip_redirect_context=context_2"}
     |DP
    ].

bridge_create_command(DP, _Node, _UUID, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    BridgeCmd = list_to_binary(["bridge "
                                ,bridge_build_channels_vars(Endpoints, JObj)
                                ,try_create_bridge_string(Endpoints, JObj)
                               ]),
    [{"application", BridgeCmd}|DP].

try_create_bridge_string(Endpoints, JObj) ->
    DialSeparator = bridge_determine_dial_separator(Endpoints, JObj),
    case ecallmgr_util:build_bridge_string(Endpoints, DialSeparator) of
        <<>> ->
            lager:warning("bridge string resulted in no enpoints"),
            throw(<<"registrar returned no endpoints">>);
        BridgeString -> BridgeString
    end.

bridge_build_channels_vars(Endpoints, JObj) ->
    Props = case wh_json:find(<<"Force-Fax">>, Endpoints, wh_json:get_value(<<"Force-Fax">>, JObj)) of
                'undefined' -> [];
                Direction ->
                    [{[<<"Custom-Channel-Vars">>, <<"Force-Fax">>], Direction}]
            end,
    ecallmgr_fs_xml:get_channel_vars(wh_json:set_values(Props, JObj)).

bridge_determine_dial_separator(Endpoints, JObj) ->
    case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
        <<"simultaneous">> when length(Endpoints) > 1 -> <<",">>;
        _Else -> <<"|">>
    end.

bridge_post_exec(DP, _, _, _) ->
    Event = ecallmgr_util:create_masquerade_event(<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>),
    [{"application", Event}
     ,{"application", "park "}
     |DP
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Store command helpers
%% @end
%%--------------------------------------------------------------------
-spec stream_over_http(atom(), ne_binary(), ne_binary(), 'put' | 'post', 'store' | 'fax', wh_json:object()) -> any().
stream_over_http(Node, UUID, File, Method, Type, JObj) ->
    Url = wh_util:to_list(wh_json:get_value(<<"Media-Transfer-Destination">>, JObj)),
    lager:debug("streaming via HTTP(~s) to ~s", [Method, Url]),

    ecallmgr_util:set(Node, UUID, [{<<"Recording-URL">>, Url}]),

    Args = list_to_binary([Url, <<" ">>, File]),
    lager:debug("execute on node ~s: http_put(~s)", [Node, Args]),
    Result = case send_fs_store(Node, Args, Method) of
                 {'ok', <<"+OK", _/binary>>} ->
                     lager:debug("successfully stored media"),
                     <<"success">>;
                 {'ok', Err} ->
                     lager:debug("store media failed: ~s", [Err]),
                     wh_notify:system_alert("Failed to store media file ~s for call ~s on ~s "
                                            ,[File, UUID, Node]
                                            ,[{<<"Details">>, Err}]
                                           ),
                     <<"failure">>;
                 {'error', E} ->
                     lager:debug("error executing http_put: ~p", [E]),
                     wh_notify:system_alert("Failed to store media file ~s for call ~s on ~s "
                                            ,[File, UUID, Node]
                                            ,[{<<"Details">>, E}]
                                           ),
                     <<"failure">>;
                 'timeout' ->
                     lager:debug("timeout waiting for http_put"),
                     wh_notify:system_alert("Failed to store media file ~s for call ~s on ~s "
                                            ,[File, UUID, Node]
                                            ,[{<<"Details">>, <<"Timeout sending http_put to node">>}]
                                           ),
                     <<"timeout">>
             end,
    case Type of
        'store' -> send_store_call_event(Node, UUID, Result);
        'fax' -> send_store_fax_call_event(UUID, Result)
    end.

-spec send_fs_store(atom(), ne_binary(), 'put' | 'post') -> fs_api_ret().
send_fs_store(Node, Args, 'put') ->
    freeswitch:api(Node, 'http_put', wh_util:to_list(Args), 20000);
send_fs_store(Node, Args, 'post') ->
    freeswitch:api(Node, 'http_post', wh_util:to_list(Args), 20000).

-spec send_store_call_event(atom(), ne_binary(), wh_json:object() | ne_binary()) -> 'ok'.
send_store_call_event(Node, UUID, MediaTransResults) ->
    Timestamp = wh_util:to_binary(wh_util:current_tstamp()),
    Prop = try freeswitch:api(Node, 'uuid_dump', wh_util:to_list(UUID)) of
               {'ok', Dump} -> ecallmgr_util:eventstr_to_proplist(Dump);
               {'error', _Err} -> [];
               'timeout' -> []
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
                   CustomProp -> [{<<"Custom-Channel-Vars">>, wh_json:from_list(CustomProp)}
                                  | EvtProp1
                                 ]
               end,
    wapi_call:publish_event(UUID, EvtProp2).

-spec send_store_fax_call_event(ne_binary(), ne_binary()) -> 'ok'.
send_store_fax_call_event(UUID, Results) ->
    Timestamp = wh_util:to_binary(wh_util:current_tstamp()),
    Prop = [{<<"Msg-ID">>, Timestamp}
            ,{<<"Call-ID">>, UUID}
            ,{<<"Application-Name">>, <<"store_fax">>}
            ,{<<"Application-Response">>, Results}
            | wh_api:default_headers(<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_call:publish_event(UUID, Prop).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_fetch_call_event(atom(), ne_binary(), wh_json:object()) -> 'ok'.
send_fetch_call_event(Node, UUID, JObj) ->
    try
        Prop = case wh_util:is_true(wh_json:get_value(<<"From-Other-Leg">>, JObj)) of
                   'true' ->
                       {'ok', OtherUUID} = freeswitch:api(Node, 'uuid_getvar', wh_util:to_list(<<UUID/binary, " bridge_uuid">>)),
                       {'ok', Dump} = freeswitch:api(Node, 'uuid_dump', wh_util:to_list(OtherUUID)),
                       ecallmgr_util:eventstr_to_proplist(Dump);
                   'false' ->
                       {'ok', Dump} = freeswitch:api(Node, 'uuid_dump', wh_util:to_list(UUID)),
                       ecallmgr_util:eventstr_to_proplist(Dump)
               end,
        EvtProp1 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                    ,{<<"Call-ID">>, UUID}
                    ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop)}
                    ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop)}
                    ,{<<"Application-Name">>, <<"fetch">>}
                    ,{<<"Application-Response">>, <<>>}
                    | wh_api:default_headers(<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
                   ],
        EvtProp2 =
            case ecallmgr_util:custom_channel_vars(Prop) of
                [] -> EvtProp1;
                CustomProp -> [{<<"Custom-Channel-Vars">>, wh_json:from_list(CustomProp)}
                               | EvtProp1
                              ]
            end,
        wapi_call:publish_event(UUID, EvtProp2)
    catch
        Type:_ ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                     ,{<<"Error-Message">>, "failed to construct or publish fetch call event"}
                     ,{<<"Call-ID">>, UUID}
                     ,{<<"Application-Name">>, <<"fetch">>}
                     ,{<<"Application-Response">>, <<>>}
                     | wh_api:default_headers(<<"error">>, wh_util:to_binary(Type), ?APP_NAME, ?APP_VERSION)
                    ],
            wapi_dialplan:publish_error(UUID, Error)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute extension helpers
%% @end
%%--------------------------------------------------------------------
execute_exten_handle_reset(DP, Node, UUID, JObj) ->
    case wh_json:is_true(<<"Reset">>, JObj) of
        'false' -> ok;
        'true' ->
            create_dialplan_move_ccvs(<<"Execute-Extension-Original-">>, Node, UUID, DP)
    end.

execute_exten_handle_ccvs(DP, _Node, UUID, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:is_empty(CCVs) of
        'true' -> DP;
        'false' ->
            ChannelVars = wh_json:to_proplist(CCVs),
            [{"application", <<"set ", (ecallmgr_util:get_fs_kv(K, V, UUID))/binary>>}
             || {K, V} <- ChannelVars] ++ DP
    end.

execute_exten_pre_exec(DP, _Node, _UUID, _JObj) ->
    [{"application", <<"set ", ?CHANNEL_VAR_PREFIX, "Executing-Extension=true">>}
     | DP
    ].

execute_exten_create_command(DP, _Node, _UUID, JObj) ->
    [{"application", <<"execute_extension ", (wh_json:get_value(<<"Extension">>, JObj))/binary>>}
     |DP
    ].

execute_exten_post_exec(DP, _Node, _UUID, _JObj) ->
    [{"application", <<"unset ", ?CHANNEL_VAR_PREFIX, "Executing-Extension">>}
     ,{"application", ecallmgr_util:create_masquerade_event(<<"execute_extension">>
                                                                ,<<"CHANNEL_EXECUTE_COMPLETE">>
                                                           )}
     ,{"application", "park "}
     |DP
    ].

-spec create_dialplan_move_ccvs(ne_binary(), atom(), ne_binary(), wh_proplist()) -> wh_proplist().
create_dialplan_move_ccvs(Root, Node, UUID, DP) ->
    case freeswitch:api(Node, 'uuid_dump', wh_util:to_list(UUID)) of
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
                           (_, Acc) -> Acc
                        end, DP, Props);
        _Error ->
            lager:debug("failed to get result from uuid_dump for ~s", [UUID]),
            DP
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Playback command helpers
%% @end
%%--------------------------------------------------------------------
-spec play(atom(), ne_binary(), wh_json:object()) ->
                  {ne_binary(), ne_binary()}.
play(Node, UUID, JObj) ->
    Routines = [fun(V) ->
                        case wh_json:get_value(<<"Group-ID">>, JObj) of
                            'undefined' -> V;
                            GID -> [{<<"media_group_id">>, GID}|V]
                        end
                end
                ,fun(V) ->
                         case get_terminators(JObj) of
                             'undefined' -> V;
                             Terminators -> [Terminators|V]
                         end
                 end
               ],
    Vars = lists:foldl(fun(F, V) -> F(V) end, [], Routines),
    _ = ecallmgr_util:set(Node, UUID, Vars),

    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    F = ecallmgr_util:media_path(MediaName, 'new', UUID, JObj),

    %% if Leg is set, use uuid_broadcast; otherwise use playback
    case ecallmgr_fs_channel:is_bridged(UUID) of
        'false' -> {<<"playback">>, F};
        'true' -> play_bridged(UUID, JObj, F)
    end.

-spec play_bridged(ne_binary(), wh_json:object(), ne_binary()) ->
                          {ne_binary(), ne_binary()}.
play_bridged(UUID, JObj, F) ->
    case wh_json:get_value(<<"Leg">>, JObj) of
        <<"B">> ->     {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" bleg">>])};
        <<"A">> ->     {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" aleg">>])};
        <<"Both">> ->  {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" both">>])};
        'undefined' -> {<<"broadcast">>, list_to_binary([UUID, <<" ">>, F, <<" both">>])}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_terminators(api_binary() | ne_binaries() | wh_json:object()) ->
                             {ne_binary(), ne_binary()} | 'undefined'.
get_terminators('undefined') -> 'undefined';
get_terminators(Ts) when is_binary(Ts) -> get_terminators([Ts]);
get_terminators([_|_]=Ts) ->
    case Ts =:= get('$prior_terminators') of
        'true' -> 'undefined';
        'false' ->
            put('$prior_terminators', Ts),
            case wh_util:is_empty(Ts) of
                'true' -> {<<"playback_terminators">>, <<"none">>};
                'false' -> {<<"playback_terminators">>, wh_util:to_binary(Ts)}
            end
    end;
get_terminators(JObj) -> get_terminators(wh_json:get_ne_value(<<"Terminators">>, JObj)).

-spec set_terminators(atom(), ne_binary(), api_binary() | ne_binaries()) ->
                             ecallmgr_util:send_cmd_ret().
set_terminators(Node, UUID, Ts) ->
    case get_terminators(Ts) of
        'undefined' -> 'ok';
        {K, V} ->
            case ecallmgr_util:set(Node, UUID, [{K, V}]) of
                {'ok', _} -> 'ok';
                E -> E
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

all_conference_flags_test() ->
    JObj = wh_json:from_list([{<<"Mute">>, 'true'}
                              ,{<<"Deaf">>, 'true'}
                              ,{<<"Moderator">>, 'true'}
                             ]),
    ?assertEqual(<<"+flags{mute,moderator,deaf}">>, get_conference_flags(JObj)).

two_conference_flags_test() ->
    JObj = wh_json:from_list([{<<"Mute">>, 'true'}
                              ,{<<"Moderator">>, 'true'}
                             ]),
    ?assertEqual(<<"+flags{mute,moderator}">>, get_conference_flags(JObj)).

one_conference_flag_test() ->
    JObj = wh_json:from_list([{<<"Mute">>, 'true'}]),
    ?assertEqual(<<"+flags{mute}">>, get_conference_flags(JObj)).

no_conference_flags_test() ->
    JObj = wh_json:new(),
    ?assertEqual(<<>>, get_conference_flags(JObj)).

-endif.
