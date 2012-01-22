%%%-------------------------------------------------------------------
%%% @copyright (C) 2010 VoIP INC
%%% @doc
%%% Execute call commands
%%% @end
%%%-------------------------------------------------------------------
-module(ecallmgr_call_command).

-export([exec_cmd/4]).

-include("ecallmgr.hrl").

-type fd() :: tuple().
-type io_device() :: pid() | fd().
-type file_stream_state() :: {'undefined' | io_device(), binary()}.

-spec exec_cmd/4 :: (atom(), ne_binary(), wh_json:json_object(), pid()) -> 'ok' | 'timeout' | {'error', 'invalid_callid' | 'failed'}.
exec_cmd(Node, UUID, JObj, ControlPID) ->
    DestID = wh_json:get_value(<<"Call-ID">>, JObj),
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    case DestID =:= UUID of
        true ->
            case get_fs_app(Node, UUID, JObj, App) of
                {'error', Msg} ->
                    _ = ecallmgr_util:fs_log(Node, wh_util:to_list(Msg), []),
                    send_error_response(App, Msg, UUID, JObj),
                    ecallmgr_call_control:event_execute_complete(ControlPID, UUID, App);
                {'error', AppName, Msg} ->
                    _ = ecallmgr_util:fs_log(Node, wh_util:to_list(Msg), []),
                    send_error_response(App, Msg, UUID, JObj),
                    ecallmgr_call_control:event_execute_complete(ControlPID, UUID, AppName);
                {return, Result} ->
                    Result;
                {AppName, noop} ->
                    ecallmgr_call_control:event_execute_complete(ControlPID, UUID, AppName);
                {<<"answer">> = AppName, AppData} ->
                    _ = send_cmd(Node, UUID, AppName, AppData),
                    %% 22:55 pyite_mac  can you sleep 0.5 seconds before continuing
                    timer:sleep(500),
                    ecallmgr_call_control:event_execute_complete(ControlPID, UUID, AppName);
                {AppName, AppData} ->
                    send_cmd(Node, UUID, AppName, AppData)
            end;
        false ->
            ?LOG("command ~s not meant for us but for ~s", [wh_json:get_value(<<"Application-Name">>, JObj), DestID]),
            ecallmgr_call_control:event_execute_complete(ControlPID, UUID, App),
            {'error', invalid_callid}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% return the app name and data (as a binary string) to send to
%% the FS ESL via mod_erlang_event
%% @end
%%--------------------------------------------------------------------
-spec get_fs_app/4 :: (atom(), ne_binary(), wh_json:json_object(), ne_binary()) -> {binary(), binary() | 'noop'}
                                                                               | {'return', 'ok'}
                                                                               | {'error', binary()}
                                                                               | {'error', binary(), binary()}.
get_fs_app(_Node, _UUID, JObj, <<"noop">>) ->
    case wapi_dialplan:noop_v(JObj) of
        false ->
            {'error', <<"noop">>, <<"noop failed to execute as JObj did not validate">>};
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
        false -> {'error', <<"playback">>, <<"play failed to execute as JObj did not validate">>};
        true ->
            F = ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID),
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
            {<<"playback">>, F}
    end;

get_fs_app(_Node, _UUID, _JObj, <<"hangup">>) ->
    {<<"hangup">>, <<>>};

get_fs_app(_Node, UUID, JObj, <<"play_and_collect_digits">>) ->
    case wapi_dialplan:play_and_collect_digits_v(JObj) of
        false -> {'error', <<"play_and_get_digits">>, <<"play_and_collect_digits failed to execute as JObj did not validate">>};
        true ->
            Min = wh_json:get_value(<<"Minimum-Digits">>, JObj),
            Max = wh_json:get_value(<<"Maximum-Digits">>, JObj),
            Timeout = wh_json:get_value(<<"Timeout">>, JObj),
            Terminators = wh_json:get_value(<<"Terminators">>, JObj),
            Media = <<$', (ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID))/binary, $'>>,
            InvalidMedia = <<$', (ecallmgr_util:media_path(wh_json:get_value(<<"Failed-Media-Name">>, JObj), UUID))/binary, $'>>,
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
            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            Media = ecallmgr_media_registry:register_local_media(MediaName, UUID),

            _ = set(Node, UUID, <<"enable_file_write_buffering=false">>), % disable buffering to see if we get all the media

            RecArg = binary_to_list(list_to_binary([Media, " "
                                                    ,wh_util:to_list(wh_json:get_value(<<"Time-Limit">>, JObj, "20")), " "
                                                    ,wh_util:to_list(wh_json:get_value(<<"Silence-Threshold">>, JObj, "500")), " "
                                                    ,wh_util:to_list(wh_json:get_value(<<"Silence-Hits">>, JObj, "5"))
                                                   ])),
            'ok' = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),

            {<<"record">>, RecArg}
    end;

get_fs_app(Node, UUID, JObj, <<"record_call">>) ->
    case wapi_dialplan:record_call_v(JObj) of
        false -> {'error', <<"record_call failed to execute as JObj did not validate">>};
        true ->
            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),

            case wh_json:get_value(<<"Record-Action">>, JObj) of
                <<"start">> ->
                    Media = ecallmgr_media_registry:register_local_media(MediaName, UUID),

                    _ = set(Node, UUID, <<"enable_file_write_buffering=false">>), % disable buffering to see if we get all the media

                    %% UUID start path/to/media limit
                    RecArg = binary_to_list(list_to_binary([
                                                            <<"start ">>
                                                            ,Media, <<" ">>
                                                            ,wh_json:get_string_value(<<"Time-Limit">>, JObj, "20"), " "
                                                           ])),
                    {<<"record_call">>, RecArg};
                <<"stop">> ->
                    MediaUrl = ecallmgr_media_registry:register_local_media(MediaName, UUID, url),
                    %% UUID stop path/to/media
                    RecArg = binary_to_list(list_to_binary([
                                                            <<"stop ">>
                                                            ,MediaUrl
                                                           ])),
                    {<<"record_call">>, RecArg}
            end
    end;

get_fs_app(Node, UUID, JObj, <<"store">>) ->
    case wapi_dialplan:store_v(JObj) of
        false -> {'error', <<"store failed to execute as JObj did not validate">>};
        true ->
            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            case ecallmgr_media_registry:is_local(MediaName, UUID) of
                {'error', not_local} ->
                    ?LOG("failed to find media ~s for storing", [MediaName]),
                    {error, <<"failed to find media requested for storage">>};
                {ok, Media} ->
                    ?LOG("Streaming media ~s", [MediaName]),
                    case filelib:is_regular(Media) andalso wh_json:get_value(<<"Media-Transfer-Method">>, JObj) of
                        <<"stream">> ->
                            %% stream file over AMQP
                            Headers = [{<<"Media-Transfer-Method">>, <<"stream">>}
                                       ,{<<"Media-Name">>, MediaName}
                                       ,{<<"Application-Name">>, <<"store">>}
                                      ],
                            ?LOG("stream ~s via AMQP", [MediaName]),
                            stream_over_amqp(Node, UUID, Media, JObj, Headers),
                            {<<"store">>, noop};
                        <<"put">> ->
                            %% stream file over HTTP PUT
                            ?LOG("stream ~s via HTTP PUT", [MediaName]),
                            stream_over_http(Node, UUID, Media, put, JObj),
                            {<<"store">>, noop};
                        <<"post">> ->
                            %% stream file over HTTP POST
                            ?LOG("stream ~s via HTTP POST", [MediaName]),
                            stream_over_http(Node, UUID, Media, post, JObj),
                            {<<"store">>, noop};
                        false ->
                            ?LOG("file ~s has gone missing!", [Media]),
                            {return, error};
                        _Method ->
                            %% unhandled method
                            ?LOG("unhandled stream method ~s", [_Method]),
                            {return, error}
                    end
            end
    end;

get_fs_app(_Node, _UUID, JObj, <<"tones">>) ->
    case wapi_dialplan:tones_v(JObj) of
        false -> {'error', <<"playback">>, <<"tones failed to execute as JObj did not validate">>};
        true ->
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

get_fs_app(_Node, _UUID, _JObj, <<"hold">>) ->
    {<<"endless_playback">>, <<"${hold_music}">>};

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
            ?LOG("say command ~s", [Arg]),
            {<<"say">>, Arg}
    end;

get_fs_app(Node, UUID, JObj, <<"bridge">>) ->
    Endpoints = wh_json:get_value(<<"Endpoints">>, JObj, []),
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
                                    ?LOG("bridge is simultaneous to multiple endpoints, starting local ringing"),
                                    %% we don't really care if this succeeds, the call will fail later on
                                    _ = send_cmd(Node, UUID, <<"ring_ready">>, ""),
                                    ",";
                                _Else ->
                                    "|"
                            end,

            KeyedEPs = [{[wh_json:get_value(<<"Invite-Format">>, Endpoint)
                         ,wh_json:get_value(<<"To-User">>, Endpoint)
                         ,wh_json:get_value(<<"To-Realm">>, Endpoint)
                         ,wh_json:get_value(<<"To-DID">>, Endpoint)
                         ,wh_json:get_value(<<"Route">>, Endpoint)], Endpoint}
                       || Endpoint <- Endpoints],

            S = self(),
            DialStrings = [D || D <- [receive {Pid, DS} -> DS end
                                      || Pid <- [spawn(fun() ->
                                                               put(callid, UUID),
                                                               S ! {self(), (catch get_bridge_endpoint(EP))}
                                                       end)
                                                 || {_, EP} <- props:unique(KeyedEPs)]
                                     ], D =/= ""],

            Generators = [fun(DP) ->
                                  case wh_json:get_integer_value(<<"Timeout">>, JObj) of
                                      undefined ->
                                          DP;
                                      TO when TO > 0 ->
                                          ?LOG("bridge will be attempted for ~p seconds", [TO]),
                                          [{"application", "set call_timeout=" ++ wh_util:to_list(TO)}|DP]
                                  end
                          end
                          ,fun(DP) ->
                                   case wh_json:get_value(<<"Ringback">>, JObj) of
                                       undefined ->
                                           {ok, RBSetting} = ecallmgr_util:get_setting(default_ringback, "%(2000,4000,440,480)"),
                                           [{"application", "set ringback=" ++ wh_util:to_list(RBSetting)}|DP];
                                       Ringback ->
                                           Stream = ecallmgr_util:media_path(Ringback, extant, UUID),
                                           ?LOG("bridge has custom ringback: ~s", [Stream]),
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
                                               undefined -> DP;
                                               Media ->
                                                   Stream = ecallmgr_util:media_path(Media, extant, UUID),
                                                   ?LOG("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
                                                   [{"application", <<"set hold_music=", Stream/binary>>}|DP]
                                           end;
                                       Media ->
                                           Stream = ecallmgr_util:media_path(Media, extant, UUID),
                                           ?LOG("bridge has custom music-on-hold: ~s", [Stream]),
                                           [{"application", <<"set hold_music=", Stream/binary>>}|DP]
                                   end
                           end
                          ,fun(DP) ->
                                   case wh_json:get_value(<<"Media">>, JObj) of
                                       <<"process">> ->
                                           ?LOG("bridge will process media through host switch"),
                                           [{"application", "set bypass_media=false"}|DP];
                                       <<"bypass">> ->
                                           ?LOG("bridge will connect the media peer-to-peer"),
                                           [{"application", "set bypass_media=true"}|DP];
                                       _ ->
                                           DP
                                   end
                           end
                          ,fun(DP) ->
                                   case wh_json:get_value(<<"Custom-Channel-Vars">>, JObj) of
                                       {struct, Props} ->
                                           [{"application", <<"set ", Var/binary, "=", (wh_util:to_binary(V))/binary>>}
                                            || {K, V} <- Props,
                                               (Var = props:get_value(K, ?SPECIAL_CHANNEL_VARS)) =/= undefined
                                           ] ++ DP;
                                       _ ->
                                           DP
                                   end
                           end
                          ,fun(DP) -> 
                                   case freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)) of
                                       {'ok', Result} -> 
                                           Props = ecallmgr_util:eventstr_to_proplist(Result),
                                           case props:get_value(<<"variable_hold_music">>, Props) of
                                               undefined ->
                                                   [{"application", "set import=hold_music"}|DP];
                                               _E ->
                                                   DP
                                           end;
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
                                   BridgeCmd = lists:flatten(["bridge ", ecallmgr_fs_xml:get_channel_vars(JObj)])
                                       ++ string:join([D || D <- DialStrings, D =/= ""], DialSeparator),
                                   [{"application", BridgeCmd}|DP]
                           end
                          ,fun(DP) ->
                                   [{"application", create_masquerade_event(<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>)}
                                    ,{"application", "park "}
                                    |DP
                                   ]
                           end
                         ],

            case DialStrings of
                [] ->
                    {error, <<"registrar returned no endpoints">>};
                _ ->
                    ?LOG("creating bridge dialplan"),
                    {<<"xferext">>, lists:foldr(fun(F, DP) -> F(DP) end, [], Generators)}
            end
    end;

get_fs_app(_Node, _UUID, JObj, <<"call_pickup">>) ->
    case wapi_dialplan:call_pickup_v(JObj) of
        false -> {'error', <<"intercept failed to execute as JObj did not validate">>};
        true ->
            Generators = [fun(DP) ->
                                  case wh_json:is_true(<<"Unbridged-Only">>, JObj) of
                                      false ->
                                          DP;
                                      true ->
                                          [{"application", "set intercept_unbridged_only=true"}|DP]
                                  end
                          end
                          ,fun(DP) ->
                                   case wh_json:is_true(<<"Unanswered-Only">>, JObj) of
                                       false ->
                                           DP;
                                       true ->
                                           [{"application", "set intercept_unanswered_only=true"}|DP]
                                   end
                           end
                          ,fun(DP) ->
                                   [{"application", "export failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH"}
                                    ,{"application", "export uuid_bridge_continue_on_cancel=true"}
                                    ,{"application", "export continue_on_fail=true"}
                                    |DP
                                   ]
                           end
                          ,fun(DP) ->
                                   Target = wh_json:get_value(<<"Target-Call-ID">>, JObj),
                                   Arg = case wh_json:is_true(<<"Other-Leg">>, JObj) of
                                             true -> <<"-bleg ", Target/binary>>;
                                             false -> Target
                                         end,
                                   [{"application", <<"intercept ", Arg/binary>>}|DP]
                           end
                          ,fun(DP) ->
                                   [{"application", create_masquerade_event(<<"intercept">>, <<"CHANNEL_EXECUTE_COMPLETE">>)}
                                    ,{"application", "park "}
                                    |DP
                                   ]
                           end
                         ],
            {<<"xferext">>, lists:foldr(fun(F, DP) -> F(DP) end, [], Generators)}
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
                                    ,{"application", create_masquerade_event(<<"execute_extension">>
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
                                  [{<<"Application-Name">>, <<"park">>} | wapi_dialplan:extract_defaults(JObj)];
                              AppJObj -> 
                                  wh_json:from_list(AppJObj ++ wapi_dialplan:extract_defaults(JObj))
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

get_fs_app(Node, UUID, JObj, <<"set">>) ->
    case wapi_dialplan:set_v(JObj) of
        false -> {'error', <<"set failed to execute as JObj did not validate">>};
        true ->
            ChannelVars = wh_json:to_proplist(wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
            _ = [ set(Node, UUID, get_fs_kv(K, V, UUID)) || {K, V} <- ChannelVars],

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

get_fs_app(_Node, _UUID, JObj, <<"presence">>) ->
    NodeHandlers = ecallmgr_fs_sup:node_handlers(),
    UUID = case wh_json:get_ne_value(<<"Msg-ID">>, JObj) of
               undefined -> wh_util:to_list(wh_util:current_tstamp());
               Else -> wh_util:to_list(Else)
           end,
    State = case wh_json:get_value(<<"State">>, JObj) of
                <<"early">> -> "early";
                <<"confirmed">> -> "confirmed";
                _ -> "terminated"
            end,
    Event = [{"unique-id", UUID}
             ,{"channel-state", "CS_ROUTING"}
             ,{"answer-state", State}
             ,{"proto", "any"}
             ,{"login", "src/mod/event_handlers/mod_erlang_event/handle_msg.c"}
             ,{"from", wh_json:get_string_value(<<"User">>, JObj)}
             ,{"rpid", "unknown"}
             ,{"status", "CS_ROUTING"}
             ,{"event_type", "presence"}
             ,{"alt_event_type", "dialog"}
             ,{"presence-call-direction", "outbound"}
             ,{"event_cound", "0"}
            ],
    [begin
         ?LOG("sending presence in event to ~p~n", [Node]),
         freeswitch:sendevent(Node, 'PRESENCE_IN', Event) 
     end
     || NodeHandler <- NodeHandlers
            ,(Node = ecallmgr_fs_node:fs_node(NodeHandler)) =/= undefined
    ],
    {<<"presence">>, noop};

get_fs_app(_Node, _UUID, JObj, <<"conference">>) ->
    case wapi_dialplan:conference_v(JObj) of
        false -> {'error', <<"conference failed to execute as JObj did not validate">>};
        true ->
            ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
            {<<"conference">>, list_to_binary([ConfName, "@default", get_conference_flags(JObj)])}
    end;

get_fs_app(_Node, _UUID, _JObj, _App) ->
    ?LOG("unknown application ~s", [_App]),
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
                    ,wh_util:to_list(ecallmgr_util:media_path(Media, extant, UUID))
                   ]);
get_fs_kv(Key, Val, _) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        false ->
            list_to_binary([?CHANNEL_VAR_PREFIX, wh_util:to_list(Key), "=", wh_util:to_list(Val)]);
        {_, Prefix} ->
            list_to_binary([Prefix, "=", wh_util:to_list(Val)])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% send the SendMsg proplist to the freeswitch node
%% @end
%%--------------------------------------------------------------------
-type send_cmd_ret() :: fs_sendmsg_ret() | fs_api_ret().
-spec send_cmd/4 :: (atom(), ne_binary(), ne_binary() | string(), ne_binary() | string()) -> send_cmd_ret().
send_cmd(Node, UUID, <<"hangup">>, _) ->
    ?LOG("terminate call on node ~s", [Node]),
    _ = ecallmgr_util:fs_log(Node, "whistle terminating call", []),
    freeswitch:api(Node, uuid_kill, wh_util:to_list(UUID));
send_cmd(Node, UUID, <<"record_call">>, Args) ->
    Cmd = list_to_binary([UUID, " ", Args]),
    ?LOG("execute on node ~s: uuid_record(~s)", [Node, Cmd]),
    Ret = freeswitch:api(Node, uuid_record, wh_util:to_list(Cmd)),
    ?LOG("executing uuid_record returned ~p", [Ret]),
    Ret;
send_cmd(Node, UUID, <<"xferext">>, Dialplan) ->
    XferExt = [begin
                   _ = ecallmgr_util:fs_log(Node, "whistle queuing command in 'xferext' extension: ~s", [V]),
                   ?LOG("building xferext on node ~s: ~s", [Node, V]),
                   {wh_util:to_list(K), wh_util:to_list(V)}    
               end || {K, V} <- Dialplan],
    ok = freeswitch:sendmsg(Node, UUID, [{"call-command", "xferext"}] ++ XferExt),
    ecallmgr_util:fs_log(Node, "whistle transfered call to 'xferext' extension", []);
send_cmd(Node, UUID, AppName, Args) ->
    ?LOG("execute on node ~s: ~s(~s)", [Node, AppName, Args]),
    _ = ecallmgr_util:fs_log(Node, "whistle executing ~s ~s", [AppName, Args]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                    ,{"execute-app-name", wh_util:to_list(AppName)}
                                    ,{"execute-app-arg", wh_util:to_list(Args)}
                                   ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_masquerade_event/2 :: (ne_binary(), ne_binary()) -> ne_binary().
create_masquerade_event(Application, EventName) ->
    wh_util:to_list(<<"event Event-Name=CUSTOM,Event-Subclass=whistle::masquerade"
                      ,",whistle_event_name=", EventName/binary
                      ,",whistle_application_name=", Application/binary>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% take an endpoint (/sofia/foo/bar), and optionally a caller id name/num
%% and create the dial string ([origination_caller_id_name=Name
%%                              ,origination_caller_id_number=Num]Endpoint)
%% @end
%%--------------------------------------------------------------------
-spec get_bridge_endpoint/1 :: (wh_json:json_object()) -> string().
get_bridge_endpoint(JObj) ->
    case ecallmgr_fs_xml:build_route(JObj, wh_json:get_value(<<"Invite-Format">>, JObj)) of
        {'error', 'timeout'} ->
            ?LOG("unable to build route to endpoint"),
            "";
        EndPoint ->
            CVs = ecallmgr_fs_xml:get_leg_vars(JObj),
            wh_util:to_list(list_to_binary([CVs, EndPoint]))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stream_over_amqp/5 :: (atom(), ne_binary(), ne_binary(), wh_json:json_object(), proplist()) -> no_return().
stream_over_amqp(Node, UUID, File, JObj, Headers) ->
    DestQ = case wh_json:get_value(<<"Media-Transfer-Destination">>, JObj) of
                undefined ->
                    wh_json:get_value(<<"Server-ID">>, JObj);
                <<"">> ->
                    wh_json:get_value(<<"Server-ID">>, JObj);
                Q ->
                    Q
            end,
    amqp_stream(DestQ, fun stream_file/1, {undefined, File}, Headers, 1),
    send_store_call_event(Node, UUID, <<"complete">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% get a chunk of the file and send it in an AMQP message to the DestQ
%% @end
%%--------------------------------------------------------------------
-spec amqp_stream/5 :: (ne_binary(), fun(), {term(), ne_binary()}, proplist(), pos_integer()) -> no_return().
amqp_stream(DestQ, F, State, Headers, Seq) ->
    ?LOG("streaming via AMQP to ~s", [DestQ]),
    case F(State) of
        {ok, Data, State1} ->
            %% send msg
            Msg = [{<<"Media-Content">>, Data}
                   ,{<<"Media-Sequence-ID">>, Seq}
                   | Headers],
            {ok, JSON} = wapi_dialplan:store_amqp_resp(Msg),
            amqp_util:targeted_publish(DestQ, JSON, <<"application/json">>),
            amqp_stream(DestQ, F, State1, Headers, Seq+1);
        eof ->
            Msg = [{<<"Media-Content">>, <<"eof">>}
                   ,{<<"Media-Sequence-ID">>, Seq}
                   | Headers],
            {ok, JSON} = wapi_dialplan:store_amqp_resp(Msg),
            amqp_util:targeted_publish(DestQ, JSON, <<"application/json">>),
            eof
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stream_over_http/5 :: (atom(), ne_binary(), ne_binary(), 'put' | 'post', wh_json:json_object()) -> no_return().
stream_over_http(Node, UUID, File, Method, JObj) ->
    Url = wh_util:to_list(wh_json:get_value(<<"Media-Transfer-Destination">>, JObj)),
    ?LOG("streaming via HTTP(~s) to ~s", [Method, Url]),
    AddHeaders = wh_json:to_proplist(wh_json:get_value(<<"Additional-Headers">>, JObj, wh_json:new())),
    Headers = [{"Content-Length", filelib:file_size(File)}
               | [ {wh_util:to_list(K), V} || {K,V} <- AddHeaders] ],

    Body = {fun stream_file/1, {undefined, File}},
    AppQ = wh_json:get_value(<<"Server-ID">>, JObj),
    case ibrowse:send_req(Url, Headers, Method, Body) of
        {ok, "504", _, _} ->
            stream_over_http(Node, UUID, File, Method, JObj);
        {ok, StatusCode, RespHeaders, RespBody} ->
            MediaTransResults = wh_json:from_list([{<<"Status-Code">>, wh_util:to_binary(StatusCode)}
                                                   ,{<<"Headers">>, wh_json:from_list([ {wh_util:to_binary(K), wh_util:to_binary(V)} || {K,V} <- RespHeaders ])}
                                                   ,{<<"Body">>, wh_util:to_binary(RespBody)}
                                                  ]),
            send_store_call_event(Node, UUID, MediaTransResults),

            JObj1 = wh_json:set_values([{<<"Media-Transfer-Results">>, MediaTransResults}
                                        ,{<<"Event-Name">>, <<"response">>}
                                        ,{<<"Event-Category">>, <<"call">>}
                                       ], JObj),

            case wapi_dialplan:store_http_resp(JObj1) of
                {ok, Payload} ->
                    ?LOG("ibrowse 'OK'ed with ~p publishing to ~s: ~s", [StatusCode, AppQ, Payload]),
                    amqp_util:targeted_publish(AppQ, Payload, <<"application/json">>);
                {'error', Msg} ->
                    ?LOG("store via HTTP ~s errored: ~p", [Method, Msg])
            end;
        {'error', Error} ->
            ?LOG("ibrowse error: ~p", [Error]);
        {ibrowse_req_id, ReqId} ->
            ?LOG("ibrowse req id: ~p", [ReqId])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stream_file/1 :: (file_stream_state()) -> {'ok', ne_binary(), file_stream_state()} | 'eof'.
stream_file({undefined, File}) ->
    true = filelib:is_regular(File),
    {ok, Iod} = file:open(File, [read, raw, binary]),
    stream_file({Iod, File});
stream_file({Iod, _File}=State) ->
    case file:read(Iod, 8192) of
        {ok, Data} ->
            {ok, Data, State};
        eof ->
            'ok' = file:close(Iod),
            eof
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_terminators/3 :: (atom(), ne_binary(), 'undefined' | binary()) -> 'ok' | fs_api_ret().
set_terminators(_Node, _UUID, undefined) -> 'ok';
set_terminators(Node, UUID, <<>>) -> set(Node, UUID, <<"none">>);
set_terminators(Node, UUID, Ts) ->
    Terms = list_to_binary(["playback_terminators=", Ts]),
    set(Node, UUID, Terms).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set/3 :: (atom(), ne_binary(), ne_binary()) -> send_cmd_ret().
set(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "set", Arg).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec export/3 :: (atom(), ne_binary(), binary()) -> send_cmd_ret().
export(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "export", wh_util:to_list(Arg)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% builds a FS specific flag string for the conference command
%% @end
%%--------------------------------------------------------------------
-spec get_conference_flags/1 :: (wh_json:json_object()) -> binary().
get_conference_flags(JObj) ->
    Flags = [
             <<Flag/binary, Delim/binary>>
                 || {Flag, Parameter} <- ?CONFERENCE_FLAGS, Delim <- [<<",">>]
                        ,wh_util:to_boolean(wh_json:get_value(Parameter, JObj, false))
            ],
    case list_to_binary(Flags) of
        <<>> ->
            <<>>;
        F ->
            <<"+flags{", (erlang:binary_part(F, {0, byte_size(F)-1}))/binary, "}">>
    end.

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
                    ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
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
            {ok, P2} = wapi_dialplan:error(Error),
            amqp_util:callevt_publish(UUID, P2, event)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_error_response/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
send_error_response(App, Msg, UUID, JObj) ->
    ?LOG("error getting FS app for ~s: ~p", [App, Msg]),
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
             ,{<<"Error-Message">>, Msg}
             ,{<<"Call-ID">>, UUID}
             ,{<<"Application-Name">>, App}
             ,{<<"Application-Response">>, <<>>}
             | wh_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wapi_dialplan:error(Error),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_store_call_event/3 :: (atom(), ne_binary(), wh_json:json_object()) -> 'ok'.
send_store_call_event(Node, UUID, MediaTransResults) ->
    Timestamp = wh_util:to_binary(wh_util:current_tstamp()),
    Prop = try
               {ok, Dump} = freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)),
               ecallmgr_util:eventstr_to_proplist(Dump)
           catch
               _E:_R ->
                   ?LOG_SYS("Failed get params from uuid_dump"),
                   ?LOG_SYS("~p : ~p", [_E, _R]),
                   ?LOG_SYS("sending less interesting call_event message"),
                   []
           end,

    EvtProp1 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop, Timestamp)}
                ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop, Timestamp)}
                ,{<<"Call-ID">>, UUID}
                ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop, <<>>)}
                ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop, <<"HANGUP">>)}
                ,{<<"Application-Name">>, <<"store">>}
                ,{<<"Application-Response">>, MediaTransResults}
                | wh_api:default_headers(<<>>, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
               ],
    EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
                   [] -> EvtProp1;
                   CustomProp -> [{<<"Custom-Channel-Vars">>, wh_json:from_list(CustomProp)} | EvtProp1]
               end,
    wapi_call:publish_event(UUID, EvtProp2).

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
            ?LOG(UUID, "failed to get result from uuid_dump", []),
            DP
    end.
        
