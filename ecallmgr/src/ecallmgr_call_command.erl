%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Execute call commands
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------

-module(ecallmgr_call_command).

-export([exec_cmd/4]).

-include("ecallmgr.hrl").

-type fd() :: tuple().
-type io_device() :: pid() | fd().

-spec(exec_cmd/4 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), ControlPID :: pid()) -> ok | timeout | tuple(error, invalid_callid | failed)).
exec_cmd(Node, UUID, JObj, ControlPID) ->
    DestID = wh_json:get_value(<<"Call-ID">>, JObj),
    case DestID =:= UUID of
	true ->
	    App = wh_json:get_value(<<"Application-Name">>, JObj),
	    case get_fs_app(Node, UUID, JObj, App) of
		{error, Msg} ->
                    send_error_response(App, Msg, UUID, JObj),
                    ControlPID ! {execute_complete, UUID, App};
		{error, AppName, Msg} ->
                    send_error_response(App, Msg, UUID, JObj),
                    ControlPID ! {execute_complete, UUID, AppName};
                {return, Result} -> Result;
		{AppName, noop} ->
                    ControlPID ! {execute_complete, UUID, AppName};
		{AppName, AppData} ->
                    send_cmd(Node, UUID, AppName, AppData)
	    end;
	false ->
	    ?LOG("command ~s not meant for us but for ~s", [wh_json:get_value(<<"Application-Name">>, JObj), DestID]),
	    {error, invalid_callid}
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec(get_fs_app/4 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), Application :: binary()) ->
			   tuple(binary(), binary() | noop) | tuple(return, ok) | tuple(error, binary())
                               | tuple(error, binary(), binary())).
get_fs_app(Node, UUID, JObj, <<"noop">>=App) ->
    spawn(fun() ->
                  send_noop_call_event(Node, UUID, JObj)
          end),
    {App, noop};
get_fs_app(Node, UUID, JObj, <<"play">>) ->
    case whistle_api:play_req_v(JObj) of
	false -> {error, <<"playback">>, <<"play failed to execute as JObj did not validate">>};
	true ->
	    F = media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID),
	    ok = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
	    {<<"playback">>, F}
    end;
get_fs_app(_Node, _UUID, _JObj, <<"hangup">>=App) ->
    {App, <<>>};
get_fs_app(_Node, UUID, JObj, <<"play_and_collect_digits">>) ->
    case whistle_api:play_collect_digits_req_v(JObj) of
	false -> {error, <<"play_and_get_digits">>, <<"play_and_collect_digits failed to execute as JObj did not validate">>};
	true ->
	    Min = wh_json:get_value(<<"Minimum-Digits">>, JObj),
	    Max = wh_json:get_value(<<"Maximum-Digits">>, JObj),
	    Timeout = wh_json:get_value(<<"Timeout">>, JObj),
	    Terminators = wh_json:get_value(<<"Terminators">>, JObj),
	    Media = <<$', (media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID))/binary, $'>>,
	    InvalidMedia = <<$', (media_path(wh_json:get_value(<<"Failed-Media-Name">>, JObj), UUID))/binary, $'>>,
	    Tries = wh_json:get_value(<<"Media-Tries">>, JObj),
	    Regex = wh_json:get_value(<<"Digits-Regex">>, JObj),
	    Storage = <<"collected_digits">>,
	    Data = list_to_binary([Min, " ", Max, " ", Tries, " ", Timeout, " ", Terminators, " "
				   ,Media, " ", InvalidMedia, " ", Storage, " ", Regex]),
	    {<<"play_and_get_digits">>, Data}
    end;
get_fs_app(Node, UUID, JObj, <<"record">>=App) ->
    case whistle_api:record_req_v(JObj) of
	false -> {error, <<"record failed to execute as JObj did not validate">>};
	true ->
	    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            Media = ecallmgr_media_registry:register_local_media(MediaName, UUID),

	    _ = set(Node, UUID, "enable_file_write_buffering=false"), % disable buffering to see if we get all the media

	    RecArg = binary_to_list(list_to_binary([Media, " "
						    ,whistle_util:to_list(wh_json:get_value(<<"Time-Limit">>, JObj, "20")), " "
						    ,whistle_util:to_list(wh_json:get_value(<<"Silence-Threshold">>, JObj, "500")), " "
						    ,whistle_util:to_list(wh_json:get_value(<<"Silence-Hits">>, JObj, "5"))
						   ])),
	    ok = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),

	    {App, RecArg}
    end;
get_fs_app(_Node, UUID, JObj, <<"store">>) ->
    case whistle_api:store_req_v(JObj) of
	false -> {error, <<"store failed to execute as JObj did not validate">>};
	true ->
	    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
	    case ecallmgr_media_registry:is_local(MediaName, UUID) of
		{error, not_local} ->
		    ?LOG("failed to find media ~s for storing", [MediaName]);
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
			    stream_over_amqp(Media, JObj, Headers),
			    {<<"store">>, noop};
			<<"put">>=Verb ->
			    %% stream file over HTTP PUT
			    ?LOG("stream ~s via HTTP PUT", [MediaName]),
			    stream_over_http(Media, Verb, JObj),
			    {<<"store">>, noop};
			<<"post">>=Verb ->
			    %% stream file over HTTP POST
			    ?LOG("stream ~s via HTTP POST", [MediaName]),
			    stream_over_http(Media, Verb, JObj),
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
    case whistle_api:tones_req_v(JObj) of
	false -> {error, <<"playback">>, <<"tones failed to execute as JObj did not validate">>};
	true ->
	    Tones = wh_json:get_value(<<"Tones">>, JObj, []),
	    FSTones = [begin
			   Vol = case wh_json:get_value(<<"Volume">>, Tone) of
				     undefined -> [];
				     %% need to map V (0-100) to FS values
				     V -> list_to_binary(["v=", whistle_util:to_list(V), ";"])
				 end,
			   Repeat = case wh_json:get_value(<<"Repeat">>, Tone) of
					undefined -> [];
					R -> list_to_binary(["l=", whistle_util:to_list(R), ";"])
				    end,
			   Freqs = string:join([ whistle_util:to_list(V) || V <- wh_json:get_value(<<"Frequencies">>, Tone) ], ","),
			   On = whistle_util:to_list(wh_json:get_value(<<"Duration-ON">>, Tone)),
			   Off = whistle_util:to_list(wh_json:get_value(<<"Duration-OFF">>, Tone)),
			   whistle_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
		       end || Tone <- Tones],
	    Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
	    {<<"playback">>, Arg}
    end;
get_fs_app(_Node, _UUID, _JObj, <<"answer">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, _JObj, <<"progress">>) ->
    {<<"pre_answer">>, <<>>};
get_fs_app(_Node, _UUID, _JObj, <<"park">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, JObj, <<"sleep">>=App) ->
    case whistle_api:sleep_req_v(JObj) of
	false -> {error, <<"sleep failed to execute as JObj did not validate">>};
	true -> {App, whistle_util:to_binary(wh_json:get_value(<<"Time">>, JObj))}
    end;
get_fs_app(_Node, _UUID, JObj, <<"say">>=App) ->
    case whistle_api:say_req_v(JObj) of
	false -> {error, <<"say failed to execute as JObj did not validate">>};
	true ->
	    Lang = wh_json:get_value(<<"Language">>, JObj),
	    Type = wh_json:get_value(<<"Type">>, JObj),
	    Method = wh_json:get_value(<<"Method">>, JObj),
	    Txt = wh_json:get_value(<<"Say-Text">>, JObj),
	    Arg = list_to_binary([Lang, " ", Type, " ", Method, " ", Txt]),
	    ?LOG("say command ~s", [Arg]),
	    {App, Arg}
    end;
get_fs_app(Node, UUID, JObj, <<"bridge">>=App) ->
    case whistle_api:bridge_req_v(JObj) of
	false -> {error, <<"bridge failed to execute as JObj did not validate">>};
	true ->
            ok = set_ringback(Node, UUID, wh_json:get_value(<<"Ringback">>, JObj)),
            %% NOTE: at this time FS is not honoring call_timeout when set in the bridge string, arg...
            ok = set_timeout(Node, UUID, wh_json:get_value(<<"Timeout">>, JObj)),
            ok = set(Node, UUID, "failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH"),
	    DialSeparator = case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
				<<"simultaneous">> -> ",";
				<<"single">> -> "|"
			    end,
            %% Taken from http://montsamu.blogspot.com/2007/02/erlang-parallel-map-and-parallel.html
            S = self(),
            DialStrings = [ receive {Pid, DS} -> DS end
                            || Pid <- [
                                       spawn(fun() -> S ! {self(), (catch get_bridge_endpoint(EP))} end)
                                       || EP <- wh_json:get_value(<<"Endpoints">>, JObj, [])
                                      ]
                          ],
            case DialStrings of
                [[]] ->
                    {error, <<"bridge failed to execute no endpoints avaliable">>};
                _ ->
                    BridgeCmd = lists:flatten(ecallmgr_fs_xml:get_channel_vars(JObj))
                        ++ string:join([D || D <- DialStrings, D =/= ""], DialSeparator),
                    {App, BridgeCmd}
            end
    end;
get_fs_app(Node, UUID, JObj, <<"tone_detect">>=App) ->
    case whistle_api:tone_detect_req_v(JObj) of
	false -> {error, <<"tone detect failed to execute as JObj did not validate">>};
	true ->
	    Key = wh_json:get_value(<<"Tone-Detect-Name">>, JObj),
	    Freqs = [ whistle_util:to_list(V) || V <- wh_json:get_value(<<"Frequencies">>, JObj) ],
	    FreqsStr = string:join(Freqs, ","),
	    Flags = case wh_json:get_value(<<"Sniff-Direction">>, JObj, <<"read">>) of
			<<"read">> -> <<"r">>;
			<<"write">> -> <<"w">>
		    end,
	    Timeout = wh_json:get_value(<<"Timeout">>, JObj, <<"+1000">>),
	    HitsNeeded = wh_json:get_value(<<"Hits-Needed">>, JObj, <<"1">>),

	    SuccessJObj = case wh_json:get_value(<<"On-Success">>, JObj, []) of
			      [] -> [{<<"Application-Name">>, <<"park">>} | whistle_api:extract_defaults(JObj)]; % default to parking the call
			      AppJObj -> {struct, AppJObj ++ whistle_api:extract_defaults(JObj)}
			  end,
	    {SuccessApp, SuccessAppData} = case get_fs_app(Node, UUID, SuccessJObj, wh_json:get_value(<<"Application-Name">>, SuccessJObj)) of
					       {error, _Str} -> {<<"park">>, <<>>}; % default to park if passed app isn't right
					       {_, _}=Success -> Success
					   end,
	    Data = list_to_binary([Key, " ", FreqsStr, " ", Flags, " ", Timeout, " ", SuccessApp, " ", SuccessAppData, " ", HitsNeeded]),
	    {App, Data}
    end;
get_fs_app(Node, UUID, JObj, <<"set">>=App) ->
    case whistle_api:set_req_v(JObj) of
        false -> {error, <<"set failed to execute as JObj did not validate">>};
        true ->
            {struct, ChannelVars} = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT),
            lists:foreach(fun({K,V}) ->
                                  Arg = list_to_binary([?CHANNEL_VAR_PREFIX
                                                        ,whistle_util:to_list(K), "=", whistle_util:to_list(V)]),
                                  set(Node, UUID, Arg)
                          end, ChannelVars),
            {struct, CallVars} = wh_json:get_value(<<"Custom-Call-Vars">>, JObj, ?EMPTY_JSON_OBJECT),
            lists:foreach(fun({K,V}) ->
                                  Arg = list_to_binary([?CHANNEL_VAR_PREFIX
                                                        ,whistle_util:to_list(K), "=", whistle_util:to_list(V)]),
                                  export(Node, UUID, Arg)
                          end, CallVars),
            {App, noop}
    end;
get_fs_app(_Node, _UUID, JObj, <<"respond">>=App) ->
    case whistle_api:respond_req_v(JObj) of
        false -> {error, <<"respond failed to execute as JObj did not validate">>};
        true ->
            Response = <<(wh_json:get_value(<<"Response-Code">>, JObj, <<>>))/binary
                         ," ", (wh_json:get_value(<<"Response-Message">>, JObj, <<>>))/binary>>,
            {App, Response}
    end;
get_fs_app(Node, UUID, JObj, <<"fetch">>=App) ->
    spawn(fun() ->
                  send_fetch_call_event(Node, UUID, JObj)
          end),
    {App, noop};
get_fs_app(_Node, _UUID, JObj, <<"conference">>=App) ->
    case whistle_api:conference_req_v(JObj) of
	false -> {error, <<"conference failed to execute as JObj did not validate">>};
	true ->
	    ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
	    {App, list_to_binary([ConfName, "@default", get_conference_flags(JObj)])}
    end;
get_fs_app(_Node, _UUID, _JObj, _App) ->
    ?LOG("unknown application ~s", [_App]),
    {error, <<"application unknown">>}.

%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(send_cmd/4 :: (Node :: atom(), UUID :: binary(), AppName :: binary() | string(), Args :: binary() | string()) -> ok | timeout | {error, string()}).
send_cmd(Node, UUID, AppName, Args) ->
    ?LOG("SendMsg: Node: ~p", [Node]),
    ?LOG("SendMsg: App: ~s", [AppName]),
    ?LOG("SendMsg: Args: ~s", [Args]),
    freeswitch:sendmsg(Node, UUID, [
				    {"call-command", "execute"}
				    ,{"execute-app-name", whistle_util:to_list(AppName)}
				    ,{"execute-app-arg", whistle_util:to_list(Args)}
				   ]).

%% take an endpoint (/sofia/foo/bar), and optionally a caller id name and number
%% and create the dial string ([origination_caller_id_name=Name,origination_caller_id_number=Num]Endpoint)
-spec(get_bridge_endpoint/1 :: (JObj :: json_object()) -> string()).
get_bridge_endpoint(JObj) ->
    case ecallmgr_fs_xml:build_route(JObj, wh_json:get_value(<<"Invite-Format">>, JObj)) of
	{error, timeout} -> "";
	EndPoint ->
	    CVs = ecallmgr_fs_xml:get_leg_vars(JObj),
	    whistle_util:to_list(list_to_binary([CVs, EndPoint]))
    end.

-spec(media_path/2 :: (MediaName :: binary(), UUID :: binary()) -> binary()).
-spec(media_path/3 :: (MediaName :: binary(), Type :: extant | continuous, UUID :: binary()) -> binary()).
media_path(MediaName, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, UUID) of
        {error, _} ->
            MediaName;
        {ok, Url} ->
            get_fs_playback(Url)
    end.

media_path(MediaName, Type, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, Type, UUID) of
        {error, _} ->
            MediaName;
        {ok, Url} ->
            get_fs_playback(Url)
    end.

-spec(get_fs_playback/1 :: (Url :: binary()) -> binary()).
get_fs_playback(<<"http://", _/binary>>=Url) ->
    RemoteAudioScript = get_setting(remote_audio_script, <<"/tmp/fetch_remote_audio.sh">>),
    <<"shell_stream://", (whistle_util:to_binary(RemoteAudioScript))/binary, " ", Url/binary>>;
get_fs_playback(Url) ->
    Url.

-spec(stream_over_amqp/3 :: (File :: binary(), JObj :: proplist(), Headers :: proplist()) -> no_return()).
stream_over_amqp(File, JObj, Headers) ->
    DestQ = case wh_json:get_value(<<"Media-Transfer-Destination">>, JObj) of
		undefined ->
		    wh_json:get_value(<<"Server-ID">>, JObj);
		<<"">> ->
		    wh_json:get_value(<<"Server-ID">>, JObj);
		Q ->
		    Q
	    end,
    stream_over_amqp(DestQ, fun stream_file/1, {undefined, File}, Headers, 1).

%% get a chunk of the file and send it in an AMQP message to the DestQ
-spec(stream_over_amqp/5 :: (DestQ :: binary(), F :: fun(), State :: tuple(), Headers :: proplist(), Seq :: pos_integer()) -> no_return()).
stream_over_amqp(DestQ, F, State, Headers, Seq) ->
    ?LOG("streaming via AMQP to ~s", [DestQ]),
    case F(State) of
	{ok, Data, State1} ->
	    %% send msg
	    Msg = [{<<"Media-Content">>, Data}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    {ok, JSON} = whistle_api:store_amqp_resp(Msg),
	    amqp_util:targeted_publish(DestQ, JSON, <<"application/json">>),
	    stream_over_amqp(DestQ, F, State1, Headers, Seq+1);
	eof ->
	    Msg = [{<<"Media-Content">>, <<"eof">>}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    {ok, JSON} = whistle_api:store_amqp_resp(Msg),
	    amqp_util:targeted_publish(DestQ, JSON, <<"application/json">>),
	    eof
    end.

-spec(stream_over_http/3 :: (File :: binary(), Verb :: binary(), JObj :: proplist()) -> no_return()).
stream_over_http(File, Verb, JObj) ->
    Url = whistle_util:to_list(wh_json:get_value(<<"Media-Transfer-Destination">>, JObj)),
    ?LOG("streaming via HTTP(~s) to ~s", [Verb, Url]),
    {struct, AddHeaders} = wh_json:get_value(<<"Additional-Headers">>, JObj, ?EMPTY_JSON_OBJECT),
    Headers = [{"Content-Length", filelib:file_size(File)}
	       | [ {whistle_util:to_list(K), V} || {K,V} <- AddHeaders] ],
    Method = whistle_util:to_atom(Verb, true),
    Body = {fun stream_file/1, {undefined, File}},
    AppQ = wh_json:get_value(<<"Server-ID">>, JObj),
    case ibrowse:send_req(Url, Headers, Method, Body) of
	{ok, StatusCode, RespHeaders, RespBody} ->
	    case whistle_api:store_http_resp(wh_json:set_value(<<"Media-Transfer-Results">>
								       ,{struct, [{<<"Status-Code">>, StatusCode}
										  ,{<<"Headers">>, {struct, [ {whistle_util:to_binary(K), whistle_util:to_binary(V)} || {K,V} <- RespHeaders ]}}
										  ,{<<"Body">>, whistle_util:to_binary(RespBody)}
										 ]}
								   ,JObj)) of
		{ok, Payload} ->
		    ?LOG("ibrowse OKed with ~p publishing to ~s: ~s", [StatusCode, AppQ, Payload]),
		    amqp_util:targeted_publish(AppQ, Payload, <<"application/json">>);
		{error, Msg} ->
		    ?LOG("store via HTTP ~s errored: ~p", [Verb, Msg])
	    end;
	{error, Error} ->
	    ?LOG("ibrowse error: ~p", [Error]);
	{ibrowse_req_id, ReqId} ->
	    ?LOG("ibrowse req id: ~p", [ReqId])
    end.

-spec(stream_file/1 :: ({undefined | io_device(), binary()}) -> {ok, list(), tuple()} | eof).
stream_file({undefined, File}) ->
    true = filelib:is_regular(File),
    {ok, Iod} = file:open(File, [read, raw]),
    stream_file({Iod, File});
stream_file({Iod, _File}=State) ->
    case file:read(Iod, 8192) of
        {ok, Data} ->
            {ok, Data, State};
        eof ->
	    ok = file:close(Iod),
	    eof
    end.

-spec(set_terminators/3 :: (Node :: atom(), UUID :: binary(), Terminators :: undefined | binary()) -> ok | timeout | {error, string()}).
set_terminators(_Node, _UUID, undefined) ->
    ok;
set_terminators(Node, UUID, <<>>) ->
    set(Node, UUID, "none");
set_terminators(Node, UUID, Ts) ->
    Terms = list_to_binary(["playback_terminators=", Ts]),
    set(Node, UUID, Terms).

-spec(set_ringback/3 :: (Node :: atom(), UUID :: binary(), RingBack :: undefined | binary()) -> ok | timeout | {error, string()}).
set_ringback(Node, UUID, undefined) ->
    RB = list_to_binary(["ringback=", get_setting(default_ringback, "%(2000,4000,440,480)")]),
    ok = set(Node, UUID, RB);
set_ringback(Node, UUID, RingBack) ->
    RB = list_to_binary(["ringback=", media_path(RingBack, <<"extant">>, UUID)]),
    ok = set(Node, UUID, RB),
    set(Node, UUID, "instant_ringback=true").

-spec(set_timeout/3 :: (Node :: atom(), UUID :: binary(), Timeout :: undefined | binary()) -> ok | timeout | {error, string()}).
set_timeout(_Node, _UUID, undefined) ->
    ok;
set_timeout(Node, UUID, Timeout) ->
    case whistle_util:to_integer(Timeout) of
        TO when TO > 0 ->
            set(Node, UUID, <<"call_timeout=", (whistle_util:to_binary(TO))/binary>>);
        _Else ->
            ok
    end.

-spec(set/3 :: (Node :: atom(), UUID :: binary(), Arg :: string() | binary()) -> ok | timeout | {error, string()}).
set(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "set", whistle_util:to_list(Arg)).

-spec(export/3 :: (Node :: atom(), UUID :: binary(), Arg :: string() | binary()) -> ok | timeout | {error, string()}).
export(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "export", whistle_util:to_list(Arg)).

%% builds a FS specific flag string for the conference command
-spec(get_conference_flags/1 :: (JObj :: json_object()) -> binary()).
get_conference_flags(JObj) ->
    Flags = [
             <<Flag/binary, Delim/binary>>
                 || {Flag, Parameter} <- ?CONFERENCE_FLAGS, Delim <- [<<",">>]
			,whistle_util:to_boolean(wh_json:get_value(Parameter, JObj, false))
            ],
    case list_to_binary(Flags) of
        <<>> ->
            <<>>;
        F ->
            <<"+flags{", (erlang:binary_part(F, {0, byte_size(F)-1}))/binary, "}">>
    end.

-spec(send_fetch_call_event/3 :: (Node :: binary(), UUID :: binary(), JObj :: json_object()) -> no_return()).
send_fetch_call_event(Node, UUID, JObj) ->
    try
        Prop = case whistle_util:is_true(wh_json:get_value(<<"From-Other-Leg">>, JObj)) of
                   true ->
                       {ok, OtherUUID} = freeswitch:api(Node, uuid_getvar, whistle_util:to_list(<<UUID/binary, " signal_bond">>)),
                       {ok, Dump} = freeswitch:api(Node, uuid_dump, whistle_util:to_list(OtherUUID)),
                       ecallmgr_util:eventstr_to_proplist(Dump);
                   false ->
                       {ok, Dump} = freeswitch:api(Node, uuid_dump, whistle_util:to_list(UUID)),
                       ecallmgr_util:eventstr_to_proplist(Dump)

               end,
        EvtProp1 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                    ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                    ,{<<"Call-ID">>, UUID}
                    ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop)}
                    ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop)}
                    ,{<<"Application-Name">>, <<"fetch">>}
                    ,{<<"Application-Response">>, <<>>}
                    | whistle_api:default_headers(<<>>, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
                   ],
        EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
                       [] -> EvtProp1;
                       CustomProp -> [{<<"Custom-Channel-Vars">>, {struct, CustomProp}} | EvtProp1]
                   end,
        {ok, P1} = whistle_api:call_event(EvtProp2),
        amqp_util:callevt_publish(UUID, P1, event)
    catch
        Type:_ ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                     ,{<<"Error-Message">>, "failed to construct or publish fetch call event"}
                     ,{<<"Call-ID">>, UUID}
                     ,{<<"Application-Name">>, <<"fetch">>}
                     ,{<<"Application-Response">>, <<>>}
                     | whistle_api:default_headers(<<>>, <<"error">>, whistle_util:to_binary(Type), ?APP_NAME, ?APP_VERSION)
                    ],
            {ok, P2} = whistle_api:error_resp(Error),
            amqp_util:callevt_publish(UUID, P2, event)
    end.

-spec(send_noop_call_event/3 :: (Node :: binary(), UUID :: binary(), JObj :: json_object()) -> no_return()).
send_noop_call_event(Node, UUID, JObj) ->
    try
        {ok, Dump} = freeswitch:api(Node, uuid_dump, whistle_util:to_list(UUID)),
        Prop = ecallmgr_util:eventstr_to_proplist(Dump),
        EvtProp1 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                    ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                    ,{<<"Call-ID">>, UUID}
                    ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop)}
                    ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop)}
                    ,{<<"Application-Name">>, <<"noop">>}
                    ,{<<"Application-Response">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                    | whistle_api:default_headers(<<>>, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
                   ],
        EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
                       [] -> EvtProp1;
                       CustomProp -> [{<<"Custom-Channel-Vars">>, {struct, CustomProp}} | EvtProp1]
                   end,
        {ok, P1} = whistle_api:call_event(EvtProp2),
        amqp_util:callevt_publish(UUID, P1, event)
    catch
        Type:_ ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                     ,{<<"Error-Message">>, "failed to construct or publish noop call event"}
                     ,{<<"Call-ID">>, UUID}
                     ,{<<"Application-Name">>, <<"noop">>}
                     ,{<<"Application-Response">>, <<>>}
                     | whistle_api:default_headers(<<>>, <<"error">>, whistle_util:to_binary(Type), ?APP_NAME, ?APP_VERSION)
                    ],
            {ok, P2} = whistle_api:error_resp(Error),
            amqp_util:callevt_publish(UUID, P2, event)
    end.

-spec(send_error_response/4 :: (App :: binary(), Msg :: binary(), UUID :: binary(), JObj :: json_object()) -> ok).
send_error_response(App, Msg, UUID, JObj) ->
    ?LOG("error getting FS app for ~s: ~p", [App, Msg]),
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
             ,{<<"Error-Message">>, Msg}
             ,{<<"Call-ID">>, UUID}
             ,{<<"Application-Name">>, App}
             ,{<<"Application-Response">>, <<>>}
             | whistle_api:default_headers(<<>>, <<"error">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = whistle_api:error_resp(Error),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

-spec(get_setting/2 :: (Setting :: term(), Default :: term()) -> term()).
get_setting(Setting, Default) ->
    case wh_cache:fetch({ecallmgr_setting, Setting}) of
        {ok, Value} -> Value;
        {error, _} ->
            case file:consult(?SETTINGS_FILE) of
                {ok, Settings} ->
                    Value = props:get_value(Setting, Settings, Default),
                    wh_cache:store({ecallmgr_setting, Setting}, Value),
                    Value;
                {error, _} ->
                    wh_cache:store({ecallmgr_setting, Setting}, Default),
                    Default
            end
    end.
