%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Execute call commands
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------

-module(ecallmgr_call_command).

-export([exec_cmd/3]).

-include("ecallmgr.hrl").

-type fd() :: tuple().
-type io_device() :: pid() | fd().

-spec(exec_cmd/3 :: (Node :: atom(), UUID :: binary(), JObj :: json_object()) -> ok | timeout | tuple(error, string())).
exec_cmd(Node, UUID, JObj) ->
    DestID = wh_json:get_value(<<"Call-ID">>, JObj),
    case DestID =:= UUID of
	true ->
	    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
	    case get_fs_app(Node, UUID, JObj, AppName) of
		{error, _Msg}=Err -> Err;
                {return, Result} -> Result;
		{_, noop} ->
                    logger:format_log(info, "CONTROL(~p): Noop for ~p~n", [self(), AppName]),
                    {CCS, ETS} = try
                                     {ok, CS} =
                                         freeswitch:api(Node, eval, whistle_util:to_list(<<"uuid:", UUID/binary, " ${channel-call-state}">>)),
                                     {ok, TS} =
                                         freeswitch:api(Node, eval, whistle_util:to_list(<<"uuid:", UUID/binary, " ${Event-Date-Timestamp}">>)),
                                     {CS, TS}
                                 catch
                                     _:_ ->
                                         {<<>>, <<>>}
                                 end,
                    Event = [
                              {<<"Timestamp">>, ETS}
                             ,{<<"Application-Name">>, <<"noop">>}
                             ,{<<"Application-Response">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                             ,{<<"Channel-Call-State">>, CCS}
                             ,{<<"Call-ID">>, UUID}
                             | whistle_api:default_headers(<<>>, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, ?APP_NAME, ?APP_VERSION)
                            ],
                    {ok, Payload} = whistle_api:call_event(Event),
                    amqp_util:callevt_publish(UUID, Payload, event);
		{App, AppData} ->
                    send_cmd(Node, UUID, App, AppData)
	    end;
	false ->
	    logger:format_log(error, "CONTROL(~p): Cmd Not for us(~p) but for ~p~n", [self(), UUID, DestID]),
	    {error, "Command not for this node"}
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec(get_fs_app/4 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), Application :: binary()) ->
			   tuple(binary(), binary() | noop) | tuple(return, ok) | tuple(error, string())).
get_fs_app(_Node, _UUID, _JObj, <<"noop">>) ->
    {ok, noop};
get_fs_app(Node, UUID, JObj, <<"play">>) ->
    case whistle_api:play_req_v(JObj) of
	false -> {error, "play failed to execute as JObj did not validate."};
	true ->
	    F = media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID),
	    ok = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
	    {<<"playback">>, F}
    end;
get_fs_app(_Node, _UUID, _JObj, <<"hangup">>=App) ->
    {App, <<>>};
get_fs_app(_Node, UUID, JObj, <<"play_and_collect_digits">>) ->
    case whistle_api:play_collect_digits_req_v(JObj) of
	false -> {error, "play_and_collect_digits failed to execute as JObj did not validate."};
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
	    Data = list_to_binary([Min, " ", Max, " ", Tries, " ", Timeout, " ", Terminators, " ",
				   Media, " ", InvalidMedia, " ", Storage, " ", Regex]),
	    {<<"play_and_get_digits">>, Data}
    end;
get_fs_app(Node, UUID, JObj, <<"record">>) ->
    case whistle_api:record_req_v(JObj) of
	false -> {error, "record failed to execute as JObj did not validate."};
	true ->
	    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            Media = ecallmgr_media_registry:register_local_media(MediaName, UUID),
	    RecArg = binary_to_list(list_to_binary([Media, " "
						    ,whistle_util:to_list(wh_json:get_value(<<"Time-Limit">>, JObj, "20")), " "
						    ,whistle_util:to_list(wh_json:get_value(<<"Silence-Threshold">>, JObj, "500")), " "
						    ,whistle_util:to_list(wh_json:get_value(<<"Silence-Hits">>, JObj, "5"))
						   ])),
	    ok = set_terminators(Node, UUID, wh_json:get_value(<<"Terminators">>, JObj)),
	    
	    {<<"record">>, RecArg}
    end;
get_fs_app(_Node, UUID, JObj, <<"store">>) ->
    case whistle_api:store_req_v(JObj) of
	false -> {error, "store failed to execute as JObj did not validate."};
	true ->
	    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
	    case ecallmgr_media_registry:is_local(MediaName, UUID) of
		{error, not_local} ->
		    logger:format_log(error, "CONTROL(~p): Failed to find ~p for storing~n~p~n", [self(), MediaName, JObj]);
		{ok, Media} ->
                    M = whistle_util:to_list(Media),
		    case filelib:is_regular(M)
                        andalso wh_json:get_value(<<"Media-Transfer-Method">>, JObj) of
			<<"stream">> ->
			    %% stream file over AMQP
			    Headers = [{<<"Media-Transfer-Method">>, <<"stream">>}
				       ,{<<"Media-Name">>, MediaName}
				       ,{<<"Application-Name">>, <<"store">>}
				      ],
			    spawn(fun() -> stream_over_amqp(M, JObj, Headers) end);
			<<"put">>=Verb ->
			    %% stream file over HTTP PUT
			    spawn(fun() -> stream_over_http(M, Verb, JObj) end);
			<<"post">>=Verb ->
			    %% stream file over HTTP PUSH
			    spawn(fun() -> stream_over_http(M, Verb, JObj) end);
			false ->
			    logger:format_log(error, "CONTROL(~p): File ~p has gone missing!~n", [self(), Media]);
			_Method ->
			    %% unhandled method
			    logger:format_log(error, "CONTROL(~p): Unhandled stream method ~p~n", [self(), _Method])
		    end
	    end,
            {return, ok}
    end;
get_fs_app(_Node, _UUID, JObj, <<"tones">>) ->
    case whistle_api:tones_req_v(JObj) of
	false -> {error, "tones failed to execute as JObj did not validate."};
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
get_fs_app(_Node, _UUID, _JObj, <<"park">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, JObj, <<"sleep">>=App) ->
    case whistle_api:sleep_req_v(JObj) of
	false -> {error, "sleep failed to execute as JObj did not validate."};
	true -> {App, whistle_util:to_binary(wh_json:get_value(<<"Time">>, JObj))}
    end;
get_fs_app(_Node, _UUID, JObj, <<"say">>=App) ->
    case whistle_api:say_req_v(JObj) of
	false -> {error, "say failed to execute as JObj did not validate."};
	true ->
	    Lang = wh_json:get_value(<<"Language">>, JObj),
	    Type = wh_json:get_value(<<"Type">>, JObj),
	    Method = wh_json:get_value(<<"Method">>, JObj),
	    Txt = wh_json:get_value(<<"Say-Text">>, JObj),
	    Arg = list_to_binary([Lang, " ", Type, " ", Method, " ", Txt]),
            logger:format_log(info, "BUILT COMMAND: conference ~p", [Arg]),
	    {App, Arg}
    end;
get_fs_app(Node, UUID, JObj, <<"bridge">>=App) ->
    case whistle_api:bridge_req_v(JObj) of
	false -> {error, "bridge failed to execute as JObj did not validate."};
	true ->
	    ok = set_timeout(Node, UUID, wh_json:get_value(<<"Timeout">>, JObj)),
	    ok = set_continue_on_fail(Node, UUID, wh_json:get_value(<<"Continue-On-Fail">>, JObj)),
	    ok = set_eff_caller_id_name(Node, UUID, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)),
	    ok = set_eff_caller_id_number(Node, UUID, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)),
	    ok = set_eff_callee_id_name(Node, UUID, wh_json:get_value(<<"Outgoing-Callee-ID-Name">>, JObj)),
	    ok = set_eff_callee_id_number(Node, UUID, wh_json:get_value(<<"Outgoing-Callee-ID-Number">>, JObj)),
	    ok = set_ringback(Node, UUID, wh_json:get_value(<<"Ringback">>, JObj)),
	    ok = set_sip_req_headers(Node, UUID, wh_json:get_value(<<"SIP-Headers">>, JObj)),

	    DialSeparator = case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj) of
				<<"simultaneous">> -> ",";
				<<"single">> -> "|"
			    end,

	    DialStrings = [ DS || DS <- [get_bridge_endpoint(EP) || EP <- wh_json:get_value(<<"Endpoints">>, JObj, [])], DS =/= "" ],

	    BridgeCmd = string:join(DialStrings, DialSeparator),

	    {App, BridgeCmd}
    end;
get_fs_app(Node, UUID, JObj, <<"tone_detect">>=App) ->
    case whistle_api:tone_detect_req_v(JObj) of
	false -> {error, "tone detect failed to execute as JObj did not validate"};
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
get_fs_app(Node, UUID, JObj, <<"set">>=AppName) ->
    {struct, ChannelVars} = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT),
    lists:foreach(fun({K,V}) ->
			  Arg = list_to_binary([?CHANNEL_VAR_PREFIX, whistle_util:to_list(K), "=", whistle_util:to_list(V)]),
			  set(Node, UUID, Arg)
		  end, ChannelVars),
    {struct, CallVars} = wh_json:get_value(<<"Custom-Call-Vars">>, JObj, ?EMPTY_JSON_OBJECT),
    lists:foreach(fun({K,V}) ->
			  Arg = list_to_binary([?CHANNEL_VAR_PREFIX, whistle_util:to_list(K), "=", whistle_util:to_list(V)]),
			  export(Node, UUID, Arg)
		  end, CallVars),
    {AppName, noop};
get_fs_app(_Node, _UUID, JObj, <<"conference">>=App) ->
    case whistle_api:conference_req_v(JObj) of
	false -> {error, "conference failed to execute as JObj did not validate."};
	true ->
	    ConfName = wh_json:get_value(<<"Conference-ID">>, JObj),
	    Flags = get_conference_flags(JObj),
	    Arg = list_to_binary([ConfName, "@default", Flags]),
	    {App, Arg}
    end;
get_fs_app(_Node, _UUID, _JObj, _App) ->
    logger:format_log(error, "CONTROL(~p): Unknown App ~p:~n~p~n", [self(), _App, _JObj]),
    {error, "Application unknown"}.

%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(send_cmd/4 :: (Node :: atom(), UUID :: binary(), AppName :: binary() | string(), Args :: binary() | string()) -> ok | timeout | {error, string()}).
send_cmd(Node, UUID, AppName, Args) ->
    logger:format_log(info, "CONTROL(~p): SendMsg -> Node: ~p UUID: ~p App: ~p Args: ~p~n", [self(), Node, UUID, whistle_util:to_list(AppName), whistle_util:to_list(Args)]),
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
media_path(MediaName, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, UUID) of
        {error, _} ->
            MediaName;
        {ok, Url} ->
            get_fs_playback(Url)
    end.

-spec(get_fs_playback/1 :: (Url :: binary()) -> binary()).
get_fs_playback(<<"http://", _/binary>>=Url) ->
    <<"shell_stream:///tmp/fetch_remote_audio.sh ", Url/binary>>;
get_fs_playback(Url) ->
    Url.

-spec(stream_over_amqp/3 :: (File :: list(), JObj :: proplist(), Headers :: proplist()) -> no_return()).
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

-spec(stream_over_http/3 :: (File :: list(), Verb :: binary(), JObj :: proplist()) -> no_return()).
stream_over_http(File, Verb, JObj) ->
    Url = whistle_util:to_list(wh_json:get_value(<<"Media-Transfer-Destination">>, JObj)),
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
										  ,{<<"Headers">>, {struct, RespHeaders}}
										  ,{<<"Body">>, whistle_util:to_binary(RespBody)}
										 ]}
								   ,JObj)) of
		{ok, Payload} ->
		    amqp_util:targeted_publish(AppQ, Payload, <<"application/json">>);
		{error, Msg} ->
		    logger:format_log(error, "CONTROL(~p): store_http_resp error: ~p~n", [self(), Msg])
	    end;
	{error, Error} ->
	    logger:format_log(error, "CONTROL(~p): Ibrowse error: ~p~n", [self(), Error]);
	{ibrowse_req_id, ReqId} ->
	    logger:format_log(error, "CONTROL(~p): Ibrowse_req_id: ~p~n", [self(), ReqId])
    end.

-spec(stream_file/1 :: ({undefined | io_device(), string()}) -> {ok, list(), tuple()} | eof).
stream_file({undefined, File}) ->
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

-spec(set_eff_caller_id_name/3 :: (Node :: atom(), UUID :: binary(), Name :: undefined | binary()) -> ok | timeout | {error, string()}).
set_eff_caller_id_name(_Node, _UUID, undefined) ->
    ok;
set_eff_caller_id_name(Node, UUID, Name) ->
    N = list_to_binary(["effective_caller_id_name=", Name]),
    set(Node, UUID, N).

-spec(set_eff_caller_id_number/3 :: (Node :: atom(), UUID :: binary(), Num :: undefined | integer() | list() | binary()) -> ok | timeout | {error, string()}).
set_eff_caller_id_number(_Node, _UUID, undefined) ->
    ok;
set_eff_caller_id_number(Node, UUID, Num) ->
    N = list_to_binary(["effective_caller_id_number=", whistle_util:to_list(Num)]),
    set(Node, UUID, N).

-spec(set_eff_callee_id_name/3 :: (Node :: atom(), UUID :: binary(), Name :: undefined | binary()) -> ok | timeout | {error, string()}).
set_eff_callee_id_name(_Node, _UUID, undefined) ->
    ok;
set_eff_callee_id_name(Node, UUID, Name) ->
    N = list_to_binary(["sip_callee_id_name=", Name]),
    set(Node, UUID, N).

-spec(set_eff_callee_id_number/3 :: (Node :: atom(), UUID :: binary(), Num :: undefined | integer() | list() | binary()) -> ok | timeout | {error, string()}).
set_eff_callee_id_number(_Node, _UUID, undefined) ->
    ok;
set_eff_callee_id_number(Node, UUID, Num) ->
    N = list_to_binary(["sip_callee_id_number=", whistle_util:to_list(Num)]),
    set(Node, UUID, N).

%% -spec(set_bypass_media(Node :: atom(), UUID :: binary(), Method :: undefined | binary()) -> ok | timeout | {error, string()}).
%% set_bypass_media(_Node, _UUID, undefined) ->
%%     ok;
%% set_bypass_media(Node, UUID, <<"true">>) ->
%%     set(Node, UUID, "bypass_media=true");
%% set_bypass_media(Node, UUID, <<"false">>) ->
%%     set(Node, UUID, "bypass_media=false").

%% -spec(set_ignore_early_media(Node :: atom(), UUID :: binary(), Method :: undefined | binary()) -> ok | timeout | {error, string()}).
%% set_ignore_early_media(_Node, _UUID, undefined) ->
%%     ok;
%% set_ignore_early_media(Node, UUID, <<"true">>) ->
%%     set(Node, UUID, "ignore_early_media=true");
%% set_ignore_early_media(Node, UUID, <<"false">>) ->
%%     set(Node, UUID, "ignore_early_media=false").

-spec(set_timeout/3 :: (Node :: atom(), UUID :: binary(), N :: undefined | integer() | list()) -> ok | timeout | {error, string()}).
set_timeout(_Node, _UUID, undefined) ->
    ok;
set_timeout(Node, UUID, N) ->
    Timeout = [ $c,$a,$l,$l,$_,$t,$i,$m,$e,$o,$u,$t,$= | whistle_util:to_list(N)],
    set(Node, UUID, Timeout).

%% -spec(set_progress_timeout/3 :: (Node :: atom(), UUID :: binary(), N :: undefined | integer() | list()) -> ok | timeout | {error, string()}).
%% set_progress_timeout(_Node, _UUID, undefined) ->
%%     ok;
%% set_progress_timeout(Node, UUID, N) ->
%%     ProgressTimeout = [ $p,$r,$o,$g,$r,$e,$s,$s,$_,$t,$i,$m,$e,$o,$u,$t,$= | whistle_util:to_list(N)],
%%     set(Node, UUID, ProgressTimeout).

-spec(set_terminators/3 :: (Node :: atom(), UUID :: binary(), Terminators :: undefined | binary()) -> ok | timeout | {error, string()}).
set_terminators(_Node, _UUID, undefined) ->
    ok;
set_terminators(Node, UUID, <<>>) ->
    set(Node, UUID, "none");
set_terminators(Node, UUID, Ts) ->
    Terms = list_to_binary(["playback_terminators=", Ts]),
    set(Node, UUID, Terms).

-spec(set_ringback/3 :: (Node :: atom(), UUID :: binary(), RingBack :: undefined | binary()) -> ok | timeout | {error, string()}).
set_ringback(_Node, _UUID, undefined) ->
    ok;
set_ringback(Node, UUID, RingBack) ->
    RB = list_to_binary(["ringback=${", RingBack, "}"]),
    set(Node, UUID, RB).

-spec(set_sip_req_headers/3 :: (Node :: atom(), UUID :: binary(), SIPHeaders :: undefined | proplist()) -> ok).
set_sip_req_headers(_Node, _UUID, undefined) ->
    ok;
set_sip_req_headers(Node, UUID, [_]=SIPHeaders) ->
    [ set(Node, UUID, list_to_binary(["sip_h_", K, "=", V])) || {K, V} <- SIPHeaders ],
    ok.

-spec(set_continue_on_fail(Node :: atom(), UUID :: binary(), Method :: undefined | binary()) -> ok | timeout | {error, string()}).
set_continue_on_fail(_Node, _UUID, undefined) ->
    ok;
set_continue_on_fail(Node, UUID, <<"true">>) ->
    set(Node, UUID, "continue_on_fail=true");
set_continue_on_fail(Node, UUID, <<"false">>) ->
    set(Node, UUID, "continue_on_fail=false").

-spec(set/3 :: (Node :: atom(), UUID :: binary(), Arg :: list() | binary()) -> ok | timeout | {error, string()}).
set(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "set", whistle_util:to_list(Arg)).

-spec(export/3 :: (Node :: atom(), UUID :: binary(), Arg :: list() | binary()) -> ok | timeout | {error, string()}).
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
            <<"+flags{", (binary_part(F, {0, byte_size(F)-1}))/binary, "}">>
    end.
