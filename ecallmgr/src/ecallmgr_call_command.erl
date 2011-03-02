%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Execute call commands
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------

-module(ecallmgr_call_command).

-export([exec_cmd/4]).

-import(logger, [log/2, format_log/3]).

-define(LOCAL_MEDIA_PATH, "/tmp/").

-include("whistle_api.hrl").
-include("ecallmgr.hrl").

-type fd() :: tuple().
-type io_device() :: pid() | fd().

-spec(exec_cmd/4 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), AmqpHost :: string()) -> ok | timeout | {error, string()}).
exec_cmd(Node, UUID, JObj, AmqpHost) ->
    DestID = whapps_json:get_value(<<"Call-ID">>, JObj),
    case DestID =:= UUID of
	true ->
	    AppName = whapps_json:get_value(<<"Application-Name">>, JObj),
	    case get_fs_app(Node, UUID, JObj, AmqpHost, AppName) of
		{error, _Msg}=Err -> Err;
		{_, noop} -> format_log(info, "CONTROL(~p): Noop for ~p~n", [self(), AppName]), ok;
		{App, AppData} -> send_cmd(Node, UUID, App, AppData)
	    end;
	false ->
	    format_log(error, "CONTROL(~p): Cmd Not for us(~p) but for ~p~n", [self(), UUID, DestID]),
	    {error, "Command not for this node"}
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec(get_fs_app/5 :: (Node :: atom(), UUID :: binary(), JObj :: json_object(), AmqpHost :: string(), Application :: binary()) -> tuple(binary(), binary() | noop) | tuple(error, string())).
get_fs_app(Node, UUID, JObj, _AmqpHost, <<"play">>) ->
    case whistle_api:play_req_v(JObj) of
	false -> {error, "play failed to execute as JObj did not validate."};
	true ->
	    F = media_path(UUID, whapps_json:get_value(<<"Media-Name">>, JObj)),
	    set_terminators(Node, UUID, whapps_json:get_value(<<"Terminators">>, JObj)),
	    {<<"playback">>, F}
    end;
get_fs_app(_Node, _UUID, _JObj, _AmqpHost, <<"hangup">>=App) ->
    {App, <<>>};
get_fs_app(_Node, UUID, JObj, _AmqpHost, <<"play_and_collect_digits">>) ->
    case whistle_api:play_collect_digits_req_v(JObj) of
	false -> {error, "play_and_collect_digits failed to execute as JObj did not validate."};
	true ->
	    Min = whapps_json:get_value(<<"Minimum-Digits">>, JObj),
	    Max = whapps_json:get_value(<<"Maximum-Digits">>, JObj),
	    Timeout = whapps_json:get_value(<<"Timeout">>, JObj),
	    Terminators = whapps_json:get_value(<<"Terminators">>, JObj),
	    Media = media_path(UUID, whapps_json:get_value(<<"Media-Name">>, JObj)),
	    InvalidMedia = media_path(UUID, whapps_json:get_value(<<"Failed-Media-Name">>, JObj)),
	    Tries = whapps_json:get_value(<<"Media-Tries">>, JObj),
	    Regex = whapps_json:get_value(<<"Digits-Regex">>, JObj),
	    Storage = <<"collected_digits">>,
	    Data = list_to_binary([Min, " ", Max, " ", Tries, " ", Timeout, " ", Terminators, " ",
				   Media, " ", InvalidMedia, " ", Storage, " ", Regex]),
	    {<<"play_and_get_digits">>, Data}
    end;
get_fs_app(Node, UUID, JObj, _AmqpHost, <<"record">>) ->
    case whistle_api:record_req_v(JObj) of
	false -> {error, "record failed to execute as JObj did not validate."};
	true ->
	    Name = whapps_json:get_value(<<"Media-Name">>, JObj),
	    Path = get_media_path(ecallmgr_media_registry:register_name(UUID, Name)),
	    RecArg = binary_to_list(list_to_binary([Path, " "
						    ,whapps_json:get_value(<<"Time-Limit">>, JObj, "20"), " "
						    ,whapps_json:get_value(<<"Silence-Threshold">>, JObj, "200"), " "
						    ,whapps_json:get_value(<<"Silence-Hits">>, JObj, "3")
						   ])),
	    set_terminators(Node, UUID, whapps_json:get_value(<<"Terminators">>, JObj)),
	    {<<"record">>, RecArg}
    end;
get_fs_app(_Node, UUID, JObj, AmqpHost, <<"store">>) ->
    case whistle_api:store_req_v(JObj) of
	false -> {error, "store failed to execute as JObj did not validate."};
	true ->
	    Name = whapps_json:get_value(<<"Media-Name">>, JObj),
	    case ecallmgr_media_registry:is_registered(UUID, Name) of
		{true, GenName} ->
		    File = get_media_path(GenName),
		    case filelib:is_regular(File) andalso whapps_json:get_value(<<"Media-Transfer-Method">>, JObj) of
			<<"stream">> ->
			    %% stream file over AMQP
			    Headers = [{<<"Media-Transfer-Method">>, <<"stream">>}
				       ,{<<"Media-Name">>, Name}
				       ,{<<"Application-Name">>, <<"store">>}
				      ],
			    spawn(fun() -> stream_over_amqp(File, JObj, Headers, AmqpHost) end);
			<<"put">>=Verb ->
			    %% stream file over HTTP PUT
			    spawn(fun() -> stream_over_http(File, Verb, JObj, AmqpHost) end);
			<<"post">>=Verb ->
			    %% stream file over HTTP PUSH
			    spawn(fun() -> stream_over_http(File, Verb, JObj, AmqpHost) end);
			false ->
			    format_log(error, "CONTROL(~p): File ~p has gone missing!~n", [self(), File]);
			_Method ->
			    %% unhandled method
			    format_log(error, "CONTROL(~p): Unhandled stream method ~p~n", [self(), _Method])
		    end;
		false ->
		    format_log(error, "CONTROL(~p): Failed to find ~p for storing~n~p~n", [self(), Name, JObj])
	    end
    end;
get_fs_app(_Node, _UUID, JObj, _AmqpHost, <<"tone">>) ->
    case whistle_api:tones_req_v(JObj) of
	false -> {error, "tones failed to execute as JObj did not validate."};
	true ->
	    Tones = whapps_json:get_value(<<"Tones">>, JObj, []),
	    FSTones = lists:map(fun(Tone) ->
					Vol = case whapps_json:get_value(<<"Volume">>, Tone) of
						  undefined -> [];
						  %% need to map V (0-100) to FS values
						  V -> list_to_binary(["v=", whistle_util:to_list(V), ";"])
					      end,
					Repeat = case whapps_json:get_value(<<"Repeat">>, Tone) of
						     undefined -> [];
						     R -> list_to_binary(["l=", whistle_util:to_list(R), ";"])
						 end,
					Freqs = string:join(lists:map(fun whistle_util:to_list/1, whapps_json:get_value(<<"Frequencies">>, Tone)), ","),
					On = whistle_util:to_list(whapps_json:get_value(<<"Duration-ON">>, Tone)),
					Off = whistle_util:to_list(whapps_json:get_value(<<"Duration-OFF">>, Tone)),
					whistle_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
				end, Tones),
	    Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
	    {<<"playback">>, Arg}
    end;
get_fs_app(_Node, _UUID, _JObj, _AmqpHost, <<"answer">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, _JObj, _AmqpHost, <<"park">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, JObj, _AmqpHost, <<"sleep">>=App) ->
    case whistle_api:sleep_req_v(JObj) of
	false -> {error, "sleep failed to execute as JObj did not validate."};
	true -> {App, whistle_util:to_binary(whapps_json:get_value(<<"Time">>, JObj))}
    end;
get_fs_app(_Node, _UUID, JObj, _AmqpHost, <<"say">>=App) ->
    case whistle_api:say_req_v(JObj) of
	false -> {error, "say failed to execute as JObj did not validate."};
	true ->
	    Lang = whapps_json:get_value(<<"Language">>, JObj),
	    Type = whapps_json:get_value(<<"Type">>, JObj),
	    Method = whapps_json:get_value(<<"Method">>, JObj),
	    Txt = whapps_json:get_value(<<"Say-Text">>, JObj),
	    Arg = list_to_binary([Lang, " ", Type, " ", Method, " ", Txt]),
	    {App, Arg}
    end;
get_fs_app(Node, UUID, JObj, AmqpHost, <<"bridge">>=App) ->
    case whistle_api:bridge_req_v(JObj) of
	false -> {error, "bridge failed to execute as JObj did not validate."};
	true ->
	    set_timeout(Node, UUID, whapps_json:get_value(<<"Timeout">>, JObj)),
	    set_continue_on_fail(Node, UUID, whapps_json:get_value(<<"Continue-On-Fail">>, JObj)),
	    set_eff_call_id_name(Node, UUID, whapps_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)),
	    set_eff_call_id_number(Node, UUID, whapps_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)),
	    set_ringback(Node, UUID, whapps_json:get_value(<<"Ringback">>, JObj)),

	    DialSeparator = case whapps_json:get_value(<<"Dial-Endpoint-Method">>, JObj) of
				<<"simultaneous">> -> ",";
				<<"single">> -> "|";
				_ -> "|"
			    end,

	    DialStrings = [get_bridge_endpoint(EP, AmqpHost) || EP <- whapps_json:get_value(<<"Endpoints">>, JObj, [])],

	    BridgeCmd = string:join(DialStrings, DialSeparator),
	    {App, BridgeCmd}
    end;
get_fs_app(Node, UUID, JObj, AmqpHost, <<"tone_detect">>=App) ->
    case whistle_api:tone_detect_req_v(JObj) of
	false -> {error, "tone detect failed to execute as JObj did not validate"};
	true ->
	    Key = whapps_json:get_value(<<"Tone-Detect-Name">>, JObj),
	    Freqs = lists:map(fun whistle_util:to_list/1, whapps_json:get_value(<<"Frequencies">>, JObj)),
	    FreqsStr = string:join(Freqs, ","),
	    Flags = case whapps_json:get_value(<<"Sniff-Direction">>, JObj, <<"read">>) of
			<<"read">> -> <<"r">>;
			<<"write">> -> <<"w">>
		    end,
	    Timeout = whapps_json:get_value(<<"Timeout">>, JObj, <<"+1000">>),
	    HitsNeeded = whapps_json:get_value(<<"Hits-Needed">>, JObj, <<"1">>),

	    SuccessJObj = case whapps_json:get_value(<<"On-Success">>, JObj, []) of
			      [] -> [{<<"Application-Name">>, <<"park">>} | whistle_api:extract_defaults(JObj)]; % default to parking the call
			      AppJObj -> {struct, AppJObj ++ whistle_api:extract_defaults(JObj)}
			  end,
	    {SuccessApp, SuccessAppData} = case get_fs_app(Node, UUID, SuccessJObj, AmqpHost, whapps_json:get_value(<<"Application-Name">>, SuccessJObj)) of
					       {error, _Str} -> {<<"park">>, <<>>}; % default to park if passed app isn't right
					       {_, _}=Success -> Success
					   end,
	    Data = list_to_binary([Key, " ", FreqsStr, " ", Flags, " ", Timeout, " ", SuccessApp, " ", SuccessAppData, " ", HitsNeeded]),
	    {App, Data}
    end;
get_fs_app(Node, UUID, JObj, _AmqpHost, <<"set">>=AppName) ->
    {struct, Custom} = whapps_json:get_value(<<"Custom-Channel-Vars">>, JObj, {struct, []}),
    lists:foreach(fun({K,V}) ->
			  Arg = list_to_binary([?CHANNEL_VAR_PREFIX, whistle_util:to_list(K), "=", whistle_util:to_list(V)]),
			  set(Node, UUID, Arg)
		  end, Custom),
    {AppName, noop};
get_fs_app(_Node, _UUID, _JObj, _AmqpHost, _App) ->
    format_log(error, "CONTROL(~p): Unknown App ~p:~n~p~n", [self(), _App, _JObj]),
    {error, "Application unknown"}.

%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(send_cmd/4 :: (Node :: atom(), UUID :: binary(), AppName :: binary() | string(), Args :: binary() | string()) -> ok | timeout | {error, string()}).
send_cmd(Node, UUID, AppName, Args) ->
    format_log(info, "CONTROL(~p): SendMsg -> Node: ~p UUID: ~p App: ~p Args: ~p~n", [self(), Node, UUID, whistle_util:to_list(AppName), whistle_util:to_list(Args)]),
    freeswitch:sendmsg(Node, UUID, [
				    {"call-command", "execute"}
				    ,{"execute-app-name", whistle_util:to_list(AppName)}
				    ,{"execute-app-arg", whistle_util:to_list(Args)}
				   ]).

%% take an endpoint (/sofia/foo/bar), and optionally a caller id name and number
%% and create the dial string ([origination_caller_id_name=Name,origination_caller_id_number=Num]Endpoint)
-spec(get_bridge_endpoint/2 :: (JObj :: json_object(), AmqpHost :: string()) -> string()).
get_bridge_endpoint(JObj, AmqpHost) ->
    case ecallmgr_fs_route:build_route(AmqpHost, JObj, whapps_json:get_value(<<"Invite-Format">>, JObj)) of
	{error, Code} -> whistle_util:to_list(list_to_binary(["error/", Code]));
	EndPoint ->
	    CVs = ecallmgr_fs_route:get_leg_vars(JObj),
	    whistle_util:to_list(list_to_binary([CVs, "sofia/sipinterface_1/", EndPoint]))
    end.

-spec(media_path/2 :: (UUID :: binary(), Name :: binary()) -> list()).
media_path(UUID, Name) ->
    case ecallmgr_media_registry:is_registered(UUID, Name) of
	{true, GenName} -> get_media_path(GenName);
        false -> whistle_util:to_binary(Name)
%%        false when is_binary(Name) -> binary_to_list(Name);
%%        false when is_list(Name) -> Name
    end.

-spec(get_media_path/1 :: (Name :: binary() | string()) -> list()).
get_media_path(Name) ->
    binary_to_list(list_to_binary([?LOCAL_MEDIA_PATH, Name, ".wav"])).

-spec(stream_over_amqp/4 :: (File :: list(), JObj :: proplist(), Headers :: proplist(), AmqpHost :: string()) -> no_return()).
stream_over_amqp(File, JObj, Headers, AmqpHost) ->
    DestQ = case whapps_json:get_value(<<"Media-Transfer-Destination">>, JObj) of
		undefined ->
		    whapps_json:get_value(<<"Server-ID">>, JObj);
		<<"">> ->
		    whapps_json:get_value(<<"Server-ID">>, JObj);
		Q ->
		    Q
	    end,
    stream_over_amqp(DestQ, fun stream_file/1, {undefined, File}, Headers, 1, AmqpHost).

%% get a chunk of the file and send it in an AMQP message to the DestQ
-spec(stream_over_amqp/6 :: (DestQ :: binary(), F :: fun(), State :: tuple(), Headers :: proplist(), Seq :: pos_integer(), AmqpHost :: string()) -> no_return()).
stream_over_amqp(DestQ, F, State, Headers, Seq, AmqpHost) ->
    case F(State) of
	{ok, Data, State1} ->
	    %% send msg
	    Msg = [{<<"Media-Content">>, Data}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    {ok, JSON} = whistle_api:store_amqp_resp(Msg),
	    amqp_util:targeted_publish(AmqpHost, DestQ, JSON, <<"application/json">>),
	    stream_over_amqp(DestQ, F, State1, Headers, Seq+1, AmqpHost);
	eof ->
	    Msg = [{<<"Media-Content">>, <<"eof">>}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    {ok, JSON} = whistle_api:store_amqp_resp(Msg),
	    amqp_util:targeted_publish(AmqpHost, DestQ, JSON, <<"application/json">>),
	    eof
    end.

-spec(stream_over_http/4 :: (File :: list(), Verb :: binary(), JObj :: proplist(), AmqpHost :: string()) -> no_return()).
stream_over_http(File, Verb, JObj, AmqpHost) ->
    Url = whistle_util:to_list(whapps_json:get_value(<<"Media-Transfer-Destination">>, JObj)),
    {struct, AddHeaders} = whapps_json:get_value(<<"Additional-Headers">>, JObj, {struct, []}),
    Headers = [{"Content-Length", filelib:file_size(File)}
	       | lists:map(fun({K, V}) -> {whistle_util:to_list(K), V} end, AddHeaders)],
    Method = whistle_util:to_atom(Verb, true),
    Body = {fun stream_file/1, {undefined, File}},
    AppQ = whapps_json:get_value(<<"Server-ID">>, JObj),
    case ibrowse:send_req(Url, Headers, Method, Body) of
	{ok, StatusCode, RespHeaders, RespBody} ->
	    case whistle_api:store_http_resp(whapps_json:set_value(<<"Media-Transfer-Results">>
								       ,{struct, [{<<"Status-Code">>, StatusCode}
										  ,{<<"Headers">>, {struct, RespHeaders}}
										  ,{<<"Body">>, whistle_util:to_binary(RespBody)}
										 ]}
								   ,JObj)) of
		{ok, JSON} ->
		    format_log(info, "CONTROL(~p): Ibrowse recv back ~p~n", [self(), JSON]),
		    amqp_util:targeted_publish(AmqpHost, AppQ, JSON, <<"application/json">>);
		{error, Msg} ->
		    format_log(error, "CONTROL(~p): store_http_resp error: ~p~n", [self(), Msg])
	    end;
	{error, Error} ->
	    format_log(error, "CONTROL(~p): Ibrowse error: ~p~n", [self(), Error]);
	{ibrowse_req_id, ReqId} ->
	    format_log(error, "CONTROL(~p): Ibrowse_req_id: ~p~n", [self(), ReqId])
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
	    file:close(Iod),
	    eof
    end.

-spec(set_eff_call_id_name/3 :: (Node :: atom(), UUID :: binary(), Name :: undefined | binary()) -> ok | timeout | {error, string()}).
set_eff_call_id_name(_Node, _UUID, undefined) ->
    ok;
set_eff_call_id_name(Node, UUID, Name) ->
    N = list_to_binary(["effective_caller_id_name=", Name]),
    set(Node, UUID, N).

-spec(set_eff_call_id_number/3 :: (Node :: atom(), UUID :: binary(), Num :: undefined | integer() | list() | binary()) -> ok | timeout | {error, string()}).
set_eff_call_id_number(_Node, _UUID, undefined) ->
    ok;
set_eff_call_id_number(Node, UUID, Num) ->
    N = list_to_binary(["effective_caller_id_number=", whistle_util:to_list(Num)]),
    set(Node, UUID, N).

-spec(set_bypass_media(Node :: atom(), UUID :: binary(), Method :: undefined | binary()) -> ok | timeout | {error, string()}).
set_bypass_media(_Node, _UUID, undefined) ->
    ok;
set_bypass_media(Node, UUID, <<"true">>) ->
    set(Node, UUID, "bypass_media=true");
set_bypass_media(Node, UUID, <<"false">>) ->
    set(Node, UUID, "bypass_media=false").

-spec(set_ignore_early_media(Node :: atom(), UUID :: binary(), Method :: undefined | binary()) -> ok | timeout | {error, string()}).
set_ignore_early_media(_Node, _UUID, undefined) ->
    ok;
set_ignore_early_media(Node, UUID, <<"true">>) ->
    set(Node, UUID, "ignore_early_media=true");
set_ignore_early_media(Node, UUID, <<"false">>) ->
    set(Node, UUID, "ignore_early_media=false").

-spec(set_timeout/3 :: (Node :: atom(), UUID :: binary(), N :: undefined | integer() | list()) -> ok | timeout | {error, string()}).
set_timeout(_Node, _UUID, undefined) ->
    ok;
set_timeout(Node, UUID, N) ->
    Timeout = [ $c,$a,$l,$l,$_,$t,$i,$m,$e,$o,$u,$t,$= | whistle_util:to_list(N)],
    set(Node, UUID, Timeout).

-spec(set_progress_timeout/3 :: (Node :: atom(), UUID :: binary(), N :: undefined | integer() | list()) -> ok | timeout | {error, string()}).
set_progress_timeout(_Node, _UUID, undefined) ->
    ok;
set_progress_timeout(Node, UUID, N) ->
    ProgressTimeout = [ $p,$r,$o,$g,$r,$e,$s,$s,$_,$t,$i,$m,$e,$o,$u,$t,$= | whistle_util:to_list(N)],
    set(Node, UUID, ProgressTimeout).

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
