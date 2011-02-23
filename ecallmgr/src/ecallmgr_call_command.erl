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
-import(props, [get_value/2, get_value/3]).

-define(LOCAL_MEDIA_PATH, "/tmp/").

-include("whistle_api.hrl").

-type fd() :: tuple().
-type io_device() :: pid() | fd().

-spec(exec_cmd/4 :: (Node :: list(), UUID :: binary(), Prop :: proplist(), AmqpHost :: string()) -> ok | timeout | {error, string()}).
exec_cmd(Node, UUID, Prop, AmqpHost) ->
    DestID = get_value(<<"Call-ID">>, Prop),
    case DestID =:= UUID of
	true ->
	    AppName = get_value(<<"Application-Name">>, Prop),
	    case get_fs_app(Node, UUID, Prop, AmqpHost, AppName) of
		{error, _Msg}=Err -> Err;
		{_, noop} -> format_log(info, "CONTROL(~p): Noop for ~p~n", [self(), AppName]), ok;
		{App, AppData} -> send_cmd(Node, UUID, App, AppData)
	    end;
	false ->
	    format_log(error, "CONTROL(~p): Cmd Not for us(~p) but for ~p~n", [self(), UUID, DestID]),
	    {error, "Command not for this node"}
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec(get_fs_app/5 :: (Node :: atom(), UUID :: binary(), Prop :: proplist(), AmqpHost :: string(), Application :: binary()) -> tuple(binary(), binary() | noop) | tuple(error, string())).
get_fs_app(Node, UUID, Prop, _AmqpHost, <<"play">>) ->
    case whistle_api:play_req_v(Prop) of
	false -> {error, "play failed to execute as Prop did not validate."};
	true ->
	    F = media_path(UUID, get_value(<<"Media-Name">>, Prop)),
	    set_terminators(Node, UUID, get_value(<<"Terminators">>, Prop)),
	    {<<"playback">>, F}
    end;
get_fs_app(_Node, _UUID, _Prop, _AmqpHost, <<"hangup">>=App) ->
    {App, <<>>};
get_fs_app(_Node, UUID, Prop, _AmqpHost, <<"play_and_collect_digits">>) ->
    case whistle_api:play_collect_digits_req_v(Prop) of
	false -> {error, "play_and_collect_digits failed to execute as Prop did not validate."};
	true ->
	    Min = get_value(<<"Minimum-Digits">>, Prop),
	    Max = get_value(<<"Maximum-Digits">>, Prop),
	    Timeout = get_value(<<"Timeout">>, Prop),
	    Terminators = get_value(<<"Terminators">>, Prop),
	    Media = media_path(UUID, get_value(<<"Media-Name">>, Prop)),
	    InvalidMedia = media_path(UUID, get_value(<<"Failed-Media-Name">>, Prop)),
	    Tries = get_value(<<"Media-Tries">>, Prop),
	    Regex = get_value(<<"Digits-Regex">>, Prop),
	    Storage = get_value(<<"Storage-Name">>, Prop, <<"ignore">>),
	    Data = list_to_binary([Min, " ", Max, " ", Tries, " ", Timeout, " ", Terminators, " ",
				   Media, " ", InvalidMedia, " ", Storage, " ", Regex]),
	    {<<"play_and_get_digits">>, Data}
    end;
get_fs_app(Node, UUID, Prop, _AmqpHost, <<"record">>) ->
    case whistle_api:record_req_v(Prop) of
	false -> {error, "record failed to execute as Prop did not validate."};
	true ->
	    Name = get_value(<<"Media-Name">>, Prop),
	    Path = get_media_path(ecallmgr_media_registry:register_name(UUID, Name)),
	    RecArg = binary_to_list(list_to_binary([Path, " "
						    ,get_value(<<"Time-Limit">>, Prop, "20"), " "
						    ,get_value(<<"Silence-Threshold">>, Prop, "200"), " "
						    ,get_value(<<"Silence-Hits">>, Prop, "3")
						   ])),
	    set_terminators(Node, UUID, get_value(<<"Terminators">>, Prop)),
	    {<<"record">>, RecArg}
    end;
get_fs_app(_Node, UUID, Prop, AmqpHost, <<"store">>) ->
    case whistle_api:store_req_v(Prop) of
	false -> {error, "store failed to execute as Prop did not validate."};
	true ->
	    Name = get_value(<<"Media-Name">>, Prop),
	    case ecallmgr_media_registry:is_registered(UUID, Name) of
		{true, GenName} ->
		    File = get_media_path(GenName),
		    case filelib:is_regular(File) andalso get_value(<<"Media-Transfer-Method">>, Prop) of
			<<"stream">> ->
			    %% stream file over AMQP
			    Headers = [{<<"Media-Transfer-Method">>, <<"stream">>}
				       ,{<<"Media-Name">>, Name}
				       ,{<<"Application-Name">>, <<"store">>}
				      ],
			    spawn(fun() -> stream_over_amqp(File, Prop, Headers, AmqpHost) end);
			<<"put">>=Verb ->
			    %% stream file over HTTP PUT
			    spawn(fun() -> stream_over_http(File, Verb, Prop, AmqpHost) end);
			<<"post">>=Verb ->
			    %% stream file over HTTP PUSH
			    spawn(fun() -> stream_over_http(File, Verb, Prop, AmqpHost) end);
			false ->
			    format_log(error, "CONTROL(~p): File ~p has gone missing!~n", [self(), File]);
			_Method ->
			    %% unhandled method
			    format_log(error, "CONTROL(~p): Unhandled stream method ~p~n", [self(), _Method])
		    end;
		false ->
		    format_log(error, "CONTROL(~p): Failed to find ~p for storing~n~p~n", [self(), Name, Prop])
	    end
    end;
get_fs_app(_Node, _UUID, Prop, _AmqpHost, <<"tone">>) ->
    case whistle_api:tones_req_v(Prop) of
	false -> {error, "tones failed to execute as Prop did not validate."};
	true ->
	    Tones = get_value(<<"Tones">>, Prop, []),
	    FSTones = lists:map(fun({struct, Tone}) ->
					Vol = case get_value(<<"Volume">>, Tone) of
						  undefined -> [];
						  %% need to map V (0-100) to FS values
						  V -> list_to_binary(["v=", whistle_util:to_list(V), ";"])
					      end,
					Repeat = case get_value(<<"Repeat">>, Tone) of
						     undefined -> [];
						     R -> list_to_binary(["l=", whistle_util:to_list(R), ";"])
						 end,
					Freqs = string:join(lists:map(fun whistle_util:to_list/1, get_value(<<"Frequencies">>, Tone)), ","),
					On = whistle_util:to_list(get_value(<<"Duration-ON">>, Tone)),
					Off = whistle_util:to_list(get_value(<<"Duration-OFF">>, Tone)),
					whistle_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
				end, Tones),
	    Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
	    {<<"playback">>, Arg}
    end;
get_fs_app(_Node, _UUID, _Prop, _AmqpHost, <<"answer">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, _Prop, _AmqpHost, <<"park">>=App) ->
    {App, <<>>};
get_fs_app(_Node, _UUID, Prop, _AmqpHost, <<"sleep">>=App) ->
    case whistle_api:sleep_req_v(Prop) of
	false -> {error, "sleep failed to execute as Prop did not validate."};
	true -> {App, whistle_util:to_binary(get_value(<<"Time">>, Prop))}
    end;
get_fs_app(_Node, _UUID, Prop, _AmqpHost, <<"say">>=App) ->
    case whistle_api:say_req_v(Prop) of
	false -> {error, "say failed to execute as Prop did not validate."};
	true ->
	    Lang = get_value(<<"Language">>, Prop),
	    Type = get_value(<<"Type">>, Prop),
	    Method = get_value(<<"Method">>, Prop),
	    Txt = get_value(<<"Say-Text">>, Prop),
	    Arg = list_to_binary([Lang, " ", Type, " ", Method, " ", Txt]),
	    {App, Arg}
    end;
get_fs_app(Node, UUID, Prop, AmqpHost, <<"bridge">>=App) ->
    case whistle_api:bridge_req_v(Prop) of
	false -> {error, "bridge failed to execute as Prop did not validate."};
	true ->
	    set_timeout(Node, UUID, get_value(<<"Timeout">>, Prop)),
	    set_bypass_media(Node, UUID, get_value(<<"Bypass-Media">>, Prop)),
	    set_eff_call_id_name(Node, UUID, get_value(<<"Outgoing-Caller-ID-Name">>, Prop)),
	    set_eff_call_id_number(Node, UUID, get_value(<<"Outgoing-Caller-ID-Number">>, Prop)),
	    set_ringback(Node, UUID, get_value(<<"Ringback">>, Prop)),
	    DialSeparator = case get_value(<<"Dial-Endpoint-Method">>, Prop) of
				<<"simultaneous">> -> ",";
				<<"single">> -> "|";
				_ -> "|"
			    end,

	    DialStrings = ["sofia/sipinterface_1/" | [ get_bridge_endpoint(EP, AmqpHost) || EP <- get_value(<<"Endpoints">>, Prop, [])]],

	    BridgeCmd = string:join(DialStrings, DialSeparator),
	    {App, BridgeCmd}
    end;
get_fs_app(Node, UUID, Prop, AmqpHost, <<"tone_detect">>=App) ->
    case whistle_api:tone_detect_req_v(Prop) of
	false -> {error, "tone detect failed to execute as Prop did not validate"};
	true ->
	    Key = get_value(<<"Tone-Detect-Name">>, Prop),
	    Freqs = lists:map(fun whistle_util:to_list/1, get_value(<<"Frequencies">>, Prop)),
	    FreqsStr = string:join(Freqs, ","),
	    Flags = case get_value(<<"Sniff-Direction">>, Prop, <<"read">>) of
			<<"read">> -> <<"r">>;
			<<"write">> -> <<"w">>
		    end,
	    Timeout = get_value(<<"Timeout">>, Prop, <<"+1000">>),
	    HitsNeeded = get_value(<<"Hits-Needed">>, Prop, <<"1">>),

	    SuccessProp = case get_value(<<"On-Success">>, Prop, []) of
			      [] -> [{<<"Application-Name">>, <<"park">>} | whistle_api:extract_defaults(Prop)]; % default to parking the call
			      AppProp -> AppProp ++ whistle_api:extract_defaults(Prop)
			  end,
	    {SuccessApp, SuccessAppData} = case get_fs_app(Node, UUID, SuccessProp, AmqpHost, get_value(<<"Application-Name">>, SuccessProp)) of
					       {error, _Str} -> {<<"park">>, <<>>}; % default to park if passed app isn't right
					       {_, _}=Success -> Success
					   end,
	    Data = list_to_binary([Key, " ", FreqsStr, " ", Flags, " ", Timeout, " ", SuccessApp, " ", SuccessAppData, " ", HitsNeeded]),
	    {App, Data}
    end;
get_fs_app(Node, UUID, Prop, _AmqpHost, <<"set">>=AppName) ->
    {struct, Custom} = get_value(<<"Custom-Channel-Vars">>, Prop, {struct, []}),
    lists:foreach(fun({K,V}) ->
			  Arg = list_to_binary([?CHANNEL_VAR_PREFIX, whistle_util:to_list(K), "=", whistle_util:to_list(V)]),
			  set(Node, UUID, Arg)
		  end, Custom),
    {AppName, noop};
get_fs_app(_Node, _UUID, _Prop, _AmqpHost, _App) ->
    format_log(error, "CONTROL(~p): Unknown App ~p:~n~p~n", [self(), _App, _Prop]),
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
-spec(get_bridge_endpoint/2 :: ({struct, EndProp :: proplist()}, AmqpHost :: string()) -> string()).
get_bridge_endpoint({struct, EndProp}, AmqpHost) ->
    case ecallmgr_fs_route:build_route(AmqpHost, EndProp, get_value(<<"Invite-Format">>, EndProp)) of
	{error, Code} -> whistle_util:to_list(list_to_binary(["error/", Code]));
	EndPoint ->
	    CVs = ecallmgr_fs_route:get_channel_vars(EndProp),
	    whistle_util:to_list(list_to_binary([CVs, EndPoint]))
    end.

-spec(media_path/2 :: (UUID :: binary(), Name :: binary()) -> list()).
media_path(UUID, Name) ->
    case ecallmgr_media_registry:is_registered(UUID, Name) of
	{true, GenName} -> get_media_path(GenName);
	false -> binary_to_list(Name)
    end.

-spec(get_media_path/1 :: (Name :: binary() | string()) -> list()).
get_media_path(Name) ->
    binary_to_list(list_to_binary([?LOCAL_MEDIA_PATH, Name, ".wav"])).

-spec(stream_over_amqp/4 :: (File :: list(), Prop :: proplist(), Headers :: proplist(), AmqpHost :: string()) -> no_return()).
stream_over_amqp(File, Prop, Headers, AmqpHost) ->
    DestQ = case get_value(<<"Media-Transfer-Destination">>, Prop) of
		undefined ->
		    get_value(<<"Server-ID">>, Prop);
		<<"">> ->
		    get_value(<<"Server-ID">>, Prop);
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

-spec(stream_over_http/4 :: (File :: list(), Verb :: binary(), Prop :: proplist(), AmqpHost :: string()) -> no_return()).
stream_over_http(File, Verb, Prop, AmqpHost) ->
    Url = binary_to_list(get_value(<<"Media-Transfer-Destination">>, Prop)),
    {struct, AddHeaders} = get_value(<<"Additional-Headers">>, Prop, {struct, []}),
    Headers = [{"Content-Length", filelib:file_size(File)}
	       | lists:map(fun({K, V}) -> {binary_to_list(K), V} end, AddHeaders)],
    Method = list_to_atom(binary_to_list(Verb)),
    Body = {fun stream_file/1, {undefined, File}},
    AppQ = get_value(<<"Server-ID">>, Prop),
    case ibrowse:send_req(Url, Headers, Method, Body) of
	{ok, StatusCode, RespHeaders, RespBody} ->
	    case whistle_api:store_http_resp([{<<"Media-Transfer-Results">>
						   ,{struct, [{<<"Status-Code">>, StatusCode}
							      ,{<<"Headers">>, {struct, RespHeaders}}
							      ,{<<"Body">>, list_to_binary(RespBody)}
							     ]}
					      }
					      | Prop]) of
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

-spec(set_timeout/3 :: (Node :: atom(), UUID :: binary(), N :: undefined | integer() | list()) -> ok | timeout | {error, string()}).
set_timeout(_Node, _UUID, undefined) ->
    ok;
set_timeout(Node, UUID, N) ->
    Timeout = [ $c,$a,$l,$l,$_,$t,$i,$m,$e,$o,$u,$t,$= | whistle_util:to_list(N)],
    set(Node, UUID, Timeout).

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

-spec(set/3 :: (Node :: atom(), UUID :: binary(), Arg :: list() | binary()) -> ok | timeout | {error, string()}).
set(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "set", whistle_util:to_list(Arg)).
