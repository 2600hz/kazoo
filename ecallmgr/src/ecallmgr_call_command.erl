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

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-type proplist() :: list(tuple(binary(), binary())). % just want to deal with binary K/V pairs
-type fd() :: tuple().
-type io_device() :: pid() | fd().

-spec(exec_cmd/3 :: (Node :: list(), UUID :: binary(), Prop :: proplist()) -> no_return()).
exec_cmd(Node, UUID, Prop) ->
    case get_value(<<"Call-ID">>, Prop) =:= UUID of
	true -> exec_cmd(Node, UUID, Prop, get_value(<<"Application-Name">>, Prop));
	false -> format_log(error, "CONTROL(~p): Cmd Not for us:~n~p~n", [self(), Prop])
    end.

-spec(exec_cmd/4 :: (Node :: list(), UUID :: binary(), Prop :: proplist(), Application :: binary()) -> no_return()).
exec_cmd(Node, UUID, Prop, <<"play">>) ->
    F = media_path(UUID, get_value(<<"Media-Name">>, Prop)),
    set_terminators(Node, UUID, get_value(<<"Terminators">>, Prop)),
    send_cmd(Node, UUID, "playback", F);
exec_cmd(Node, UUID, _Prop, <<"hangup">>) ->
    send_cmd(Node, UUID, "hangup", "");
exec_cmd(Node, UUID, Prop, <<"play_and_collect_digits">>) ->
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
    send_cmd(Node, UUID, "play_and_get_digits", Data);
exec_cmd(Node, UUID, Prop, <<"record">>) ->
    Name = get_value(<<"Media-Name">>, Prop),
    Path = get_media_path(ecallmgr_media_registry:register_name(UUID, Name)),
    RecArg = binary_to_list(list_to_binary([Path, " "
					    ,get_value(<<"Time-Limit">>, Prop, "20"), " "
					    ,get_value(<<"Silence-Threshold">>, Prop, "200"), " "
					    ,get_value(<<"Silence-Hits">>, Prop, "3")
					   ])),
    set_terminators(Node, UUID, get_value(<<"Terminators">>, Prop)),
    send_cmd(Node, UUID, "record", RecArg);
exec_cmd(_Node, UUID, Prop, <<"store">>) ->
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
		    spawn(fun() -> stream_over_amqp(File, Prop, Headers) end);
		<<"put">>=Verb ->
		    %% stream file over HTTP PUT
		    spawn(fun() -> stream_over_http(File, Verb, Prop) end);
		<<"post">>=Verb ->
		    %% stream file over HTTP PUSH
		    spawn(fun() -> stream_over_http(File, Verb, Prop) end);
		false ->
		    format_log(error, "CONTROL(~p): File ~p has gone missing!~n", [self(), File]);
		_Method ->
		    %% unhandled method
		    format_log(error, "CONTROL(~p): Unhandled stream method ~p~n", [self(), _Method])
	    end;
	false ->
	    format_log(error, "CONTROL(~p): Failed to find ~p for storing~n~p~n", [self(), Name, Prop])
    end;
exec_cmd(Node, UUID, Prop, <<"tone">>) ->
    Tones = get_value(<<"Tones">>, Prop, {struct, []}),
    FSTones = lists:map(fun({struct, Tone}) ->
				Vol = case get_value(<<"Volume">>, Tone) of
					  undefined -> [];
					  %% need to map V (0-100) to FS values
					  V -> list_to_binary(["v=", integer_to_list(V), ";"])
				      end,
				Repeat = case get_value(<<"Repeat">>, Tone) of
					     undefined -> [];
					     R -> list_to_binary(["l=", integer_to_list(R), ";"])
					 end,
				Freqs = string:join(lists:map(fun erlang:integer_to_list/1, get_value(<<"Frequencies">>, Tone)), ","),
				On = integer_to_list(get_value(<<"Duration-ON">>, Tone)),
				Off = integer_to_list(get_value(<<"Duration-OFF">>, Tone)),
				binary_to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
			end, Tones),
    Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
    send_cmd(Node, UUID, "playback", Arg);
exec_cmd(Node, UUID, _Prop, <<"answer">>) ->
    send_cmd(Node, UUID, "answer", "");
exec_cmd(Node, UUID, _Prop, <<"park">>) ->
    send_cmd(Node, UUID, "park", "");
exec_cmd(Node, UUID, Prop, <<"sleep">>) ->
    send_cmd(Node, UUID, "sleep", integer_to_list(<<"Amount">>, Prop));
exec_cmd(Node, UUID, Prop, <<"say">>) ->
    Lang = get_value(<<"Language">>, Prop),
    Type = get_value(<<"Type">>, Prop),
    Method = get_value(<<"Method">>, Prop),
    Txt = get_value(<<"Say-Text">>, Prop),
    Arg = list_to_binary([Lang, " ", Type, " ", Method, " ", Txt]),
    send_cmd(Node, UUID, "say", Arg);
exec_cmd(Node, UUID, Prop, <<"bridge">>) ->
    set_timeout(Node, UUID, get_value(<<"Timeout">>, Prop)),
    set_bypass_media(Node, UUID, get_value(<<"Bypass-Media">>, Prop)),
    set_eff_call_id_name(Node, UUID, get_value(<<"Outgoing-Caller-ID-Name">>, Prop)),
    set_eff_call_id_number(Node, UUID, get_value(<<"Outgoing-Caller-ID-Number">>, Prop)),
    set_ringback(Node, UUID, get_value(<<"Ringback">>, Prop)),
    DialSeparator = case get_value(<<"Dial-Endpoint-Method">>, Prop) of
			<<"single">> -> "|";
			<<"simultaneous">> -> ","
		    end,
    DialStrings = lists:map(fun get_bridge_endpoint/1, get_value(<<"Endpoints">>, Prop)),
    BridgeCmd = string:join(DialStrings, DialSeparator),
    send_cmd(Node, UUID, "bridge", BridgeCmd);
exec_cmd(_Node, _UUID, _Prop, _App) ->
    format_log(error, "CONTROL(~p): Unknown App ~p:~n~p~n", [self(), _App, _Prop]).

%%%===================================================================
%%% Internal helper functions
%%%===================================================================
%% send the SendMsg proplist to the freeswitch node
-spec(send_cmd/4 :: (Node :: binary(), UUID :: binary(), AppName :: string(), Args :: string()) -> no_return()).
send_cmd(Node, UUID, AppName, Args) when is_binary(Args) ->
    send_cmd(Node, UUID, AppName, binary_to_list(Args));
send_cmd(Node, UUID, AppName, Args) ->
    format_log(info, "CONTROL(~p): SendMsg: App: ~p Args: ~p~n", [self(), AppName, Args]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
				    ,{"execute-app-name", AppName}
				    ,{"execute-app-arg", Args}
				   ]).

%% take an endpoint (/sofia/foo/bar), and optionally a caller id name and number
%% and create the dial string ([origination_caller_id_name=Name,origination_caller_id_number=Num]Endpoint)
-spec(get_bridge_endpoint/1 :: ({struct, EndProp :: proplist()}) -> string()).
get_bridge_endpoint({struct, EndProp}) ->
    CIDName = get_value(<<"Caller-ID-Name">>, EndProp),
    CIDNumber = get_value(<<"Caller-ID-Number">>, EndProp),
    EndPoint = get_value(<<"Endpoint">>, EndProp),
    EP = case {CIDName, CIDNumber} of
	     {undefined, undefined} -> EndPoint;
	     {undefined, Num} -> list_to_binary(["[origination_caller_id_number=", Num, "]", EndPoint]);
	     {Name, undefined} -> list_to_binary(["[origination_caller_id_name=", Name, "]", EndPoint]);
	     {Name, Num} ->
		 list_to_binary(["[origination_caller_id_name=", Name
				 , ",origination_caller_id_number=", Num, "]"
				 ,EndPoint
				])
	 end,
    binary_to_list(EP).

-spec(media_path/2 :: (UUID :: binary(), Name :: binary()) -> list()).
media_path(UUID, Name) ->
    case ecallmgr_media_registry:is_registered(UUID, Name) of
	{true, GenName} -> get_media_path(GenName);
	false -> binary_to_list(Name)
    end.

-spec(get_media_path/1 :: (Name :: binary()) -> string()).
get_media_path(Name) ->
    binary_to_list(list_to_binary(["/tmp/", Name, ".wav"])).

-spec(stream_over_amqp/3 :: (File :: list(), Prop :: proplist(), Headers :: proplist()) -> no_return()).
stream_over_amqp(File, Prop, Headers) ->
    DestQ = case get_value(<<"Media-Transfer-Destination">>, Prop) of
		undefined ->
		    get_value(<<"Server-ID">>, Prop);
		<<"">> ->
		    get_value(<<"Server-ID">>, Prop);
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
	    ecallmgr_amqp:publish(JSON, targeted, DestQ),
	    stream_over_amqp(DestQ, F, State1, Headers, Seq+1);
	eof ->
	    Msg = [{<<"Media-Content">>, <<"eof">>}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    {ok, JSON} = whistle_api:store_amqp_resp(Msg),
	    ecallmgr_amqp:publish(JSON, targeted, DestQ),
	    eof
    end.

-spec(stream_over_http/3 :: (File :: list(), Verb :: binary(), Prop :: proplist()) -> no_return()).
stream_over_http(File, Verb, Prop) ->
    Url = binary_to_list(get_value(<<"Media-Transfer-Destination">>, Prop)),
    {struct, AddHeaders} = get_value(<<"Additional-Headers">>, Prop, {struct, []}),
    Headers = [{"Content-Length", filelib:file_size(File)}
	       | lists:map(fun({K, V}) -> {binary_to_list(K), V} end, AddHeaders)],
    Method = list_to_atom(binary_to_list(Verb)),
    Body = {fun stream_file/1, {undefined, File}},
    AppQ = get_value(<<"Server-ID">>, Prop),
    case ibrowse:send_req(Url, Headers, Method, Body) of
	{ok, StatusCode, Headers, Body} ->
	    case whistle_api:store_http_resp([{<<"Media-Transfer-Results">>
						   ,{struct, [{<<"Status-Code">>, StatusCode}
							      ,{<<"Headers">>, {struct, Headers}}
							      ,{<<"Body">>, list_to_binary(Body)}
							     ]}
					      }
					      | Prop]) of
		{ok, JSON} ->
		    format_log(info, "CONTROL(~p): Ibrowse recv back ~p~n", [self(), JSON]),
		    ecallmgr_amqp:publish(JSON, targeted, AppQ);
		{error, Msg} ->
		    format_log(error, "CONTROL(~p): store_http_resp error: ~p~n", [self(), Msg]);
		_Other ->
		    format_log(error, "CONTROL(~p): ibrowse returned unexpected: ~p~n", [self(), _Other])
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

-spec(set_eff_call_id_name/3 :: (Node :: binary(), UUID :: binary(), Name :: undefined | binary()) -> no_return()).
set_eff_call_id_name(_Node, _UUID, undefined) ->
    ok;
set_eff_call_id_name(Node, UUID, Name) ->
    N = list_to_binary(["effective_caller_id_name=", Name]),
    set(Node, UUID, binary_to_list(N)).

-spec(set_eff_call_id_number/3 :: (Node :: binary(), UUID :: binary(), Num :: undefined | binary()) -> no_return()).
set_eff_call_id_number(_Node, _UUID, undefined) ->
    ok;
set_eff_call_id_number(Node, UUID, Num) when is_integer(Num) ->
    set_eff_call_id_number(Node, UUID, integer_to_list(Num));
set_eff_call_id_number(Node, UUID, Num) ->
    N = list_to_binary(["effective_caller_id_number", Num]),
    set(Node, UUID, binary_to_list(N)).

-spec(set_bypass_media(Node :: binary(), UUID :: binary(), Method :: undefined | binary()) -> no_return()).
set_bypass_media(_Node, _UUID, undefined) ->
    ok;
set_bypass_media(Node, UUID, <<"true">>) ->
    set(Node, UUID, "bypass_media=true");
set_bypass_media(Node, UUID, <<"false">>) ->
    set(Node, UUID, "bypass_media=false").

-spec(set_timeout/3 :: (Node :: binary(), UUID :: binary(), N :: undefined | integer() | list()) -> no_return()).
set_timeout(_Node, _UUID, undefined) ->
    ok;
set_timeout(Node, UUID, N) when is_integer(N) ->
    set_timeout(Node, UUID, integer_to_list(N));
set_timeout(Node, UUID, N) ->
    Timeout = [ $c,$a,$l,$l,$_,$t,$i,$m,$e,$o,$u,$t,$= | N],
    set(Node, UUID, Timeout).

-spec(set_terminators/3 :: (Node :: binary(), UUID :: binary(), Terminators :: undefined | binary()) -> no_return()).
set_terminators(_Node, _UUID, undefined) ->
    ok;
set_terminators(Node, UUID, <<"">>) ->
    set(Node, UUID, "none");
set_terminators(Node, UUID, Ts) ->
    Terms = binary_to_list(list_to_binary(["playback_terminators=", Ts])),
    set(Node, UUID, Terms).

-spec(set_ringback/3 :: (Node :: binary(), UUID :: binary(), RingBack :: undefined | binary()) -> no_return()).
set_ringback(_Node, _UUID, undefined) ->
    ok;
set_ringback(Node, UUID, RingBack) ->
    RB = list_to_binary(["ringback=${", RingBack, "}"]),
    set(Node, UUID, binary_to_list(RB)).

-spec(set/3 :: (Node :: binary(), UUID :: binary(), Arg :: list()) -> no_return()).
set(Node, UUID, Arg) ->
    send_cmd(Node, UUID, "set", Arg).
