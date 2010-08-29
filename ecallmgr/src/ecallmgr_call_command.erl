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

exec_cmd(Node, UUID, Prop) ->
    case get_value(<<"Call-ID">>, Prop) =:= UUID of
	true -> exec_cmd(Node, UUID, Prop, get_value(<<"Application-Name">>, Prop));
	false -> format_log(error, "CONTROL(~p): Cmd Not for us:~n~p~n", [self(), Prop])
    end.

exec_cmd(Node, UUID, Prop, <<"play">>) ->
    Tmp = get_value(<<"Filename">>, Prop),
    F = case ecallmgr_media_registry:is_registered(UUID, Tmp) of
	    {true, GenName} -> get_media_path(GenName);
	    false -> Tmp
	end,
    format_log(info, "CONTROL(~p): CMD: Play F: ~p~n", [self(), F]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
				    ,{"execute-app-name", "playback"}
					    ,{"execute-app-arg", F}
				   ]);
exec_cmd(Node, UUID, _Prop, <<"hangup">>) ->
    format_log(info, "CONTROL(~p): CMD: Hangup~n", [self()]),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
				    ,{"execute-app-name", "hangup"}
				    ,{"execute-app-arg", ""}
				   ]);
exec_cmd(Node, UUID, Prop, <<"record">>) ->
    format_log(info, "CONTROL(~p): CMD: Hangup~n", [self()]),
    Name = get_value(<<"Media-Name">>, Prop),
    GenName = ecallmgr_media_registry:register_name(UUID, Name),
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
				    ,{"execute-app-name", "record"}
				    ,{"execute-app-arg", get_media_path(GenName)}
				   ]);
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
exec_cmd(_Node, _UUID, _Prop, _App) ->
    format_log(error, "CONTROL(~p): Unknown App ~p:~n~p~n", [self(), _App, _Prop]).

get_media_path(Name) ->
    list_to_binary(["/tmp/", Name, ".wav"]).

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
stream_over_amqp(DestQ, F, State, Headers, Seq) ->
    case F(State) of
	{ok, Data, State1} ->
	    %% send msg
	    Msg = [{<<"Media-Content">>, Data}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    ecallmgr_amqp_publisher:publish(Msg, targeted, DestQ),
	    stream_over_amqp(DestQ, F, State1, Headers, Seq+1);
	eof ->
	    Msg = [{<<"Media-Content">>, <<"eof">>}
		   ,{<<"Media-Sequence-ID">>, Seq}
		   | Headers],
	    ecallmgr_amqp_publisher:publish(Msg, targeted, DestQ),
	    ok
    end.

stream_over_http(File, Verb, Prop) ->
    Url = binary_to_list(get_value(<<"Media-Transfer-Destination">>, Prop)),
    {struct, AddHeaders} = get_value(<<"Additional-Headers">>, Prop, {struct, []}),
    Headers = [{<<"Content-Length">>, filelib:file_size(File)}
	       | lists:map(fun({K, V}) -> {binary_to_list(K), V} end, AddHeaders)],
    Method = list_to_atom(binary_to_list(Verb)),
    Body = {fun stream_file/1, {undefined, File}},
    case ibrowse:send_req(Url, Headers, Method, Body) of
	{ok, StatusCode, RespHeaders, RespBody} ->
	    %% send response to Server-ID
	    1;
	{error, Reason} ->
	    %% send to Server-ID
	    2;
	_Other ->
	    %% ignore
	    ok
    end.

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
