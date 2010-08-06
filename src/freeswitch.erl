-module(freeswitch).

%%% Module for freeSWITCH related functions that get called all over the place.

-include("../include/fs.hrl").

-export([new_fs_socket/1, clear_socket/1, read_socket/1, read_loop/2
	,send_cmd/2, event_to_proplist/1]).

%% with timeouts
-export([clear_socket/2, read_socket/2, read_loop/3]).

%% new_fs_socket(#fs_conn{}) -> #fs_conn{}
new_fs_socket(#fs_conn{host=H, port=P, auth=A}=Fs) ->
    Fs#fs_conn{socket= case gen_tcp:connect(H, P, [list, {active, false}]) of
			   {ok, FsSock} ->
			       inet:setopts(FsSock, [{packet, line}]),
			       clear_socket(FsSock),
			       ok = gen_tcp:send(FsSock, lists:concat(["auth ", A, "\n\n"])),
			       clear_socket(FsSock),
			       FsSock;
			   {error, _Reason} ->
			       undefined
		       end}.

send_cmd(Socket, Cmd) ->
    Cmd1 = case lists:reverse(Cmd) of
	       ["\n\n" | _] -> Cmd;
	       ["\n" | _]=Almost -> lists:reverse([$\n | Almost]);
	       Nope -> lists:reverse([$\n, $\n | Nope])
	   end,
    ok = gen_tcp:send(Socket, Cmd1).

%% read_socket(Socket) -> {ok, Headers, Body} | {error, Reason}
%% read_socket(Socket, Timeout) -> {ok, Headers, Body} | {error, Reason}
read_socket(Socket) ->
    read_socket(Socket, [], 0, infinity).

read_socket(Socket, Timeout) ->
    read_socket(Socket, [], 0, Timeout).

read_socket(Socket, Headers, ContentLength, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, "\n"} ->
            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, Body} = gen_tcp:recv(Socket, ContentLength, Timeout),
		    inet:setopts(Socket, [{packet, line}]),
		    {ok, Headers, Body};
		_ ->
		    {ok, Headers, []}
            end;
        {ok, Data} ->
            %% Parse the line
	    KV = split(Data),
	    {K, V} = KV,

            %% Is this a content-length string? If so, we'll need to gather extra data later
            case K =:= "Content-Length" of
		true ->
		    Length = list_to_integer(V);
		_ ->
		    Length = ContentLength
            end,

            read_socket(Socket, [KV | Headers], Length, Timeout);
	Other ->
	    Other
    end.

%% clear_socket(Socket) -> ok | {error, Reason}.
%% clear_socket(Socket, Timeout) -> ok | {error, Reason}
%% clears the next FS socket message but doesn't return it
clear_socket(Socket) ->
    clear_socket(Socket, [], 0, infinity).

clear_socket(Socket, Timeout) ->
    clear_socket(Socket, [], 0, Timeout).

clear_socket(Socket, Data, ContentLength, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, "\n"} ->
            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, _Body} = gen_tcp:recv(Socket, ContentLength, Timeout),
		    inet:setopts(Socket, [{packet, line}]);
		_ ->
		    inet:setopts(Socket, [{packet, line}])
            end;
        {ok, Datum} ->
	    KV = split(Datum),
	    {K, V} = KV,

            case K =:= "Content-Length" of
		true ->
		    Length = list_to_integer(V);
		_ ->
		    Length = ContentLength
            end,

            clear_socket(Socket, [KV | Data], Length, Timeout);
	Other ->
	    Other
    end.

%% read_loop(Socket, Fun/1)
%% read_loop(Socket, Fun/1, Timeout)
%% Fun will be called with {ok, Headers, Body} or {error, Reason}
%% and reader loop will go back to blocking on the socket
read_loop(Socket, F) ->
    reader_loop(Socket, F, [], 0, infinity).

read_loop(Socket, F, Timeout) ->
    reader_loop(Socket, F, [], 0, Timeout).

reader_loop(Socket, Fun, Headers, ContentLength, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, "\n"} ->
            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, Body} = gen_tcp:recv(Socket, ContentLength, Timeout),
		    inet:setopts(Socket, [{packet, line}]),
		    Fun({ok, Headers, Body});
		_ ->
		    Fun({ok, Headers, []})
            end,
	    reader_loop(Socket, Fun, [], 0, Timeout);
        {ok, Data} ->
            %% Parse the line
	    KV = split(Data),
	    {K, V} = KV,

            %% Is this a content-length string? If so, we'll need to gather extra data later
            Length = case K of
			 "Content-Length" -> list_to_integer(V);
			 false -> ContentLength
		     end,

            reader_loop(Socket, Fun, [KV | Headers], Length, Timeout);
        Err ->
            Fun(Err)
    end.

event_to_proplist(Str) ->
    L = string:tokens(Str, "\n"),
    lists:map(fun(S) -> [K, V0] = string:tokens(S, ":"),
			V1 = string:strip(V0, left, $ ),
			{V2, []} = mochiweb_util:parse_qs_value(V1),
			{K, V2}
	      end, L).

%% Takes K: V\n and returns {K, V}
split(Data) ->
    [K | V] = string:tokens(Data, ": "),
    V1 = case length(V) of
	     0 -> "";
	     1 -> string:strip(hd(V), right, $\n);
	     _ -> lists:map(fun(S) -> string:strip(S, right, $\n) end, V)
	 end,
    {K, V1}.
