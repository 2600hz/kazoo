%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(shout).

%% In one window > shout:start()
%% in another window xmms http://localhost:3000/stream

-export([start/3]).
-import(lists, [map/2, reverse/1]).

-include("media.hrl").

-spec(start/3 :: (Port :: port(), Media :: #media_file{}, Type :: single | continuous) -> pid()).
start(Port, Media, Type) ->
    spawn(fun() -> par_connect(Port, Media, Type) end).

par_connect(Listen, Media, single) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, false}]),
    get_request(Socket, Media, []);
par_connect(Listen, Media, continuous) ->
    {ok, Socket} = gen_tcp:accept(Listen),

    %% if a one-time server, don't eval this expression
    spawn(fun() -> par_connect(Listen, Media, continuous) end),

    inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, false}]),
    get_request(Socket, Media, []).

get_request(Socket, Media, L) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Bin} ->
	    L1 = L ++ binary_to_list(Bin),
	    %% split checks if the header is complete
	    case split(L1, []) of
		more ->
		    %% the header is incomplete we need more data
		    get_request(Socket, Media, L1);
		{Request, _Rest} ->
		    io:format("recv request: ~p~n", [Request]),
		    %% header is complete
		    got_request_from_client(Socket, Media, Request)
	    end;
	{error, closed} ->
	    void;
	_Any  ->
	    %% skip this
	    get_request(Socket, Media, L)
    end.
		      
split("\r\n\r\n" ++ T, L) -> {reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.

got_request_from_client(Socket, Media, Request) ->
    Cmds = string:tokens(Request, "\r\n"),
    Cmds1 = map(fun(I) -> string:tokens(I, " ") end, Cmds),
    is_request_for_stream(Cmds1),

    Media1 = case binary:referenced_byte_size(Media#media_file.contents) of
		 X when X < ?CHUNKSIZE -> Media#media_file{chunk_size=X};
		 _ -> Media#media_file{chunk_size=?CHUNKSIZE}
	     end,

    Resp = response(Media),
    io:format("resp: ~p~n", [Resp]),
    gen_tcp:send(Socket, [Resp]),
    play_songs(Socket, Media1, <<>>).

play_songs(Socket, _, done) ->
    io:format("Done playing~n", []),
    gen_tcp:close(Socket);
play_songs(Socket, #media_file{contents=Song}=Media, SoFar) ->
    {PrintStr, Header} = unpack_song_descriptor(Media),
    {Start, Stop} = {0, binary:referenced_byte_size(Song)},
    io:format("Playing:~p~n",[PrintStr]),
    %% {ok, S} = file:open(File, [read,binary,raw]), 
    SoFar1 = send_file(Media, {0,Header}, Start, Stop, Socket, SoFar),
    %% file:close(S),
    play_songs(Socket, Media, SoFar1).

send_file(#media_file{contents=Song, chunk_size=ChunkSize}=Media, Header, OffSet, Stop, Socket, SoFar) ->
    %% OffSet = first byte to play
    %% Stop   = The last byte we can play
    Need = ChunkSize - size(SoFar),
    Last = OffSet + Need,
    case Last >= Stop of
	true ->
	    %% not enough data so read as much as possible and return
	    Max = Stop - OffSet,
	    Bin = binary:part(Song, OffSet, Max),
	    write_data(Socket, SoFar, Bin, Header, Media),
	    done;
	false ->
	    Bin = binary:part(Song, OffSet, Need),
	    write_data(Socket, SoFar, Bin, Header, Media),
	    send_file(Media, bump(Header),
		      OffSet + Need,  Stop, Socket, <<>>)
    end.

write_data(Socket, B0, B1, Header, #media_file{chunk_size=ChunkSize}) ->
    %% Check that we really have got a block of the right size
    %% this is a very useful check that our program logic is
    %% correct
    case size(B0) + size(B1) of
	ChunkSize ->
	    case gen_tcp:send(Socket, [B0, B1, the_header(Header)]) of
		ok -> true;
		{error, closed} ->
		    %% this happens if the player 
		    %% terminates the connection
		    exit(playerClosed)
	    end;
	Size when Size < ChunkSize ->
	    TheHeader = the_header(Header),
	    Padding = binary:copy(<<0>>, ChunkSize - Size - size(TheHeader)),
	    Send = [B0, B1, TheHeader, Padding],
	    %% io:format("B0: ~p B1: ~p CS: ~p S: ~p", [size(B0), size(B1), ChunkSize, size(Send)]),
	    case gen_tcp:send(Socket, Send) of
		ok -> true;
		{error, closed} ->
		    %% this happens if the player 
		    %% terminates the connection
		    exit(playerClosed)
	    end;
	_Other ->
	    %% don't send the block - report an error
	    io:format("Block length Error: B0 = ~p b1=~p~n",
		      [size(B0), size(B1)])
    end.

bump({K, H})     -> {K+1, H}.

the_header({K, H}) ->
    case K rem 5 of
	0 -> H;
	_ -> <<0>>
    end.

is_request_for_stream(_) -> true.

response(Media) ->
    ["ICY 200 OK\r\n"
     ,"icy-notice1: <BR>This stream requires"
     ,"mod_shout for FreeSWITCH/Whistle\r\n"
     ,"icy-notice2: MediaMgr Shoutcast server<BR>\r\n"
     ,"icy-name: ", Media#media_file.media_name, "\r\n"
     ,"icy-genre: Fancy Pants\r\n"
     ,"icy-url: ", Media#media_file.stream_url , "\r\n"
     ,"content-type: ", Media#media_file.content_type, "\r\n"
     ,"icy-pub: 1\r\n"
     ,"icy-metaint: ", integer_to_list(Media#media_file.chunk_size), "\r\n"
     ,"\r\n"
     %% ,"icy-br: 96\r\n\r\n"
    ]. 

unpack_song_descriptor(Media) -> %% {File, {_Tag,Info}}) ->
    %% PrintStr = list_to_binary(make_header1(Info)),
    PrintStr = "MediaMgr",
    L1 = ["StreamTitle='",Media#media_file.media_name
	  ,"';StreamUrl='", Media#media_file.stream_url, "';"],
    %% io:format("L1=~p~n",[L1]),
    Bin = list_to_binary(L1),
    Nblocks = ((size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - size(Bin), 
    Extra = lists:duplicate(NPad, 0),
    Header = list_to_binary([Nblocks, Bin, Extra]),
    %% Header is the Shoutcast header
    {PrintStr, Header}.
