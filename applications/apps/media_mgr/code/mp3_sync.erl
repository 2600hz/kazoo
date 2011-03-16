%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(mp3_sync).

-export([find_sync/2]).

%% find_sync(Bin, N) -> {ok, K} | error.

%% Bin is a continuous block of MP3 data.
%% At some offset K in Bin we can find the start of
%% an MPEG header   
%% Start searching at byte N in Bin
%% and look for the next word K where we
%% are synced - require three in a row to achieve sync


find_sync(Bin, N) ->
    case is_header(N, Bin) of
	{ok, Len1, _} ->
	    case is_header(N + Len1, Bin) of
		{ok, Len2, _} ->
		    case is_header(N + Len1 + Len2, Bin) of
			{ok, _, _} ->
			    {ok, N};
			error ->
			    find_sync(Bin, N+1)
		    end;
		error ->
		    find_sync(Bin, N+1)
	    end;
	error ->
	    find_sync(Bin, N+1)
    end.

%% is_header(N, Bin) -> {ok, FrameLength, Info} | error.


is_header(N, Bin) ->
    unpack_header(get_word(N, Bin)).

get_word(N, Bin) ->
    {_,<<C:4/binary,_/binary>>} = split_binary(Bin, N),
    C.

unpack_header(X) ->
    try decode_header(X)
    catch
	_:_ -> error
    end.



decode_header(<<2#11111111111:11,B:2,C:2,_D:1,E:4,F:2,G:1,Bits:9>>) ->
    Vsn = case B of
	      0 -> {2,5};
	      1 -> exit(badVsn);
	      2 -> 2;
	      3 -> 1
	  end,
    Layer = case C of
		0 -> exit(badLayer);
		1 -> 3;
		2 -> 2;
		3 -> 1
	    end,
    %% Protection = D,
    BitRate = bitrate(Vsn, Layer, E) * 1000,
    SampleRate = samplerate(Vsn, F),
    Padding = G,
    FrameLength = framelength(Layer, BitRate, SampleRate, Padding),
    if 
	FrameLength < 21 ->
	    exit(frameSize);
	true ->
	    {ok, FrameLength, {Layer,BitRate,SampleRate,Vsn,Bits}}
    end;
decode_header(_) ->
    exit(badHeader).

    
bitrate(_,_,15) -> exit(1);
bitrate(1,1,E) ->		      
    element(E+1, {free,32,64,96,128,160,192,224,256,288,
		  320,352,384,416,448});
bitrate(1,2,E) ->
    element(E+1, {free,32,48,56,64,80,96,112,128,160,
		  192,224,256,320,384});
bitrate(1,3,E) ->
    element(E+1, {free,32,40,48,56,64,80,96,112,128,160,192,
		  224,256,320});
bitrate(2,1,E) ->
    element(E+1, {free,32,48,56,64,80,96,112,128,144,160,
		  176,192,224,256});
bitrate(2,2,E) ->
    element(E+1, {free,8,16,24,32,40,48,56,64,80,96,112,
		  128,144,160});
bitrate(2,3,E) -> bitrate(2,2,E);
bitrate({2,5}, L, E) -> bitrate(2, L, E).
    
%% samplerate Vsn F
samplerate(1, 0) -> 44100;
samplerate(1, 1) -> 48000;
samplerate(1, 2) -> 32000;
samplerate(2, 0) -> 22050;
samplerate(2, 1) -> 24000;
samplerate(2, 2) -> 16000;
samplerate({2,5}, 0) -> 11025;
samplerate({2,5}, 1) -> 12000;
samplerate({2,5}, 2) -> 8000.

framelength(1, BitRate, SampleRate, Padding) ->
    ((12*BitRate div SampleRate) + Padding) * 4;
framelength(_, BitRate, SampleRate, Padding) ->
    (144 * BitRate div SampleRate) + Padding.
    
