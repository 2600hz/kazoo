%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(mp3_manager).
-import(lists, [map/2, reverse/1]).
-compile(export_all).

start1() ->
    Files = lib_files_find:files("/home/joe/music_keep", "*.mp3", true),
    V = map(fun handle/1, Files),
    lib_misc:dump("mp3data", V).

start2() ->
    Files = lib_files_find:files("/windows/backup/music/artists", "*.mp3", true),
    V = map(fun handle/1, Files),
    lib_misc:dump("mp3data", V).

handle(File) -> 
    (catch read_id3_tag(File)).
		
read_id3_tag(File) ->
    case file:open(File, [read,binary,raw]) of
        {ok, S} ->
	    Size = filelib:file_size(File),
            Result = (catch analyse1(S, Size)),
	    file:close(S),
	    {File, Result};
        Error ->
            {File, Error}
    end.

analyse1(S, Size) ->
    {ok, B1} = file:pread(S, 0, 10),
    case parse_start_tag(B1) of
	{"ID3v2.3.0", {_Unsync, Extended, _Experimental, Len}} ->
	    {ok, Data} = file:pread(S, 10, Len),
	    case Extended of
		1 ->
		    {Extended, Data1} = split_binary(Data, 10),
		    parse_frames(Data1);
		0 ->
		    parse_frames(Data)
	    end;
	no ->
	    %% see if we can find a tag at the end
	    {ok, B2} = file:pread(S, Size-128, 128),
	    parse_v1_tag(B2)
    end.

parse_start_tag(<<$I,$D,$3,3,0,Unsync:1,Extended:1,Experimental:1,_:5,K:32>>) ->
    Tag = "ID3v2.3.0",
    Size = syncsafe2int(K),
    {Tag, {Unsync, Extended, Experimental, Size}};
parse_start_tag(_) ->
    no.

parse_frames(B) ->
    F = gather_frames(B),
    G = map(fun parse_frame/1, F),
    H = [{I,J}||{I,J}<-G],
    {ok, H}.

parse_frame({"TIT2", _, Txt}) -> {title, parse_txt(Txt)};
parse_frame({"TPE1", _, Txt}) -> {performer, parse_txt(Txt)};
parse_frame({"TALB", _, Txt}) -> {album, parse_txt(Txt)};
parse_frame({"TRCK", _, Txt}) -> {track, parse_txt(Txt)};
parse_frame(_) -> skipped.

parse_txt(<<0:8,Txt/binary>>) ->
    Txt;
parse_txt(<<1:8,16#ff,16#fe, Txt/binary>>) ->
    unicode_to_ascii(Txt).

unicode_to_ascii(Bin) -> list_to_binary(uni_to_ascii1(binary_to_list(Bin))).

uni_to_ascii1([X,_|Y]) -> [X|uni_to_ascii1(Y)];
uni_to_ascii1([]) -> [].

gather_frames(B) when size(B) < 10 ->
    [];
gather_frames(<<0,0,0,0,_/binary>>) ->
    [];
gather_frames(<<$P,$R,$I,$V,_/binary>>) ->
    [];
gather_frames(<<Id1,Id2,Id3,Id4,SafeN:32,Flags:16,Rest/binary>>) ->
    <<_A:1,_B:1,_C:1,_:5,I:1,J:1,_K:1,_:5>> = <<Flags:16>>,
    case {I, J}  of
	{0, 0}  ->
	    Tag = [Id1,Id2,Id3,Id4],
	    case is_tag(Tag) of
		true ->
		    Size = syncsafe2int(SafeN),
		    {Data, Next} = split_binary(Rest, Size),
		    [{Tag,Flags,Data}|gather_frames(Next)];
		false ->
		    []
	    end;
	_ ->
	    %% bad flags
	    []
    end;
gather_frames(CC) ->
    [{error, CC}].

is_tag([H|T]) when $A =< H, H =< $Z -> is_tag(T);
is_tag([H|T]) when $0 =< H, H =< $9 -> is_tag(T);
is_tag([]) -> true;
is_tag(_) -> false.
    

syncsafe2int(N) ->
    <<_:1,N1:7,_:1,N2:7,_:1,N3:7,_:1,N4:7>> = <<N:32>>,
    <<I:32>> = <<0:4,N1:7,N2:7,N3:7,N4:7>>,
    I.

parse_v1_tag(<<$T,$A,$G,B/binary>>) ->
    {Title, B1}  = split_binary(B, 30),
    {Artist, B2} = split_binary(B1, 30),
    {Album, B3}  = split_binary(B2, 30),
    {_Year, B4}  = split_binary(B3, 4),
    {_Comment, <<K, Track,_Gendre>>} = split_binary(B4, 28),
    L = [{title,trim(Title)},{artist,trim(Artist)}, {album, trim(Album)}],
    case K of
	0 ->
	    {"ID3v1.1", [{track,Track}|L]};
	_ ->
	    {"ID3v1", L}
    end;
parse_v1_tag(_) ->
    no.

trim(Bin) -> 
    list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(X) -> reverse(skip_blanks_and_zero(reverse(X))).

skip_blanks_and_zero([$\s|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T])   -> skip_blanks_and_zero(T);
skip_blanks_and_zero(X)       -> X.
