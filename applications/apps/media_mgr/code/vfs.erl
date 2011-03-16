%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(vfs).

-compile(export_all).
-import(lib_misc, [downcase_str/1]).
-import(lists, [member/2]).

big_text() -> "./lib_misc.erl".

big_media() -> "track030.mp3".
    

%% @spec read_file(string()) -> {Compressed:bool(), int(), binary()}.
read_file(File) ->
    {ok, Bin} = file:read_file(File),
    {Flag, Size, Data} = case is_media_file(File) of
			     true ->
				 {false, size(Bin), Bin};
			     false ->
				 Bin1 = term_to_binary(Bin, [compressed]),
				 {true, size(Bin1), Bin1}
			 end,
    Md5 = lib_md5:binAsBin(Data),
    {Flag, Size, Md5, Data}.



is_media_file(File) ->
    member(downcase_str(filename:extension(File)),
	   [".jpg", ".mp3", ".mpg"]).


write_file_to_store(Pid, File, Md5, Compressed, Data) ->
    case rpc(Pid, {should_i_store, File, Md5}) of
	{yes, Vsn} ->
	    Data = term_to_binary({File,Vsn,Md5,Compressed,Data}),
	    Size = size(Data) + 32,
	    case rpc(Pid, {need_some_space, File, Vsn, Md5, Size}) of
		{yes, Handle, _Start} ->
		    file:pwrite(Handle, 
				[<<16#111111111:32, Size:32>>,Data/binary,
				 <<16#222222222:32>>]);
		no ->
		    exit(internal)
	    end;
	no ->
	    true
    end.


rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

loop(Env) ->
    receive
	{From, {should_i_store, File, Md5}} ->
	    case should_i_store(File, Md5, Env) of
		{yes, Vsn, Env1} ->
		    From ! {self(), {yes, Vsn}},
		    loop(Env1);
		no ->
		    From ! {self(), no},
		    loop(Env)
	    end;
	Any ->
	    io:format("Any=~p~n",[Any])
    end.

should_i_store(_File, _Md5, _Dict) ->
    true.
%%     case dict:find(File, Dict) ->
%% 	    end.

%%   11111111
%%   Size
%%     Data = term_to_binary({File,Compressed,Vsn,Data}}
%%   22222222

%% Globals
			
-record(env, 
	{maxSize, 
	 allocated,
	 index,
	 dirty,
	 maxVersions,
	 freeList}).

%% Freelist = [{Address, Space}, ...]
%% sorted by address

%% MaxSize is in MB

new_vfs(FileName, MaxSize, MaxVersions) ->
    case filelib:is_regular(FileName) of
	true ->
	    exit(eFileExists);
	false ->
	    FreeList = [{1, MaxSize}],
	    Env = #env{allocated=0,
		       index = dict:new(),
		       dirty=false,
		       maxSize=MaxSize*1000000, 
		       freeList=FreeList, 
		       maxVersions = MaxVersions},
	    B = term_to_binary(Env),
	    Size = size(B),
	    file:write_file(FileName,
			    [<<16#01010101:32,
			      Size:32,
			      B/binary,
			      16#02020202:32,
			      Size:32,
			      16#003030303:32>>])
    end.

del() ->
    file:delete("./test1.vfs").

test() ->
    new_vfs("./test1.vfs", 20, 3).


start(File) ->
    Self = self(),
    Pid = spawn_link(fun() -> start(Self, File) end),
    receive
	{Pid, Result} ->
	    Result
    end.

start(Parent, File) ->
    case (catch try_to_open(File)) of
	{ok, Handle, Env} ->
	    Parent ! {self(), {ok, Handle}},
	    loop(Env);
	{'EXIT', Why} ->
	    Parent ! {self(), {error, Why}}
    end.

%% write as if it will work -- no error checking

try_to_open(File) ->
    true = filelib:is_regular(File),
    Size = filelib:file_size(File),
    {ok, Handle} = file:open(File, [binary, raw, read, write]),
    {ok, Bin}    = doread(Handle, Size - 12, 12),
    case Bin of
	<<16#02020202:32, EnvSize:32,16#03030303:32>> ->
	    Pos = Size - EnvSize - 12, 
	    {ok, B1} = doread(Handle, Pos, Size),
	    Env = binary_to_term(B1),
	    io:format("recovered:~p~n",[Env]),
	    {ok, Handle, Env};
	_Other ->
	    io:format("Bin=~p~n",[Bin]),
	    exit(eCorruptArchive)
    end.


doread(Handle, Start, Size) ->
    io:format("pread:~p ~p~n",[Start, Size]),
    Bin = file:pread(Handle, Start, Size),
    io:format("Bin=~p~n",[Bin]),
    Bin.

    
		     
	 
	      
