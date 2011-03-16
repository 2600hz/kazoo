%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(indexer_server).

-export([cold_start/2, 
	 start/1, 
	 filename2index/1, 
	 next_dir/0,
	 ets_table/0, 
	 checkpoint/0,
	 schedule_stop/0,
	 search/1,
	 should_i_stop/0,
	 outdir/0, 
	 stop/0]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-import(filename, [join/2]).

%% indexer_server:cold_start(OutDir, SearchDirs).    %% do once
%% indexer_server:start().                           %% start the server
%% indexer_server:stop().                            %% stop
%% indexer_server:next_dir() -> Dir                  %% next dir to work at
%% indexer_server:file2index(File) -> Index          %% filename2index
%% indexer_server:advance_dir()   %% advance the dir pointer and set
%%                                %% a new checkpoint
%% indexer_server:outdir()        %% The ouput directory
%% indexer:search(Str) -> [File]  %% search

start(Dir) ->
    io:format("starting ~p ~p~n",[?MODULE, Dir]),
    gen_server:start({local,?MODULE}, ?MODULE, Dir, []).

schedule_stop() ->
    gen_server:call(?MODULE, schedule_stop).

should_i_stop() ->
    gen_server:call(?MODULE, should_i_stop).

stop() ->
    gen_server:cast(?MODULE, stop).

filename2index(File) ->  gen_server:call(?MODULE, {f2i, File}).

next_dir()   -> gen_server:call(?MODULE, next_dir).
checkpoint() -> gen_server:call(?MODULE, checkpoint).
outdir()     -> gen_server:call(?MODULE, outdir).
ets_table()  -> gen_server:call(?MODULE, ets_table).    
search(Str)  -> gen_server:call(?MODULE, {search, Str}).

%% Cold start is done ONCE *before* the server
%% is started


cold_start(OutputDir, SearchDirs) ->
    case file:list_dir(OutputDir) of
	{ok, []} ->
	    OutputDir1 = join(OutputDir,"index"),
	    io:format("Creating ~p~n",[OutputDir1]),
	    file:make_dir(OutputDir1),
	    Cont = indexer_dir_crawler:start(SearchDirs),
	    Check = {OutputDir, Cont},
	    io:format("creating checkpoint:~p~n",[Check]),
	    indexer_checkpoint:init(OutputDir, Check);
	_ ->
	    exit({eDirNotEmptyOrMissing, OutputDir})
    end.


-record(env,{ets, cont, nextCP, outdir, stop=false}).

init(Dir) ->
    io:format("restarting:~p~n", [Dir]),
    {Next, {OutDir, Cont}} = indexer_checkpoint:resume(Dir),
    io:format("resume with:~p ~p~n",[Next, Cont]),
    indexer_filenames_dets:open(join(Dir,"filenames.dets")),
    io:format("opening trigrams ~n"),
    Tab = indexer_trigrams:open(),
    {ok, #env{ets = Tab, outdir=OutDir, cont=Cont, nextCP = Next}}.

handle_call({f2i, File}, _From, S) ->
    B = list_to_binary(File),
    {reply, indexer_filenames_dets:filename2index(B), S};
handle_call(ets_table, _From, S) ->
    {reply, S#env.ets, S};
handle_call(next_dir, _From, S) ->
    Cont = S#env.cont,
    case indexer_dir_crawler:next(Cont) of
	{dir, Dir, _} ->
	    {reply, {ok, Dir}, S};
	done ->
	    {reply, done, S}
    end;
handle_call(checkpoint, _From, S) ->
    Cont = S#env.cont,
    case indexer_dir_crawler:next(Cont) of
	{dir, _Dir, Cont1} ->
	    Next = S#env.nextCP,
	    OutDir = S#env.outdir,
	    Next1 = indexer_checkpoint:checkpoint(Next, {OutDir, Cont1}),
	    S1 = S#env{nextCP = Next1, cont=Cont1},
	    {reply, ok, S1};
	done ->
	    {reply, done, S}
    end;
handle_call(schedule_stop, _From, S) ->
    {reply, ack, S#env{stop=true}};
handle_call({search, Str}, _From,S) ->
    Result = indexer_misc:search(Str, S#env.outdir, S#env.ets),
    {reply, Result, S};
handle_call(should_i_stop, _From, S) ->
    {reply, S#env.stop, S};
handle_call(outdir, _From, S) ->
    {reply, S#env.outdir, S}.

handle_cast(stop, S) ->
    {stop, normal, S}.

terminate(Reason, S) ->
    Ets = S#env.ets,
    indexer_trigrams:close(Ets),
    indexer_filenames_dets:close(),
    io:format("stopping ~p~n",[Reason]).

    



    


