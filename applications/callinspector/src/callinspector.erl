%% -*- coding: utf-8 -*-
%% 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
-module(callinspector).

%% callinspector: 

-export([ main/1
        , find/2
        ]).


%% API

main (Paths) ->
    Master = self(),
    io:format("Indexing using workers:\n"),
    Workers =
        [ begin
              Pid = spawn(fun () ->  %% Could be run on another machine
                                  do(Path),
                                  Master ! 'im_ready!',
                                  serve(Path)
                          end),
              io:format("\tproc ~p  will be indexing  ~p\n", [Pid, Path]),
              Pid
          end || Path <- Paths],
    io:format("   --\n"),
    wait_for_workers(length(Workers)),
    io:format("\n\nAll ~p workers ready to serve\n\n", [length(Workers)]),
    Workers.


find (Workers, Stuff) ->
    Master   = self(),
    WorkUnit = make_ref(),
    lists:foreach(fun (Worker) ->
                          Worker ! {Master, find, WorkUnit, Stuff}
                  end, Workers),
    gather_results(length(Workers), WorkUnit, []).

%% Internals

do (Path) ->
    {ok, IoDevice} = file:open(Path, [read,raw,binary,read_ahead]),%read+append??
    do_read(IoDevice),
    ok = file:close(IoDevice),
    %% Docs = fetch(Path),%load files from fs to mem
    %% Tokenizeds = analyze(Docs, [fun normalizer/1]),%lexe each file's text in parallel
    %% ok = build(Tokenizeds),%fills hashmap. MUST NOT be concurrent
    io:format("~p done\n", [self()]).

serve (Id) ->
    receive
        {From, search, Stuff, Ref}
          when is_binary(Stuff) ->
            case get(Stuff) of
                undefined -> LocalResults = [];
                Urls      -> LocalResults = [get(Url) || Url <- Urls]%whatevs
            end,
            From ! {Ref, results, LocalResults},
            serve(Id);
        {From, stop} ->
            From ! {self(), stopped},
            ok
    end.

wait_for_workers (0) -> ok;
wait_for_workers (N) ->
    receive 'im_ready!' ->
            wait_for_workers(N -1) end.


do_read (Dev) ->
    case file:read_line(Dev) of
        eof -> ok;
        %match stuff, enter some special case, fill pdict.
        {ok, Line} ->
            io:format(">>> ~ts", [Line]),
            do_read(Dev)
    end.


gather_results (0, _Ref, Acc) ->
    lists:append(Acc);
gather_results (N, Ref, Acc) ->
    receive
        {Ref, results, Part} ->
            gather_results(N -1, Ref, [Part|Acc]);
        ImpromptuMessage     ->
            io:format("Yo! Got this weird msg: ~p\n", [ImpromptuMessage]),
            gather_results(N, Ref, Acc)
    after 2000 -> %% in ms
            io:format("Search timed out! ~p ~p ~p\n", [N,Ref,Acc]),
            timeout
    end.

%% End of Module.
