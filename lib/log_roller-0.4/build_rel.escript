#!/usr/bin/env escript
%% -*- erlang -*-

main([AppName]) ->
    {ok, [{application,_,AppProps}]} = file:consult("ebin/" ++ AppName ++ ".app"),
    AppDeps = proplists:get_value(applications, AppProps, []),
    AppVsn = proplists:get_value(vsn, AppProps),
    {ok, FD} = file:open("release/" ++ AppName ++ ".rel", [write]),
    RelInfo = {release,
               {AppName, AppVsn},
               {erts, erts_vsn()}, 
               [{Pkg, lib_vsn(Pkg)} || Pkg <- AppDeps]
              },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    systools:make_script("release/" ++ AppName, [{exref, AppDeps}, {outdir, "release"}]),
    systools:make_tar("release/" ++ AppName, [{dirs, tar_dirs()}, {outdir, "release"}]),
    ok.

erts_vsn() ->
    erlang:system_info(version).
	
lib_vsn(App) ->
    load(App),
    {ok, Vsn} = application:get_key(App, vsn),
    Vsn.
    
load(App) ->
    case application:load(App) of
        ok -> 
            ok;             
        {error, {already_loaded, _}} -> 
            ok;
        E -> 
            io:format(standard_error, "Warning - can't load ~p (~p)~n", [App, E]),
            erlang:exit(E)
    end.

tar_dirs() ->
    {ok, Files} = file:list_dir("."),
    [list_to_atom(Dir) || Dir <- lists:filter(
        fun ("." ++ _) -> false;
            (File) ->
            filelib:is_dir(File) andalso not lists:member(File, ["ebin", "include", "t"])
        end, Files)].
        
        
