%% -*- erlang -*-
-module(edown_make).

-export([from_script/1]).
-export([main/1]).

%% @spec main(Args::[Config]) -> no_return()
%% @doc Escript entry point for building edown (or edoc) documentation
%%
%% Usage: edown_make -config ConfigFile [-pa P] [-pz P]
%%
%% Calls {@link from_script/1. from_script(ConfigFile)} and then terminates,
%% with a normal or non-normal exit code, depending on the outcome.
%%
%% Make sure `$EDOWN/edown_make' is runnable, and in the command path, and
%% that the edown BEAM files are in the Erlang path (e.g. using $ERL_LIBS).
%% The `edown_make' escript also accepts `-pa P' and/or `-pz P' flags as a
%% means of locating the edown byte code.
%%
%% Note, however, that the function `edoc_make:main/1' only expects the
%% config file as an input argument, corresponding to
%%
%% `escript edoc_make.beam ConfigFile'
%%
%% (The reason for this is that if the beam file can be passed directly to
%% the escript command, setting the path should also be doable that way).
%%
%% @end
%%
main([Config]) ->
    case from_script(Config) of
	ok ->
	    halt();
	{error, _} ->
	    halt(1)
    end.

%% @spec from_script(ConfigFile) -> ok | {error, Reason}
%% @doc Reads ConfigFile and calls {@link edoc:application/3}
%%
%% The ConfigFile will be read using {@link file:script/1}, and should return
%% `{App, Dir, Options}', as required by {@link edoc:application/3}.
%%
%% This function does not manage dependencies. It is simply a wrapper around
%% {@link edoc:application/3}.
%% @end
%%
from_script(Config) ->
    case file:script(Config) of
	{ok, {App, Dir, Options}} ->
	    R = edoc:application(App, Dir, Options),
	    case R of
		ok ->
		    ok;
		Err ->
		    io:fwrite("~p~n", [Err]),
		    Err
	    end;
	Other ->
	    io:fwrite("Error reading config ~s:~n"
		      "~p~n", [Config, Other]),
	    {error, {config, Other}}
    end.

