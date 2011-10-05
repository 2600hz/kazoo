-module(logger).

-export([format_log/3, start_link/0, start_link/1, start_link/2]).

-export([emerg/2, alert/2, crit/2, err/2, warning/2, notice/2, info/2, debug/2]).

-spec start_link/0 :: () -> {ok, pid()}.
start_link() ->
    start_link(local0).

-spec start_link/1 :: (Facility) -> {ok, pid()} when
      Facility :: atom().
start_link(Facility) ->
    start_link(Facility, [cons, perror, pid, odelay, ndelay]).

-spec start_link/2 :: (Facility, Opts) -> {ok, pid()} when
      Facility :: atom(),
      Opts :: [cons | perror | pid | odelay | ndelay,...].
start_link(Facility, Opts) ->
    Resp = syslog:start_link(),
    syslog:open(atom_to_list(node()), Opts, Facility),
    Resp.

-spec(emerg/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(alert/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(crit/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(err/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(warning/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(notice/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(info/2 :: (Format :: string(), Data :: list(term())) -> ok).
-spec(debug/2 :: (Format :: string(), Data :: list(term())) -> ok).

emerg(Format, Data) ->
    format_log(emerg, Format, Data).

alert(Format, Data) ->
    format_log(alert, Format, Data).

crit(Format, Data) ->
    format_log(crit, Format, Data).

err(Format, Data) ->
    format_log(err, Format, Data).

warning(Format, Data) ->
    format_log(warning, Format, Data).

notice(Format, Data) ->
    format_log(notice, Format, Data).

info(Format, Data) ->
    format_log(info, Format, Data).

debug(Format, Data) ->
    format_log(debug, Format, Data).

format_log(Type, Format, Data) when not is_list(Data) ->
    format_log(Type, Format, [Data]);

format_log(error, Format, Data) ->
    format_log(err, Format, Data);

format_log(Type, Format, Data) ->
    try
    	Str = io_lib:format(Format, Data),
    	syslog:log(Type, binary_to_list(list_to_binary(Str)))
    catch
    	A:B -> io:format("Logger error ~p:~p~n~p~n", [A, B, erlang:get_stacktrace()]), ok
    end.
