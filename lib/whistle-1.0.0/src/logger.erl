-module(logger).

-export([format_log/3, start/0, start/1, start/2]).

-export([emerg/2, alert/2, crit/2, err/2, warning/2, notice/2, info/2, debug/2]).

-spec(start/0 :: () -> no_return()).
start() ->
    start(local0).

-spec(start/1 :: (Facility :: atom()) -> no_return()).
start(Facility) ->
    start(Facility, [cons, perror, pid, odelay, ndelay]).

-spec(start/2 :: (Facility :: atom(), Opts :: list(atom())) -> no_return()).
start(Facility, Opts) ->
    syslog:start(),
    syslog:open(atom_to_list(node()), Opts, Facility).

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
    Str = io_lib:format(Format, Data),
    try syslog:log(Type, binary_to_list(list_to_binary(Str))) catch _:_ -> ok end.
