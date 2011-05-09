-module(logger).

-export([format_log/3, start/0]).

%% available Priorities:
    %% * emerg
    %% * alert
    %% * crit
    %% * err
    %% * warning
    %% * notice
    %% * info
    %% * debug
    %% * error <- legacy; mapped to err

start() ->
    syslog:start(),
    syslog:open(atom_to_list(node()), [cons, perror, pid, odelay, ndelay], local0).

format_log(Type, Format, Data) when not is_list(Data) ->
    format_log(Type, Format, [Data]);

format_log(error, Format, Data) ->
    format_log(err, Format, Data);

format_log(Type, Format, Data) ->
    Str = io_lib:format(Format, Data),
    try syslog:log(Type, binary_to_list(list_to_binary(Str))) catch _:_ -> ok end.

