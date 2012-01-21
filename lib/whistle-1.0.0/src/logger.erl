-module(logger).

-export([start_link/0, start_link/1, start_link/2]).

-export([format_log/3, format_log/4, format_log/5]).

-export([emerg/2, alert/2, crit/2, err/2, warning/2, notice/2, info/2, debug/2]).

-spec start_link/0 :: () -> {'ok', pid()}.
start_link() ->
    start_link(local0).

-type options() :: ['cons' | 'perror' | 'pid' | 'odelay' | 'ndelay',...].

-spec start_link/1 :: (atom()) -> {'ok', pid()}.
start_link(Facility) ->
    start_link(Facility, [cons, perror, pid, odelay, ndelay]).

-spec start_link/2 :: (atom(), options()) -> {'ok', pid()}.
start_link(Facility, Opts) ->
    Resp = syslog:start_link(),
    syslog:open(atom_to_list(node()), Opts, Facility),
    Resp.

-spec emerg/2 :: (string(), list(term())) -> 'ok'.
-spec alert/2 :: (string(), list(term())) -> 'ok'.
-spec crit/2 :: (string(), list(term())) -> 'ok'.
-spec err/2 :: (string(), list(term())) -> 'ok'.
-spec warning/2 :: (string(), list(term())) -> 'ok'.
-spec notice/2 :: (string(), list(term())) -> 'ok'.
-spec info/2 :: (string(), list(term())) -> 'ok'.
-spec debug/2 :: (string(), list(term())) -> 'ok'.

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

-ifdef(TEST).
format_log(Type, Format, Data) when not is_list(Data) ->
    format_log(Type, Format, [Data]);

format_log(error, Format, Data) ->
    format_log(err, Format, Data);

format_log(Type, Format, CallID) when is_atom(Type), is_binary(CallID) ->
    format_log(Type, Format, [Type, CallID]);

format_log(Format, Data, CallID) when is_binary(CallID) ->
    format_log(debug, Format, [debug, CallID | Data]);

format_log(Type, Format, Data) when is_atom(Type) ->
    try
        Str = io_lib:format(Format, Data),
        syslog:log(Type, binary_to_list(list_to_binary(Str)))
    catch
        A:B ->
            io:format("logger exception: ~p:~p", [A,B])
    end.

-else.
format_log(Type, Format, Data) when not is_list(Data) ->
    format_log(Type, Format, [Data]);

format_log(error, Format, Data) ->
    format_log(err, Format, Data);

format_log(Type, Format, CallID) when is_atom(Type), is_binary(CallID) ->
    format_log(Type, Format, [Type, CallID]);

format_log(Format, Data, CallID) when is_binary(CallID) ->
    format_log(debug, Format, [debug, CallID | Data]);

format_log(Type, Format, Data) when is_atom(Type) ->
    try
        Str = io_lib:format(Format, Data),
        syslog:log(Type, binary_to_list(list_to_binary(Str)))
    catch
        A:B ->
            ST = erlang:get_stacktrace(),
            syslog:log(debug, "logger error: ~p: ~p", [A, B]),
            syslog:log(debug, "type: ~p", [Type]),
            syslog:log(debug, "format: ~p", [Format]),
            syslog:log(debug, "data: ~p", [Data]),
            [syslog:log(debug, "st line: ~p", [STLine]) || STLine <- ST],
            ok
    end.
-endif.


format_log(Type, Format, Data, CallID) when is_atom(Type) ->
    format_log(Type, Format, [Type, CallID | Data]);
format_log(CallID, Format, Data, _) ->
    format_log(debug, Format, [debug, CallID | Data]).

format_log(Type, BaseFmt, BaseArgs, DebugFmt, CallID) when is_atom(Type) ->
    format_log(Type
               ,BaseFmt ++ DebugFmt
               ,[Type, CallID | BaseArgs]
              );
format_log(DebugFmt, BaseFmt, BaseArgs, DebugArgs, CallID) when is_list(DebugFmt) ->
    format_log(debug
               ,BaseFmt ++ DebugFmt
               ,[debug, CallID | BaseArgs] ++ DebugArgs
               );
format_log(CallID, BaseFmt, BaseArgs, DebugFmt, DebugArgs) when is_binary(CallID) ->
    format_log(debug
               ,BaseFmt ++ DebugFmt
               ,[debug, CallID | BaseArgs] ++ DebugArgs
               ).
