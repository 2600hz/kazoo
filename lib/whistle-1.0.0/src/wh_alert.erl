-module(wh_alert).

-include("../include/wh_log.hrl").

-export([start_link/0, start_link/1, start_link/2]).
-export([format/1, format/2, format/3, format/4, format/5, format/6]).
-export([emerg/2, alert/2, crit/2, err/2, warning/2, notice/2, info/2, debug/2]).

-define(APP_VERSION, "2.0").
-define(APP_NAME, "Whistle Alert").

-record(alert, {level=debug
                ,req_id=?LOG_SYSTEM_ID
                ,section=sys
                ,module=?MODULE
                ,line=0
                ,pid=self()
                ,msg= <<>>
                ,args=[]}).

-type msg() :: string() | binary().

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
emerg(Msg, Args) ->
    format(#alert{level=emergency, msg=Msg, args=Args}).

-spec alert/2 :: (string(), list(term())) -> 'ok'.
alert(Msg, Args) ->
    format(#alert{level=alert, msg=Msg, args=Args}).

-spec crit/2 :: (string(), list(term())) -> 'ok'.
crit(Msg, Args) ->
    format(#alert{level=critical, msg=Msg, args=Args}).

-spec err/2 :: (string(), list(term())) -> 'ok'.
err(Msg, Args) ->
    format(#alert{level=error, msg=Msg, args=Args}).

-spec warning/2 :: (string(), list(term())) -> 'ok'.
warning(Msg, Args) ->
    format(#alert{level=warning, msg=Msg, args=Args}).

-spec notice/2 :: (string(), list(term())) -> 'ok'.
notice(Msg, Args) ->
    format(#alert{level=notice, msg=Msg, args=Args}).

-spec info/2 :: (string(), list(term())) -> 'ok'.
info(Msg, Args) ->
    format(#alert{level=info, msg=Msg, args=Args}).

-spec debug/2 :: (string(), list(term())) -> 'ok'.
debug(Msg, Args) ->
    format(#alert{level=debug, msg=Msg, args=Args}).

-spec format/1 :: (#alert{} | msg()) -> ok.
format(#alert{req_id=ReqId, level=Level, section=Section, module=Module, line=Line, pid=Pid, msg=Msg, args=Args}) ->
    try
        ExtraData = lists:keyfind(extra_data, 1, Args),
        Format = "|~s|~s|~s|~p:~b (~w) " ++ Msg,
        Str = io_lib:format(Format, [ReqId, Level, Section, Module, Line, Pid | list:keydelete(extra_data, 1, Args)]),
        syslog:log(Level, binary_to_list(list_to_binary(Str))),
        case lists:member(Level, ?LOG_PUBLISH_LEVELS) of
            false -> ok;
            true ->
                Notify = [{<<"Level">>, Level}
                          ,{<<"Request-ID">>, ReqId}
                          ,{<<"Section">>, Section}
                          ,{<<"Module">>, Module}
                          ,{<<"Line">>, Line}
                          ,{<<"Pid">>, Pid}
                          ,{<<"Message">>, Str}
                          ,{<<"Account-ID">>, props:get_value(account_id, ExtraData)}
                          ,{<<"Details">>, props:get_value(details, ExtraData)}
                          | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                         ],
                wapi_notifications:publish_system_alert(Notify)
        end
    catch
        A:B ->
            ST = erlang:get_stacktrace(),
            syslog:log(debug, io_lib:format("|000000000000|debug|sys|~p:~b (~w) logger error: ~p: ~p"
                                            ,[?MODULE, ?LINE, self(), A, B])),
            syslog:log(debug, io_lib:format("|000000000000|debug|sys|~p:~b (~w) type: ~p"
                                            ,[?MODULE, ?LINE, self(), Level])),
            syslog:log(debug, io_lib:format("|000000000000|debug|sys|~p:~b (~w) format: ~p"
                                            ,[?MODULE, ?LINE, self(), Msg])),
            syslog:log(debug, io_lib:format("|000000000000|debug|sys|~p:~b (~w) data: ~p"
                                            ,[?MODULE, ?LINE, self(), Args])),
            [syslog:log(debug, io_lib:format("|000000000000|debug|sys|~p:~b (~w) st line: ~p"
                                             ,[?MODULE, ?LINE, self(), STLine])) || STLine <- ST],
            ok
    end;
format(Msg) ->
    format(#alert{section=sys, req_id=erlang:get(callid), module=?MODULE, line=?LINE, pid=self(), msg=Msg}).

-spec format/2 :: (atom() | msg(), msg() | list()) -> ok.
format(Level, Msg) when is_atom(Level) ->
    format(#alert{section=sys, req_id=erlang:get(callid), module=?MODULE, line=?LINE, pid=self(), level=Level, msg=Msg});
format(Msg, Args) ->
    format(#alert{section=sys, req_id=erlang:get(callid), module=?MODULE, line=?LINE, pid=self(), msg=Msg, args=Args}).

-spec format/3 :: (atom(), list(), msg()) -> ok.
format(Section, [ReqId, Module, Line, Pid], Msg) ->
    format(#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, msg=Msg}).

-spec format/4 :: (atom(), list(), atom() | msg(), msg() | list()) -> ok.
format(Section, [ReqId, Module, Line, Pid], Level, Msg) when is_atom(Level) ->
    format(#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, level=Level, msg=Msg});
format(Section, [ReqId, Module, Line, Pid], Msg, Args) ->
    format(#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, msg=Msg, args=Args}).

-spec format/5 :: (atom(), list(), undefined | atom() | binary(), msg(), list()) -> ok.
format(Section, [ReqId | _]=Defaults, undefined, Msg, Args) ->
    format(Section, Defaults, ReqId, Msg, Args);
format(Section, [ReqId, Module, Line, Pid], Level, Msg, Args) when is_atom(Level) ->
    format(#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, level=Level, msg=Msg, args=Args});
format(Section, [_, Module, Line, Pid], ReqId, Msg, Args) ->
    format(#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, msg=Msg, args=Args}).

-spec format/6 :: (atom(), list(), atom(), undefined | binary(), msg(), list()) -> ok.
format(Section, [ReqId | _]=Defaults, Level, undefined, Msg, Args) ->
    format(Section, Defaults, Level, ReqId, Msg, Args);
format(Section, [_, Module, Line, Pid], Level, ReqId, Msg, Args) ->
    format(#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, level=Level, msg=Msg, args=Args}).
