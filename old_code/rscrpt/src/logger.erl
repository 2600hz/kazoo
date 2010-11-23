-module(logger).

-ifdef(debug).
-define(INFO(Msg), error_logger:info_report(Msg)).
-define(WARN(Msg), error_logger:warning_report(Msg)).
-define(F_INFO(Format, Msg), error_logger:info_msg(Format, Msg)).
-define(F_WARN(Format, Msg), error_logger:warning_msg(Format, Msg)).
-else.
-define(INFO(_Msg), ok).
-define(WARN(_Msg), ok).
-define(F_INFO(_Format, _Msg), ok).
-define(F_WARN(_Format, _Msg), ok).
-endif.

-define(ERR(Msg), error_logger:error_report(Msg)).
-define(F_ERR(Format, Msg), error_logger:error_msg(Format, Msg)).

-export([log/2, format_log/3]).

%% log(Level, Msg) -> see error_logger:info_report/1 for Msg options
log(info, Msg) ->
    ?INFO(Msg);
log(warning, Msg) ->
    ?WARN(Msg);
log(_, Msg) ->
    ?ERR(Msg).

format_log(Type, Format, Data) when not is_list(Data) -> format_log(Type, Format, [Data]);
format_log(info, Format, Data) -> ?F_INFO(Format, Data);
format_log(warning, Format, Data) -> ?F_WARN(Format, Data);
format_log(_, Format, Data) -> ?F_ERR(Format, Data).
