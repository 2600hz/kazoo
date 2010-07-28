-module(rscrpt_logger).

-export([log/2, format_log/3]).

%% log(Level, Msg) -> see error_logger:info_report/1 for Msg options
log(info, Msg) ->
    error_logger:info_report(Msg);
log(warning, Msg) ->
    error_logger:warning_report(Msg);
log(_, Msg) ->
    error_logger:error_report(Msg).

format_log(Type, Format, Data) when not is_list(Data) -> format_log(Type, Format, [Data]);
format_log(info, Format, Data) -> error_logger:info_msg(Format, Data);
format_log(warning, Format, Data) -> error_logger:warning_msg(Format, Data);
format_log(_, Format, Data) -> error_logger:error_msg(Format, Data).
