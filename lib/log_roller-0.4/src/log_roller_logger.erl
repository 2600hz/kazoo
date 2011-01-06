%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(log_roller_logger).

-export([do_log/5, do_log/6, set/2]).

%% Error levels:
-define(LOG_LEVELS, [{no_log, 0},
                     {critical, 1},
                     {error, 2},
                     {warning, 3},
                     {info, 4},
                     {debug, 5}]).
                     
do_log(ReportType, Module, LoggerModule, Line, Args) when is_atom(ReportType), is_atom(Module), is_atom(LoggerModule), is_integer(Line) ->
    ensure_loaded(LoggerModule),
    erlang:apply(LoggerModule, ReportType, [Module, Line, Args]).
    
do_log(ReportType, Module, LoggerModule, Line, Format, Args) when is_atom(ReportType), is_atom(Module), is_atom(LoggerModule), is_integer(Line) ->
    ensure_loaded(LoggerModule),
    erlang:apply(LoggerModule, ReportType, [Module, Line, Format, Args]).
    
ensure_loaded(LoggerModule) ->
    case erlang:module_loaded(LoggerModule) of
        true -> ok;
        false -> set_internal(atom_to_list(LoggerModule), info)
    end.

set(Module, LogLevel) when is_atom(Module), is_atom(LogLevel) ->
    set_internal(atom_to_list(Module) ++ "_logger", LogLevel);
    
set(Regexp, LogLevel) when is_list(Regexp), is_atom(LogLevel) ->
    {ok, MP} = re:compile(Regexp),
    [case re:run(atom_to_list(Module), MP) of
        {match,_} ->
            case re:run(atom_to_list(Module), ".*_logger$") of
                {match,_} -> ok;
                _ -> set_internal(atom_to_list(Module) ++ "_logger", LogLevel)
            end;
        _ ->
            ok
     end || Module <- erlang:loaded()].
    
set_internal(ModuleName, LogLevel) when is_list(ModuleName), is_atom(LogLevel) ->
    {Mod,Code} = dynamic_compile:from_string(logger_src(ModuleName, proplists:get_value(LogLevel, ?LOG_LEVELS, 4))),
    code:load_binary(Mod, ModuleName ++ ".erl", Code).

logger_src(ModuleName, LogLevel) when is_integer(LogLevel) ->
 L = integer_to_list(LogLevel),
 "-module(" ++ ModuleName ++ ").

  -export([
         debug_report/3,
         debug_msg/4,
         info_report/3,
         info_msg/4,
         warning_report/3,
         warning_msg/4,
         error_report/3,
         error_msg/4,
         critical_report/3,
         critical_msg/4
    ]).

    debug_report(Module, Line, Args) when " ++ L ++ " >= 5 -> error_logger:info_report([Module, Line, Args]);
    debug_report(_, _, _) -> ok.
    
    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 -> error_logger:info_msg(\"[\~p:\~w] \" ++ Format, [Module,Line|Args]);
    debug_msg(_, _, _, _) -> ok.

    info_report(Module, Line, Args) when " ++ L ++ " >= 4 -> error_logger:info_report([Module, Line, Args]);
    info_report(_, _, _) -> ok.
    
    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 -> error_logger:info_msg(\"[\~p:\~w] \" ++ Format, [Module,Line|Args]);
    info_msg(_, _, _, _) -> ok.
    
    warning_report(Module, Line, Args) when " ++ L ++ " >= 3 -> error_logger:warning_report([Module, Line, Args]);
    warning_report(_, _, _) -> ok.
    
    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 -> error_logger:warning_msg(\"[\~p:\~w] \" ++ Format, [Module,Line|Args]);
    warning_msg(_, _, _, _) -> ok.
    
    error_report(Module, Line, Args) when " ++ L ++ " >= 2 -> error_logger:error_report([Module, Line, Args]);
    error_report(_, _, _) -> ok.
    
    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 -> error_logger:error_msg(\"[\~p:\~w] \" ++ Format, [Module,Line|Args]);
    error_msg(_, _, _, _) -> ok.

    critical_report(Module, Line, Args) when " ++ L ++ " >= 1 -> error_logger:error_report([Module, Line, Args]);
    critical_report(_, _, _) -> ok.
    
    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 -> error_logger:error_msg(\"[\~p:\~w] \" ++ Format, [Module,Line|Args]);
    critical_msg(_, _, _, _) -> ok.
    
    ".
