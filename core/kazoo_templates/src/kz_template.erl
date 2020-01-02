%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_template).

-include("kazoo_template.hrl").

-export([compile/2, compile/3
        ,render/2, render_with_options/3
        ,render/3, render/4, render/5
        ]).

-define(COMPILE_OPTS(Options)
       ,[{'out_dir', 'false'}
        ,'return'
        ,'report'
         | Options
        ]).

-type template() :: nonempty_string() | kz_term:ne_binary().

-type template_result() :: {'ok', iolist() | atom()} |
                           {'error', any()}.

%% copied from erlydtl.erl
-type position() :: non_neg_integer().
-type location() :: 'none' | position() | {Line::position(), Column::position()}.
-type info() :: {location()
                ,Module::atom()
                ,ErrorDesc::term()
                }.

-type error_info() :: {File::list()
                      ,[info()]
                      }.
-type errors() :: list(error_info()).
-type warnings() :: list(error_info()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec render(atom(), kz_term:proplist()) -> template_result().
render(Module, TemplateData) ->
    render_with_options(Module, TemplateData, []).

-spec render_with_options(atom(), kz_term:proplist(), kz_term:proplist()) -> template_result().
render_with_options(Module, TemplateData, RenderOpts) ->
    render_template(Module, TemplateData, RenderOpts).

-spec render(template(), atom(), kz_term:proplist()) -> template_result().
render(Template, Module, TemplateData) ->
    render(Template, Module, TemplateData, []).

-spec render(template(), atom(), kz_term:proplist(), kz_term:proplist()) -> template_result().
render(Template, Module, TemplateData, CompileOpts) ->
    render(Template, Module, TemplateData, CompileOpts, []).

-spec render(template(), atom(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> template_result().
render(Template, Module, TemplateData, CompileOpts, RenderOpts) ->
    case compile(Template, Module, CompileOpts) of
        {'ok', Module} -> render_template(Module, TemplateData, RenderOpts);
        {'error', _} = Error -> Error
    end.


-spec compile(template(), atom()) -> template_result().
compile(Template, Module) ->
    compile(Template, Module, []).

-spec compile(template(), atom(), kz_term:proplist()) -> template_result().
compile(Template, Module, CompileOpts) when is_binary(Template) ->
    try erlydtl:compile_template(Template, Module, ?COMPILE_OPTS(CompileOpts)) of
        Result ->
            handle_compile_result(Template, Module, Result)
    catch
        _E:_R ->
            ?LOG_DEBUG("exception compiling template: ~s: ~p", [_E, _R]),
            {'error', 'failed_to_compile'}
    end;
compile(Path, Module, CompileOpts) ->
    try erlydtl:compile_file(Path, Module, ?COMPILE_OPTS(CompileOpts)) of
        Result ->
            handle_compile_result(Path, Module, Result)
    catch
        _E:_R ->
            ?LOG_DEBUG("exception compiling template: ~s: ~p", [_E, _R]),
            {'error', 'failed_to_compile'}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec render_template(atom(), kz_term:proplist(), kz_term:proplist()) -> template_result().
render_template(Module, TemplateData, RenderOpts) ->
    lager:debug("rendering using ~s", [Module]),
    try Module:render(props:filter_empty(TemplateData), RenderOpts) of
        {'ok', _IOList}=OK ->
            lager:debug("rendered template successfully"),
            OK;
        {'error', _E}=E ->
            ?LOG_DEBUG("failed to render template: ~p", [_E]),
            E
    catch
        ?STACKTRACE('error', 'undef', ST)
        ?LOG_DEBUG("something in the template ~s is undefined", [Module]),
        kz_log:log_stacktrace(ST),
        {'error', 'undefined'};
        ?STACKTRACE(_E, R, ST)
        ?LOG_DEBUG("crashed rendering template ~s: ~s: ~p", [Module, _E, R]),
        kz_log:log_stacktrace(ST),
        {'error', R}
        end.

%%%=============================================================================
%%% Log functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_compile_result(template(), atom(), template_result()) ->
          template_result().
handle_compile_result(_Template, Module, {'ok', Module} = OK) ->
    lager:debug("built renderer for ~p", [Module]),
    OK;
handle_compile_result(_Template, Module, {'ok', Module, []}) ->
    lager:debug("built renderer for ~p", [Module]),
    {'ok', Module};
handle_compile_result(Template, Module, {'ok', Module, Warnings}) ->
    ?LOG_DEBUG("compiling template renderer for ~p produced warnings: ~p"
              ,[Module, Warnings]
              ),
    log_warnings(Warnings, Template),
    {'ok', Module};
handle_compile_result(_Template, _Module, 'error') ->
    ?LOG_DEBUG("failed to compile template for ~p", [_Module]),
    {'error', 'failed_to_compile'};
handle_compile_result(Template, _Module, {'error', Errors, Warnings}) ->
    ?LOG_DEBUG("failed to compile template for ~p", [_Module]),
    log_errors(Errors, Template),
    log_warnings(Warnings, Template),
    {'error', 'failed_to_compile'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec log_errors(errors(), template()) -> 'ok'.
log_errors(Es, Template) ->
    _ = [log_infos("error", Module, Errors, Template) || {Module, Errors} <- Es],
    'ok'.

-spec log_warnings(warnings(), template()) -> 'ok'.
log_warnings(Ws, Template) ->
    _ = [log_infos("warning", Module, Warnings, Template) || {Module, Warnings} <- Ws],
    'ok'.

-spec log_infos(string(), string(), [info()], template()) -> 'ok'.
log_infos(Type, Module, Errors, Template) ->
    ?LOG_INFO("~s in module ~s", [Type, Module]),
    lists:foreach(fun (Error) -> catch log_info(Error, Template, Module) end, Errors).

-spec log_info(info(), template(), atom()) -> 'ok'.
log_info({{Row, Column}, _ErlydtlModule, Msg}, Template, Module) when not is_binary(Template) ->
    ?LOG_INFO("~s:~b:~b: ~s", [Module, Row, Column, Msg]);
log_info({Line, _ErlydtlModule, Msg}, Template, Module) when not is_binary(Template) ->
    ?LOG_INFO("~s:~p: ~s", [Module, Line, Msg]);
log_info({{Row, Column}, _ErlydtlModule, Msg}, Template, Module) ->
    Rows = binary:split(Template, <<"\n">>, ['global']),
    %% Is ErlyDTL count lines as 0-based or 1-based?
    MaybeRow = case Row of
                   0 -> 1;
                   _ -> Row
               end,
    %% Is ErlyDTL count lines as 0-based or 1-based?
    ErrorRow = lists:nth(MaybeRow, Rows),

    %% This tends to crash silently, don't use it:
    %% <<Pre:Column/binary, Rest/binary>> = ErrorRow,
    %% ?LOG_INFO("~s:~b:~b: error msg: '~p'; pre: '~s'; Rest: '~s'"
    %%          ,[Module, Row, Column, Msg, Pre, Rest]
    %%          );

    ?LOG_INFO("~s:~b:~b (row: '~p'): ~p"
             ,[Module, Row, Column, ErrorRow, Msg]
             );
log_info({Line, _ErlydtlModule, Msg}, Template, Module) ->
    Rows = binary:split(Template, <<"\n">>, ['global']),
    %% Is ErlyDTL count lines as 0-based or 1-based?
    MaybeRow = case Line of
                   0 -> 1;
                   _ -> Line
               end,
    %% Is ErlyDTL count lines as 0-based or 1-based?
    ErrorRow = lists:nth(MaybeRow, Rows),
    ?LOG_INFO("~s:~p (row: '~p'): ~p", [Module, Line, ErrorRow, Msg]).
