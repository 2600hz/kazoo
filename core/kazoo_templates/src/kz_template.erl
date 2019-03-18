%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_template).

-include("kazoo_template.hrl").

-export([compile/2, compile/3,
         render/2, render/3, render/4
        ]).

-define(COMPILE_OPTS(Options)
       ,[{'out_dir', 'false'}
        ,'return'
        ,'report'
         | Options
        ]).

-type template() :: nonempty_string() | kz_term:ne_binary().

-type template_result() :: {'ok', iolist() | atom()} |
                           'ok' |
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
    render_template(Module, TemplateData).

-spec render(template(), atom(), kz_term:proplist()) -> template_result().
render(Template, Module, TemplateData) ->
    render(Template, Module, TemplateData, []).

-spec render(template(), atom(), kz_term:proplist(), kz_term:proplist()) -> template_result().
render(Template, Module, TemplateData, CompileOpts) ->
    case compile(Template, Module, CompileOpts) of
        {'ok', Module} -> render_template(Module, TemplateData);
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
-spec render_template(atom(), kz_term:proplist()) -> template_result().
render_template(Module, TemplateData) ->
    lager:debug("rendering using ~s", [Module]),
    try Module:render(props:filter_empty(TemplateData)) of
        {'ok', _IOList}=OK ->
            lager:debug("rendered template successfully"),
            OK;
        {'error', _E}=E ->
            ?LOG_DEBUG("failed to render template: ~p", [_E]),
            E
    catch
        'error':'undef':ST ->
            ?LOG_DEBUG("something in the template ~s is undefined", [Module]),
            kz_util:log_stacktrace(ST),
            {'error', 'undefined'};
        _E:R:ST ->
            ?LOG_DEBUG("crashed rendering template ~s: ~s: ~p", [Module, _E, R]),
            kz_util:log_stacktrace(ST),
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
              ,[Module, Warnings]),
    log_warnings(Warnings, Template),
    {'ok', Module};
handle_compile_result(_Template, Module, 'ok') ->
    lager:debug("build renderer for ~p from template file", [Module]),
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
    lists:foreach(fun (Error) -> catch log_info(Error, Template) end, Errors).

-spec log_info(info(), template()) -> 'ok'.
log_info(_, Template) when not is_binary(Template) -> 'ok';
log_info({{Row, Column}, _ErlydtlModule, Msg}, Template) ->
    Rows = binary:split(Template, <<"\n">>, ['global']),
    ErrorRow = lists:nth(Row + 1, Rows),
    <<Pre:Column/binary, Rest/binary>> = ErrorRow,
    ?LOG_INFO("~p: '~s' '~s'", [Msg, Pre, Rest]);
log_info({Line, _ErlydtlModule, Msg}, Template) ->
    Rows = binary:split(Template, <<"\n">>, ['global']),
    ErrorRow = lists:nth(Line + 1, Rows),
    ?LOG_INFO("~p on line ~p: ~s", [Msg, Line, ErrorRow]).
