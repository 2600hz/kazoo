%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_renderer).

-behaviour(gen_server).

-include("teletype.hrl").

-define(SERVER, ?MODULE).

-export([start_link/1
        ,render/3
        ]).

-export([log_errors/2
        ,log_warnings/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

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

-spec start_link(any()) -> startlink_ret().
start_link(Args) ->
    gen_server:start_link(?SERVER, [], [Args]).

-spec render(ne_binary(), binary(), kz_proplist()) ->
                    {'ok', iolist()} |
                    {'error', any()}.
render(TemplateId, Template, TemplateData) ->
    Renderer = next_renderer(),
    try gen_server:call(Renderer
                       ,{'render', TemplateId, Template, TemplateData}
                       ,?MILLISECONDS_IN_HOUR
                       )
    catch
        _E:_R ->
            lager:debug("rendering failed: ~s: ~p", [_E, _R]),
            {'error', 'render_failed'}
    after
        poolboy:checkin(teletype_sup:render_farm_name(), Renderer)
    end.

-spec next_renderer() -> pid().
-spec next_renderer(pos_integer()) -> pid().
next_renderer() ->
    next_renderer(?MILLISECONDS_IN_SECOND).

next_renderer(BackoffMs) ->
    try poolboy:checkout(teletype_sup:render_farm_name()
                        ,'false'
                        ,2 * ?MILLISECONDS_IN_SECOND
                        )
    of
        'full' ->
            lager:critical("render farm pool is full! waiting ~bms", [BackoffMs]),
            timer:sleep(BackoffMs),
            next_renderer(next_backoff(BackoffMs));
        WorkerPid when is_pid(WorkerPid) -> WorkerPid
    catch
        'exit':{'timeout', {'gen_server', 'call', _Args}} ->
            lager:critical("render farm overwhelmed!! back off ~b", [BackoffMs]),
            timer:sleep(BackoffMs),
            next_renderer(next_backoff(BackoffMs));
        _E:_R ->
            lager:warning("failed to checkout: ~s: ~p", [_E, _R]),
            timer:sleep(BackoffMs),
            next_renderer(next_backoff(BackoffMs))
    end.

-spec next_backoff(pos_integer()) -> pos_integer().
next_backoff(BackoffMs) ->
    BackoffMs * 2 + backoff_fudge().

-spec backoff_fudge() -> pos_integer().
backoff_fudge() ->
    Fudge = kapps_config:get_integer(?NOTIFY_CONFIG_CAT, <<"backoff_fudge_ms">>, 5000),
    random:uniform(Fudge).

-spec init(list()) -> {'ok', atom()}.
init(_) ->
    Self = kz_util:to_hex_binary(list_to_binary(pid_to_list(self()))),

    Module = kz_util:to_atom(
               list_to_binary(["teletype_", Self, "_", kz_util:rand_hex_binary(4)])
                            ,'true'
              ),
    kz_util:put_callid(Module),
    lager:debug("starting template renderer, using ~s as compiled module name", [Module]),

    {'ok', Module}.

handle_call({'render', _TemplateId, Template, TemplateData}, _From, TemplateModule) ->
    lager:debug("trying to compile template ~s as ~s for ~p", [_TemplateId, TemplateModule, _From]),
    try erlydtl:compile_template(Template
                                ,TemplateModule
                                ,[{'out_dir', 'false'}
                                 ,'return'
                                 ]
                                )
    of
        {'ok', TemplateModule} ->
            {'reply'
            ,render_template(TemplateModule, TemplateData)
            ,TemplateModule
            ,'hibernate'
            };
        {'ok', TemplateModule, []} ->
            {'reply'
            ,render_template(TemplateModule, TemplateData)
            ,TemplateModule
            ,'hibernate'
            };
        {'ok', TemplateModule, Warnings} ->
            log_warnings(Warnings, Template),
            {'reply'
            ,render_template(TemplateModule, TemplateData)
            ,TemplateModule
            ,'hibernate'
            };
        'error' ->
            lager:debug("failed to compile template"),
            {'reply'
            ,{'error', 'failed_to_compile'}
            ,TemplateModule
            ,'hibernate'
            };
        {'error', Errors, Warnings} ->
            lager:debug("failed to compile template"),
            log_errors(Errors, Template),
            log_warnings(Warnings, Template),
            {'reply'
            ,{'error', 'failed_to_compile'}
            ,TemplateModule
            ,'hibernate'
            }
    catch
        _E:_R ->
            lager:debug("exception compiling template: ~s: ~p", [_E, _R]),
            {'reply'
            ,{'error', 'failed_to_compile'}
            ,TemplateModule
            ,'hibernate'
            }
    end;
handle_call(_Req, _From, TemplateModule) ->
    {'noreply', TemplateModule}.

handle_cast(_Req, TemplateModule) ->
    {'noreply', TemplateModule}.

handle_info(_Msg, TemplateModule) ->
    {'noreply', TemplateModule}.

terminate(_Reason, _TemplateModule) ->
    lager:debug("terminating: ~p", [_Reason]).

code_change(_Old, TemplateModule, _Extra) ->
    {'ok', TemplateModule}.

-spec render_template(atom(), kz_proplist()) ->
                             {'ok', iolist()} |
                             {'error', any()}.
render_template(TemplateModule, TemplateData) ->
    try TemplateModule:render(TemplateData) of
        {'ok', _IOList}=OK ->
            lager:debug("rendered template successfully: '~s'", [_IOList]),
            OK;
        {'error', _E}=E ->
            lager:debug("failed to render template: ~p", [_E]),
            E
    catch
        'error':'undef' ->
            ST = erlang:get_stacktrace(),
            lager:debug("something in the template ~s is undefined", [TemplateModule]),
            kz_util:log_stacktrace(ST),
            {'error', 'undefined'};
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("crashed rendering template ~s: ~s: ~p", [TemplateModule, _E, R]),
            kz_util:log_stacktrace(ST),
            {'error', R}
    end.

-spec log_errors(errors(), binary()) -> 'ok'.
log_errors(Es, Template) ->
    _ = [log_infos("error", Module, Errors, Template) || {Module, Errors} <- Es],
    'ok'.

-spec log_warnings(warnings(), binary()) -> 'ok'.
log_warnings(Ws, Template) ->
    _ = [log_infos("warning", Module, Warnings, Template) || {Module, Warnings} <- Ws],
    'ok'.

-spec log_infos(string(), string(), [info()], binary()) -> 'ok'.
log_infos(Type, Module, Errors, Template) ->
    lager:info("~s in module ~s", [Type, Module]),
    _ = [catch log_info(Error, Template) || Error <- Errors],
    'ok'.

-spec log_info(info(), binary()) -> 'ok'.
log_info({{Row, Column}, _ErlydtlModule, Msg}, Template) ->
    Rows = binary:split(Template, <<"\n">>, ['global']),
    ErrorRow = lists:nth(Row+1, Rows),
    <<Pre:Column/binary, Rest/binary>> = ErrorRow,
    lager:info("~p: '~s' '~s'", [Msg, Pre, Rest]);
log_info({Line, _ErlydtlModule, Msg}, Template) ->
    Rows = binary:split(Template, <<"\n">>, ['global']),
    ErrorRow = lists:nth(Line+1, Rows),
    lager:info("~p on line ~p: ~s", [Msg, Line, ErrorRow]).
