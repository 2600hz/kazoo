%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_renderer).

-behaviour(gen_server).

-include("teletype.hrl").

-export([start_link/1
         ,render/3
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-spec start_link(any()) -> startlink_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, [], [Args]).

-spec render(ne_binary(), binary(), wh_proplist()) ->
                    {'ok', iolist()} |
                    {'error', any()}.
render(TemplateId, Template, TemplateData) ->
    Renderer = next_renderer(),
    try gen_server:call(Renderer
                        ,{'render', TemplateId, Template, TemplateData}
                       )
    of
        Resp -> Resp
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
            next_renderer(BackoffMs * 2);
        P -> P
    catch
        _E:_R ->
            lager:warning("failed to checkout: ~s: ~p", [_E, _R]),
            throw({'error', 'no_worker'})
    end.

-spec init(list()) -> {'ok', atom()}.
init(_) ->
    Self = wh_util:to_hex_binary(list_to_binary(pid_to_list(self()))),

    Module = wh_util:to_atom(
               list_to_binary(["teletype_", Self, "_", wh_util:rand_hex_binary(4)])
               ,'true'
              ),
    wh_util:put_callid(Module),
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
            lager:debug("compiling template produced warnings: ~p", [Warnings]),
            lager:debug("template: ~s", [Template]),

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
            log_errors(Errors),
            log_warnings(Warnings),
            lager:debug("template: ~s", [Template]),
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

-spec render_template(atom(), wh_proplist()) ->
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
            wh_util:log_stacktrace(ST),
            {'error', 'undefined'};
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("crashed rendering template ~s: ~s: ~p", [TemplateModule, _E, R]),
            wh_util:log_stacktrace(ST),
            {'error', R}
    end.

log_errors([]) -> 'ok';
log_errors(Es) ->
    lager:debug("errors:"),
    log_error_infos(Es).

log_warnings([]) -> 'ok';
log_warnings(Ws) ->
    lager:debug("warnings:"),
    log_error_infos(Ws).

log_error_infos([]) -> 'ok';
log_error_infos([{_File, Info}|Infos]) ->
    _ = [log_error_info(EI) || EI <- Info],
    log_error_infos(Infos).

log_error_info({Location, _Module, Desc}) ->
    LocationLog = location_to_log(Location),
    lager:debug("  at ~s: ~s", [LocationLog, Desc]).

-spec location_to_log('non' | integer() | {integer(), integer()}) -> iolist().
location_to_log('none') -> "pos:none";
location_to_log({Line, Col}) ->
    ["pos:{", wh_util:to_list(Line), ",", wh_util:to_list(Col), "}"];
location_to_log(Pos) ->
    ["pos:", wh_util:to_list(Pos)].
