%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_renderer).

-behaviour(gen_server).

-include("teletype.hrl").

-export([start_link/2
         ,render/3
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-spec start_link(atom(), ne_binary()) -> startlink_ret().
start_link(RenderId, TemplateId) ->
    gen_server:start_link({'local', RenderId}, ?MODULE, [TemplateId], []).

-spec render(pid(), binary(), wh_proplist()) ->
                    {'ok', iolist()} |
                    {'error', _}.
render(RenderId, Template, TemplateData) ->
    gen_server:call(RenderId, {'render', Template, TemplateData}).

-spec init(ne_binaries()) -> {'ok', atom()}.
init([TemplateId]) ->
    wh_util:put_callid(TemplateId),
    lager:debug("starting template renderer for ~s", [TemplateId]),
    {'ok', template_module(TemplateId)}.

-spec template_module(ne_binary()) -> atom().
template_module(TemplateId) ->
    wh_util:to_atom(<<"template_", TemplateId/binary>>, 'true').

handle_call({'render', Template, TemplateData}, _From, TemplateModule) ->
    lager:debug("trying to compile template for ~p", [_From]),
    try erlydtl:compile_template(Template, TemplateModule, [{'out_dir', 'false'}]) of
        {'ok', TemplateModule} ->
            Resp = render_template(TemplateModule, TemplateData),
            {'reply', Resp, TemplateModule};
        {'ok', TemplateModule, Warnings} ->
            lager:debug("compiling template produced warnings: ~p", [Warnings]),
            lager:debug("template: ~s", [Template]),
            Resp = render_template(TemplateModule, TemplateData),
            {'reply', Resp, TemplateModule};
        {'error', Errors, Warnings} ->
            lager:debug("failed to compile template"),
            lager:debug("errors: ~p", [Errors]),
            lager:debug("warnings: ~p", [Warnings]),
            lager:debug("template: ~s", [Template]),
            {'reply', {'error', 'failed_to_compile'}, TemplateModule}
    catch
        _E:_R ->
            lager:debug("exception compiling template: ~s: ~p", [_E, _R]),
            {'reply', {'error', 'failed_to_compile'}, TemplateModule}
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
                             {'error', _}.
render_template(TemplateModule, TemplateData) ->
    try TemplateModule:render(TemplateData) of
        {'ok', _IOList}=OK ->
            lager:debug("rendered template successfully: '~s'", [_IOList]),
            OK;
        {'error', _E}=E ->
            lager:debug("failed to render template: ~p", [_E]),
            E
    catch
        _E:R -> {'error', R}
    end.
