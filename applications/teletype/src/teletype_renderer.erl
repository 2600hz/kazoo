%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_renderer).
-behaviour(gen_server).

-include("teletype.hrl").

-define(SERVER, ?MODULE).
-define(RENDER_TIMEOUT_MS, ?MILLISECONDS_IN_SECOND * 600).

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

-type state() :: module().

-spec start_link(any()) -> kz_types:startlink_ret().
start_link(Args) ->
    gen_server:start_link(?SERVER, [], [Args]).

-spec render(kz_term:ne_binary(), binary(), kz_term:proplist()) ->
                    {'ok', iolist()} |
                    {'error', any()}.
render(TemplateId, Template, TemplateData) ->
    Renderer = next_renderer(),
    render(Renderer, TemplateId, Template, TemplateData, 3).

-spec render(pid(), kz_term:ne_binary(), binary(), kz_term:proplist(), integer()) ->
                    {'ok', iolist()} |
                    {'error', any()}.

render(Renderer, TemplateId, _Template, _TemplateData, 0) ->
    ?LOG_ERROR("rendering of ~p failed after several tries", [TemplateId]),
    exit(Renderer, 'kill'),
    {'error', 'render_failed'};

render(Renderer, TemplateId, Template, TemplateData, Tries) ->
    Start = kz_time:start_time(),
    PoolStatus = poolboy:status(teletype_farms_sup:render_farm_name()),

    lager:info("starting render of ~p", [TemplateId]),
    case do_render(Renderer, TemplateId, Template, TemplateData) of
        {'error', Reason} ->
            ?LOG_INFO("render failed in ~p, pool: ~p with reason ~p", [kz_time:elapsed_s(Start), PoolStatus, Reason]),
            render(Renderer, TemplateId, Template, TemplateData, Tries-1);
        GoodReturn ->
            lager:info("render completed in ~p, pool: ~p", [kz_time:elapsed_s(Start), PoolStatus]),
            poolboy:checkin(teletype_farms_sup:render_farm_name(), Renderer),
            GoodReturn
    end.

-spec do_render(pid(), kz_term:ne_binary(), binary(), kz_term:proplist()) ->
                       {'ok', iolist()} |
                       {'error', any()}.
do_render(Renderer, TemplateId, Template, TemplateData) ->
    try gen_server:call(Renderer
                       ,{'render', TemplateId, Template, TemplateData}
                       ,?RENDER_TIMEOUT_MS
                       )
    catch
        _E:_R ->
            lager:debug("rendering failed: ~s: ~p", [_E, _R]),
            {'error', 'render_failed'}
    end.

-spec next_renderer() -> pid().
next_renderer() ->
    next_renderer(?MILLISECONDS_IN_SECOND).

-spec next_renderer(pos_integer()) -> pid().
next_renderer(BackoffMs) ->
    Farm = teletype_farms_sup:render_farm_name(),
    try poolboy:checkout(Farm, 'false', 2 * ?MILLISECONDS_IN_SECOND) of
        'full' ->
            ?LOG_CRITICAL("render farm pool is full! waiting ~bms", [BackoffMs]),
            timer:sleep(BackoffMs),
            next_renderer(next_backoff(BackoffMs));
        WorkerPid when is_pid(WorkerPid) -> WorkerPid
    catch
        'exit':{'timeout', {'gen_server', 'call', _Args}} ->
            ?LOG_CRITICAL("render farm overwhelmed!! back off ~b", [BackoffMs]),
            timer:sleep(BackoffMs),
            next_renderer(next_backoff(BackoffMs));
        _E:_R ->
            ?LOG_CRITICAL("failed to checkout: ~s: ~p", [_E, _R]),
            timer:sleep(BackoffMs),
            next_renderer(next_backoff(BackoffMs))
    end.

-spec next_backoff(pos_integer()) -> pos_integer().
next_backoff(BackoffMs) ->
    (BackoffMs * 2) + backoff_fudge().

-spec backoff_fudge() -> pos_integer().
backoff_fudge() ->
    FudgeMs = kapps_config:get_integer(?NOTIFY_CONFIG_CAT, <<"backoff_fudge_ms">>, 5 * ?MILLISECONDS_IN_SECOND),
    rand:uniform(FudgeMs).

-spec init(list()) -> {'ok', atom()}.
init(_) ->
    Self = kz_term:to_hex_binary(list_to_binary(pid_to_list(self()))),
    ModuleBin = <<"teletype_", Self/binary, "_", (kz_binary:rand_hex(4))/binary>>,
    Module = kz_term:to_atom(ModuleBin, 'true'),
    kz_log:put_callid(Module),
    %% ?LOG_DEBUG("starting template renderer, using ~s as compiled module name", [Module]),
    lager:debug("starting template renderer, using ~s as compiled module name", [Module]),
    {'ok', Module}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'render', _TemplateId, Template, TemplateData}, _From, TemplateModule) ->
    %% l?LOG_DEBUG("trying to compile template ~s as ~s for ~w", [_TemplateId, TemplateModule, _From]),
    lager:debug("trying to compile template ~s as ~s for ~w", [_TemplateId, TemplateModule, _From]),
    {'reply'
    ,kz_template:render(Template, TemplateModule, TemplateData)
    ,TemplateModule
    ,'hibernate'
    };
handle_call(_Req, _From, TemplateModule) ->
    {'noreply', TemplateModule}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Req, TemplateModule) ->
    {'noreply', TemplateModule}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Msg, TemplateModule) ->
    {'noreply', TemplateModule}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _TemplateModule) ->
    lager:debug("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Old, TemplateModule, _Extra) ->
    {'ok', TemplateModule}.
