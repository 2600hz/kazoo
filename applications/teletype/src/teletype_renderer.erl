%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_renderer).
-behaviour(gen_server).

-include("teletype.hrl").

-define(SERVER, ?MODULE).
-define(RENDER_TIMEOUT, 1000 * 60 * 10).

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

-spec start_link(any()) -> startlink_ret().
start_link(Args) ->
    gen_server:start_link(?SERVER, [], [Args]).

-spec render(ne_binary(), binary(), kz_proplist()) ->
                    {'ok', iolist()} |
                    {'error', any()}.
render(TemplateId, Template, TemplateData) ->
    Renderer = next_renderer(),
    render(Renderer, TemplateId, Template, TemplateData, 3).

-spec render(pid(), ne_binary(), binary(), kz_proplist(), integer()) ->
                    {'ok', iolist()} |
                    {'error', any()}.

render(Renderer, TemplateId, _Template, _TemplateData, 0) ->
    lager:error("rendering of ~p failed after several tries", [TemplateId]),
    exit(Renderer, 'kill'),
    {'error', 'render_failed'};

render(Renderer, TemplateId, Template, TemplateData, Tries) ->
    Start = kz_time:now_s(),
    PoolStatus = poolboy:status(teletype_sup:render_farm_name()),
    lager:info("starting render of ~p", [TemplateId]),
    case do_render(Renderer, TemplateId, Template, TemplateData) of
        {'error', 'render_failed'} ->
            lager:info("render failed in ~p, pool: ~p", [kz_time:now_s() - Start, PoolStatus]),
            render(Renderer, TemplateId, Template, TemplateData, Tries-1);
        GoodReturn ->
            lager:info("render completed in ~p, pool: ~p", [kz_time:now_s() - Start, PoolStatus]),
            poolboy:checkin(teletype_sup:render_farm_name(), Renderer),
            GoodReturn
    end.

-spec do_render(pid(), ne_binary(), binary(), kz_proplist()) ->
                       {'ok', iolist()} |
                       {'error', any()}.
do_render(Renderer, TemplateId, Template, TemplateData) ->
    try gen_server:call(Renderer
                       ,{'render', TemplateId, Template, TemplateData}
                       ,?RENDER_TIMEOUT
                       )
    catch
        _E:_R ->
            lager:debug("rendering failed: ~s: ~p", [_E, _R]),
            {'error', 'render_failed'}
    end.

-spec next_renderer() -> pid().
-spec next_renderer(pos_integer()) -> pid().
next_renderer() ->
    next_renderer(?MILLISECONDS_IN_SECOND).

next_renderer(BackoffMs) ->
    Farm = teletype_sup:render_farm_name(),
    try poolboy:checkout(Farm, false, 2 * ?MILLISECONDS_IN_SECOND) of
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
    rand:uniform(Fudge).

-spec init(list()) -> {'ok', atom()}.
init(_) ->
    Self = kz_term:to_hex_binary(list_to_binary(pid_to_list(self()))),
    ModuleBin = <<"teletype_", Self/binary, "_", (kz_binary:rand_hex(4))/binary>>,
    Module = kz_term:to_atom(ModuleBin, true),
    kz_util:put_callid(Module),
    lager:debug("starting template renderer, using ~s as compiled module name", [Module]),
    {'ok', Module}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'render', _TemplateId, Template, TemplateData}, _From, TemplateModule) ->
    lager:debug("trying to compile template ~s as ~s for ~p", [_TemplateId, TemplateModule, _From]),
    {'reply'
    ,kz_template:render(Template, TemplateModule, TemplateData)
    ,TemplateModule
    ,'hibernate'
    };
handle_call(_Req, _From, TemplateModule) ->
    {'noreply', TemplateModule}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Req, TemplateModule) ->
    {'noreply', TemplateModule}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Msg, TemplateModule) ->
    {'noreply', TemplateModule}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _TemplateModule) ->
    lager:debug("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_Old, TemplateModule, _Extra) ->
    {'ok', TemplateModule}.
