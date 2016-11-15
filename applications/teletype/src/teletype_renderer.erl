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
                        ,?MILLISECONDS_IN_HOUR
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
            next_renderer(BackoffMs * 2 + backoff_fudge());
        P -> P
    catch
        'exit':{'timeout', {'gen_server', 'call', _Args}} ->
            lager:critical("render farm overwhelmed!! back off ~b", [BackoffMs]),
            timer:sleep(BackoffMs),
            next_renderer(BackoffMs * 2 + backoff_fudge());
        _E:_R ->
            lager:warning("failed to checkout: ~s: ~p", [_E, _R]),
            timer:sleep(BackoffMs),
            next_renderer(BackoffMs * 2 + backoff_fudge())
    end.

-spec backoff_fudge() -> pos_integer().
backoff_fudge() ->
    Fudge = whapps_config:get_integer(?NOTIFY_CONFIG_CAT, <<"backoff_fudge_ms">>, 5000),
    random:uniform(Fudge).

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
    {'reply'
     ,kz_template:render(Template, TemplateModule, TemplateData)
     ,TemplateModule
     ,'hibernate'
    };
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
