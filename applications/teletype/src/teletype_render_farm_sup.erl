%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_render_farm_sup).

-behaviour(supervisor).

-export([start_link/0
         ,render/3
        ]).
-export([init/1]).

-include("teletype.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_TYPE('teletype_renderer', 'temporary')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec render(ne_binary(), ne_binary(), wh_proplist()) ->
                    'ok' |
                    {'error', _}.
render(TemplateId, Template, TemplateData) ->
    {'ok', Renderer} = renderer(),
    lager:debug("found renderer for '~s': ~p", [TemplateId, Renderer]),
    teletype_renderer:render(Renderer, Template, TemplateData).

-spec renderer() -> {'ok', api_pid()}.
renderer() ->
    try poolboy:checkout(teletype_sup:render_farm_name(), 'false', 2000) of
        'full' ->
            lager:critical("render farm pool is full!"),
            timer:sleep(1000),
            renderer();
        P -> {'ok', P}
    catch
        _E:_R ->
            lager:warning("failed to checkout: ~s: ~p", [_E, _R]),
            {'ok', 'undefined'}
    end.


-spec start_renderer(atom(), ne_binary()) -> sup_startchild_ret().
start_renderer(RenderId, TemplateId) ->
    supervisor:start_child(?MODULE, [RenderId, TemplateId]).

-spec render_id(ne_binary()) -> atom().
render_id(TemplateId) ->
    wh_util:to_atom(<<"renderer_", TemplateId/binary>>, 'true').

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
