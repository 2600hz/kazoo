%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is main application supervisor module
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_sup).

-behaviour(supervisor).

-include("circlemaker.hrl").

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(POOL(N),  {N, {'poolboy', 'start_link', [[{'name', {'local', N}}
                                                  ,{'worker_module', 'cm_worker'}
                                                  ,{'size', whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 5)}
                                                  ,{'max_overflow', 0}
                                                ]]}
                   ,'permanent', 5000, 'worker', ['poolboy']}).

-define(CHILDREN, [?WORKER('cm_init')
                   ,?POOL(?WORKER_POOL)
                   ,?WORKER('cm_pool_mgr')
                   ,?WORKER('cm_listener')
                   ,?WORKER('cm_udp_svr')]).

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
    wh_util:set_startup(),
    _ = init_sysconfig_circlemaker_doc(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

%% Default AAA document (it's not empty but AAA is not enabled,
%% so this kind of the document description is helpful for editing)
-define(DEFAULT_AAA_DOCUMENT,
    [
        {<<"aaa_mode">>, <<"off">>},
        {<<"servers">>, [wh_json:from_list([
            {<<"enabled">>, 'false'},
            {<<"name">>, <<"unique_server_name">>},
            {<<"address">>, <<"127.0.0.1">>},
            {<<"port">>, 1812},
            {<<"secret">>, <<"secret_phrase">>},
            {<<"aaa_engine">>, <<"radius">>},
            {<<"dicts">>, [
                <<"dictionary_3gpp">>, <<"dictionary">>
            ]},
            {<<"avp">>, <<"strict">>},
            {<<"retries">>, 3},
            {<<"timeout">>, 5000}
        ])]},
        {<<"authentication">>, [<<"unique_server_name">>]},
        {<<"authorization">>, [<<"unique_server_name">>]},
        {<<"accounting">>, [<<"unique_server_name">>]},
        {<<"workers">>, 5},
        {<<"nas_address">>, <<"127.0.0.1">>},
        {<<"nas_port">>, 1812},
        {<<"authz_apps">>, [<<"jonny5">>]}
    ]
).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply default 'circlemaker' document to the system_config DB, if it isn't exist.
%% @end
%%--------------------------------------------------------------------
-spec init_sysconfig_circlemaker_doc() -> 'ok'.
init_sysconfig_circlemaker_doc() ->
    lists:foreach(fun({Key, Value}) -> whapps_config:get(?APP_NAME, Key, Value) end, ?DEFAULT_AAA_DOCUMENT).
