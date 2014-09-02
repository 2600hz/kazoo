%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnipresence_sup).

-behaviour(supervisor).

-export([start_link/0
         ,subscriptions_srv/0
         ,get_appid/3
        ]).
-export([init/1]).

-include("omnipresence.hrl").

-define(SIP_APP, <<"omni">>).

-define(SUBS_ETS_OPTS, [{'table_id', omnip_subscriptions:table_id()}
                        ,{'table_options', omnip_subscriptions:table_config()}
                        ,{'find_me_function', fun ?MODULE:subscriptions_srv/0}
                       ]).



-define(PROXYSUPER(I,A), {A, {I, 'start_link', [A,[]]}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(APPID(AppName, Module, Opts), get_appid(AppName, Module, Opts)).
-define(PROXYOPTIONS,
         [
        {plugins, [nksip_gruu, nksip_event_compositor, nksip_registrar, nksip_trace]}                      
        ,{transports, [{udp, all, 5090}, {tls, all, 5091}]}
        ,{from, "sip:kazoo@sipproxy-04.90e9.com"}
        ,{log_level, info}
        ,{event_expires, 3600}
        ,{event_expires_offset, 10}
        ,{events, "dialog, message-summary, presence, presence.winfo, call-info, sla, line-seize, vq-rtcpxr"}
        ,{allow, "ACK,OPTIONS,SUBSCRIBE,PUBLISH,NOTIFY"}
        ]        
         ).
-define(PROXY,?APPID(?SIP_APP,'omnipresence_proxy', ?PROXYOPTIONS)).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_NAME_ARGS('kazoo_etsmgr_srv', 'omnipresence_subscriptions_tbl', [?SUBS_ETS_OPTS])
                   ,?WORKER('omnip_subscriptions')
                   ,?WORKER('omnipresence_listener')
                   ,?WORKER('omnipresence_shared_listener')
%                   ,?PROXYSUPER('nksip_sipapp_sup',?PROXY)
                   ,?SUPER('omnipresence_pkg_sup')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

get_appid(AppName, Module, Opts) ->
    Config = nksip_config_cache:app_config(),
    Opts1 = Config ++ [{name, AppName}, {module, Module}|Opts],
    case nksip_sipapp_config:start(Opts1) of
        {ok, AppId} -> AppId;
        {error, Error} -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec subscriptions_srv() -> pid() | 'undefined'.
subscriptions_srv() ->
    case [P || {_, P, 'worker', ['omnip_subscriptions']} <- supervisor:which_children(?MODULE)] of
        [] -> 'undefined';
        [Pid] -> Pid
    end.

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
