%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(fax_sup).

-behaviour(supervisor).

-include("fax.hrl").

-export([start_link/0]).
-export([cache_proc/0]).
-export([listener_proc/0]).
-export([smtp_sessions/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(POOL(N),  {N, {'poolboy', 'start_link', [[{'name', {'local', N}}
                                                  ,{'worker_module', 'fax_worker'}
                                                  ,{'size', whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 5)}
                                                  ,{'max_overflow', 0}
                                                 ]]}
                   ,'permanent', 5000, 'worker', ['poolboy']}).

-define(ORIGIN_BINDINGS, [[{'db', ?WH_FAXES}, {'type', <<"faxbox">>}]]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(CHILDREN, [?POOL('fax_worker_pool')
                   ,?CACHE_ARGS(?FAX_CACHE, ?CACHE_PROPS)
                   ,?WORKER('fax_jobs')
                   ,?SUPER('fax_requests_sup')
                   ,?SUPER('fax_xmpp_sup')
                   ,?WORKER('fax_shared_listener')
                   ,?WORKER('fax_listener')
                   ,?WORKER_ARGS('gen_smtp_server',['fax_smtp'
                                                    ,[[{'port', whapps_config:get_integer(?CONFIG_CAT, <<"smtp_port">>, 19025)}]]
                                                   ])
                  ]).

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

-spec cache_proc() -> {'ok', ?FAX_CACHE}.
cache_proc() ->
    {'ok', ?FAX_CACHE}.

-spec listener_proc() -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= 'fax_listener'],
    {'ok', P}.

-spec smtp_sessions() -> any().
smtp_sessions() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= 'gen_smtp_server'],
    Sessions = gen_smtp_server:sessions(P),
    length(Sessions).

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
