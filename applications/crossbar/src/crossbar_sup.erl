%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([upgrade/0]).
-export([child_spec/1]).
-export([find_proc/1]).
-export([init/1]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'type', kz_notification:pvt_type()}]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(CHILDREN, [?WORKER('crossbar_init')
                   ,?SUPER('crossbar_module_sup')
                   ,?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                   ,?WORKER('crossbar_cleanup')
                   ,?WORKER('crossbar_bindings')
                  ]
       ).

-define(DISPATCH_FILE, [code:lib_dir('crossbar', 'priv'), "/dispatch.conf"]).
-define(DEFAULT_LOG_DIR, kz_term:to_binary(code:lib_dir('crossbar', 'log'))).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec child_spec(atom()) -> ?WORKER(atom()).
child_spec(Mod) ->
    ?WORKER(Mod).

-spec find_proc(atom()) -> pid().
find_proc(Mod) ->
    [P] = [P || {Mod1, P, _, _} <- supervisor:which_children(?SERVER), Mod =:= Mod1],
    P.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec upgrade() -> 'ok'.
upgrade() ->
    {'ok', {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?SERVER)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    lists:foreach(fun (Id) ->
                          _ = supervisor:terminate_child(?SERVER, Id),
                          supervisor:delete_child(?SERVER, Id)
                  end, sets:to_list(Kill)),
    lists:foreach(fun(Spec) -> supervisor:start_child(?SERVER, Spec) end, Specs),
    'ok'.

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
-spec init(any()) -> sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    {'ok', {{'one_for_one', 10, 10}, ?CHILDREN}}.
