%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ci_parsers_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/2
	,stop_child/1
	,children/0
        ]).

-include("call_inspector.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, []).

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

-type parser() :: 'ci_parser_freeswitch' | 'ci_parser_kamailio' | 'ci_parser_hep'.
-spec start_child(parser(), [{'parser_args',any(),any()} | {'parser_args',any(),any(),any()}]) ->
                         {'ok', atom()}.
start_child(Module, Args) ->
    Id = ci_parsers_util:make_name(lists:keyfind('parser_args', 1, Args)),
    ChildSpec = ?WORKER_NAME_ARGS(Module, Id, Args),
    case supervisor:start_child(?SERVER, ChildSpec) of
        {'ok', _Pid} -> {'ok', Id};
        {'error', {'already_started', _Pid}} -> {'ok', Id}
    end.

-spec stop_child(atom()) -> 'ok'.
stop_child(Id) ->
    'ok' = supervisor:terminate_child(?SERVER, Id),
    'ok' = supervisor:delete_child(?SERVER, Id).

-spec children() -> [atom()].
children() ->
    [Id
     || {Id, _Pid, _Type, _Modules} <- supervisor:which_children(?SERVER)
    ].

%% Internals

%% End of Module.
