%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_xmpp_sup).

-behaviour(supervisor).

-include("fax.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([start_printer/1, stop_printer/1]).
-export([printers/0]).

%% Supervisor callbacks
-export([init/1]).

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

-spec start_printer(ne_binary()) -> sup_startchild_ret().
start_printer(PrinterId) ->
    Name = kz_util:to_atom(PrinterId, 'true'),
    ChildSpec = ?WORKER_NAME_ARGS('fax_xmpp', Name, [PrinterId]),
    supervisor:start_child(?SERVER, ChildSpec).

-spec stop_printer(ne_binary()) -> 'ok'.
stop_printer(PrinterId) ->
    _ = [begin
             _ = supervisor:terminate_child(?SERVER, Id),
             supervisor:delete_child(?SERVER, Id)
         end
         || {Id, _Pid, 'worker', [_]} <- supervisor:which_children(?SERVER),
            Id == kz_util:to_atom(PrinterId, 'true')
        ],
    'ok'.

-spec printers() -> [{ne_binary(), pid()},...].
printers() ->
    [ {Id, Pid} || {Id, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER)].

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
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
