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

%% API
-export([start_link/0]).
-export([start_printer/1, stop_printer/1]).
-export([printers/0]).

%% Supervisor callbacks
-export([init/1]).

-define(XMPP_PRINTER(PrinterId) ,?WORKER_NAME_ARGS('fax_xmpp',wh_util:to_atom(PrinterId, 'true'),[PrinterId])).

-define(CHILDREN, [?SUPER('exmpp_sup')]).

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

-spec start_printer(ne_binary()) -> sup_startchild_ret().
start_printer(PrinterId) ->
    supervisor:start_child(?MODULE, ?XMPP_PRINTER(PrinterId)).

-spec stop_printer(ne_binary()) -> 'ok'.
stop_printer(PrinterId) ->
    _ = [begin
             supervisor:terminate_child(?MODULE, Id),
             supervisor:delete_child(?MODULE, Id)
         end
         || {Id, _Pid, 'worker', [_]} <- supervisor:which_children(?MODULE),
            (Id == wh_util:to_atom(PrinterId, 'true'))
        ],
    'ok'.

-spec printers() -> [{ne_binary(), pid()},...].
printers() ->
    [ {Id, Pid} || {Id, Pid, 'worker', [_]} <- supervisor:which_children(?MODULE)].

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
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
