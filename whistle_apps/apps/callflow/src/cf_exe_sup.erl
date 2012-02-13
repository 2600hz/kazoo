%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  27 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_exe_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include("callflow.hrl").

%% API
-export([start_link/0, new/4]).
-export([workers/0]).
-export([show_calls/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Restart, Shutdown, Type),
        {Name, {Name, start_link, []}, Restart, Shutdown, Type, [Name]}).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec new/4 :: (wh_json:json_object(), ne_binary(), ne_binary(), #cf_call{}) -> sup_startchild_ret().
new(Flow, ControlQ, CallId, Call) ->
    supervisor:start_child(?MODULE, [Flow, ControlQ, CallId, Call]).

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [_]} <- supervisor:which_children(?MODULE)].


show_calls() ->
    do_show_calls(workers()).

do_show_calls([]) ->
    ok;
do_show_calls([Srv|T]) ->
    case catch(cf_exe:get_call_info(Srv)) of
        {ok, Call} ->
            io:format("CF_EXE(~p): ~p~n", [Srv, cf_util:call_to_proplist(Call)]),
            do_show_calls(T);            
        _ -> 
            do_show_calls(T)
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
-spec init(Args) -> sup_init_ret() when
      Args :: [].
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(cf_exe, temporary, 2000, worker)]}}.
