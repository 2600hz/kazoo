%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(webhooks_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, cache_proc/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec cache_proc() -> {'ok', pid()}.
cache_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= webhooks_cache],
    {ok, P}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CACHE(webhooks_cache)
                                  ,?CHILD(hook_acct_sup, supervisor)
                                  ,?CHILD(hook_req_sup, supervisor)
                                  ,?CHILD(webhooks_listener, worker) % listens for config changes to webhook docs
                                 ]} }.
