%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_request_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([bridge/2]).
-export([local_extension/2]).
-export([originate/2]).
-export([init/1]).

-include("stepswitch.hrl").

-define(CHILDREN, []).

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

bridge(Endpoints, JObj) -> 
    Name = <<(wh_json:get_value(<<"Call-ID">>, JObj))/binary
             ,"-", (wh_util:rand_hex_binary(3))/binary>>,
    supervisor:start_child(?MODULE
                           ,?WORKER_NAME_ARGS_TYPE(Name
                                                   ,'stepswitch_bridge'
                                                   ,[Endpoints, JObj]
                                                   ,'temporary')).

local_extension(Props, JObj) ->
    Name = <<(wh_json:get_value(<<"Call-ID">>, JObj))/binary
             ,"-", (wh_util:rand_hex_binary(3))/binary>>,
    supervisor:start_child(?MODULE
                           ,?WORKER_NAME_ARGS_TYPE(Name
                                                   ,'stepswitch_local_extension'
                                                   ,[Props, JObj]
                                                   ,'temporary')).

originate(Endpoints, JObj) ->
    Name = <<(wh_json:get_value(<<"Outbound-Call-ID">>, JObj))/binary
             ,"-", (wh_util:rand_hex_binary(3))/binary>>,
    supervisor:start_child(?MODULE
                           ,?WORKER_NAME_ARGS_TYPE(Name
                                                   ,'stepswitch_originate'
                                                   ,[Endpoints, JObj]
                                                   ,'temporary')).

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
