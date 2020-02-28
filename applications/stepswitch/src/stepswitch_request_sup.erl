%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_request_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([bridge/2]).
-export([local_extension/2]).
-export([originate/2]).
-export([init/1]).
-export([sms/2]).

-include("stepswitch.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, []).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec child_name(kapi_offnet_resource:req()) -> kz_term:ne_binary().
child_name(OffnetReq) ->
    list_to_binary([kapi_offnet_resource:call_id(OffnetReq)
                   ,"-", kz_binary:rand_hex(3)
                   ]).

-spec outbound_child_name(kapi_offnet_resource:req()) -> kz_term:ne_binary().
outbound_child_name(OffnetReq) ->
    list_to_binary([kapi_offnet_resource:outbound_call_id(OffnetReq)
                   ,"-", kz_binary:rand_hex(3)
                   ]).

-spec bridge(stepswitch_resources:endpoints(), kapi_offnet_resource:req()) ->
          kz_types:sup_startchild_ret().
bridge(Endpoints, OffnetReq) ->
    supervisor:start_child(?SERVER
                          ,?WORKER_NAME_ARGS_TYPE(child_name(OffnetReq)
                                                 ,'stepswitch_bridge'
                                                 ,[Endpoints, OffnetReq]
                                                 ,'temporary'
                                                 )
                          ).

-spec local_extension(knm_options:extra_options(), kapi_offnet_resource:req()) ->
          kz_types:sup_startchild_ret().
local_extension(Props, OffnetReq) ->
    supervisor:start_child(?SERVER
                          ,?WORKER_NAME_ARGS_TYPE(child_name(OffnetReq)
                                                 ,'stepswitch_local_extension'
                                                 ,[Props, OffnetReq]
                                                 ,'temporary'
                                                 )
                          ).

-spec originate(stepswitch_resources:endpoints(), kapi_offnet_resource:req()) -> kz_types:sup_startchild_ret().
originate(Endpoints, OffnetReq) ->
    supervisor:start_child(?SERVER
                          ,?WORKER_NAME_ARGS_TYPE(outbound_child_name(OffnetReq)
                                                 ,'stepswitch_originate'
                                                 ,[Endpoints, OffnetReq]
                                                 ,'temporary'
                                                 )
                          ).

-spec sms(stepswitch_resources:endpoints(), kapi_offnet_resource:req()) -> kz_types:sup_startchild_ret().
sms(Endpoints, OffnetReq) ->
    supervisor:start_child(?SERVER
                          ,?WORKER_NAME_ARGS_TYPE(child_name(OffnetReq)
                                                 ,'stepswitch_sms'
                                                 ,[Endpoints, OffnetReq]
                                                 ,'temporary'
                                                 )
                          ).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
