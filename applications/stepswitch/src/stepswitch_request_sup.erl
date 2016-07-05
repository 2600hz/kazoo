%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
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
-export([sms/2]).

-include("stepswitch.hrl").

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

-spec child_name(kapi_offnet_resource:req()) -> ne_binary().
child_name(OffnetReq) ->
    <<(kapi_offnet_resource:call_id(OffnetReq))/binary
      ,"-", (kz_util:rand_hex_binary(3))/binary
    >>.

-spec outbound_child_name(kapi_offnet_resource:req()) -> ne_binary().
outbound_child_name(OffnetReq) ->
    <<(kapi_offnet_resource:outbound_call_id(OffnetReq))/binary
      ,"-", (kz_util:rand_hex_binary(3))/binary
    >>.

-spec bridge(kz_json:objects(), kapi_offnet_resource:req()) -> sup_startchild_ret().
bridge(Endpoints, OffnetReq) ->
    supervisor:start_child(?SERVER
			  ,?WORKER_NAME_ARGS_TYPE(child_name(OffnetReq)
						 ,'stepswitch_bridge'
						 ,[Endpoints, OffnetReq]
						 ,'temporary'
						 )
                          ).

-spec local_extension(knm_number_options:extra_options(), kapi_offnet_resource:req()) ->
                             sup_startchild_ret().
local_extension(Props, OffnetReq) ->
    supervisor:start_child(?SERVER
			  ,?WORKER_NAME_ARGS_TYPE(child_name(OffnetReq)
						 ,'stepswitch_local_extension'
						 ,[Props, OffnetReq]
						 ,'temporary'
						 )
                          ).

-spec originate(kz_json:objects(), kapi_offnet_resource:req()) -> sup_startchild_ret().
originate(Endpoints, OffnetReq) ->
    supervisor:start_child(?SERVER
			  ,?WORKER_NAME_ARGS_TYPE(outbound_child_name(OffnetReq)
						 ,'stepswitch_originate'
						 ,[Endpoints, OffnetReq]
						 ,'temporary'
						 )
                          ).

-spec sms(kz_json:objects(), kapi_offnet_resource:req()) -> sup_startchild_ret().
sms(Endpoints, OffnetReq) ->
    supervisor:start_child(?SERVER
			  ,?WORKER_NAME_ARGS_TYPE(child_name(OffnetReq)
						 ,'stepswitch_sms'
						 ,[Endpoints, OffnetReq]
						 ,'temporary'
						 )
                          ).

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
