%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_node_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/2]).

-export([node_srv/1
         ,authn_srv/1
         ,route_srv/1
         ,channel_srv/1
         ,config_srv/1
         ,resource_srv/1
         ,notify_srv/1
         ,authz_srv/1
         ,cdr_srv/1
         ,conference_srv/1
         ,event_stream_sup/1
        ]).

-export([init/1]).

-include("ecallmgr.hrl").

-define(CHILD(Name, Mod, Args), fun('ecallmgr_fs_event_stream_sup' = N, M, A) ->
                                        {N, {M, 'start_link', A}, 'permanent', 'infinity', 'supervisor', [N]};
                                   (N, M, A) ->
                                        {N, {M, 'start_link', A}, 'permanent', 6000, 'worker', [N]}
                                end(Name, Mod, Args)).
-define(CHILDREN, [<<"_node">>, <<"_authn">>, <<"_route">>, <<"_channel">>
                   ,<<"_config">>, <<"_resource">>, <<"_notify">>
                   ,<<"_conference">>, <<"_event_stream_sup">>
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
-spec start_link(atom(), wh_proplist()) -> startlink_ret().
start_link(Node, Options) -> supervisor:start_link({'local', Node}, ?MODULE, [Node, Options]).

node_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "edon_").

authn_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "nhtua_").

route_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "etuor_").

channel_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "lennahc_").

config_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "gifnoc_").

resource_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "ecruoser_").

notify_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "yfiton_").

authz_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "zhtua_").

cdr_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "rdc_").

conference_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "ecnerefnoc_").

event_stream_sup(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "pus_maerts_tneve_").

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
-spec init(list()) -> sup_init_ret().
init([Node, Options]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 6,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    NodeB = wh_util:to_binary(Node),
    Children = [ begin
                     Name = wh_util:to_atom(<<NodeB/binary, H/binary>>, 'true'),
                     Mod = wh_util:to_atom(<<"ecallmgr_fs", H/binary>>, 'true'),
                     lager:debug("starting handler ~s", [Name]),
                     ?CHILD(Name, Mod, [Node, Options])
                 end
                 || H <- ?CHILDREN
               ],

    {'ok', {SupFlags, Children}}.

-spec srv([{atom(), pid(), _, _},...] | [], list()) -> api_pid().
srv([], _) -> 'undefined';
srv([{Name, Pid, _, _} | Children], Suffix) ->
    case lists:prefix(Suffix, lists:reverse(wh_util:to_list(Name))) of
        'true' -> Pid;
        'false' -> srv(Children, Suffix)
    end.
