%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
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
        ,msg_srv/1
        ]).

-export([init/1]).

-include("ecallmgr.hrl").

-define(CHILDREN, [<<"authn">>
                  ,<<"channel">>
                  ,<<"conference">>
                  ,<<"config">>
                  ,<<"event_stream_sup">>
                  ,<<"msg">>
                  ,<<"node">>
                  ,<<"notify">>
                  ,<<"resource">>
                  ,<<"route">>
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node, Options) ->
    supervisor:start_link({'local', Node}, ?MODULE, [Node, Options]).

-spec node_srv(pid()) -> api_pid().
node_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "edon_").

-spec authn_srv(pid()) -> api_pid().
authn_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "nhtua_").

-spec route_sup_srv(pid()) -> api_pid().
route_sup_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "pus_etuor_").

-spec route_srv(pid()) -> api_pid().
route_srv(Supervisor) ->
    srv(supervisor:which_children(route_sup_srv(Supervisor)), "etuor_").

-spec channel_srv(pid()) -> api_pid().
channel_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "lennahc_").

-spec config_srv(pid()) -> api_pid().
config_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "gifnoc_").

-spec resource_srv(pid()) -> api_pid().
resource_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "ecruoser_").

-spec notify_srv(pid()) -> api_pid().
notify_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "yfiton_").

-spec authz_srv(pid()) -> api_pid().
authz_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "zhtua_").

-spec cdr_srv(pid()) -> api_pid().
cdr_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "rdc_").

-spec conference_srv(pid()) -> api_pid().
conference_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "ecnerefnoc_").

-spec event_stream_sup(pid()) -> api_pid().
event_stream_sup(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "pus_maerts_tneve_").

-spec msg_srv(pid()) -> api_pid().
msg_srv(Supervisor) ->
    srv(supervisor:which_children(Supervisor), "gsm_").

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

    NodeB = kz_util:to_binary(Node),
    Args = [Node, Options],
    Children = [child_name(NodeB, Args, H)
                || H <- ecallmgr_config:get(<<"modules">>, ?CHILDREN)
               ],

    {'ok', {SupFlags, Children}}.

-spec child_name(binary(), list(), binary() | tuple() | kz_json:object()) ->
                        supervisor:child_spec().
child_name(NodeB, Args, {<<"supervisor">>, Module}) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?SUPER_NAME_ARGS(Mod, Name, Args);
child_name(NodeB, Args, {<<"worker">>, Module}) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?WORKER_NAME_ARGS(Mod, Name, Args);
child_name(NodeB, Args, <<"event_stream_sup">>=Module) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?SUPER_NAME_ARGS(Mod, Name, Args);
child_name(NodeB, Args, <<"route">>) ->
    child_name(NodeB, Args, <<"route_sup">>);
child_name(NodeB, Args, <<"route_sup">>=Module) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?SUPER_NAME_ARGS(Mod, Name, Args);
child_name(NodeB, Args, <<_/binary>>=Module) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?WORKER_NAME_ARGS(Mod, Name, Args);
child_name(NodeB, Args, MaybeJObj) ->
    try kz_json:get_values(MaybeJObj) of
        {[Module], [Type]} -> child_name(NodeB, Args, {Type, Module})
    catch
        _E:_R ->
            lager:error("error determining child type : ~p : ~p", [MaybeJObj, Args])
    end.


-spec srv([{atom(), pid(), any(), any()}], list()) -> api_pid().
srv([], _) -> 'undefined';
srv([{Name, Pid, _, _} | Children], Suffix) ->
    %% FIXME: use lists:suffix
    case lists:prefix(Suffix, lists:reverse(kz_util:to_list(Name))) of
        'true' -> Pid;
        'false' -> srv(Children, Suffix)
    end.
