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

-define(NODE_CHILD_TYPE(Type), kz_json:from_list([{<<"type">>, Type}])).
-define(NODE_WORKER, ?NODE_CHILD_TYPE(<<"worker">>)).
-define(NODE_SUPERVISOR, ?NODE_CHILD_TYPE(<<"supervisor">>)).

-define(CHILDREN, kz_json:from_list(
		    [{<<"node">>, ?NODE_WORKER}
		    ,{<<"authn">>, ?NODE_WORKER}
		    ,{<<"channel">>, ?NODE_WORKER}
		    ,{<<"conference">>, ?NODE_WORKER}
		    ,{<<"config">>, ?NODE_WORKER}
		    ,{<<"event_stream_sup">>, ?NODE_SUPERVISOR}
		    ,{<<"msg">>, ?NODE_WORKER}
		    ,{<<"notify">>, ?NODE_WORKER}
            ,{<<"recordings">>, ?NODE_WORKER}
		    ,{<<"resource">>, ?NODE_WORKER}
		    ,{<<"route_sup">>, ?NODE_SUPERVISOR}
		    ])).

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
    Modules = ecallmgr_config:get(<<"modules">>, ?CHILDREN),
    JObj = maybe_correct_modules(Modules),
    Children = kz_json:foldl(fun(Module, V, Acc) ->
                                     Type = kz_json:get_ne_binary_value(<<"type">>, V),
                                     [child_name(NodeB, Args, Module, Type) | Acc]
                             end, [], JObj),
    {'ok', {SupFlags, Children}}.

-spec child_name(binary(), list(), binary(), binary()) -> any().
child_name(NodeB, Args, Module, <<"supervisor">>) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?SUPER_NAME_ARGS(Mod, Name, Args);
child_name(NodeB, Args, Module, <<"worker">>) ->
    Name = kz_util:to_atom(<<NodeB/binary, "_", Module/binary>>, 'true'),
    Mod = kz_util:to_atom(<<"ecallmgr_fs_", Module/binary>>, 'true'),
    ?WORKER_NAME_ARGS(Mod, Name, Args).

-spec srv([{atom(), pid(), any(), any()}], list()) -> api_pid().
srv([], _) -> 'undefined';
srv([{Name, Pid, _, _} | Children], Suffix) ->
    %% FIXME: use lists:suffix
    case lists:prefix(Suffix, lists:reverse(kz_util:to_list(Name))) of
        'true' -> Pid;
        'false' -> srv(Children, Suffix)
    end.

maybe_correct_modules(Modules)
  when is_list(Modules) ->
    FixedModules = [fix_module(Mod) || Mod <- Modules],
    maybe_correct_modules(kz_json:from_list(FixedModules));
maybe_correct_modules(JObj) -> kz_json:merge_jobjs(JObj, ?CHILDREN).

fix_module_type(<<"pus_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"supervisor">>}]);
fix_module_type(_) ->
    kz_json:from_list([{<<"type">>, <<"worker">>}]).

maybe_module_deprecated(<<"route">>) -> <<"route_sup">>;
maybe_module_deprecated(Mod) -> Mod.

fix_module(Mod) ->
    Module = maybe_module_deprecated(Mod),
    ModInv = list_to_binary(lists:reverse(binary_to_list(Module))),
    {Module, fix_module_type(ModInv)}.
