%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(lineman_toolbag_sup).

-behaviour(supervisor).

-include("lineman.hrl").

-export([start_link/0]).
-export([reset_all/0]).
-export([set_parameter/3]).
-export([prepare/3]).
-export([execute/3]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type), fun(N, cache) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).
-define(CHILDREN, [{lineman_tool_freeswitch, worker}]). %%, {lineman_tool_couchdb, worker}]).

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

-spec reset_all/0 :: () -> 'ok'.
reset_all() ->
    [P ! reset || {_, P, _, _} <- supervisor:which_children(?MODULE)],
    ok.

-spec set_parameter/3 :: (text(), string(), xml_el()) -> term().
set_parameter(Tool, Name, Xml) ->
    {ok, Mod} = maybe_get_tool(Tool),
    Mod:set_parameter(Name, Xml).

-spec prepare/3 :: (text(), xml_el(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare(Tool, Xml, Workorder) ->
    {ok, Mod} = maybe_get_tool(Tool),
    Mod:prepare(Xml, Workorder).

-spec execute/3 :: (text(), xml_el(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(Tool, Xml, Workorder) ->
    {ok, Mod} = maybe_get_tool(Tool),
    Mod:execute(Xml, Workorder).


-spec maybe_get_tool/1 :: (text()) -> {'ok', atom()} | {'error', 'not_found' | 'load_failed'}.
maybe_get_tool(Tool) ->
    ModBin = list_to_binary(["lineman_tool_", Tool]),
    try wh_util:to_atom(ModBin) of
        Mod -> {ok, Mod}
    catch
        error:badarg ->
            case code:where_is_file(wh_util:to_list(<<ModBin/binary, ".beam">>)) of
                non_existing -> 
                    lager:debug("tool ~s not found", [ModBin]),
                    {error, not_found};
                _Path ->
                    wh_util:to_atom(ModBin, true),
                    maybe_get_tool(ModBin)
            end;
        _T:_R ->
            lager:debug("failed to load tool ~s", [ModBin]),
            {error, load_failed}
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
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
