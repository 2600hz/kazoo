%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created :  Tue, 07 Dec 2010 19:26:22 GMT: James Aimonetti <james@2600hz.org>
-module(crossbar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start, [Args]}, permanent, 5000, Type, dynamic}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = ?CHILD(webmachine_mochiweb, worker, WebConfig),
    BindingServer = ?CHILD(crossbar_bindings, worker),
    SessionServer = ?CHILD(crossbar_session, worker),
    Processes = [Web
		 ,BindingServer
		 ,SessionServer
		], %% Put list of ?CHILD(crossbar_server, worker) or ?CHILD(crossbar_other_sup, supervisor)
    {ok, { {one_for_one, 10, 10}, Processes} }.
