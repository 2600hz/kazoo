%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start, [Args]}, permanent, 5000, Type, dynamic}).
-define(DISPATCH_FILE, [code:lib_dir(crossbar, priv), "/dispatch.conf"]).
-define(DEFAULT_LOG_DIR, wh_util:to_binary(code:lib_dir(crossbar, log))).

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec upgrade/0 :: () -> ok.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    lists:foreach(fun (Id) ->
			  _ = supervisor:terminate_child(?MODULE, Id),
			  supervisor:delete_child(?MODULE, Id)
		  end, sets:to_list(Kill)),
    lists:foreach(fun(Spec) -> supervisor:start_child(?MODULE, Spec) end, Specs),
    ok.

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
    {ok, Dispatch} = file:consult(?DISPATCH_FILE),
    IP = whapps_config:get_list(<<"crossbar">>, <<"ip">>, <<"0.0.0.0">>),
    Port = whapps_config:get_list(<<"crossbar">>, <<"port">>, <<"8000">>),
    Name = whapps_config:get_list(<<"crossbar">>, <<"service_name">>, <<"crossbar">>),
    WebConfig = [{ip, IP}
                 ,{port, Port}
                 ,{name, Name}
                 ,{dispatch, Dispatch}
                 ,{log_dir, whapps_config:get_list(<<"crossbar">>, <<"log_dir">>, ?DEFAULT_LOG_DIR)}
                 ,{ssl, whapps_config:get_is_true(<<"crossbar">>, <<"ssl">>, false)}
                 ,{ssl_opts, wh_json:to_proplist(whapps_config:get(<<"crossbar">>, <<"ssl_opts">>, wh_json:new()))}],
    ?LOG("starting webmachine ~s:~s as ~s", [IP, Port, Name]),
    Web = ?CHILD(webmachine_mochiweb, worker, WebConfig),
    ModuleSup = ?CHILD(crossbar_module_sup, supervisor),
    BindingServer = ?CHILD(crossbar_bindings, worker),
    {ok, { {one_for_one, 10, 10}, [Web, BindingServer, ModuleSup]} }.
