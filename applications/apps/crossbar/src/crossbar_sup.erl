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
-define(DISPATCH_FILE, [code:lib_dir(crossbar, priv), "/dispatch.conf"]).
-define(WEBMACHINE_CONF, [code:lib_dir(crossbar, priv), "/webmachine.conf"]).
-define(LOG_DIR, code:lib_dir(crossbar, log)).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
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

init([]) ->
    {ok, Dispatch} = file:consult(?DISPATCH_FILE),
    {ok, Configuration} = file:consult(?WEBMACHINE_CONF),
    WebConfig = case props:get_value(ssl, Configuration, false) of
                    true ->
                        [{ip, props:get_value(ip, Configuration, "0.0.0.0")}
                         ,{port, props:get_value(port, Configuration, "8000")}
                         ,{name, props:get_value(name, Configuration, "crossbar")}
                         ,{log_dir,  props:get_value(log_dir, Configuration, ?LOG_DIR)}
                         ,{dispatch, Dispatch}
                         ,{ssl, true}
                         ,{ssl_opts, [
                                       {certfile, props:get_value("certfile", Configuration, "server_cert.pem")}
                                      ,{keyfile, props:get_value("keyfile", Configuration, "server_key.pem")}
                                     ]}];
                    false ->
                        [{ip, props:get_value(ip, Configuration, "0.0.0.0")},
                         {port, props:get_value(port, Configuration, "8000")},
                         {name, props:get_value(name, Configuration, "crossbar")},
                         {log_dir,  props:get_value(log_dir, Configuration, ?LOG_DIR)},
                         {dispatch, Dispatch}]
                end,
    logger:format_log(info, "Starting webmachine ~p", [WebConfig]),
    Web = ?CHILD(webmachine_mochiweb, worker, WebConfig),
    ModuleSup = ?CHILD(crossbar_module_sup, supervisor),
    BindingServer = ?CHILD(crossbar_bindings, worker),
    SessionServer = ?CHILD(crossbar_session, worker),
    Processes = [
		 Web
		 ,BindingServer
		 ,SessionServer
		 ,ModuleSup
		], %% Put list of ?CHILD(crossbar_server, worker) or ?CHILD(crossbar_other_sup, supervisor)
    {ok, { {one_for_one, 10, 10}, Processes} }.
