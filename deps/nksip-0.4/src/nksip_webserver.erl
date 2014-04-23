%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkSIP Webserver control

-module(nksip_webserver).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-include("nksip.hrl").

-export([start_server/6, stop_server/4, get_port/3, stop_all/0]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, 
         handle_info/2]).
-export([ranch_start_link/6, do_stop_server/1]).

-type server_ref() :: {nksip:protocol(), inet:ip_address(), inet:port_number()}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Starts a new webserver, or returns a already started one
-spec start_server(nksip:app_id(), tcp|tls|ws|wss, inet:ip_address(), inet:port_number(), 
                   term(), nksip_lib:optslist()) ->
    {ok, pid()} | {error, term()}.

start_server(AppId, Proto, Ip, Port, Disp, Opts) 
        when 
            (Proto==tcp orelse Proto==tls orelse Proto==ws orelse Proto==wss) andalso
            is_list(Disp) andalso is_list(Opts) ->
    case nksip_transport_sup:get_pid(AppId) of
        AppPid when is_pid(AppPid) ->
            case catch cowboy_router:compile(Disp) of
                {'EXIT', _} -> 
                    {error, invalid_dispatch};
                _ ->
                    Ref = {Proto, Ip, Port},
                    gen_server:call(?MODULE, {start, AppId, AppPid, Ref, Disp, Opts})
            end;
        _ ->
            {error, sipapp_not_found}
    end.


%% @doc Stops a started webserver
-spec stop_server(nksip:app_id(), tcp|tls|ws|wss, 
                  inet:ip_address(), inet:port_number()) ->
    ok | {error, in_use} | {error, not_found}.

stop_server(AppId, Proto, Ip, Port) ->
    gen_server:call(?MODULE, {stop, AppId, {Proto, Ip, Port}}).


%% @doc Get the real port of a webserver
-spec get_port(tcp|tls|ws|wws, inet:ip_address(), inet:port_number()) ->
    inet:port_number() | undefined.

get_port(Proto, Ip, Port) ->
    case catch ranch:get_port({Proto, Ip, Port}) of
        Port1 when is_integer(Port1) -> Port1;
        _ -> undefined
    end.


%% @doc Stops all servers
stop_all() ->
    gen_server:cast(?MODULE, stop_all).

    
%% ===================================================================
%% gen_server
%% ===================================================================

-record(server_info, {
    ref :: server_ref(),
    apps = [] :: [nksip:app_id()],
    dispatch = [] :: list(),
    pid :: pid(), 
    mon :: reference()
}).


-record(app_info, {
    index :: {nksip:app_id(), server_ref()}, 
    dispatch = [] :: list(),
    mon :: reference()
}).


-record(state, {
    servers = [] :: [#server_info{}],
    apps = [] :: [#app_info{}]
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
        

%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    {ok, #state{servers=[], apps=[]}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({start, AppId, AppPid, Ref, Disp, Opts}, _From, State) ->
    #state{servers=Servers, apps=Apps} = State,
    case lists:keytake(Ref, #server_info.ref, Servers) of
        false ->
            case do_start_server(Ref, Disp, Opts) of
                {ok, WebPid} ->
                    % We will receive a {webserver_started, _, _} msg
                    App = #app_info{
                        index = {AppId, Ref},
                        dispatch = Disp,
                        mon = erlang:monitor(process, AppPid)
                    },
                    State1 = State#state{apps=[App|Apps]},
                    {reply, {ok, WebPid}, State1};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        {value, #server_info{apps=WebApps, pid=WebPid}=Server, Servers1} ->
            Apps1 = case lists:keytake({AppId, Ref}, #app_info.index, Apps) of
                false -> 
                    App = #app_info{
                        index = {AppId, Ref},
                        dispatch = Disp,
                        mon = erlang:monitor(process, AppPid)
                    },
                    [App|Apps];
                {value, #app_info{}=App, RestApps} ->
                    [App#app_info{dispatch=Disp}|RestApps]
            end,
            case do_update_server(Ref, Apps1) of
                ok ->
                    WebApps1 = nksip_lib:store_value(AppId, WebApps),
                    Server1 = Server#server_info{apps=WebApps1},
                    State1 = State#state{
                        servers = [Server1|Servers1], 
                        apps = Apps1
                    },
                    {reply, {ok, WebPid}, State1};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call({stop, AppId, Ref}, _From, State) ->
    #state{servers=Servers, apps=Apps} = State,
    case lists:keytake(Ref, #server_info.ref, Servers) of
        false ->
            {reply, {error, not_found}, State};
        {value, #server_info{apps=WebApps, mon=WebMon}=Server, Servers1} ->
            Apps1 = case lists:keytake({AppId, Ref}, #app_info.index, Apps) of
                false -> 
                    Apps;
                {value, #app_info{mon=AppMon}, RestApps} -> 
                    erlang:demonitor(AppMon), 
                    RestApps
            end,
            State1 = State#state{apps=Apps1},
            case lists:member(AppId, WebApps) of
                true ->
                    case WebApps -- [AppId] of
                        [] ->
                            erlang:demonitor(WebMon),
                            Reply = do_stop_server(Ref),
                            {reply, Reply, State1#state{servers=Servers1}};
                        WebApps1 ->
                            do_update_server(Ref, Apps1),
                            Server1 = Server#server_info{apps=WebApps1},
                            {reply, ok, State1#state{servers=[Server1|Servers1]}}
                    end;
                false ->
                    {reply, {error, not_found}, State1}
            end
    end;

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.

%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({webserver_started, Ref, WebPid}, State) ->
    #state{apps=Apps, servers=Servers} = State,
    WebApps = [AppId || #app_info{index={AppId, WebRef}} <- Apps, WebRef==Ref],
    Server = #server_info{
        ref = Ref, 
        apps = WebApps, 
        pid = WebPid, 
        mon = erlang:monitor(process, WebPid)
    },
    {noreply, State#state{servers=[Server|Servers]}};

handle_cast(stop_all, State) ->
    nksip_webserver_sup:terminate_all(),
    {noreply, State#state{servers=[]}};

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({'DOWN', MRef, process, _Pid, _Reason}, State) ->
    #state{apps=Apps, servers=Servers} = State,
    case lists:keyfind(MRef, #app_info.mon, Apps) of
        #app_info{index={AppId, Ref}} -> 
            {reply, _, State1} = handle_call({stop, AppId, Ref}, none, State),
            {noreply, State1};
        false ->
            case lists:keytake(MRef, #server_info.mon, Servers) of
                false ->
                    {noreply, State};
                {value, #server_info{ref=Ref}, Servers1} ->
                    lager:warning("Web server ~p has failed!", [Ref]),
                    {noreply, State#state{servers=Servers1}}
            end
    end.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, _State) ->  
    ok.


%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec do_start_server(server_ref(), list(), nksip_lib:optslist()) ->
    {ok, pid()} | {error, term()}.

do_start_server(Ref, Dispatch, Opts) ->
    {Proto, Ip, Port} = Ref,
    Env = {env, [{dispatch, cowboy_router:compile(Dispatch)}]},
    Listeners = nksip_lib:get_value(listeners, Opts, 1), 
    ListenOpts = listen_opts(Proto, Ip, Port, Opts), 
    TransMod = if
        Proto==tcp; Proto==ws -> ranch_tcp;
        Proto==tls; Proto==wss -> ranch_ssl
    end,
    Spec = ranch:child_spec(Ref, Listeners,
                            TransMod, ListenOpts, cowboy_protocol, [Env]),
    % Hack to hook after process creation and use our registry
    {ranch_listener_sup, start_link, StartOpts} = element(2, Spec),
    Spec1 = setelement(2, Spec, {?MODULE, ranch_start_link, StartOpts}),
    nksip_webserver_sup:start_child(Spec1).
 
%% @private
-spec do_stop_server(server_ref()) ->
    ok | {error, term()}.

do_stop_server(Ref) ->
    SupRef = {ranch_listener_sup, Ref},
    nksip_webserver_sup:terminate_child(SupRef).


%% @private
-spec do_update_server(server_ref(), [#app_info{}]) ->
    ok | {error, invalid_dispatch}.

do_update_server(Ref, Apps) ->
    Rules = lists:flatten([
        Disp0 ||
        #app_info{index={_, AppRef}, dispatch=Disp0} <- Apps,
        AppRef == Ref
    ]),
    case get_dispatch(Rules, []) of
        {ok, Compiled} -> cowboy:set_env(Ref, dispatch, Compiled);
        error -> {error, invalid_dispatch}
    end.


%% @private
get_dispatch([], Acc) ->
    case catch cowboy_router:compile(Acc) of
        {'EXIT', _} -> error;
        Compiled -> {ok, Compiled}
    end;

get_dispatch([{Host, Paths}|Rest], Acc) ->
    get_dispatch([{Host, [], Paths}|Rest], Acc);

get_dispatch([{Host, Consts, Paths}|Rest], Acc) ->
    case lists:keytake(Host, 1, Acc) of
        false ->
            get_dispatch(Rest, [{Host, Consts, Paths}|Acc]);
        {value, {_, Consts, PrevPaths}, RestAcc} ->
            get_dispatch(Rest, [{Host, Consts, PrevPaths++Paths}|RestAcc]);
        _ ->
            % We don't allow sharing a server and host with different constraints
            error
    end.


%% @private Gets socket options for listening connections
-spec listen_opts(nksip:protocol(), inet:ip_address(), inet:port_number(), 
                    nksip_lib:optslist()) ->
    nksip_lib:optslist().

listen_opts(ws, Ip, Port, _Opts) ->
    lists:flatten([
        {ip, Ip}, {port, Port}, 
        % {keepalive, true}, 
        case nksip_config:get(max_connections) of
            undefined -> [];
            Max -> {max_connections, Max}
        end
    ]);

listen_opts(wss, Ip, Port, Opts) ->
    case code:priv_dir(nksip) of
        PrivDir when is_list(PrivDir) ->
            DefCert = filename:join(PrivDir, "cert.pem"),
            DefKey = filename:join(PrivDir, "key.pem");
        _ ->
            DefCert = "",
            DefKey = ""
    end,
    Cert = nksip_lib:get_value(certfile, Opts, DefCert),
    Key = nksip_lib:get_value(keyfile, Opts, DefKey),
    lists:flatten([
        {ip, Ip}, {port, Port}, 
        % {keepalive, true}, 
        case Cert of "" -> []; _ -> {certfile, Cert} end,
        case Key of "" -> []; _ -> {keyfile, Key} end,
        case nksip_config:get(max_connections) of
            undefined -> [];
            Max -> {max_connections, Max}
        end
    ]).


%% @private Our version of ranch_listener_sup:start_link/5
-spec ranch_start_link(any(), non_neg_integer(), module(), term(), module(), term())-> 
    {ok, pid()}.

ranch_start_link(Ref, NbAcceptors, RanchTransp, TransOpts, Protocol, [Env]) ->
    case 
        ranch_listener_sup:start_link(Ref, NbAcceptors, RanchTransp, TransOpts, 
                                      Protocol, [Env])
    of
        {ok, Pid} ->
            {Proto, Ip, _} = Ref,
            Port = ranch:get_port(Ref),
            nksip_proc:put({nksip_webserver, {Proto, Ip, Port}}, [], Pid),
            gen_server:cast(?MODULE, {webserver_started, Ref, Pid}),
            {ok, Pid};
        Other ->
            Other
    end.





