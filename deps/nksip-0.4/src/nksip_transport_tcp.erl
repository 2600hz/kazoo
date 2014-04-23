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

%% @private TCP/TLS Transport.
%% This module is used for both inbound and outbound TCP and TLS connections.

-module(nksip_transport_tcp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_listener/3, connect/3]).
-export([ranch_start_link/6, start_link/4]).

-include("nksip.hrl").
-include("nksip_call.hrl").

-define(MAX_BUFFER, 64*1024*1024).


%% ===================================================================
%% Private
%% ===================================================================

%% @private Starts a new listening server
-spec get_listener(nksip:app_id(), nksip:transport(), nksip_lib:optslist()) ->
    term().

get_listener(AppId, Transp, Opts) ->
    #transport{proto=Proto, listen_ip=Ip, listen_port=Port} = Transp,
    Listeners = nksip_lib:get_value(listeners, Opts, 1),
    Module = case Proto of
        tcp -> ranch_tcp;
        tls -> ranch_ssl
    end,
    Spec = ranch:child_spec(
        {AppId, Proto, Ip, Port}, 
        Listeners, 
        Module,
        listen_opts(Proto, Ip, Port, Opts), 
        ?MODULE,
        [AppId, Transp, Opts]),
    % Little hack to use our start_link instead of ranch's one
    {ranch_listener_sup, start_link, StartOpts} = element(2, Spec),
    setelement(2, Spec, {?MODULE, ranch_start_link, StartOpts}).

    
%% @private Starts a new connection to a remote server
-spec connect(nksip:app_id(), nksip:transport(), nksip_lib:optslist()) ->
    {ok, term()} | {error, term()}.
         
connect(AppId, Transp, Opts) ->
    #transport{proto=Proto, remote_ip=Ip, remote_port=Port} = Transp,
    SocketOpts = outbound_opts(Proto, Opts),
    {InetMod, TranspMod} = case Proto of
        tcp -> {inet, gen_tcp};
        tls -> {ssl, ssl}
    end,
    case TranspMod:connect(Ip, Port, SocketOpts) of
        {ok, Socket} -> 
            {ok, {LocalIp, LocalPort}} = InetMod:sockname(Socket),
            Transp1 = Transp#transport{
                local_ip = LocalIp,
                local_port = LocalPort,
                remote_ip = Ip,
                remote_port = Port
            },
            Timeout = 1000*nksip_lib:get_value(tcp_timeout, Opts),
            Spec = {
                {AppId, Proto, Ip, Port, make_ref()},
                {nksip_connection, start_link, 
                    [AppId, Transp1, Socket, Timeout]},
                temporary,
                5000,
                worker,
                [?MODULE]
            },
            {ok, Pid} = nksip_transport_sup:add_transport(AppId, Spec),
            TranspMod:controlling_process(Socket, Pid),
            InetMod:setopts(Socket, [{active, once}]),
            ?debug(AppId, <<>>, "~p connected to ~p", [Proto, {Ip, Port}]),
            {ok, Pid, Transp1};
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private Gets socket options for outbound connections
-spec outbound_opts(nksip:protocol(), nksip_lib:optslist()) ->
    nksip_lib:optslist().

outbound_opts(Proto, Opts) when Proto==tcp; Proto==tls ->
    Opts1 = listen_opts(Proto, {0,0,0,0}, 0, Opts),
    [binary|nksip_lib:delete(Opts1, [ip, port, max_connections])].


%% @private Gets socket options for listening connections
-spec listen_opts(nksip:protocol(), inet:ip_address(), inet:port_number(), 
                    nksip_lib:optslist()) ->
    nksip_lib:optslist().

listen_opts(tcp, Ip, Port, _Opts) ->
    lists:flatten([
        {ip, Ip}, {port, Port}, {active, false}, 
        {nodelay, true}, {keepalive, true}, {packet, raw},
        case nksip_config:get(max_connections) of
            undefined -> [];
            Max -> {max_connections, Max}
        end
    ]);

listen_opts(tls, Ip, Port, Opts) ->
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
        {ip, Ip}, {port, Port}, {active, false}, 
        {nodelay, true}, {keepalive, true}, {packet, raw},
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

ranch_start_link(Ref, NbAcceptors, RanchTransp, TransOpts, Protocol, 
                    [AppId, Transp, Opts]) ->
    case 
        ranch_listener_sup:start_link(Ref, NbAcceptors, RanchTransp, TransOpts, 
                                      Protocol, [AppId, Transp, Opts])
    of
        {ok, Pid} ->
            Port = ranch:get_port(Ref),
            Transp1 = Transp#transport{local_port=Port, listen_port=Port},
            nksip_proc:put(nksip_transports, {AppId, Transp1}, Pid),
            nksip_proc:put({nksip_listen, AppId}, Transp1, Pid),
            {ok, Pid};
        Other ->
            Other
    end.
   

%% @private Ranch's callback, called for every new inbound connection
-spec start_link(pid(), port(), atom(), term()) ->
    {ok, pid()}.

start_link(_ListenerPid, Socket, Module, [AppId, Transp, Opts]) ->
    {ok, {LocalIp, LocalPort}} = Module:sockname(Socket),
    {ok, {RemoteIp, RemotePort}} = Module:peername(Socket),
    Transp1 = Transp#transport{
        local_ip = LocalIp,
        local_port = LocalPort,
        remote_ip = RemoteIp,
        remote_port = RemotePort,
        listen_ip = LocalIp,
        listen_port = LocalPort
    },
    Module:setopts(Socket, [{nodelay, true}, {keepalive, true}]),
    Timeout = 1000*nksip_lib:get_value(tcp_timeout, Opts),
    nksip_connection:start_link(AppId, Transp1, Socket, Timeout).


