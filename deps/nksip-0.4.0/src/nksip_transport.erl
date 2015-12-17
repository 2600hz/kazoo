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

%% @doc NkSIP Transport control module
-module(nksip_transport).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_all/0, get_all/1, get_listening/3, get_connected/2, get_connected/5]).
-export([is_local/2, is_local_ip/1]).
-export([start_transport/5, default_port/1]).
-export([get_listenhost/3, make_route/6]).
-export([send/4]).
-export([get_all_connected/0, get_all_connected/1, stop_all_connected/0]).

-export_type([transport/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").

-compile({no_auto_import,[get/1]}).


%% ===================================================================
%% Types
%% ===================================================================

-type transport() :: #transport{}.

-type connection() :: 
    {nksip:protocol(), inet:ip_address(), inet:port_number(), binary()}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Gets all registered transports in all SipApps.
-spec get_all() -> 
    [{nksip:app_id(), transport(), pid()}].

get_all() ->
    All = [{AppId, Transp, Pid} 
            || {{AppId, Transp}, Pid} <- nksip_proc:values(nksip_transports)],
    lists:sort(All).


%% @doc Gets all registered transports for a SipApp.
-spec get_all(nksip:app_id()) -> 
    [{transport(), pid()}].

get_all(AppId) ->
    [{Transp, Pid} || {A, Transp, Pid} <- get_all(), AppId==A].


%% @private Finds a listening transport of Proto.
-spec get_listening(nksip:app_id(), nksip:protocol(), ipv4|ipv6) -> 
    [{transport(), pid()}].

get_listening(AppId, Proto, Class) ->
    Fun = fun({#transport{proto=TProto, listen_ip=TListen}, _}) -> 
        case TProto==Proto of
            true ->
                case Class of
                    ipv4 when size(TListen)==4 -> true;
                    ipv6 when size(TListen)==8 -> true;
                    _ -> false
                end;
            false ->
                false
        end
    end,
    lists:filter(Fun, nksip_proc:values({nksip_listen, AppId})).


%% @private Finds a listening transport of Proto
-spec get_connected(nksip:app_id(), nksip:transport()|undefined) ->
    [{nksip_transport:transport(), pid()}].

get_connected(AppId, Transp) ->
    case Transp of
        #transport{proto=Proto, remote_ip=Ip, remote_port=Port, resource=Res} ->
            get_connected(AppId, Proto, Ip, Port, Res);
        _ ->
            []
    end.


%% @private Finds a listening transport of Proto
-spec get_connected(nksip:app_id(), nksip:protocol(), 
                    inet:ip_address(), inet:port_number(), binary()) ->
    [{nksip_transport:transport(), pid()}].

get_connected(AppId, Proto, Ip, Port, Res) ->
    nksip_proc:values({nksip_connection, {AppId, Proto, Ip, Port, Res}}).


%% @doc Checks if an `nksip:uri()' or `nksip:via()' refers to a local started transport.
-spec is_local(nksip:app_id(), Input::nksip:uri()|nksip:via()) -> 
    boolean().

is_local(AppId, #uri{}=Uri) ->
    Listen = [
        {Proto, Ip, Port, Res} ||
        {#transport{proto=Proto, listen_ip=Ip, listen_port=Port, resource=Res}, _Pid} 
        <- nksip_proc:values({nksip_listen, AppId})
    ],
    is_local(Listen, nksip_dns:resolve(Uri), nksip_config_cache:local_ips());

is_local(AppId, #via{}=Via) ->
    {Proto, Host, Port} = nksip_parse:transport(Via),
    Transp = {<<"transport">>, nksip_lib:to_binary(Proto)},
    Uri = #uri{domain=Host, port=Port, opts=[Transp]},
    is_local(AppId, Uri).


%% @private
is_local(Listen, [{Proto, Ip, Port, Res}|Rest], LocalIps) -> 
    case lists:member(Ip, LocalIps) of
        true ->
            case lists:member({Proto, Ip, Port, Res}, Listen) of
                true ->
                    true;
                false ->
                    case 
                        is_tuple(Ip) andalso size(Ip)==4 andalso
                        lists:member({Proto, {0,0,0,0}, Port, Res}, Listen) 
                    of
                        true -> 
                            true;
                        false -> 
                            case 
                                is_tuple(Ip) andalso size(Ip)==8 andalso
                                lists:member({Proto, {0,0,0,0,0,0,0,0}, Port, Res}, Listen) 
                            of
                                true -> true;
                                false -> is_local(Listen, Rest, LocalIps)
                            end
                    end
            end;
        false ->
            is_local(Listen, Rest, LocalIps)
    end;

is_local(_, [], _) ->
    false.


%% @doc Checks if an IP is local to this node.
-spec is_local_ip(inet:ip_address()) -> 
    boolean().

is_local_ip({0,0,0,0}) ->
    true;
is_local_ip({0,0,0,0,0,0,0,0}) ->
    true;
is_local_ip(Ip) ->
    lists:member(Ip, nksip_config_cache:local_ips()).


%% @doc Start a new listening transport.
%% Opts should have the transport options ++ SipApp configuration
-spec start_transport(nksip:app_id(), nksip:protocol(), inet:ip_address(), 
                      inet:port_number(), nksip:optslist()) ->
    {ok, pid()} | {error, term()}.

start_transport(AppId, Proto, Ip, Port, Opts) ->
    Class = case size(Ip) of 4 -> ipv4; 8 -> ipv6 end,
    Listening = [
        {{LIp, LPort}, Pid} || 
            {#transport{listen_ip=LIp, listen_port=LPort}, Pid} 
            <- get_listening(AppId, Proto, Class)
    ],
    case nksip_lib:get_value({Ip, Port}, Listening) of
        undefined -> 
            Transp = #transport{
                proto = Proto,
                local_ip = Ip, 
                local_port = Port,
                listen_ip = Ip,
                listen_port = Port,
                remote_ip = {0,0,0,0},
                remote_port = 0
            },
            Spec = case Proto of
                udp -> nksip_transport_udp:get_listener(AppId, Transp, Opts);
                tcp -> nksip_transport_tcp:get_listener(AppId, Transp, Opts);
                tls -> nksip_transport_tcp:get_listener(AppId, Transp, Opts);
                sctp -> nksip_transport_sctp:get_listener(AppId, Transp, Opts);
                ws -> nksip_transport_ws:get_listener(AppId, Transp, Opts);
                wss -> nksip_transport_ws:get_listener(AppId, Transp, Opts)
            end,
            nksip_transport_sup:add_transport(AppId, Spec);
        Pid when is_pid(Pid) -> 
            {ok, Pid}
    end.



%% @private 
-spec get_listenhost(nksip:app_id(), inet:ip_address(), nksip:optslist()) ->
    binary().

get_listenhost(AppId, Ip, Opts) ->
    case size(Ip) of
        4 ->
            Host = case nksip_lib:get_value(local_host, Opts) of
                undefined -> AppId:config_local_host();
                Host0 -> Host0
            end,
            case Host of
                auto when Ip == {0,0,0,0} -> 
                    nksip_lib:to_host(nksip_config_cache:main_ip()); 
                auto ->
                    nksip_lib:to_host(Ip);
                _ -> 
                    Host
            end;
        8 ->
            Host = case nksip_lib:get_value(local_host6, Opts) of
                undefined -> AppId:config_local_host6();
                Host0 -> Host0
            end,
            case Host of
                auto when Ip == {0,0,0,0,0,0,0,0} -> 
                    nksip_lib:to_host(nksip_config_cache:main_ip6(), true);
                auto -> 
                    nksip_lib:to_host(Ip, true);
                _ -> 
                    Host
            end
    end.

    
%% @private Makes a route record
-spec make_route(nksip:scheme(), nksip:protocol(), binary(), inet:port_number(),
                 binary(), nksip:optslist()) ->
    #uri{}.

make_route(Scheme, Proto, ListenHost, Port, User, Opts) ->
    UriOpts = case Proto of
        tls when Scheme==sips -> Opts;
        udp when Scheme==sip -> Opts;
        _ -> [{<<"transport">>, nksip_lib:to_binary(Proto)}|Opts] 
    end,
    #uri{
        scheme = Scheme,
        user = User,
        domain = ListenHost,
        port = Port,
        opts = UriOpts
    }.




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
-spec send(nksip:app_id(), [TSpec], function(), nksip:optslist()) ->
    {ok, nksip:request()|nksip:response()} | error
    when TSpec :: #uri{} | connection() | {current, connection()} | 
                  {flow, {pid(), nksip:transport()}}.

send(AppId, [#uri{}=Uri|Rest], MakeMsg, Opts) ->
    Resolv = nksip_dns:resolve(Uri),
    ?call_debug("Transport send to ~p (~p)", [Resolv, Rest]),
    send(AppId, Resolv++Rest, MakeMsg, [{transport_uri, Uri}|Opts]);

send(AppId, [{current, {udp, Ip, Port, Res}}|Rest], MakeMsg, Opts) ->
    send(AppId, [{udp, Ip, Port, Res}|Rest], MakeMsg, Opts);

send(AppId, [{current, {Proto, Ip, Port, Res}=D}|Rest], MakeMsg, Opts) ->
    ?call_debug("Transport send to current ~p (~p)", [D, Rest]),
    case get_connected(AppId, Proto, Ip, Port, Res) of
        [{Transp, Pid}|_] -> 
            SipMsg = MakeMsg(Transp),
            case nksip_connection:send(Pid, SipMsg) of
                ok -> 
                    {ok, SipMsg};
                {error, _Error} -> 
                    send(AppId, Rest, MakeMsg, Opts)
            end;
        [] ->
            send(AppId, Rest, MakeMsg, Opts)
    end;

send(AppId, [{flow, {Pid, Transp}=D}|Rest], MakeMsg, Opts) ->
    ?call_debug("Transport send to flow ~p (~p)", [D, Rest]),
    SipMsg = MakeMsg(Transp),
    case nksip_connection:send(Pid, SipMsg) of
        ok -> 
            {ok, SipMsg};
        {error, _} -> 
            send(AppId, Rest, MakeMsg, Opts)
    end;

send(AppId, [{Proto, Ip, 0, Res}|Rest], MakeMsg, Opts) ->
    send(AppId, [{Proto, Ip, default_port(Proto), Res}|Rest], MakeMsg, Opts);

send(AppId, [{Proto, Ip, Port, Res}=D|Rest], MakeMsg, Opts) ->
    case get_connected(AppId, Proto, Ip, Port, Res) of
        [{Transp, Pid}|_] -> 
            ?call_debug("Transport send to connected ~p (~p)", [D, Rest]),
            SipMsg = MakeMsg(Transp),
            case nksip_connection:send(Pid, SipMsg) of
                ok -> 
                    {ok, SipMsg};
                {error, udp_too_large} ->
                    send(AppId, [{tcp, Ip, Port, Res}|Rest], MakeMsg, Opts);
                {error, _} -> 
                    send(AppId, Rest, MakeMsg, Opts)
            end;
        [] ->
            ?call_debug("Transport send to new ~p (~p)", [D, Rest]),
            case connect(AppId, Proto, Ip, Port, Res, Opts) of
                {ok, Pid, Transp} ->
                    SipMsg = MakeMsg(Transp),
                    case nksip_connection:send(Pid, SipMsg) of
                        ok -> 
                            {ok, SipMsg};
                        {error, udp_too_large} ->
                            send(AppId, [{tcp, Ip, Port, Res}|Rest], MakeMsg, Opts);
                        {error, Error} -> 
                            ?call_warning("Error sending to new transport: ~p", [Error]),
                            send(AppId, Rest, MakeMsg, Opts)
                    end;
                {error, Error} ->
                    ?call_notice("error connecting to ~p:~p (~p): ~p",
                                [Ip, Port, Proto, Error]),
                    send(AppId, Rest, MakeMsg, Opts)
            end
    end;

send(AppId, [Other|Rest], MakeMsg, Opts) ->
    ?call_warning("invalid send specification: ~p", [Other]),
    send(AppId, Rest, MakeMsg, Opts);

send(_, [], _MakeMsg, _Opts) ->
    error.
        


%% ===================================================================
%% Private
%% ===================================================================


%% @private Starts a new outbound connection.
-spec connect(nksip:app_id(), nksip:protocol(),
                       inet:ip_address(), inet:port_number(), binary(), 
                       nksip:optslist()) ->
    {ok, pid(), nksip_transport:transport()} | {error, term()}.

%% Do not open simultanous connections to the same destination
connect(AppId, Proto, Ip, Port, Res, Opts) ->
    try_connect(AppId, Proto, Ip, Port, Res, Opts, 300).
    

%% @private
try_connect(_, _, _, _, _, _, 0) ->
    {error, connection_busy};

try_connect(AppId, udp, Ip, Port, Res, Opts, _Try) ->
    do_connect(AppId, udp, Ip, Port, Res, Opts);

try_connect(AppId, Proto, Ip, Port, Res, Opts, Try) ->
    ConnId = {AppId, Proto, Ip, Port, Res},
    case nksip_sipapp_srv:put_new(AppId, {nksip_connect_block, ConnId}, true) of
        true ->
            try 
                do_connect(AppId, Proto, Ip, Port, Res, Opts)
            catch
                error:Value -> 
                    ?call_warning("Exception ~p launching connection: ~p", 
                                  [Value, erlang:get_stacktrace()]),
                    {error, Value}
            after
                nksip_sipapp_srv:del(AppId, {nksip_connect_block, ConnId})
            end;
        false ->
            timer:sleep(100),
            try_connect(AppId, Proto, Ip, Port, Res, Opts, Try-1);
        {error, _} ->
            {error, locking_error}
    end.
                

%% @private Starts a new connection to a remote server
-spec do_connect(nksip:app_id(), nksip:protocol(), inet:ip_address(), inet:port_number(), 
              binary(), nksip:optslist()) ->
    {ok, pid(), nksip_transport:transport()} | {error, term()}.
         
do_connect(AppId, Proto, Ip, Port, Res, Opts) ->
    Class = case size(Ip) of 4 -> ipv4; 8 -> ipv6 end,
    case nksip_transport:get_listening(AppId, Proto, Class) of
        [{Transp, Pid}|_] -> 
            Transp1 = Transp#transport{remote_ip=Ip, remote_port=Port, resource=Res},
            case Proto of
                udp -> nksip_transport_udp:connect(Pid, Transp1);
                tcp -> nksip_transport_tcp:connect(AppId, Transp1);
                tls -> nksip_transport_tcp:connect(AppId, Transp1);
                sctp -> nksip_transport_sctp:connect(Pid, Transp1);
                ws -> nksip_transport_ws:connect(AppId, Transp1, Opts);
                wss -> nksip_transport_ws:connect(AppId, Transp1, Opts)
            end;
        [] ->
            {error, no_listening_transport}
    end.


%% @private
default_port(udp) -> 5060;
default_port(tcp) -> 5060;
default_port(tls) -> 5061;
default_port(sctp) -> 5060;
default_port(ws) -> 80;
default_port(wss) -> 443;
default_port(_) -> 0.



%% ===================================================================
%% Only testing
%% ===================================================================


%% @private
get_all_connected() ->
    nksip_proc:fold_names(
        fun(Name, Values, Acc) ->
            case Name of
                {nksip_connection, {AppId, _Proto, _Ip, _Port, _Res}} -> 
                    [{AppId, Transp, Pid} || {val, Transp, Pid} <- Values] ++ Acc;
                _ ->
                    Acc
            end
        end,
        []).


%% @private
get_all_connected(AppId) ->
    [{Transp, Pid} || {LAppId, Transp, Pid} <- get_all_connected(), AppId==LAppId].


%% @private
stop_all_connected() ->
    lists:foreach(
        fun({_, _, Pid}) -> nksip_connection:stop(Pid, normal) end,
        get_all_connected()).




