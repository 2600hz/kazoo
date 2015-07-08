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

%% @doc Outbound support
-module(nksip_outbound_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([add_headers/6, check_several_reg_id/1]).
-export([proxy_opts/2, registrar/1]).
-export([decode_flow/1]).

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-spec add_headers(nksip:request(), nksip:optslist(), nksip:scheme(),
                  nksip:protocol(), binary(), inet:port_number()) ->
    nksip:request().

add_headers(Req, Opts, Scheme, Proto, ListenHost, ListenPort) ->
    #sipmsg{
        class = {req, Method},
        app_id = AppId, 
        from = {From, _},
        vias = Vias,
        contacts = Contacts,
        headers = Headers
    } = Req,    
    case nksip_lib:get_value(record_flow, Opts) of
        FlowPid when is_pid(FlowPid) -> FlowOb = false;
        {FlowPid, ob} -> FlowOb = true;
        undefined -> FlowPid = FlowOb = false
    end,
    RouteUser = case FlowPid of
        false -> 
            GlobalId = nksip_config_cache:global_id(),
            RouteBranch = case Vias of
                [#via{opts=RBOpts}|_] -> nksip_lib:get_binary(<<"branch">>, RBOpts);
                _ -> <<>>
            end,
            RouteHash = nksip_lib:hash({GlobalId, AppId, RouteBranch}),
            <<"NkQ", RouteHash/binary>>;
        FlowPid -> 
            FlowToken = encode_flow(FlowPid),
            <<"NkF", FlowToken/binary>>
    end,
    RecordRoute = case lists:member(record_route, Opts) of
        true when Method=='INVITE'; Method=='SUBSCRIBE'; Method=='NOTIFY';
                  Method=='REFER' -> 
            nksip_transport:make_route(sip, Proto, ListenHost, ListenPort,
                                       RouteUser, [<<"lr">>]);
        _ ->
            []
    end,
    Path = case lists:member(path, Opts) of
        true when Method=='REGISTER' ->
            case RouteUser of
                <<"NkQ", _/binary>> ->
                    nksip_transport:make_route(sip, Proto, ListenHost, ListenPort,
                                               RouteUser, [<<"lr">>]);
                <<"NkF", _/binary>> ->
                    PathOpts = case FlowOb of
                        true -> [<<"lr">>, <<"ob">>];
                        false -> [<<"lr">>]
                    end,
                    nksip_transport:make_route(sip, Proto, ListenHost, ListenPort,
                                               RouteUser, PathOpts)
            end;
        _ ->
            []
    end,
    Contacts1 = case Contacts==[] andalso lists:member(contact, Opts) of
        true ->
            Contact0 = nksip_transport:make_route(Scheme, Proto, ListenHost, 
                                                  ListenPort, From#uri.user, []),
            [make_contact(Req, Contact0, Opts)];
        false ->
            Contacts
    end,
    Headers1 = nksip_headers:update(Headers, [
                                {before_multi, <<"record-route">>, RecordRoute},
                                {before_multi, <<"path">>, Path}]),
    Req#sipmsg{headers=Headers1, contacts=Contacts1}.


%% @private
-spec make_contact(nksip:request(), nksip:uri(), nksip:optslist()) ->
    nksip:uri().

make_contact(#sipmsg{app_id=AppId, class={req, 'REGISTER'}}=Req, Contact, Opts) ->
    #uri{ext_opts=CExtOpts} = Contact,
    {ok, UUID} = nksip:get_uuid(AppId),
    CExtOpts1 = [{<<"+sip.instance">>, <<$", UUID/binary, $">>}|CExtOpts],
    case 
        nksip_sipmsg:supported(<<"outbound">>, Req) andalso 
        nksip_lib:get_integer(reg_id, Opts)
    of
        RegId when is_integer(RegId), RegId>0 -> 
            CExtOpts2 = [{<<"reg-id">>, nksip_lib:to_binary(RegId)}|CExtOpts1],
            Contact#uri{ext_opts=CExtOpts2};
        _ ->
            Contact#uri{ext_opts=CExtOpts1}
    end;

% 'ob' parameter means we want to use the same flow for in-dialog requests
make_contact(Req, Contact, _Opts) ->
    case 
        nksip_sipmsg:supported(<<"outbound">>, Req) 
        andalso nksip_sipmsg:is_dialog_forming(Req)
    of
        true ->
            #uri{opts=COpts} = Contact,
            Contact#uri{opts=nksip_lib:store_value(<<"ob">>, COpts)};
        false ->
            Contact
    end.


%% @doc Checks if we have several contacts with a 'reg-id' having expires>0
-spec check_several_reg_id([#uri{}]) ->
    ok.

check_several_reg_id(Contacts) ->
    check_several_reg_id(Contacts, false).


%% @private
-spec check_several_reg_id([#uri{}], boolean()) ->
    ok.

check_several_reg_id([], _Found) ->
    ok;

check_several_reg_id([#uri{ext_opts=Opts}|Rest], Found) ->
    case nksip_lib:get_value(<<"reg-id">>, Opts) of
        undefined -> 
            check_several_reg_id(Rest, Found);
        _ ->
            Expires = case nksip_lib:get_list(<<"expires">>, Opts) of
                [] ->
                    default;
                Expires0 ->
                    case catch list_to_integer(Expires0) of
                        Expires1 when is_integer(Expires1) -> Expires1;
                        _ -> default
                    end
            end,
            case Expires of
                0 -> 
                    check_several_reg_id(Rest, Found);
                _ when Found ->
                    throw({invalid_request, "Several 'reg-id' Options"});
                _ ->
                    check_several_reg_id(Rest, true)
            end
    end.


%% @private
%% Can add options record_flow and route_flow
-spec proxy_opts(nksip:request(), nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, Error}
    when Error :: flow_failed | forbidden.

proxy_opts(#sipmsg{app_id=AppId, class={req, 'REGISTER'}}=Req, Opts) ->
    #sipmsg{
        app_id = AppId,
        vias = Vias, 
        transport = Transp, 
        contacts = Contacts
    } = Req,
    Supported = AppId:config_supported(),
    Opts1 = case 
        lists:member(path, Opts) andalso
        nksip_sipmsg:supported(<<"path">>, Req) andalso 
        lists:member(<<"outbound">>, Supported) andalso
        Contacts
    of
        [#uri{ext_opts=ContactOpts}] ->
            case lists:keymember(<<"reg-id">>, 1, ContactOpts) of
                true ->
                    case nksip_transport:get_connected(AppId, Transp) of
                        [{Transp, Pid}|_] ->
                            case length(Vias)==1 of
                                true -> [{record_flow, {Pid, ob}}|Opts];
                                false -> [{record_flow, Pid}|Opts]
                            end;
                        _ -> 
                            Opts
                    end;
                false ->
                    Opts
            end;
        _ ->
            Opts
    end,
    {ok, Opts1};

proxy_opts(Req, Opts) ->
    #sipmsg{app_id=AppId, routes=Routes, contacts=Contacts, transport=Transp} = Req,
    Supported = AppId:config_supported(),
    case 
        nksip_sipmsg:supported(<<"outbound">>, Req) andalso 
        lists:member(<<"outbound">>, Supported)
    of
        true ->
            case do_proxy_opts(Req, Opts, Routes) of
                {ok, Opts1} ->
                    case 
                        not lists:keymember(record_flow, 1, Opts1) andalso
                        Contacts
                    of
                       [#uri{opts=COpts}|_] ->
                            case lists:member(<<"ob">>, COpts) of
                                true ->
                                    Opts2 = case 
                                        nksip_transport:get_connected(AppId, Transp) 
                                    of
                                        [{_, Pid}|_] -> [{record_flow, Pid}|Opts1];
                                        _ -> Opts1
                                    end,
                                    {ok, Opts2};
                                false ->
                                    {ok, Opts1}
                            end;
                        _ ->
                            {ok, Opts1}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            {ok, Opts}
    end.


%% @private
do_proxy_opts(_Req, Opts, []) ->
    {ok, Opts};

do_proxy_opts(Req, Opts, [Route|RestRoutes]) ->
    #sipmsg{app_id=AppId, transport=Transp} = Req,
    case nksip_transport:is_local(AppId, Route) andalso Route of
        #uri{user = <<"NkF", Token/binary>>, opts=RouteOpts} ->
            case decode_flow(Token) of
                {ok, Pid, FlowTransp} ->
                    Opts1 = case flow_type(Transp, FlowTransp) of
                        outcoming -> 
                            % Came from the same flow
                            [{record_flow, Pid}|Opts];
                        incoming ->
                            [{route_flow, {FlowTransp, Pid}} |
                                case lists:member(<<"ob">>, RouteOpts) of
                                    true -> [{record_flow, Pid}|Opts];
                                    false -> Opts
                                end]
                    end,
                    {ok, Opts1};
                {error, flow_failed} ->
                    {error, flow_failed};
                {error, invalid} ->
                    ?call_notice("Received invalid flow token", []),
                    {error, forbidden}
            end;
        #uri{opts=RouteOpts} ->
            case lists:member(<<"ob">>, RouteOpts) of
                true ->
                    Opts1 = case nksip_transport:get_connected(AppId, Transp) of
                        [{_, Pid}|_] -> [{record_flow, Pid}|Opts];
                        _ -> Opts
                    end,
                    {ok, Opts1};
                false ->
                    do_proxy_opts(Req, Opts, RestRoutes)
            end;
        false -> 
            {ok, Opts}
    end.


%% @private
flow_type(#transport{proto=Proto, remote_ip=Ip, remote_port=Port, resource=Res}, 
          #transport{proto=Proto, remote_ip=Ip, remote_port=Port, resource=Res}) ->
    outcoming;

flow_type(_, _) ->
    incoming.


%% @private
%% Add registrar_otbound
-spec registrar(nksip:request()) ->
    {boolean(), nksip:request()} | no_outbound.

registrar(Req) ->
    #sipmsg{app_id=AppId, vias=Vias, transport=Transp} = Req,
    case 
        lists:member(<<"outbound">>, AppId:config_supported()) andalso
        nksip_sipmsg:supported(<<"outbound">>, Req)
    of
        true when length(Vias)==1 ->     % We are the first host
            #transport{
                proto = Proto, 
                listen_ip = ListenIp, 
                listen_port = ListenPort
            } = Transp,
            case nksip_transport:get_connected(AppId, Transp) of
                [{_, Pid}|_] ->
                    Flow = encode_flow(Pid),
                    Host = nksip_transport:get_listenhost(AppId, ListenIp, []),
                    Path = nksip_transport:make_route(sip, Proto, Host, ListenPort, 
                                                      <<"NkF", Flow/binary>>, 
                                                      [<<"lr">>, <<"ob">>]),
                    Headers1 = nksip_headers:update(Req, 
                                                [{before_single, <<"path">>, Path}]),
                    Req1 = Req#sipmsg{headers=Headers1},
                    {true, Req1};
                [] ->
                    {false, Req}
            end;
        true ->
            case nksip_sipmsg:header(<<"path">>, Req, uris) of
                error ->
                    {error, {invalid_request, <<"Invalid Path">>}};
                [] ->
                    {false, Req};
                Paths ->
                    [#uri{opts=PathOpts}|_] = lists:reverse(Paths),
                    Ob = lists:member(<<"ob">>, PathOpts),
                    {Ob, Req}
            end;
        false ->
            no_outbound
    end.


decode_flow(Token) ->
    PidList = lists:flatten(["<0.", binary_to_list(Token), ">"]),
    case catch list_to_pid(PidList) of
        Pid when is_pid(Pid) ->
            case catch nksip_connection:get_transport(Pid) of
                {ok, FlowTransp} ->  {ok, Pid, FlowTransp};
                _ -> {error, flow_failed}
            end;
        _ ->
            {error, invalid}
    end.


encode_flow(Pid) when is_pid(Pid) ->
    encode_flow(pid_to_list(Pid), []).

encode_flow([$<, $0, $.|Rest], Acc) -> encode_flow(Rest, Acc);
encode_flow([$>|_], Acc) -> list_to_binary(lists:reverse(Acc));
encode_flow([Ch|Rest], Acc) -> encode_flow(Rest, [Ch|Acc]).





