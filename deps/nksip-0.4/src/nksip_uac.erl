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

%% @doc Request sending functions as UAC.
%%
%% In case of using a SIP URI as destination, is is possible to include
%% custom headers: `"<sip:host;method=REGISTER?contact=*&expires=10>"'
%% 
-module(nksip_uac).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([options/3, options/2, register/3, invite/3, invite/2, ack/2, bye/2, cancel/2]).
-export([info/2, update/2, subscribe/2, subscribe/3, notify/2]).
-export([message/3, message/2, refer/3, refer/2, publish/3, publish/2]).
-export([request/3, request/2, refresh/2, stun/3]).
-export_type([uac_result/0, uac_ack_result/0, uac_cancel_result/0]).


%% ===================================================================
%% Types
%% ===================================================================


-type uac_result() ::  
    {async, nksip:handle()} | {ok, nksip:sip_code(), nksip:optslist()} | {error, term()}.
    
-type uac_ack_result() ::
    ok | async | {error, term()}.

-type uac_cancel_result() ::
    ok | {error, term()}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Sends an out-of-dialog OPTIONS request.
-spec options(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    uac_result().

options(App, Uri, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send(App, 'OPTIONS', Uri, Opts1).


%% @doc Sends an in-dialog OPTIONS request.
-spec options(nksip:handle(), nksip:optslist()) ->
    uac_result().

options(Handle, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('OPTIONS', Handle, Opts1).


%% @doc Sends a REGISTER request.
-spec register(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    uac_result().

register(App, Uri, Opts) ->
    Opts1 = [to_as_from, supported, allow, allow_event | Opts],
    send(App, 'REGISTER', Uri, Opts1).


%% @doc Sends an out-of-dialog INVITE request.
-spec invite(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    uac_result().

invite(App, Uri, Opts) ->
    Opts1 = [contact, supported, allow, allow_event | Opts],
    send(App, 'INVITE', Uri, Opts1).


%% @doc Sends an in-dialog INVITE request.
-spec invite(nksip:handle(), nksip:optslist()) ->
    uac_result().

invite(Handle, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('INVITE', Handle, Opts1).



%% @doc Sends an ACK after a successful INVITE response.
-spec ack(nksip:handle(), nksip:optslist()) ->
    uac_ack_result().

ack(Handle, Opts) ->
    send_dialog('ACK', Handle, Opts).


%% @doc Sends an BYE for a current dialog, terminating the session.
-spec bye(nksip:handle(), nksip:optslist()) -> 
    uac_result().

bye(Handle, Opts) ->
    send_dialog('BYE', Handle, Opts).


%% @doc Sends an <i>INFO</i> for a current dialog.
-spec info(nksip:handle(), nksip:optslist()) -> 
    uac_result().

info(Handle, Opts) ->
    send_dialog('INFO', Handle, Opts).


%% @doc Sends an <i>CANCEL</i> for a currently ongoing <i>INVITE</i> request.
-spec cancel(nksip:handle(), nksip:optslist()) ->
    uac_cancel_result().

cancel(Handle, Opts) ->
    send_cancel(Handle, Opts).


%% @doc Sends a UPDATE on a currently ongoing dialog.
-spec update(nksip:handle(), nksip:optslist()) ->
    uac_result().

update(Handle, Opts) ->
    Opts1 = [supported, accept, allow | Opts],
    send_dialog('UPDATE', Handle, Opts1).



%% @doc Sends an out-of-dialog SUBSCRIBE request.
-spec subscribe(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    uac_result().

subscribe(App, Uri, Opts) ->
    case lists:keymember(event, 1, Opts) of
        true ->
            Opts1 = [contact, supported, allow, allow_event | Opts],
            send(App, 'SUBSCRIBE', Uri, Opts1);
        false ->
            {error, invalid_event}
    end.


%% @doc Sends an in-dialog or in-subscription SUBSCRIBE request.
-spec subscribe(nksip:handle(), nksip:optslist()) ->
    uac_result().

subscribe(Handle, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('SUBSCRIBE', Handle, Opts1).



%% @doc Sends an NOTIFY for a current server subscription.
-spec notify(nksip:handle(), nksip:optslist()) -> 
    uac_result().

notify(Handle, Opts) ->
    Opts1 = case lists:keymember(subscription_state, 1, Opts) of
        true -> Opts;
        false -> [{subscription_state, active}|Opts]
    end,
    send_dialog('NOTIFY', Handle, Opts1).


%% @doc Sends an out-of-dialog MESSAGE request.
-spec message(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    uac_result().

message(App, Uri, Opts) ->
    Opts1 = case lists:keymember(expires, 1, Opts) of
        true -> [date|Opts];
        _ -> Opts
    end,
    send(App, 'MESSAGE', Uri, Opts1).


%% @doc Sends an in-dialog MESSAGE request.
-spec message(nksip:handle(), nksip:optslist()) ->
    uac_result().

message(Handle, Opts) ->
    Opts1 = case lists:keymember(expires, 1, Opts) of
        true -> [date|Opts];
        _ -> Opts
    end,
    send_dialog('MESSAGE', Handle, Opts1).



%% @doc Sends an <i>REFER</i> for a remote party
-spec refer(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) -> 
    uac_result().

refer(App, Uri, Opts) ->
    case nksip_lib:get_binary(refer_to, Opts) of
        <<>> ->
            {error, invalid_refer_to};
        ReferTo ->
            Opts1 = [{insert, "refer-to", ReferTo} | nksip_lib:delete(Opts, refer_to)],
            send(App, 'REFER', Uri, Opts1)
    end.


-spec refer(nksip:handle(), nksip:optslist()) -> 
    uac_result() |  {error, invalid_refer_to}.

refer(Handle, Opts) ->
    case nksip_lib:get_binary(refer_to, Opts) of
        <<>> ->
            {error, invalid_refer_to};
        ReferTo ->
            Opts1 = [{insert, "refer-to", ReferTo} | nksip_lib:delete(Opts, refer_to)],
            send_dialog('REFER', Handle, Opts1)
    end.


%% @doc Sends an out-of-dialog PUBLISH request.
-spec publish(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    uac_result().

publish(App, Uri, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send(App, 'PUBLISH', Uri, Opts1).


%% @doc Sends an in-dialog PUBLISH request.
-spec publish(nksip:handle(), nksip:optslist()) ->
    uac_result().

publish(Handle, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('PUBLISH', Handle, Opts1).



%% @doc Sends an out-of-dialog request constructed from a SIP-Uri
-spec request(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) -> 
    uac_result().

request(App, Dest, Opts) ->
    send(App, undefined, Dest, Opts).


%% @doc Sends an in-dialog request constructed from a SIP-Uri
-spec request(nksip:handle(), nksip:optslist()) -> 
    uac_result().

request(Handle, Opts) ->
    send_dialog(undefined, Handle, Opts).



%% @doc Sends a update on a currently ongoing dialog using INVITE.
%%
%% This function sends a in-dialog INVITE, using the same current
%% parameters of the dialog, only to refresh it. The current local SDP version
%% will be incremented before sending it.
%%
%% Available options are the same as {@link reinvite/2} and also:
%% <ul>
%%  <li>`active': activate the medias on SDP (sending `a=sendrecv')</li>
%%  <li>`inactive': deactivate the medias on SDP (sending `a=inactive')</li>
%%  <li>`hold': activate the medias on SDP (sending `a=sendonly')</li>
%% </ul>
%%
-spec refresh(nksip:handle(), nksip:optslist()) ->
    uac_result().

refresh(Handle, Opts) ->
    Body1 = case nksip_lib:get_value(body, Opts) of
        undefined ->
            case nksip_dialog:meta(invite_local_sdp, Handle) of
                {ok, #sdp{} = SDP} -> SDP;
                _ -> <<>>
            end;
        Body ->
            Body
    end,
    Op = case lists:member(active, Opts) of
        true -> 
            sendrecv;
        false ->
            case lists:member(inactive, Opts) of
                true -> 
                    inactive;
                false ->
                    case lists:member(hold, Opts) of
                        true -> sendonly;
                        false -> none
                    end
            end
    end,
    Body2 = case Body1 of
        #sdp{} when Op /= none -> nksip_sdp:update(Body1, Op);
        #sdp{} -> nksip_sdp:increment(Body1);
        _ -> Body1
    end,
    Opts2 = nksip_lib:delete(Opts, [body, active, inactive, hold]),
    invite(Handle, [{body, Body2}|Opts2]).



%% @doc Sends a <i>STUN</i> binding request.
%%
%% Use this function to send a STUN binding request to a remote STUN or 
%% STUN-enabled SIP server, in order to get our remote ip and port.
%% If the remote server is a standard STUN server, use port 3478 
%% (i.e. `sip:stunserver.org:3478'). If it is a STUN server embedded into a SIP UDP
%% server, use a standard SIP uri.
%%
-spec stun(nksip:app_name()|nksip:app_id(), nksip:user_uri(), nksip:optslist()) ->
    {ok, {LocalIp, LocalPort}, {RemoteIp, RemotePort}} | {error, Error}
    when LocalIp :: inet:ip_address(), LocalPort :: inet:port_number(),
         RemoteIp :: inet:ip_address(), RemotePort :: inet:port_number(),
         Error :: unknown_core | invalid_uri | no_host | service_unavailable.

stun(App, UriSpec, _Opts) ->
    case nksip:find_app_id(App) of
        {ok, AppId} ->
            case nksip_transport:get_listening(AppId, udp, ipv4) of
                [] -> 
                    {error, no_udp_transport};
                [{#transport{listen_ip=LIp, listen_port=LPort}, Pid}|_] ->
                    case nksip_parse:uris(UriSpec) of
                        [Uri] ->
                            Transp = nksip_dns:resolve(Uri),
                            case nksip_lib:extract(Transp, udp) of
                                [{udp, Ip, Port, _}|_] -> 
                                    case nksip_transport_udp:send_stun_sync(Pid, Ip, Port) of
                                        {ok, SIp, SPort} ->
                                            {ok, {LIp, LPort}, {SIp, SPort}};
                                        error ->
                                            {error, service_unavailable}
                                    end;
                                _ ->
                                    {error, no_host}
                            end;
                        _ ->
                            {error, invalid_uri}
                    end
            end;
        not_found ->
            {error, unkown_sipapp}
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
-spec send(nksip:app_name()|nksip:app_id(), nksip:method(), nksip:user_uri(), 
           nksip:optslist()) ->
    uac_result() | {error, term()}.

send(App, Method, Uri, Opts) ->
    case nksip:find_app_id(App) of
        {ok, AppId} -> 
            case nksip_lib:get_binary(call_id, Opts) of
                <<>> -> CallId = nksip_lib:luid();
                CallId -> ok
            end,
            nksip_call:send(AppId, CallId, Method, Uri, Opts);
        not_found -> 
            {error, sipapp_not_found}
    end.


%% @private
-spec send_dialog(nksip:method(), nksip:handle(), nksip:optslist()) ->
    uac_result() | uac_ack_result() | {error, term()}.

send_dialog(Method, Handle, Opts) ->
    case nksip_dialog:get_handle(Handle) of
        {ok, DlgHandle} ->
            Opts1 = case Handle of 
                <<$U, $_, _/binary>>=SubsHandle ->
                    [{subscription, SubsHandle}|Opts];
                _ ->
                    Opts
            end,
            {AppId, DialogId, CallId} = nksip_dialog_lib:parse_handle(DlgHandle),
            nksip_call:send_dialog(AppId, CallId, Method, DialogId, Opts1);
        {error, Error} ->
            {error, Error}
    end.

%% @private
-spec send_cancel(nksip:handle(), nksip:optslist()) ->
    uac_ack_result().
    
send_cancel(Handle, Opts) ->
    case nksip_sipmsg:parse_handle(Handle) of
        {req, AppId, ReqId, CallId} ->
            nksip_call:send_cancel(AppId, CallId, ReqId, Opts);
        _ ->
            {error, invalid_request}
    end.



