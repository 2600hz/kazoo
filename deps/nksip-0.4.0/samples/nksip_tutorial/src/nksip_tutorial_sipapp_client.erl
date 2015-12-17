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

%% @doc SipApp Tutorial client callback module implementation.
%% This modules implements a client callback module for NkSIP Tutorial.

-module(nksip_tutorial_sipapp_client).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([sip_invite/2, sip_options/2]).

%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc Called when an INVITE is received.
%% If the request has a SDP body, reply 180 Ringing, wait 2 seconds and reply 
%% 200 Ok with the same body (spawns a new process to avoid blocking the process).
%% If not, reply 488 Not Acceptable with a Warning header.
sip_invite(Req, _Call) ->
    {ok, Body} = nksip_request:body(Req),
    case nksip_sdp:is_sdp(Body) of
        true ->
            {ok, ReqId} = nksip_request:get_handle(Req),
            Fun = fun() ->
                nksip_request:reply(ringing, ReqId),
                timer:sleep(2000),
                nksip_request:reply({answer, Body}, ReqId)
            end,
            spawn(Fun),
            noreply;
        false ->
            {reply, {not_acceptable, <<"Invalid SDP">>}}
    end.


%% @doc Called when an OPTIONS is received.
%% Reply 200 Ok with a custom header and some options.
sip_options(Req, _Call) ->
    {ok, AppName} = nksip_request:app_name(Req),
    {reply, {ok, [{add, "x-nk-id", AppName}, contact, allow, accept, supported]}}.


