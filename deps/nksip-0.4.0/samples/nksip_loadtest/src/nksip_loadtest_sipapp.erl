%% -------------------------------------------------------------------
%%
%% Sipapp callback for high load tests
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

%% @doc SipApp callback module

-module(nksip_loadtest_sipapp).

-export([sip_route/5, sip_invite/2]).

-define(PROXY_URI, "<sip:127.0.0.1:5061;transport=tcp>").

 
%% @doc Request routing callback
sip_route(_Scheme, <<"stateless">>, _Domain, _Req, _Call) ->
    process_stateless;

sip_route(_, <<"stateful">>, _, _, _) ->
    process;

sip_route(_, <<"proxy_stateful">>, _, _, _) ->
    {proxy, ?PROXY_URI};
        
sip_route(_, <<"proxy_stateless">>, _, _, _) ->
    {proxy, ?PROXY_URI, [stateless]};

sip_route(_Request, _Scheme, _User, _Domain, _From) ->
    process.

%% @doc Answer the call with the same SDP body
sip_invite(Req, _Call) ->
	{ok, SDP} = nksip_request:meta(body, Req),
    {reply, {ok, [{body, SDP}]}}.

