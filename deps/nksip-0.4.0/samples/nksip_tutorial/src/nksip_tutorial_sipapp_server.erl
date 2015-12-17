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

%% @doc SipApp Tutorial server callback module implementation.
%%
%% This modules implements a proxy server callback module for NkSIP Tutorial.
%% It allows any request from any user in domain "nksip", having password "1234".
%% Requests without user and domain "nksip" are processed internally (i.e. "sip:nksip").
%% Request with user and domain "nksip" are found (as a registrar) and proxied.
%% Other requests are proxied to the same origin Request-URI

-module(nksip_tutorial_sipapp_server).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([init/1, sip_get_user_pass/4, sip_authorize/3, sip_route/5, handle_call/3]).



%% ===================================================================
%% Callbacks
%% ===================================================================

-record(state, {
    started
}).

%% @doc SipApp intialization.
init([]) ->
    nksip:put(server, started, httpd_util:rfc1123_date()),
    {ok, []}.


%% @doc Called to check user's password.
%%
%% If the incoming user's realm is "nksip", the password for any user is "1234". 
%% For other realms, no password is valid.
%%
sip_get_user_pass(_User, <<"nksip">>, _Req, _Call) -> 
    <<"1234">>;
sip_get_user_pass(_User, _Realm, _Req, _Call) -> 
    false.


%% @doc Called to check if a request should be authorized.
%%
%% 1) We first check to see if the request is an in-dialog request, coming from 
%%    the same ip and port of a previously authorized request.
%%
%% 2) If not, we check if we have a previous authorized REGISTER request from 
%%    the same ip and port.
%%
%% 3) Next, we check if the request has a valid authentication header with realm 
%%    "nksip". If `{{digest, <<"nksip">>}, true}' is present, the user has 
%%    provided a valid password and it is authorized. 
%%    If `{{digest, <<"nksip">>}, false}' is present, we have presented 
%%    a challenge, but the user has failed it. We send 403.
%%
%% 4) If no digest header is present, reply with a 407 response sending 
%%    a challenge to the user.
%%
sip_authorize(AuthList, _Req, _Call) ->
    case lists:member(dialog, AuthList) orelse lists:member(register, AuthList) of
        true -> 
            ok;
        false ->
            case proplists:get_value({digest, <<"nksip">>}, AuthList) of
                true -> 
                    ok;            % Password is valid
                false -> 
                    forbidden;     % User has failed authentication
                undefined -> 
                    {proxy_authenticate, <<"nksip">>}
                    
            end
    end.


%% @doc Called to decide how to route every new request.
%%
%% - If the Request-Uri has no user part, and domain is "nksip", process the request
%%   internally. If domain is not "nksip", and the destination is
%%   not local (for example because of a Route header), proxy the request.
%%
%% - If it has user part, and domain is "nksip", find if it is registered and proxy.
%%   For other domain, proxy the request if it is not for ourselves.
%%
sip_route(_Scheme, <<>>, <<"nksip">>, _Req, _Call) ->
    process;
sip_route(Scheme, User, <<"nksip">>, _Req, _Call) ->
    UriList = nksip_registrar:find(server, Scheme, User, <<"nksip">>),
    {proxy, UriList, [record_route]};
sip_route(_Scheme, _User, _Domain, Req, _Call) ->
    case nksip_request:is_local_ruri(Req) of
        true -> process;
        false -> proxy
    end.


%% @doc Synchronous user call.
handle_call(get_started, _From, #state{started=Started}=State) ->
    {reply, {ok, Started}, State}.