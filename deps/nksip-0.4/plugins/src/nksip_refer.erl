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

%% @doc NkSIP REFER Plugin
-module(nksip_refer).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").

-export([process/2]).
-export([version/0, deps/0]).


%% ===================================================================
%% Plugin specific
%% ===================================================================

%% @doc Version
-spec version() ->
    string().

version() ->
    "0.1".


%% @doc Dependant plugins
-spec deps() ->
    [{atom(), string()}].
    
deps() ->
    [].





%% ===================================================================
%% Public
%% ===================================================================


%% @doc Use this function to process an incoming REFER
-spec process(nksip:request(), nksip:call()) ->
    nksip:sipreply().

process(Req, #call{app_id=AppId, call_id=CallId}=Call) ->
    case nksip_sipmsg:header(<<"refer-to">>, Req, uris) of
        [ReferTo] -> 
            case catch AppId:sip_refer(ReferTo, Req, Call) of
                true ->
                    {ok, SubsId} = nksip_subscription:get_handle(Req), 
                    InvCallId = <<"nksip_refer_", CallId/binary>>,
                    Opts = [async, auto_2xx_ack, {call_id, InvCallId}, 
                           {refer_subscription_id, SubsId}],
                    spawn(fun() -> nksip_uac:invite(AppId, ReferTo, Opts) end),
                    ok;
                false ->
                    forbidden;
                {'EXIT', Error} ->
                    ?call_error("Error calling callback sip_refer/3: ~p", [Error]),
                    {internal_error, "SipApp Error"}
            end;
        _ ->
            invalid_request
    end.

