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

%% @private Call UAC Timers
-module(nksip_call_uac_timer).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([timer/3]).

-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").



%% ===================================================================
%% Timers
%% ===================================================================


%% @private
-spec timer(nksip_call_lib:timer(), nksip_call:trans_id(), nksip_call:call()) ->
    nksip_call:call().

timer(Tag, TransId, #call{trans=Trans}=Call) ->
    case lists:keyfind(TransId, #trans.id, Trans) of
        #trans{class=uac}=UAC ->
            do_timer(Tag, UAC, Call);
        false ->
            ?call_warning("Call ignoring uac timer (~p, ~p)", [Tag, TransId]),
            Call
    end.


%% @private
-spec do_timer(nksip_call_lib:timer(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().


% INVITE retrans
do_timer(timer_a, UAC, Call) ->
    #trans{id=TransId, request=Req, status=Status} = UAC,
    case nksip_call_uac_transp:resend_request(Req, []) of
        {ok, _} ->
            ?call_info("UAC ~p (~p) retransmitting 'INVITE'", [TransId, Status]),
            UAC1 = nksip_call_lib:retrans_timer(timer_a, UAC, Call),
            update(UAC1, Call);
        error ->
            ?call_notice("UAC ~p (~p) could not retransmit 'INVITE'", [TransId, Status]),
            Reply = {service_unavailable, <<"Resend Error">>},
            {Resp, _} = nksip_reply:reply(Req, Reply),
            nksip_call_uac:response(Resp, Call)
    end;

% INVITE timeout
do_timer(timer_b, #trans{id=TransId, request=Req, status=Status}, Call) ->
    ?call_notice("UAC ~p 'INVITE' (~p) timeout (Timer B) fired", [TransId, Status]),
    {Resp, _} = nksip_reply:reply(Req, {timeout, <<"Timer B Timeout">>}),
    nksip_call_uac:response(Resp, Call);

% INVITE after provisional
do_timer(timer_c, #trans{id=TransId, request=Req}, Call) ->
    ?call_notice("UAC ~p 'INVITE' Timer C Fired", [TransId]),
    {Resp, _} = nksip_reply:reply(Req, {timeout, <<"Timer C Timeout">>}),
    nksip_call_uac:response(Resp, Call);

% Finished in INVITE completed
do_timer(timer_d, #trans{id=TransId, status=Status}=UAC, Call) ->
    ?call_debug("UAC ~p 'INVITE' (~p) Timer D fired", [TransId, Status]),
    UAC1 = UAC#trans{status=finished, timeout_timer=undefined},
    update(UAC1, Call);

% INVITE accepted finished
do_timer(timer_m,  #trans{id=TransId, status=Status}=UAC, Call) ->
    ?call_debug("UAC ~p 'INVITE' (~p) Timer M fired", [TransId, Status]),
    UAC1 = UAC#trans{status=finished, timeout_timer=undefined},
    update(UAC1, Call);

% No INVITE retrans
do_timer(timer_e, UAC, Call) ->
    #trans{id=TransId, status=Status, method=Method, request=Req} = UAC,
    case nksip_call_uac_transp:resend_request(Req, []) of
        {ok, _} ->
            ?call_info("UAC ~p (~p) retransmitting ~p", [TransId, Status, Method]),
            UAC1 = nksip_call_lib:retrans_timer(timer_e, UAC, Call),
            update(UAC1, Call);
        error ->
            ?call_notice("UAC ~p (~p) could not retransmit ~p", [TransId, Status, Method]),
            Msg = {service_unavailable, <<"Resend Error">>},
            {Resp, _} = nksip_reply:reply(Req, Msg),
            nksip_call_uac:response(Resp, Call)
    end;

% No INVITE timeout
do_timer(timer_f, #trans{id=TransId, status=Status, method=Method, request=Req}, Call) ->
    ?call_notice("UAC ~p ~p (~p) timeout (Timer F) fired", [TransId, Method, Status]),
    {Resp, _} = nksip_reply:reply(Req, {timeout, <<"Timer F Timeout">>}),
    nksip_call_uac:response(Resp, Call);

% No INVITE completed finished
do_timer(timer_k,  #trans{id=TransId, status=Status, method=Method}=UAC, Call) ->
    ?call_debug("UAC ~p ~p (~p) Timer K fired", [TransId, Method, Status]),
    UAC1 = UAC#trans{status=finished, timeout_timer=undefined},
    update(UAC1, Call);

do_timer(expire, #trans{id=TransId, status=Status}=UAC, Call) ->
    UAC1 = UAC#trans{expire_timer=undefined},
    if
        Status==invite_calling; Status==invite_proceeding ->
            ?call_debug("UAC ~p 'INVITE' (~p) Timer Expire fired, sending CANCEL", 
                        [TransId, Status]),
            UAC2 = UAC1#trans{status=invite_proceeding},
            nksip_call_uac:cancel(UAC2, [], update(UAC2, Call));
        true ->
            ?call_debug("UAC ~p 'INVITE' (~p) Timer Expire fired", [TransId, Status]),
            update(UAC1, Call)
    end.

