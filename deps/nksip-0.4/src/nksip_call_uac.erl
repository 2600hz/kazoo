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

%% @doc Call UAC Management
-module(nksip_call_uac).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cancel/3, timer/3, transaction_id/1]).
-export_type([status/0, id/0]).

-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type status() :: invite_calling | invite_proceeding | invite_accepted |
                  invite_completed |
                  trying | proceeding | completed |
                  finished | ack.

-type id() :: integer().



%% ===================================================================
%% Cancel
%% ===================================================================


%% @doc Tries to cancel an ongoing invite request with a reason
-spec cancel(id()|nksip_call:trans(), nksip:error_reason()|undefined, 
             nksip_call:call()) ->
    nksip_call:call().

cancel(Id, Reason, #call{trans=Trans}=Call) when is_integer(Id) ->
    case lists:keyfind(Id, #trans.id, Trans) of
        #trans{class=uac, method='INVITE'} = UAC ->
            cancel(UAC, Reason, Call);
        _ -> 
            ?call_debug("UAC ~p not found to CANCEL", [Id]),
            Call
    end;

cancel(#trans{id=Id, class=uac, cancel=Cancel, status=Status}=UAC, Reason, Call)
       when Cancel==undefined; Cancel==to_cancel ->
    case Status of
        invite_calling ->
            ?call_debug("UAC ~p (invite_calling) delaying CANCEL", [Id]),
            UAC1 = UAC#trans{cancel=to_cancel},
            update(UAC1, Call);
        invite_proceeding ->
            ?call_debug("UAC ~p (invite_proceeding) generating CANCEL", [Id]),
            CancelReq = nksip_uac_lib:make_cancel(UAC#trans.request, Reason),
            UAC1 = UAC#trans{cancel=cancelled},
            nksip_call_uac_req:request(CancelReq, [no_dialog], none, update(UAC1, Call))
    end;

cancel(#trans{id=Id, class=uac, cancel=Cancel, status=Status}, _Reason, Call) ->
    ?call_debug("UAC ~p (~p) cannot CANCEL request: (~p)", [Id, Status, Cancel]),
    Call.



%% ===================================================================
%% Timers
%% ===================================================================


%% @private
-spec timer(nksip_call_lib:timer(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

timer(timer_c, #trans{id=Id, request=Req}, Call) ->
    ?call_notice("UAC ~p 'INVITE' Timer C Fired", [Id]),
    {Resp, _} = nksip_reply:reply(Req, {timeout, <<"Timer C Timeout">>}),
    nksip_call_uac_resp:response(Resp, Call);

% INVITE retrans
timer(timer_a, UAC, Call) ->
    #trans{id=Id, request=Req, status=Status} = UAC,
    case nksip_transport_uac:resend_request(Req, []) of
        {ok, _} ->
            ?call_info("UAC ~p (~p) retransmitting 'INVITE'", [Id, Status]),
            UAC1 = nksip_call_lib:retrans_timer(timer_a, UAC, Call),
            update(UAC1, Call);
        error ->
            ?call_notice("UAC ~p (~p) could not retransmit 'INVITE'", [Id, Status]),
            Reply = {service_unavailable, <<"Resend Error">>},
            {Resp, _} = nksip_reply:reply(Req, Reply),
            nksip_call_uac_resp:response(Resp, Call)
    end;

% INVITE timeout
timer(timer_b, #trans{id=Id, request=Req, status=Status}, Call) ->
    ?call_notice("UAC ~p 'INVITE' (~p) timeout (Timer B) fired", [Id, Status]),
    {Resp, _} = nksip_reply:reply(Req, {timeout, <<"Timer B Timeout">>}),
    nksip_call_uac_resp:response(Resp, Call);

% Finished in INVITE completed
timer(timer_d, #trans{id=Id, status=Status}=UAC, Call) ->
    ?call_debug("UAC ~p 'INVITE' (~p) Timer D fired", [Id, Status]),
    UAC1 = UAC#trans{status=finished, timeout_timer=undefined},
    update(UAC1, Call);

% INVITE accepted finished
timer(timer_m,  #trans{id=Id, status=Status}=UAC, Call) ->
    ?call_debug("UAC ~p 'INVITE' (~p) Timer M fired", [Id, Status]),
    UAC1 = UAC#trans{status=finished, timeout_timer=undefined},
    update(UAC1, Call);

% No INVITE retrans
timer(timer_e, UAC, Call) ->
    #trans{id=Id, status=Status, method=Method, request=Req} = UAC,
    case nksip_transport_uac:resend_request(Req, []) of
        {ok, _} ->
            ?call_info("UAC ~p (~p) retransmitting ~p", [Id, Status, Method]),
            UAC1 = nksip_call_lib:retrans_timer(timer_e, UAC, Call),
            update(UAC1, Call);
        error ->
            ?call_notice("UAC ~p (~p) could not retransmit ~p", [Id, Status, Method]),
            Msg = {service_unavailable, <<"Resend Error">>},
            {Resp, _} = nksip_reply:reply(Req, Msg),
            nksip_call_uac_resp:response(Resp, Call)
    end;

% No INVITE timeout
timer(timer_f, #trans{id=Id, status=Status, method=Method, request=Req}, Call) ->
    ?call_notice("UAC ~p ~p (~p) timeout (Timer F) fired", [Id, Method, Status]),
    {Resp, _} = nksip_reply:reply(Req, {timeout, <<"Timer F Timeout">>}),
    nksip_call_uac_resp:response(Resp, Call);

% No INVITE completed finished
timer(timer_k,  #trans{id=Id, status=Status, method=Method}=UAC, Call) ->
    ?call_debug("UAC ~p ~p (~p) Timer K fired", [Id, Method, Status]),
    UAC1 = UAC#trans{status=finished, timeout_timer=undefined},
    update(UAC1, Call);

timer(expire, #trans{id=Id, status=Status}=UAC, Call) ->
    UAC1 = UAC#trans{expire_timer=undefined},
    if
        Status==invite_calling; Status==invite_proceeding ->
            ?call_debug("UAC ~p 'INVITE' (~p) Timer Expire fired, sending CANCEL", 
                        [Id, Status]),
            UAC2 = UAC1#trans{status=invite_proceeding},
            cancel(UAC2, undefined, update(UAC2, Call));
        true ->
            ?call_debug("UAC ~p 'INVITE' (~p) Timer Expire fired", [Id, Status]),
            update(UAC1, Call)
    end.




%% ===================================================================
%% Util
%% ===================================================================


%% @private
-spec transaction_id(nksip:request()) -> 
    integer().

transaction_id(#sipmsg{cseq={_, Method}, vias=[Via|_]}) ->
    Branch = nksip_lib:get_value(<<"branch">>, Via#via.opts),
    erlang:phash2({Method, Branch}).

