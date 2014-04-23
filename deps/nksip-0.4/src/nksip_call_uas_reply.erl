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

%% @doc Call UAS Management: Reply
-module(nksip_call_uas_reply).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([reply/3]).
-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").

-type incoming() :: 
    nksip:sipreply() | {nksip:response(), nksip_lib:optslist()}.

-type reply_error() :: 
    invalid_call | pending_prack | stateless_not_allowed | service_unavailable.

-type reply_return() :: 
    {{ok, nksip:response()} | {error, reply_error()}, nksip_call:call()}.


%% ===================================================================
%% Private
%% ===================================================================

%% @doc Sends a transaction reply
-spec reply(incoming(), nksip_call:trans(), nksip_call:call()) ->
    reply_return().

reply(Reply, #trans{method='ACK', id=Id, status=Status}=UAS, Call) ->
    ?call_notice("UAC ~p 'ACK' (~p) trying to send a reply ~p", [Id, Status, Reply]),
    UAS1 = UAS#trans{status=finished},
    {{error, invalid_call}, update(UAS1, Call)};

reply(Reply, #trans{status=Status, method=Method}=UAS, Call)
          when Status==authorize; Status==route ->
    UAS1 = case Method of
        'INVITE' -> UAS#trans{status=invite_proceeding};
        _ -> UAS#trans{status=trying}
    end,
    reply(Reply, UAS1, update(UAS1, Call));

reply({#sipmsg{class={resp, Code, _Reason}}=Resp, SendOpts}, 
           #trans{status=Status, code=LastCode}=UAS, Call)
           when Status==invite_proceeding orelse 
                Status==trying orelse 
                Status==proceeding orelse
                (
                    (
                        Status==invite_accepted orelse 
                        Status==invite_confirmed orelse
                        Status==completed
                    ) andalso (
                        Code>=200 andalso Code<300 andalso 
                        LastCode>=200 andalso LastCode<300
                    )
                ) ->
    dialog({Resp, SendOpts}, UAS, Call);

reply({#sipmsg{class={resp, Code, _Reason}}, _}, #trans{code=LastCode}=UAS, Call) ->
    #trans{status=Status, id=Id, method=Method} = UAS,
    ?call_info("UAS ~p ~p cannot send ~p response in ~p (last code was ~p)", 
               [Id, Method, Code, Status, LastCode]),
    {{error, invalid_call}, Call};

reply(SipReply, #trans{request=#sipmsg{}=Req}=UAS, Call) ->
    reply(nksip_reply:reply(Req, SipReply), UAS, Call);

reply(SipReply, #trans{id=Id, method=Method, status=Status}, Call) ->
    ?call_info("UAS ~p ~p cannot send ~p response in ~p (no stored request)", 
               [Id, Method, SipReply, Status]),
    {{error, invalid_call}, Call}.


%% @private
-spec dialog(incoming(), nksip_call:trans(), nksip_call:call()) ->
    reply_return().

dialog({Resp, SendOpts}, #trans{request=Req}=UAS, Call) ->
    {Resp1, SendOpts1} = 
        nksip_call_uas_dialog:update_response(Req, {Resp, SendOpts}, Call),
    case lists:member(rseq, SendOpts1) of
        true ->
            case check_prack(Resp1, UAS) of
                {ok, Resp2, UAS1} ->
                    send({Resp2, SendOpts1}, UAS1, update(UAS1, Call));
                {error, Error} ->
                    {{error, Error}, Call}
            end;
        false ->
            send({Resp1, SendOpts1}, UAS, Call)
    end.


%% @private
-spec check_prack(nksip:response(), nksip_call:trans()) ->
    {ok, nksip:response(), nksip_call:trans()} | {error, Error}
    when Error :: stateless_not_allowed | pending_prack.

check_prack(_, #trans{stateless=true}) ->
    {error, stateless_not_allowed};

check_prack(Resp, UAS) ->
    #sipmsg{dialog_id=DialogId, cseq={CSeq, Method}} = Resp,
    #trans{rseq=LastRSeq, pracks=WaitPRAcks} = UAS,
    case WaitPRAcks of
        [] ->
            RSeq = case LastRSeq of
                0 -> crypto:rand_uniform(1, 2147483647);
                _ -> LastRSeq+1
            end,
            Headers1 = nksip_headers:update(Resp, [{single, <<"rseq">>, RSeq}]),
            Resp1 = Resp#sipmsg{headers=Headers1},
            PRAcks = [{RSeq, CSeq, Method, DialogId}],
            UAS1 = UAS#trans{rseq=RSeq, pracks=PRAcks},
            {ok, Resp1, UAS1};
        _ ->
            
            {error, pending_prack}
    end.


%% @private
-spec send(incoming(), nksip_call:trans(), nksip_call:call()) ->
    reply_return().

send({Resp, SendOpts}, UAS, Call) ->
    #sipmsg{
        class ={resp, Code, _Reason}, 
        id = MsgId, 
        dialog_id = DialogId
    } = Resp,
    #trans{
        id = Id, 
        status = Status,
        opts = Opts,
        method = Method,
        request = Req,
        stateless = Stateless,
        code = LastCode
    } = UAS,    
    #call{msgs = Msgs} = Call,
    case nksip_transport_uas:send_response(Resp, SendOpts) of
        {ok, Resp2} -> 
            UserReply = {ok, Resp2};
        error -> 
            UserReply = {error, service_unavailable},
            {Resp2, _} = nksip_reply:reply(Req, service_unavailable)
    end,
    #sipmsg{class={resp, Code1, _}} = Resp2,
    Call1 = case Req of
        #sipmsg{} when Code1>=200, Code<300 ->
            nksip_call_lib:update_auth(DialogId, Req, Call);
        _ ->
            Call
    end,
    Call2 = case lists:member(no_dialog, Opts) of
        true -> Call1;
        false -> nksip_call_uas_dialog:response(Req, Resp2, Call1)
    end,
    UAS1 = case LastCode<200 of
        true -> UAS#trans{response=Resp2, code=Code};
        false -> UAS
    end,
    Msg = {MsgId, Id, DialogId},
    Call3 = Call2#call{msgs=[Msg|Msgs]},
    case Stateless of
        true when Method/='INVITE' ->
            ?call_debug("UAS ~p ~p stateless reply ~p", [Id, Method, Code1]),
            UAS2 = UAS1#trans{status=finished},
            UAS3 = nksip_call_lib:timeout_timer(cancel, UAS2, Call),
            {UserReply, update(UAS3, Call3)};
        _ ->
            Rel = lists:member(rseq, SendOpts),
            ?call_debug("UAS ~p ~p stateful reply ~p", [Id, Method, Code1]),
            UAS2 = stateful_reply(Status, Code1, Rel, UAS1, Call3),
            {UserReply, update(UAS2, Call3)}
    end.


%% @private
-spec stateful_reply(nksip_call_uas:status(), nksip:response_code(), 
                     boolean(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:trans().

stateful_reply(invite_proceeding, Code, true, UAS, Call) when Code < 200 ->
    UAS1 = nksip_call_lib:timeout_timer(prack_timeout, UAS, Call),
    nksip_call_lib:retrans_timer(prack_retrans, UAS1, Call);

stateful_reply(invite_proceeding, Code, false, UAS, Call) when Code < 200 ->
    nksip_call_lib:timeout_timer(timer_c, UAS, Call);

% RFC6026 accepted state, to wait for INVITE retransmissions
% Dialog will send 2xx retransmissionshrl
stateful_reply(invite_proceeding, Code, _, UAS, Call) when Code < 300 ->
    #trans{id=Id, request=Req, response=Resp, callback_timer=AppTimer} = UAS,
    UAS1 = case Id < 0 of
        true -> 
            % In old-style transactions, save Id to be used in
            % detecting ACKs
            #sipmsg{to=To} = Resp,
            ACKTrans = nksip_call_uas:transaction_id(Req#sipmsg{to=To}),
            UAS#trans{ack_trans_id=ACKTrans};
        _ ->
            UAS
    end,
    % If the invite/3 call has not returned, maintain values
    UAS2 = case AppTimer of
        undefined -> 
            UAS1#trans{status=invite_accepted, request=undefined, response=undefined};
        _ -> 
            UAS1#trans{status=invite_accepted}
    end,
    UAS3 = nksip_call_lib:expire_timer(cancel, UAS2, Call),
    nksip_call_lib:timeout_timer(timer_l, UAS3, Call);

stateful_reply(invite_proceeding, Code, _, UAS, Call) when Code >= 300 ->
    #trans{proto=Proto, callback_timer=AppTimer} = UAS,
    UAS1 = UAS#trans{status=invite_completed},
    UAS2 = nksip_call_lib:expire_timer(cancel, UAS1, Call),
    UAS3 = case AppTimer of
        undefined -> UAS2#trans{request=undefined};
        _ -> UAS2
    end,
    UAS4 = nksip_call_lib:timeout_timer(timer_h, UAS3, Call),
    case Proto of 
        udp -> nksip_call_lib:retrans_timer(timer_g, UAS4, Call);
        _ -> UAS4#trans{response=undefined}
    end;

stateful_reply(trying, Code, Rel, UAS, Call) ->
    stateful_reply(proceeding, Code, Rel, UAS#trans{status=proceeding}, Call);

stateful_reply(proceeding, Code, _, UAS, _Call) when Code < 200 ->
    UAS;

stateful_reply(proceeding, Code, _, UAS, Call) when Code >= 200 ->
    #trans{proto=Proto, callback_timer=AppTimer} = UAS,
    UAS1 = UAS#trans{request=undefined, status=completed},
    case Proto of
        udp -> 
            UAS2 = case AppTimer of
                undefined ->  UAS1#trans{request=undefined};
                _ -> UAS1
            end,
            nksip_call_lib:timeout_timer(timer_j, UAS2, Call);
        _ -> 
            UAS2 = UAS1#trans{status=finished},
            nksip_call_lib:timeout_timer(cancel, UAS2, Call)
    end;

stateful_reply(_, _Code, _, UAS, _Call) ->
    UAS.






