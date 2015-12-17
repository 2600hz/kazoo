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

%% @private Call UAS Management: Reply
-module(nksip_call_uas_reply).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([reply/3]).
-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Private
%% ===================================================================

%% @doc Sends a transaction reply
-spec reply(nksip_call_uas:incoming(), nksip_call:trans(), nksip_call:call()) ->
    {ok | {error, term()}, nksip_call:call()}.


reply(Reply, #trans{method='ACK', id=Id, status=Status}=UAS, Call) ->
    ?call_notice("UAC ~p 'ACK' (~p) trying to send a reply ~p", [Id, Status, Reply]),
    UAS1 = UAS#trans{status=finished},
    {{error, invalid_call}, update(UAS1, Call)};

reply({#sipmsg{class={resp, Code, _Reason}}=Resp, SendOpts}, 
           #trans{status=Status, code=LastCode, request=Req}=UAS, Call)
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
    {Resp1, SendOpts1} = 
        nksip_call_uas_dialog:update_response(Req, {Resp, SendOpts}, Call),
    #call{app_id=AppId} = Call,
    case AppId:nkcb_uas_send_reply({Resp1, SendOpts1}, UAS, Call) of
        {continue, [{Resp2, SendOpts2}, UAS2, Call2]} ->
            send({Resp2, SendOpts2}, UAS2, update(UAS2, Call2));
        {error, Error} ->
            {{error, Error}, Call}
    end;

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
-spec send(nksip_call_uas:incoming(), nksip_call:trans(), nksip_call:call()) ->
    {ok, nksip_call:call()} | {error, term()}.

send({Resp, SendOpts}, UAS, Call) ->
    #sipmsg{
        id = MsgId, 
        dialog_id = DialogId
    } = Resp,
    #trans{
        id = Id, 
        opts = Opts,
        method = Method,
        request = Req,
        stateless = Stateless
    } = UAS,    
    #call{
        app_id = AppId, 
        msgs = Msgs
    } = Call,
    case nksip_call_uas_transp:send_response(Resp, SendOpts) of
        {ok, Resp2} -> 
            UserReply = ok;
        error -> 
            UserReply = {error, service_unavailable},
            {Resp2, _} = nksip_reply:reply(Req, service_unavailable)
    end,
    #sipmsg{class={resp, Code2, _}} = Resp2,
    Call1 = case Req of
        #sipmsg{} when Code2>=200, Code2<300 ->
            nksip_call_lib:update_auth(DialogId, Req, Call);
        _ ->
            Call
    end,
    Call2 = case lists:member(no_dialog, Opts) of
        true -> Call1;
        false -> nksip_call_uas_dialog:response(Req, Resp2, Call1)
    end,
    Msg = {MsgId, Id, DialogId},
    Call3 = Call2#call{msgs=[Msg|Msgs]},
    case Stateless of
        true when Method/='INVITE' ->
            ?call_debug("UAS ~p ~p stateless reply ~p", [Id, Method, Code2]),
            UAS1 = UAS#trans{status=finished, response=Resp2, code=Code2},
            UAS2 = nksip_call_lib:timeout_timer(cancel, UAS1, Call3),
            {UserReply, update(UAS2, Call3)};
        _ ->
            ?call_debug("UAS ~p ~p stateful reply ~p", [Id, Method, Code2]),
            UAS2 = UAS#trans{response=Resp2, code=Code2},
            Call4 = update(UAS2, Call3),
            case AppId:nkcb_uas_sent_reply(Call4) of
                {ok, Call5} ->
                    {UserReply, Call5};
                {continue, [Call5]} ->
                    #call{trans=[#trans{status=Status}=UAS5|_]} = Call5,
                    UAS6 = stateful_reply(Status, Code2, UAS5, Call5),
                    {UserReply, update(UAS6, Call5)}
            end
    end.


%% @private
-spec stateful_reply(nksip_call_uas:status(), nksip:sip_code(), 
                     nksip_call:trans(), nksip_call:call()) ->
    nksip_call:trans().

stateful_reply(invite_proceeding, Code, UAS, Call) when Code < 200 ->
    nksip_call_lib:timeout_timer(timer_c, UAS, Call);

% RFC6026 accepted state, to wait for INVITE retransmissions
% Dialog will send 2xx retransmissionshrl
stateful_reply(invite_proceeding, Code, UAS, Call) when Code < 300 ->
    #trans{id=Id, request=Req, response=Resp} = UAS,
    UAS1 = case Id < 0 of
        true -> 
            % In old-style transactions, save Id to be used in
            % detecting ACKs
            #sipmsg{to=To} = Resp,
            ACKTrans = nksip_call_lib:uas_transaction_id(Req#sipmsg{to=To}),
            UAS#trans{ack_trans_id=ACKTrans};
        _ ->
            UAS
    end,
    UAS2 = UAS1#trans{status=invite_accepted, request=undefined, response=undefined},
    UAS3 = nksip_call_lib:expire_timer(cancel, UAS2, Call),
    nksip_call_lib:timeout_timer(timer_l, UAS3, Call);

stateful_reply(invite_proceeding, Code, UAS, Call) when Code >= 300 ->
    #trans{proto=Proto} = UAS,
    UAS1 = UAS#trans{status=invite_completed, request=undefined},
    UAS2 = nksip_call_lib:expire_timer(cancel, UAS1, Call),
    UAS3 = nksip_call_lib:timeout_timer(timer_h, UAS2, Call),
    case Proto of 
        udp -> 
            nksip_call_lib:retrans_timer(timer_g, UAS3, Call);
        _ -> 
            UAS3#trans{response=undefined}
    end;

stateful_reply(trying, Code, UAS, Call) ->
    stateful_reply(proceeding, Code, UAS#trans{status=proceeding}, Call);

stateful_reply(proceeding, Code, UAS, _Call) when Code < 200 ->
    UAS;

stateful_reply(proceeding, Code, UAS, Call) when Code >= 200 ->
    #trans{proto=Proto} = UAS,
    case Proto of
        udp -> 
            UAS1 = UAS#trans{status=completed, request=undefined},
            nksip_call_lib:timeout_timer(timer_j, UAS1, Call);
        _ -> 
            UAS1 = UAS#trans{status=finished},
            nksip_call_lib:timeout_timer(cancel, UAS1, Call)
    end;

stateful_reply(_, _Code, UAS, _Call) ->
    UAS.






