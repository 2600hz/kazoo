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

%% @doc Call UAC Management: Request sending
-module(nksip_call_uac_req).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/4, resend/3]).
-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-type uac_from() :: none | {srv, from()} | {fork, nksip_call_fork:id()}.


%% ===================================================================
%% Private
%% ===================================================================


%% @doc Starts a new UAC transaction.
-spec request(nksip:request(), nksip_lib:optslist(), uac_from(), nksip_call:call()) ->
    nksip_call:call().

request(Req, Opts, From, Call) ->
    #sipmsg{class={req, Method}, id=MsgId} = Req,
    Req1 = case From of 
        {fork, _} -> nksip_call_timer:proxy_request(Req, Call);
        _ -> Req
    end,
    {#trans{id=Id}=UAC, Call1} = new_uac(Req1, Opts, From, Call),
    case lists:member(async, Opts) andalso From of
        {srv, SrvFrom} when Method=='ACK' -> 
            gen_server:reply(SrvFrom, async);
        {srv, SrvFrom} ->
            gen_server:reply(SrvFrom, {async, nksip_sipmsg:get_id(Req)});
        _ ->
            ok
    end,
    case From of
        {fork, ForkId} ->
            ?call_debug("UAC ~p sending request ~p ~p (~s, fork: ~p)", 
                        [Id, Method, Opts, MsgId, ForkId]);
        _ ->
            ?call_debug("UAC ~p sending request ~p ~p (~s)", 
                        [Id, Method, Opts, MsgId])
    end,
    send(UAC, Call1).


%% @private
-spec new_uac(nksip:request(), nksip_lib:optslist(), uac_from(), nksip_call:call()) ->
    {nksip_call:trans(), nksip_call:call()}.

new_uac(Req, Opts, From, Call) ->
    #sipmsg{class={req, Method}, id=MsgId, ruri=RUri} = Req, 
    #call{next=Id, trans=Trans, msgs=Msgs} = Call,
    Status = case Method of
        'ACK' -> ack;
        'INVITE'-> invite_calling;
        _ -> trying
    end,
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    Opts1 = case 
        IsProxy andalso 
        (Method=='SUBSCRIBE' orelse Method=='NOTIFY' orelse Method=='REFER') 
    of
        true -> [no_dialog|Opts];
        false -> Opts
    end,
    DialogId = nksip_call_uac_dialog:uac_id(Req, IsProxy, Call),
    UAC = #trans{
        id = Id,
        class = uac,
        status = Status,
        start = nksip_lib:timestamp(),
        from = From,
        opts = Opts1,
        trans_id = undefined,
        request = Req#sipmsg{dialog_id=DialogId},
        method = Method,
        ruri = RUri,
        response = undefined,
        code = 0,
        to_tags = [],
        cancel = undefined,
        iter = 1
    },
    Msg = {MsgId, Id, DialogId},
    {UAC, Call#call{trans=[UAC|Trans], msgs=[Msg|Msgs], next=Id+1}}.


%% @private
%% Resend a requests using same Call-Id, incremented CSeq
-spec resend(nksip:request(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

resend(Req, UAC, Call) ->
     #trans{
        id = Id,
        status = Status,
        opts = Opts,
        method = Method,
        iter = Iter,
        from = From
    } = UAC,
    #sipmsg{vias=[_|Vias], cseq={_, CSeqMethod}} = Req,
    ?call_info("UAC ~p ~p (~p) resending updated request", [Id, Method, Status]),
    {CSeq, Call1} = nksip_call_uac_dialog:new_local_seq(Req, Call),
    Req1 = Req#sipmsg{vias=Vias, cseq={CSeq, CSeqMethod}},
    % Contact would be already generated
    Opts1 = Opts -- [contact],
    {NewUAC, Call2} = new_uac(Req1, Opts1, From, Call1),
    NewUAC1 = NewUAC#trans{iter=Iter+1},
    send(NewUAC1, update(NewUAC1, Call2)).


%% @private
-spec send(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

send(#trans{method='ACK'}=UAC, Call) ->
    #trans{id=Id, request=Req, opts=Opts} = UAC,
    case nksip_transport_uac:send_request(Req, Opts) of
        {ok, SentReq} ->
            sent_request(SentReq, UAC, Call);
        {error, Error} ->
            ?call_debug("UAC ~p error sending 'ACK' request: ~p", [Id, Error]),
            Call1 = nksip_call_uac_reply:reply({error, Error}, UAC, Call),
            UAC1 = UAC#trans{status=finished},
            update(UAC1, Call1)
    end;

send(UAC, Call) ->
    #trans{method=Method, id=Id, request=Req, opts=Opts} = UAC,
    #sipmsg{to={_, ToTag}} = Req,
    NoDialog = lists:member(no_dialog, Opts),
    % For proxies sending SUBSCRIBE or NOTIFY, NoDialog will be true
    TestDialog = case NoDialog of
        true -> 
            ok;
        false when ToTag == <<>> ->
            ok;
        false -> 
            OnlyUpdate = lists:member(update_dialog, Opts),
            case nksip_call_uac_dialog:pre_request(Req, Call) of
                ok ->
                    ok;
                {error, DlgError} when OnlyUpdate ->
                    ?call_debug("UAC ~p error updating dialog: ~p", [Id, DlgError]),
                    ok;
                {error, DlgError} ->
                    {error, DlgError}
            end
    end,
    case TestDialog of
        ok ->
            Send = case Method of 
                'CANCEL' -> nksip_transport_uac:resend_request(Req, Opts);
                _ -> nksip_transport_uac:send_request(Req, Opts)
            end,
            case Send of
                {ok, SentReq} -> 
                    sent_request(SentReq, UAC, Call);
                {error, Error} ->
                    ?call_debug("UAC ~p error sending ~p request: ~p", 
                            [Id, Method, Error]),
                    Call1 = nksip_call_uac_reply:reply({error, Error}, UAC, Call),
                    update(UAC#trans{status=finished}, Call1)
            end;
        {error, Error} ->
            ?call_info("UAC ~p dialog error: ~p", [Id, Error]),
            Call1 = nksip_call_uac_reply:reply({error, Error}, UAC, Call),
            update(UAC#trans{status=finished}, Call1)
    end.


%% @private
-spec sent_request(nksip:request(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

sent_request(#sipmsg{class={req, 'ACK'}}=Req, UAC, Call) ->
    #trans{id=Id, opts=Opts} = UAC,
    ?call_debug("UAC ~p sent 'ACK' request", [Id]),
    Call1 = nksip_call_uac_reply:reply({req, Req}, UAC, Call),
    Call2 = case lists:member(no_dialog, Opts) of
        true -> Call1;
        false -> nksip_call_uac_dialog:ack(Req, Call1)
    end,
    UAC1 = UAC#trans{
        status = finished, 
        request = Req,
        trans_id = nksip_call_uac:transaction_id(Req)
    },
    update(UAC1, Call2);

sent_request(Req, UAC, Call) ->
    #sipmsg{
        class = {req, Method}, 
        to = {_, ToTag}, 
        transport = #transport{proto=Proto}
    } = Req,
    #trans{id=Id, opts=Opts, from=From} = UAC,
    ?call_debug("UAC ~p sent ~p request", [Id, Method]),
    UAC1 = UAC#trans{
        request = Req, 
        proto = Proto,
        trans_id = nksip_call_uac:transaction_id(Req)
    },
    Call1 = update(UAC1, Call),
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    Call2 = case lists:member(no_dialog, Opts) of
        true -> Call1;
        false when ToTag == <<>> -> Call1;
        false -> nksip_call_uac_dialog:request(Req, IsProxy, Call1)
    end,
    Call3 = nksip_call_uac_reply:reply({req, Req}, UAC1, Call2),
    sent_update(Req, UAC1, Call3).


%% @private 
-spec sent_update(nksip:request(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

sent_update(#sipmsg{class={req, 'INVITE'}}, #trans{proto=Proto}=UAC, Call) ->
    UAC1 = UAC#trans{status=invite_calling},
    UAC2 = nksip_call_lib:expire_timer(expire, UAC1, Call),
    UAC3 = nksip_call_lib:timeout_timer(timer_b, UAC2, Call),
    UAC4 = case Proto of 
        udp -> nksip_call_lib:retrans_timer(timer_a, UAC3, Call);
        _ -> UAC3
    end,
    update(UAC4, Call);

sent_update(#sipmsg{class={req, Method}}=Req, #trans{proto=Proto}=UAC, Call) ->
    UAC1 = UAC#trans{status=trying},
    UAC2 = nksip_call_lib:timeout_timer(timer_f, UAC1, Call),
    UAC3 = case Proto of 
        udp -> nksip_call_lib:retrans_timer(timer_e, UAC2, Call);
        _ -> UAC2
    end,
    #sipmsg{to={_, ToTag}} = Req,
    Call1 = case 
        (Method=='SUBSCRIBE' orelse Method=='REFER') andalso ToTag == <<>> 
    of
        true -> nksip_call_event:create_prov_event(Req, Call);
        false -> Call
    end,
    update(UAC3, Call1).

    

