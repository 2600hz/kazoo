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

%% @private Call dialog UAC processing module
-module(nksip_call_uac_dialog).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-include("nksip_call.hrl").

-export([pre_request/2, request/3, response/4]).
-export([make/4, new_local_seq/2, uac_dialog_id/3]).
-import(nksip_call_dialog, [find/2, update/3, store/2]).

%% Offer/Answer Model
%%
%% - If I send an offer in INVITE
%%   - If provisional does not have SDP, nothing happens
%%   - If provisional have SDP, it is the answer
%%   - If new provisional/final have, SDP, reuses the same offer and is the new answer
%%   - If provisional is reliable, and have SDP:
%%     -  The PRACK can start a new offer, reply of ACK must answer
%%     -  IF PRACK has no SDP, nothing happens
%%
%% - If INVITE does not have an offer
%%   - Provisional can have the offer
%%   - If it is reliable, can reply in PRACK
%%   - If not, must reply in ACK when final
%%
%% - Can only send UPDATE when no offer is already sent or received
 
%% The PRACK stuff should be in the nksip_100rel plugin,
%% but the offer/answer model processing is still here

%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec pre_request(nksip:request(), nksip_call:call()) ->
    ok | {error, Error} 
    when Error :: unknown_dialog | request_pending.

pre_request(#sipmsg{class={req, 'ACK'}}, _) ->
    ok;

pre_request(Req, Call) ->
    #sipmsg{class={req, Method}, dialog_id=DialogId} = Req,
    case find(DialogId, Call) of
        #dialog{invite=Invite}=Dialog ->
            if
                Method=='INVITE'; Method=='UPDATE';  
                Method=='BYE'; Method=='PRACK'  ->
                    case Invite of
                        #invite{status=Status} = Invite ->
                            {HasSDP, _SDP, Offer, _} = get_sdp(Req, Invite),
                            if 
                                Method=='INVITE', Status/=confirmed -> 
                                    {error, request_pending};
                                Method=='INVITE', HasSDP, Offer/=undefined -> 
                                    {error, request_pending};
                                Method=='PRACK', Status/=proceeding_uac ->
                                    {error, request_pending};
                                Method=='UPDATE', HasSDP, Offer/=undefined ->
                                    {error, request_pending};
                                true ->
                                    ok 
                            end;
                        undefined when Method=='INVITE' ->
                            % Sending a INVITE over a event-created dialog
                            ok;
                        undefined ->
                            {error, unknown_dialog}
                    end;
                Method=='SUBSCRIBE'; Method=='NOTIFY' ->
                    nksip_call_event:uac_pre_request(Req, Dialog, Call);
                true ->
                    ok
            end;
        not_found ->
            {error, unknown_dialog}
    end.


%% @private
-spec request(nksip:request(), true|false|undefined, nksip_call:call()) ->
    nksip_call:call().

request(#sipmsg{class={req, 'ACK'}}=Req, undefined, Call) ->
    do_ack(Req, Call);

request(#sipmsg{class={req, Method}, dialog_id=DialogId}=Req, IsProxy, Call) ->
    ?call_debug("Dialog ~s UAC request ~p", [DialogId, Method]), 
    #dialog{local_seq=LocalSeq} = Dialog = find(DialogId, Call),
    #sipmsg{cseq={CSeq, _}} = Req,
    Dialog1 = case CSeq > LocalSeq of
        true -> 
            Dialog#dialog{local_seq=CSeq};
        false -> 
            Dialog
    end,
    do_request(Method, Req, IsProxy, Dialog1, Call).
        
      
%% @private
-spec do_request(nksip:method(), nksip:request(), boolean(), 
                 nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().

do_request('INVITE', Req, IsProxy, #dialog{invite=undefined}=Dialog, Call) ->
    Invite = #invite{status=confirmed},
    do_request('INVITE', Req, IsProxy, Dialog#dialog{invite=Invite}, Call);

do_request('INVITE', Req, IsProxy, #dialog{invite=Invite}=Dialog, Call) ->
    confirmed = Invite#invite.status,
    {HasSDP, SDP, _Offer, _} = get_sdp(Req, Invite),
    Offer1 = case HasSDP of 
        true -> {local, invite, SDP};
        false -> undefined
    end,
    Invite1 = Invite#invite{
        status = proceeding_uac,
        class = case IsProxy of true -> proxy; false -> uac end,
        request = Req, 
        response = undefined, 
        ack = undefined,
        sdp_offer = Offer1,
        sdp_answer = undefined
    },
    store(Dialog#dialog{invite=Invite1}, Call);

do_request('BYE', _Req, _IsProxy, Dialog, Call) ->
    update({invite, bye}, Dialog, Call);

do_request('PRACK', Req, _IsProxy, #dialog{invite=Invite}=Dialog, Call) ->
    proceeding_uac = Invite#invite.status,
    {HasSDP, SDP, Offer, _Answer} = get_sdp(Req, Invite),
    case Offer of
        undefined when HasSDP -> 
            Invite1 = Invite#invite{sdp_offer={local, prack, SDP}},
            store(Dialog#dialog{invite=Invite1}, Call);
        {remote, invite, _} when HasSDP -> 
            Invite1 = Invite#invite{sdp_answer={local, prack, SDP}},
            update(prack, Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            % If {remote, invite, _} and no SDP, ACK must answer or delete
            store(Dialog, Call)
    end;

do_request('UPDATE', Req, _IsProxy, #dialog{invite=Invite}=Dialog, Call) ->
    {HasSDP, SDP, Offer, _} = get_sdp(Req, Invite),
    case Offer of
        undefined when HasSDP -> 
            Invite1 = Invite#invite{sdp_offer={local, update, SDP}},
            store(Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            store(Dialog, Call)
    end;    

do_request('SUBSCRIBE', Req, _IsProxy, Dialog, Call) ->
    Dialog1 = nksip_call_event:uac_request(Req, Dialog, Call),
    store(Dialog1, Call);
        
do_request('NOTIFY', Req, _IsProxy, Dialog, Call) ->
    Dialog1 = nksip_call_event:uac_request(Req, Dialog, Call),
    store(Dialog1, Call);

do_request('REFER', Req, IsProxy, Dialog, Call) ->
    do_request('SUBSCRIBE', Req, IsProxy, Dialog, Call);

do_request(_Method, _Req, _IsProxy, Dialog, Call) ->
    store(Dialog, Call).


%% @private
-spec do_ack(nksip:request(), nksip_call:call()) ->
    nksip_call:call().

do_ack(#sipmsg{class={req, 'ACK'}, to={_, <<>>}}, Call) ->
    ?call_notice("Dialog UAC invalid ACK", []),
    Call;

do_ack(#sipmsg{class={req, 'ACK'}, cseq={CSeq, _}, dialog_id=DialogId}=AckReq, Call) ->
    case find(DialogId, Call) of
        #dialog{invite=#invite{}=Invite}=Dialog1 ->
            #invite{status=Status, request=InvReq} = Invite,
            #sipmsg{cseq={InvCSeq, _}} = InvReq,
            case Status of
                accepted_uac when CSeq==InvCSeq ->
                    ?call_debug("Dialog ~s (~p) UAC request 'ACK'", [DialogId, Status]),
                    {HasSDP, SDP, Offer, Answer} = get_sdp(AckReq, Invite), 
                    {Offer1, Answer1} = case Offer of
                        {remote, invite, _} when HasSDP -> 
                            {Offer, {local, ack, SDP}};
                        {remote, invite, _} -> 
                            {undefined, undefined};
                        _ -> 
                            {Offer, Answer}
                    end,
                    Invite1 = Invite#invite{
                        ack = AckReq, 
                        sdp_offer = Offer1, 
                        sdp_answer = Answer1
                    },
                    Dialog2 = Dialog1#dialog{invite=Invite1},
                    update({invite, confirmed}, Dialog2, Call);
                _ ->
                    ?call_notice("Dialog ~s (~p) ignoring ACK", [DialogId, Status]),
                    store(Dialog1, Call)
            end;
        not_found ->
            ?call_notice("Dialog ~s not found for UAC ACK", [DialogId]),
            Call
    end.


%% @private
-spec response(nksip:request(), nksip:response(), boolean(), nksip_call:call()) ->
    nksip_call:call().

response(Req, Resp, IsProxy, Call) ->
    #sipmsg{class={req, Method}, body=ReqBody} = Req,
    #sipmsg{class={resp, Code, _Reason}, dialog_id=DialogId} = Resp,
    case find(DialogId, Call) of
        #dialog{} = Dialog ->
            ?call_debug("Dialog ~s UAC response ~p ~p", [DialogId, Method, Code]),
            do_response(Method, Code, Req, Resp, Dialog, Call);
        not_found when Code>100, Code<300, Method=='INVITE' ->
            ?call_debug("Dialog ~s UAC response ~p ~p", [DialogId, Method, Code]),
            Dialog1 = nksip_call_dialog:create(uac, Req, Resp, Call),
            Offer = case ReqBody of 
                #sdp{}=SDP -> 
                    {local, invite, SDP};
                _ -> 
                    undefined
            end,
            Invite = #invite{
                status = proceeding_uac,
                class = case IsProxy of true -> proxy; false -> uac end,
                request = Req, 
                response = undefined, 
                ack = undefined,
                sdp_offer = Offer,
                sdp_answer = undefined
            },
            Dialog2 = Dialog1#dialog{invite=Invite},
            do_response(Method, Code, Req, Resp, Dialog2, Call);
        not_found when Code>=200 andalso Code<300 andalso
                       (Method=='SUBSCRIBE' orelse Method=='REFER') ->
            ?call_debug("Dialog ~s UAC response ~p ~p", [DialogId, Method, Code]),
            Dialog1 = nksip_call_dialog:create(uac, Req, Resp, Call),
            do_response(Method, Code, Req, Resp, Dialog1, Call);
        not_found ->
            Call
    end.


%% @private
-spec do_response(nksip:method(), nksip:sip_code(), nksip:request(),
                  nksip:response(), nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().

do_response(_Method, Code, _Req, _Resp, _Dialog, Call) when Code < 101 ->
    Call;

%% Full dialog stop reasons (RFC5057)
do_response(_Method, Code, _Req, _Resp, Dialog, Call) 
            when Code==404; Code==410; Code==416; Code==482; Code==483; Code==484;
                 Code==485; Code==502; Code==604 ->
    nksip_call_dialog:stop(Code, Dialog, Call);

do_response(_Method, Code, _Req, _Resp, #dialog{invite=#invite{}}=Dialog, Call) 
            when Code==481 ->
    update({invite, {stop, Code}}, Dialog, Call);

do_response('INVITE', Code, Req, Resp, 
            #dialog{invite=#invite{status=proceeding_uac}=Invite}=Dialog, Call) 
            when Code>100 andalso Code<300 ->
    {HasSDP, SDP, Offer, Answer} = get_sdp(Resp, Invite),
    {Offer1, Answer1} = case Offer of
        {local, invite, _} when HasSDP ->
            {Offer, {remote, invite, SDP}};
        {local, invite, _} when Code>=200 ->
            {undefined, undefined};
        undefined when HasSDP, element(1, Req#sipmsg.body)==sdp ->
            % We have completed a previous offer/answer (in a provisional response)
            % Now we have a new answer, must reuse the same offer
           {{local, invite, Req#sipmsg.body}, {remote, invite, SDP}};
        undefined when HasSDP ->
            {{remote, invite, SDP}, undefined};
        {remote, invite, _} when HasSDP ->
            % We are repeating a remote request
            {{remote, invite, SDP}, undefined};
        _ ->
            {Offer, Answer}
    end,
    Invite1 = Invite#invite{
        response = Resp,
        sdp_offer = Offer1,
        sdp_answer = Answer1
    },
    Dialog1 = Dialog#dialog{invite=Invite1},
    case Code < 200 of
        true -> 
            update({invite, proceeding_uac}, Dialog1, Call);
        false -> 
            update({invite, accepted_uac}, Dialog1, Call)
    end;
   
do_response('INVITE', Code, _Req, Resp, 
            #dialog{invite=#invite{status=proceeding_uac}=Invite}=Dialog, Call) 
            when Code>=300 ->
    case Invite#invite.answered of
        undefined -> 
            update({invite, {stop, Code}}, Dialog, Call);
        _ -> 
            Offer1 = case Invite#invite.sdp_offer of
                {_, invite, _} -> undefined;
                {_, prack, _} -> undefined;
                Offer -> Offer
            end,
            Invite1 = Invite#invite{response=Resp, sdp_offer=Offer1},
            update({invite, confirmed}, Dialog#dialog{invite=Invite1}, Call)
    end;

do_response('INVITE', Code, _Req, _Resp, 
            #dialog{invite=#invite{status=accepted_uac}=Invite}=Dialog, Call) 
            when Code<300 ->
    #dialog{id=DialogId, invite=Invite} = Dialog,
    case Invite#invite.ack of
        #sipmsg{}=ACK ->
            case nksip_call_uac_transp:resend_request(ACK, []) of
                {ok, _} ->
                    ?call_info("Dialog ~s (accepted_uac) retransmitting 'ACK'", 
                               [DialogId]),
                    store(Dialog, Call);
                error ->
                    ?call_notice("Dialog ~s (accepted_uac) could not retransmit 'ACK'", 
                                 [DialogId]),
                    update({invite, {stop, 503}}, Dialog, Call)
            end;
        _ ->
            ?call_info("Dialog ~s (accepted_uac) received 'INVITE' ~p but "
                       "we have sent no ACK yet", [DialogId, Code]),
            store(Dialog, Call)
    end;

do_response('INVITE', Code, _Req, _Resp, #dialog{id=DialogId}=Dialog, Call) ->
    case Dialog#dialog.invite of
        #invite{status=Status} -> ok;
        _ -> Status = undefined
    end,
    case Status of
        bye -> 
            ok;
        _ ->
            ?call_notice("Dialog UAC ~s ignoring unexpected INVITE response ~p in ~p", 
                         [DialogId, Code, Status])
    end,
    store(Dialog, Call);

do_response('BYE', _Code, Req, _Resp, Dialog, Call) ->
    #dialog{caller_tag=CallerTag} = Dialog,
    Reason = case Req#sipmsg.from of
        {_, CallerTag} -> caller_bye;
        _ -> callee_bye
    end,
    update({invite, {stop, Reason}}, Dialog, Call);

do_response('PRACK', Code, _Req, Resp, 
            #dialog{invite=#invite{}=Invite}=Dialog, Call) 
            when Code>=200, Code<300 ->
    {HasSDP, SDP, Offer, _Answer} = get_sdp(Resp, Invite),
    case Offer of
        {local, prack, _} when HasSDP -> 
            Invite1 = Invite#invite{sdp_answer={remote, prack, SDP}},
            update(prack, Dialog#dialog{invite=Invite1}, Call);
        {local, prack, _} -> 
            Invite1 = Invite#invite{sdp_offer=undefined, sdp_answer=undefined},
            store(Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            store(Dialog, Call)
    end;

do_response('PRACK', Code, _Req, _Resp, 
            #dialog{invite=#invite{}=Invite}=Dialog, Call) 
            when Code>300 ->
    case Invite#invite.sdp_offer  of
        {local, prack, _} -> 
            Invite1 = Invite#invite{sdp_offer=undefined, sdp_answer=undefined},
            store(Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            store(Dialog, Call)
    end;
    
do_response('UPDATE', Code, Req, Resp,
            #dialog{invite=#invite{}=Invite}=Dialog, Call)
            when Code>=200, Code<300 ->
    {HasSDP, SDP, Offer, Answer} = get_sdp(Resp, Invite),
    {Offer1, Answer1} = case Offer of
        {local, update, _} when HasSDP -> 
            {Offer, {remote, update, SDP}};
        {local, update, _} -> 
            {undefined, undefined};
        _ -> 
            {Offer, Answer}
    end,
    Invite1 = Invite#invite{sdp_offer=Offer1, sdp_answer=Answer1},
    update({update, uac, Req, Resp}, Dialog#dialog{invite=Invite1}, Call);

do_response('UPDATE', Code, _Req, _Resp, 
            #dialog{invite=#invite{}=Invite}=Dialog, Call)
            when Code>300 ->
    case Invite#invite.sdp_offer of
        {local, update, _} -> 
            Invite1 = Invite#invite{sdp_offer=undefined, sdp_answer=undefined},
            store(Dialog#dialog{invite=Invite1}, Call);
        _ ->
            store(Dialog, Call)
    end;

do_response('SUBSCRIBE', Code, Req, Resp, Dialog, Call) when Code>=200, Code<300 ->
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    update({subscribe, uac, Req, Resp}, Dialog1, Call);
        
do_response('SUBSCRIBE', Code, Req, Resp, Dialog, Call) when Code>=300 ->
    % If subscription ends, it will call nksip_call_dialog:update/3, removing
    % the dialog if no other use
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    store(Dialog1, Call);

do_response('NOTIFY', Code, Req, Resp, Dialog, Call) when Code>=200, Code<300 ->
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    update({notify, uac, Req, Resp}, Dialog1, Call);

do_response('NOTIFY', Code, Req, Resp, Dialog, Call) when Code>=300 ->
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    store(Dialog1, Call);

do_response('REFER', Code, Req, Resp, Dialog, Call) ->
    do_response('SUBSCRIBE', Code, Req, Resp, Dialog, Call);
    
do_response(_, _Code, _Req, _Resp, Dialog, Call) ->
    store(Dialog, Call).
   

 %% @private
-spec make(nksip_dialog_lib:id(), nksip:method(), nksip:optslist(), nksip_call:call()) ->
    {ok, RUri::nksip:uri(), nksip:optslist(), nksip_call:call()} | {error, Error}
    when Error :: invalid_dialog | unknown_dialog | unknown_subscription.

make(DialogId, Method, Opts, #call{dialogs=Dialogs}=Call) ->
    case lists:keyfind(DialogId, #dialog.id, Dialogs) of
        #dialog{invite=Invite}=Dialog ->
            ?call_debug("Dialog ~s make ~p request", [DialogId, Method]),
            case Invite of
                #invite{status=Status} when Method=='ACK', Status/=accepted_uac ->
                    {error, invalid_dialog};
                _ when Method=='SUBSCRIBE'; Method=='NOTIFY' ->
                    case nksip_call_event:request_uac_opts(Method, Opts, Dialog) of
                        {ok, Opts1} -> 
                            do_make(Method, Opts1, Dialog, Call);
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    do_make(Method, Opts, Dialog, Call)
            end;
        _ ->
            {error, unknown_dialog}
    end.


%% @private
do_make(Method, Opts, Dialog, #call{app_id=AppId}=Call) ->
    {RUri, Opts1, Dialog1} = generate(Method, Opts, Dialog, Call),
    Call1 = store(Dialog1, Call),
    {continue, [_, RUri2, Opts2, Call2]} = 
        AppId:nkcb_make_uac_dialog(Method, RUri, Opts1, Call1),
    {ok, RUri2, Opts2, Call2}.


%% @private
-spec new_local_seq(nksip:request(), nksip_call:call()) ->
    {nksip:cseq(), nksip_call:call()}.

new_local_seq(#sipmsg{dialog_id = <<>>}, Call) ->
    {nksip_config:cseq(), Call};

new_local_seq(#sipmsg{dialog_id=DialogId}, Call) ->
    case find(DialogId, Call) of
        #dialog{local_seq=LocalSeq}=Dialog ->
            Dialog1 = Dialog#dialog{local_seq=LocalSeq+1},
            {LocalSeq+1, store(Dialog1, Call)};
        not_found ->
            {nksip_config:cseq(), Call}
    end.


%% @private Helps generating the dialog id
-spec uac_dialog_id(nksip:request()|nksip:response(), boolean(), nksip_call:call()) ->
    nksip_dialog_lib:id().

uac_dialog_id(SipMsg, IsProxy, #call{dialogs=Dialogs}) ->
    case nksip_dialog_lib:make_id(uac, SipMsg) of
        <<>> ->
            <<>>;
        DlgIdA when not IsProxy ->
            DlgIdA;
        DlgIdA ->
            % If it is a proxy, we can be proxying a request in the opposite
            % direction, DlgIdA is not goint to exist, but DlgIdB is the
            % original dialog, use it
            case lists:keymember(DlgIdA, #dialog.id, Dialogs) of
                true ->
                    DlgIdA;
                false ->
                    DlgIdB = nksip_dialog_lib:make_id(uas, SipMsg),
                    case lists:keymember(DlgIdB, #dialog.id, Dialogs) of
                        true -> DlgIdB;
                        false -> DlgIdA
                    end
            end
    end.




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
-spec get_sdp(nksip:request()|nksip:response(), nksip:invite()) ->
    {boolean(), #sdp{}|undefined, 
     nksip_call_dialog:sdp_offer(), nksip_call_dialog:sdp_offer()}.

get_sdp(#sipmsg{body=Body}, #invite{sdp_offer=Offer, sdp_answer=Answer}) ->
    case Body of
        #sdp{} = SDP -> 
            {true, SDP, Offer, Answer};
        _ -> 
            {false, undefined, Offer, Answer}
    end.


%% @private
-spec generate(nksip:method(), nksip:optslist(), nksip:dialog(), nksip_call:call()) ->
    {RUri, Opts, nksip:dialog()} 
    when RUri::nksip:uri(), Opts::nksip:optslist().

generate(Method, Opts, Dialog, _Call) ->
    #dialog{
        call_id = CallId,
        local_uri = From,
        remote_uri = To,
        local_seq = CurrentCSeq, 
        local_target = LocalTarget,
        remote_target = RUri, 
        route_set = RouteSet,
        invite = Invite
    } = Dialog,
    case nksip_lib:get_integer(cseq_num, Opts) of
        0 when Method == 'ACK' -> 
            #invite{request=#sipmsg{cseq={RCSeq, _}}} = Invite,
            LCSeq = CurrentCSeq;
        0 when CurrentCSeq > 0 -> 
            RCSeq = LCSeq = CurrentCSeq+1;
        0 -> 
            RCSeq = LCSeq = nksip_config:cseq()+1000;
        RCSeq when CurrentCSeq > 0 -> 
            LCSeq = CurrentCSeq;
        RCSeq -> 
            LCSeq = RCSeq
    end,
    Opts1 = 
        case Method of
            'ACK' ->
                #invite{request=#sipmsg{headers=Headers}} = Invite,
                Auths = nksip_lib:extract(Headers,
                                    [<<"authorization">>, <<"proxy-authorization">>]),
                [{add, Auth} || Auth <-Auths];
            _ ->
                []
        end
        ++
        [
            {from, From},
            {to, To},
            {call_id, CallId},
            {cseq_num, RCSeq},
            {route, RouteSet},
            case lists:member(contact, Opts) of
                true ->
                    ignore;
                false ->
                    case nksip_lib:get_value(contact, Opts, []) of
                        [] -> {contact, LocalTarget};
                        _ -> ignore
                    end
            end
            | Opts
        ],
    {RUri, Opts1, Dialog#dialog{local_seq=LCSeq}}.





