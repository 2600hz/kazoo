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

-export([pre_request/2, request/3, ack/2, response/4]).
-export([make/4, new_local_seq/2, uac_id/3]).
-import(nksip_call_dialog, [find/2, update/3, store/2]).


%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec pre_request(nksip:request(), nksip_call:call()) ->
    ok | {error, Error} 
    when Error :: unknown_dialog | request_pending.

pre_request(#sipmsg{class={req, 'ACK'}}, _) ->
    error(ack_in_dialog_pre_request);

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
        _ ->
            {error, unknown_dialog}
    end.


%% @private
-spec request(nksip:request(), boolean(), nksip_call:call()) ->
    nksip_call:call().

request(#sipmsg{class={req, Method}, dialog_id=DialogId}=Req, IsProxy, Call) ->
    ?call_debug("Dialog ~s UAC request ~p", [DialogId, Method]), 
    #dialog{local_seq=LocalSeq} = Dialog = find(DialogId, Call),
    #sipmsg{cseq={CSeq, _}} = Req,
    Dialog1 = case CSeq > LocalSeq of
        true -> Dialog#dialog{local_seq=CSeq};
        false -> Dialog
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
    update(none, Dialog#dialog{invite=Invite1}, Call);

do_request('BYE', _Req, _IsProxy, Dialog, Call) ->
    update({invite, bye}, Dialog, Call);

do_request('PRACK', Req, _IsProxy, #dialog{invite=Invite}=Dialog, Call) ->
    proceeding_uac = Invite#invite.status,
    {HasSDP, SDP, Offer, _Answer} = get_sdp(Req, Invite),
    case Offer of
        undefined when HasSDP -> 
            Invite1 = Invite#invite{sdp_offer={local, prack, SDP}},
            update(none, Dialog#dialog{invite=Invite1}, Call);
        {remote, invite, _} when HasSDP -> 
            Invite1 = Invite#invite{sdp_answer={local, prack, SDP}},
            update(prack, Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            % If {remote, invite, _} and no SDP, ACK must answer or delete
            update(none, Dialog, Call)
    end;

do_request('UPDATE', Req, _IsProxy, #dialog{invite=Invite}=Dialog, Call) ->
    {HasSDP, SDP, Offer, _} = get_sdp(Req, Invite),
    case Offer of
        undefined when HasSDP -> 
            Invite1 = Invite#invite{sdp_offer={local, update, SDP}},
            update(none, Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            update(none, Dialog, Call)
    end;    

do_request('SUBSCRIBE', Req, _IsProxy, Dialog, Call) ->
    Dialog1 = nksip_call_event:uac_request(Req, Dialog, Call),
    update(none, Dialog1, Call);
        
do_request('NOTIFY', Req, _IsProxy, Dialog, Call) ->
    Dialog1 = nksip_call_event:uac_request(Req, Dialog, Call),
    update(none, Dialog1, Call);

do_request('REFER', Req, IsProxy, Dialog, Call) ->
    do_request('SUBSCRIBE', Req, IsProxy, Dialog, Call);

do_request(_Method, _Req, _IsProxy, Dialog, Call) ->
    update(none, Dialog, Call).


%% @private
-spec response(nksip:request(), nksip:response(), boolean(), nksip_call:call()) ->
    nksip_call:call().

response(Req, Resp, IsProxy, Call) ->
    #sipmsg{class={req, Method}, body=Body} = Req,
    #sipmsg{class={resp, Code, _Reason}, dialog_id=DialogId} = Resp,
    case find(DialogId, Call) of
        #dialog{} = Dialog ->
            ?call_debug("Dialog ~s UAC response ~p ~p", [DialogId, Method, Code]),
            do_response(Method, Code, Req, Resp, Dialog, Call);
        not_found when Code>100, Code<300, Method=='INVITE' ->
            ?call_debug("Dialog ~s UAC response ~p ~p", [DialogId, Method, Code]),
            Dialog1 = nksip_call_dialog:create(uac, Req, Resp, Call),
            Offer = case Body of 
                #sdp{}=SDP -> {local, invite, SDP};
                _ -> undefined
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
-spec do_response(nksip:method(), nksip:response_code(), nksip:request(),
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
            % New answer to previous INVITE offer, it is not a new offer
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
        true -> update({invite, proceeding_uac}, Dialog1, Call);
        false -> update({invite, accepted_uac}, Dialog1, Call)
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
            case nksip_transport_uac:resend_request(ACK, []) of
                {ok, _} ->
                    ?call_info("Dialog ~s (accepted_uac) retransmitting 'ACK'", 
                               [DialogId]),
                    update(none, Dialog, Call);
                error ->
                    ?call_notice("Dialog ~s (accepted_uac) could not retransmit 'ACK'", 
                                 [DialogId]),
                    update({invite, {stop, 503}}, Dialog, Call)
            end;
        _ ->
            ?call_info("Dialog ~s (accepted_uac) received 'INVITE' ~p but no ACK yet", 
                       [DialogId, Code]),
            update(none, Dialog, Call)
    end;

do_response('INVITE', Code, _Req, _Resp, #dialog{id=DialogId}=Dialog, Call) ->
    case Dialog#dialog.invite of
        #invite{status=Status} -> ok;
        _ -> Status = undefined
    end,
    ?call_notice("Dialog UAC ~s ignoring unexpected INVITE response ~p in ~p", 
                 [DialogId, Code, Status]),
    update(none, Dialog, Call);

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
            update(none, Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            update(none, Dialog, Call)
    end;

do_response('PRACK', Code, _Req, _Resp, 
            #dialog{invite=#invite{}=Invite}=Dialog, Call) 
            when Code>300 ->
    case Invite#invite.sdp_offer  of
        {local, prack, _} -> 
            Invite1 = Invite#invite{sdp_offer=undefined, sdp_answer=undefined},
            update(none, Dialog#dialog{invite=Invite1}, Call);
        _ -> 
            update(none, Dialog, Call)
    end;
    
do_response('UPDATE', Code, Req, Resp,
            #dialog{invite=#invite{}=Invite}=Dialog, Call)
            when Code>=200, Code<300 ->
    {HasSDP, SDP, Offer, Answer} = get_sdp(Resp, Invite),
    {Offer1, Answer1} = case Offer of
        {local, update, _} when HasSDP -> {Offer, {remote, update, SDP}};
        {local, update, _} -> {undefined, undefined};
        _ -> {Offer, Answer}
    end,
    Invite1 = Invite#invite{sdp_offer=Offer1, sdp_answer=Answer1},
    update({update, uac, Req, Resp}, Dialog#dialog{invite=Invite1}, Call);

do_response('UPDATE', Code, _Req, _Resp, 
            #dialog{invite=#invite{}=Invite}=Dialog, Call)
            when Code>300 ->
    case Invite#invite.sdp_offer of
        {local, update, _} -> 
            Invite1 = Invite#invite{sdp_offer=undefined, sdp_answer=undefined},
            update(none, Dialog#dialog{invite=Invite1}, Call);
        _ ->
            update(none, Dialog, Call)
    end;

do_response('SUBSCRIBE', Code, Req, Resp, Dialog, Call) when Code>=200, Code<300 ->
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    update({subscribe, uac, Req, Resp}, Dialog1, Call);
        
do_response('SUBSCRIBE', Code, Req, Resp, Dialog, Call) when Code>=300 ->
    % If subscription ends, it will call nksip_call_dialog:update/3, removing
    % the dialog if no other use
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    update(none, Dialog1, Call);

do_response('NOTIFY', Code, Req, Resp, Dialog, Call) when Code>=200, Code<300 ->
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    update({notify, uac, Req, Resp}, Dialog1, Call);

do_response('NOTIFY', Code, Req, Resp, Dialog, Call) when Code>=300 ->
    Dialog1 = nksip_call_event:uac_response(Req, Resp, Dialog, Call),
    update(none, Dialog1, Call);

do_response('REFER', Code, Req, Resp, Dialog, Call) ->
    do_response('SUBSCRIBE', Code, Req, Resp, Dialog, Call);
    
do_response(_, _Code, _Req, _Resp, Dialog, Call) ->
    update(none, Dialog, Call).


%% @private
-spec ack(nksip:request(), nksip_call:call()) ->
    nksip_call:call().

ack(#sipmsg{class={req, 'ACK'}, to={_, <<>>}}, Call) ->
    ?call_notice("Dialog UAC invalid ACK1", []),
    Call;

ack(#sipmsg{class={req, 'ACK'}, cseq={CSeq, _}, dialog_id=DialogId}=AckReq, Call) ->
    case find(DialogId, Call) of
        #dialog{invite=#invite{}=Invite}=Dialog1 ->
            #invite{status=Status, request=InvReq} = Invite,
            #sipmsg{cseq={InvCSeq, _}} = InvReq,
            case Status of
                accepted_uac when CSeq==InvCSeq ->
                    ?call_debug("Dialog ~s (~p) UAC request 'ACK'", 
                                [DialogId, Status]),
                    {HasSDP, SDP, Offer, Answer} = get_sdp(AckReq, Invite), 
                    {Offer1, Answer1} = case Offer of
                        {remote, invite, _} when HasSDP -> {Offer, {local, ack, SDP}};
                        {remote, invite, _} -> {undefined, undefined};
                        _ -> {Offer, Answer}
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
                    update(none, Dialog1, Call)
            end;
        not_found ->
            ?call_notice("Dialog ~s not found for UAC ACK", [DialogId]),
            Call
    end.
    

 %% @private
-spec make(integer(), nksip:method(), nksip_lib:optslist(), nksip_call:call()) ->
    {ok, {AppId, RUri, Opts}, nksip_call:call()} | {error, Error}
    when Error :: invalid_dialog | unknown_dialog | unknown_subscription,
         AppId::nksip:app_id(), RUri::nksip:uri(), Opts::nksip_lib:optslist().

make(DialogId, Method, Opts, #call{dialogs=Dialogs}=Call) ->
    case lists:keyfind(DialogId, #dialog.id, Dialogs) of
        #dialog{invite=Invite}=Dialog ->
            ?call_debug("Dialog ~s make ~p request", [DialogId, Method]),
            case Invite of
                #invite{status=Status} when
                    Method=='ACK' andalso Status/=accepted_uac ->
                    {error, invalid_dialog};
                _ when Method=='SUBSCRIBE'; Method=='NOTIFY' ->
                    case nksip_call_event:request_uac_opts(Method, Opts, Dialog) of
                        {ok, Opts1} -> 
                            {Result, Dialog1} = generate(Method, Opts1, Dialog, Call),
                            {ok, Result, store(Dialog1, Call)};
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    {Result, Dialog1} = generate(Method, Opts, Dialog, Call),
                    {ok, Result, store(Dialog1, Call)}
            end;
        _ ->
            {error, unknown_dialog}
    end.


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


%% @private Helps generating the transaction id
-spec uac_id(nksip:request()|nksip:response(), boolean(), nksip_call:call()) ->
    nksip_dialog:id().

uac_id(SipMsg, IsProxy, #call{dialogs=Dialogs}) ->
    case nksip_dialog:make_id(uac, SipMsg) of
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
                    DlgIdB = nksip_dialog:make_id(uas, SipMsg),
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
        #sdp{} = SDP -> {true, SDP, Offer, Answer};
        _ -> {false, undefined, Offer, Answer}
    end.


%% @private
-spec generate(nksip:method(), nksip_lib:optslist(), nksip:dialog(), nksip_call:call()) ->
    {{RUri, Opts}, nksip:dialog()} 
    when RUri::nksip:uri(), Opts::nksip_lib:optslist().

generate(Method, Opts, Dialog, Call) ->
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
        case lists:keymember(session_expires, 1, Opts) of
            true -> [];
            false -> nksip_call_timer:uac_update_timer(Method, Dialog, Call)
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
    {{RUri, Opts1}, Dialog#dialog{local_seq=LCSeq}}.





