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

%% @private Call dialog library module.
-module(nksip_call_dialog).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-include("nksip_call.hrl").

-export([create/4, update/3, stop/3, find/2, store/2, get_meta/3, update_meta/4]).
-export([target_update/5, session_update/2, route_update/4]).
-export([sip_dialog_update/3, sip_session_update/3, reason/1]).
-export([timer/3]).
-export_type([sdp_offer/0]).

-type sdp_offer() ::
    {local|remote, nksip:method(), nksip_sdp:sdp()} | undefined.


%% ===================================================================
%% Private
%% ===================================================================

%% @private Creates a new dialog
-spec create(uac|uas, nksip:request(), nksip:response(), nksip_call:call()) ->
    nksip:dialog().

create(Class, Req, Resp, Call) ->
    #sipmsg{ruri=#uri{scheme=Scheme}} = Req,
    #sipmsg{
        app_id = AppId,
        call_id = CallId, 
        dialog_id = DialogId,
        from = {From, FromTag},
        to = {To, _},
        cseq = {CSeq, _},
        transport = #transport{proto=Proto}
    } = Resp,
    UA = case Class of uac -> "UAC"; uas -> "UAS" end,
    ?call_debug("Dialog ~s ~s created", [DialogId, UA]),
    nksip_counters:async([nksip_dialogs]),
    Now = nksip_lib:timestamp(),
    Dialog = #dialog{
        id = DialogId,
        app_id = AppId,
        call_id = CallId, 
        created = Now,
        updated = Now,
        local_target = #uri{},
        remote_target = #uri{},
        route_set = [],
        blocked_route_set = false,
        early = true,
        secure = Proto==tls andalso Scheme==sips,
        caller_tag = FromTag,
        invite = undefined,
        subscriptions = [],
        meta = []
    },
    sip_dialog_update(start, Dialog, Call),
    case Class of 
        uac ->
            Dialog#dialog{
                local_seq = CSeq,
                remote_seq = 0,
                local_uri = From,
                remote_uri = To
            };
        uas ->
            Dialog#dialog{
                local_seq = 0,
                remote_seq = CSeq,
                local_uri = To,
                remote_uri = From
            }
    end.


%% @private
-spec update(term(), nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().


update(Type, Dialog, #call{app_id=AppId}=Call) ->
    case AppId:nkcb_dialog_update(Type, Dialog, Call) of
        {continue, [Type1, Dialog1, Call1]} ->
            do_update(Type1, Dialog1, Call1);
        {ok, Call1} ->
            Call1
    end.


%% @private
-spec do_update(term(), nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().

do_update({invite, {stop, Reason}}, #dialog{invite=Invite}=Dialog, Call) ->
    #invite{
        media_started = Media,
        retrans_timer = RetransTimer,
        timeout_timer = TimeoutTimer
    } = Invite,    
    cancel_timer(RetransTimer),
    cancel_timer(TimeoutTimer),
    sip_dialog_update({invite_status, {stop, reason(Reason)}}, Dialog, Call),
    case Media of
        true -> sip_session_update(stop, Dialog, Call);
        _ -> ok
    end,
    store(Dialog#dialog{invite=undefined}, Call);

do_update({invite, Status}, Dialog, Call) ->
    #dialog{
        id = DialogId, 
        blocked_route_set = BlockedRouteSet,
        invite = #invite{
            status = OldStatus, 
            media_started = Media,
            class = Class,
            request = Req, 
            response = Resp
        } = Invite
    } = Dialog,
    Dialog1 = case Status of
        OldStatus -> 
            Dialog;
        _ -> 
            sip_dialog_update({invite_status, Status}, Dialog, Call),
            Dialog#dialog{invite=Invite#invite{status=Status}}
    end,
    ?call_debug("Dialog ~s ~p -> ~p", [DialogId, OldStatus, Status]),
    Dialog2 = if
        Status==proceeding_uac; Status==proceeding_uas; 
        Status==accepted_uac; Status==accepted_uas ->
            D1 = route_update(Class, Req, Resp, Dialog1),
            D2 = target_update(Class, Req, Resp, D1, Call),
            session_update(D2, Call);
        Status==confirmed ->
            session_update(Dialog1, Call);
        Status==bye ->
            case Media of
                true -> 
                    sip_session_update(stop, Dialog1, Call),
                    #dialog{invite=I1} = Dialog1,
                    Dialog1#dialog{invite=I1#invite{media_started=false}};
                _ ->
                    Dialog1
            end
    end,
    Dialog3 = case 
        (not BlockedRouteSet) andalso 
        (Status==accepted_uac orelse Status==accepted_uas)
    of
        true -> Dialog2#dialog{blocked_route_set=true};
        false -> Dialog2
    end,
    Dialog4 = timer_update(Req, Resp, Class, Dialog3, Call),
    store(Dialog4, Call);

do_update(prack, Dialog, Call) ->
    Dialog1 = session_update(Dialog, Call),
    store(Dialog1, Call);

do_update({update, Class, Req, Resp}, Dialog, Call) ->
    Dialog1 = target_update(Class, Req, Resp, Dialog, Call),
    Dialog2 = session_update(Dialog1, Call),
    Dialog3 = timer_update(Req, Resp, Class, Dialog2, Call),
    store(Dialog3, Call);

do_update({subscribe, Class, Req, Resp}, Dialog, Call) ->
    Dialog1 = route_update(Class, Req, Resp, Dialog),
    Dialog2 = target_update(Class, Req, Resp, Dialog1, Call),
    store(Dialog2, Call);

do_update({notify, Class, Req, Resp}, Dialog, Call) ->
    Dialog1 = route_update(Class, Req, Resp, Dialog),
    Dialog2 = target_update(Class, Req, Resp, Dialog1, Call),
    Dialog3 = case Dialog2#dialog.blocked_route_set of
        true -> Dialog2;
        false -> Dialog2#dialog{blocked_route_set=true}
    end,
    store(Dialog3, Call);

do_update(none, Dialog, Call) ->
    store(Dialog, Call).
    

%% @private Performs a target update
-spec target_update(uac|uas, nksip:request(), nksip:response(), 
                    nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

target_update(Class, Req, Resp, Dialog, Call) ->
    #dialog{
        id = DialogId,
        early = Early, 
        secure = Secure,
        remote_target = RemoteTarget,
        local_target = LocalTarget,
        invite = Invite
    } = Dialog,
    #sipmsg{contacts=ReqContacts} = Req,
    #sipmsg{class={resp, Code, _Reason}, contacts=RespContacts} = Resp,
    if 
        Class==uac; Class==proxy ->
            RemoteTargets = RespContacts,
            LocalTargets = ReqContacts;
        Class==uas -> 
            RemoteTargets = ReqContacts,
            LocalTargets = RespContacts
    end,
    RemoteTarget1 = case RemoteTargets of
        [RT] ->
            case Secure of
                true -> RT#uri{scheme=sips};
                false -> RT
            end;
        [] ->
            ?call_notice("Dialog ~s: no Contact in remote target", [DialogId]),
            RemoteTarget;
        RTOther -> 
            ?call_notice("Dialog ~s: invalid Contact in remote rarget: ~p",
                         [DialogId, RTOther]),
            RemoteTarget
    end,
    LocalTarget1 = case LocalTargets of
        [LT] -> LT;
        _ -> LocalTarget
    end,
    Now = nksip_lib:timestamp(),
    Early1 = Early andalso Code >= 100 andalso Code < 200,
    case RemoteTarget of
        #uri{domain = <<"invalid.invalid">>} -> ok;
        RemoteTarget1 -> ok;
        _ -> sip_dialog_update(target_update, Dialog, Call)
    end,
    Invite1 = case Invite of
        #invite{answered=InvAnswered, class=InvClass, request=InvReq} ->
            InvAnswered1 = case InvAnswered of
                undefined when Code >= 200 -> Now;
                _ -> InvAnswered
            end,
            % If we are updating the remote target inside an uncompleted INVITE UAS
            % transaction, update original INVITE so that, when the final
            % response is sent, we don't use the old remote target but the new one.
            InvReq1 = if
                InvClass==uac; InvClass==proxy ->
                    case InvReq of
                        #sipmsg{contacts=[LocalTarget1]} -> InvReq; 
                        #sipmsg{} -> InvReq#sipmsg{contacts=[LocalTarget1]}
                    end;
                InvClass==uas ->
                    case InvReq of
                        #sipmsg{contacts=[RemoteTarget1]} -> InvReq; 
                        #sipmsg{} -> InvReq#sipmsg{contacts=[RemoteTarget1]}
                    end
            end,
            Invite#invite{answered=InvAnswered1, request=InvReq1};
        undefined ->
            undefined
    end,
    Dialog#dialog{
        updated = Now,
        local_target = LocalTarget1,
        remote_target = RemoteTarget1,
        early = Early1,
        invite = Invite1
    }.


%% @private
-spec route_update(uac|uas, nksip:request(), nksip:response(), nksip:dialog()) ->
    nksip:dialog().

route_update(Class, Req, Resp, #dialog{blocked_route_set=false}=Dialog) ->
    #dialog{app_id=AppId} = Dialog,
    RouteSet = if
        Class==uac; Class==proxy ->
            RR = nksip_sipmsg:header(<<"record-route">>, Resp, uris),
            case lists:reverse(RR) of
                [] ->
                    [];
                [FirstRS|RestRS] ->
                    % If this a proxy, it has inserted Record-Route,
                    % and wants to send an in-dialog request (for example to send BYE)
                    % we must remove our own inserted Record-Route
                    case nksip_transport:is_local(AppId, FirstRS) of
                        true -> RestRS;
                        false -> [FirstRS|RestRS]
                    end
            end;
        Class==uas ->
            RR = nksip_sipmsg:header(<<"record-route">>, Req, uris),
            case RR of
                [] ->
                    [];
                [FirstRS|RestRS] ->
                    case nksip_transport:is_local(AppId, FirstRS) of
                        true -> RestRS;
                        false -> [FirstRS|RestRS]
                    end
            end
    end,
    Dialog#dialog{route_set=RouteSet};

route_update(_Class, _Req, _Resp, Dialog) ->
    Dialog.


% %% @private Performs a session update
-spec session_update(nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

session_update(
            #dialog{
                invite = #invite{
                    sdp_offer = {OfferParty, _, #sdp{}=OfferSDP},
                    sdp_answer = {AnswerParty, _, #sdp{}=AnswerSDP},
                    local_sdp = LocalSDP,
                    remote_sdp = RemoteSDP,
                    media_started = Started
                } = Invite
            } = Dialog,
            Call) ->
    {LocalSDP1, RemoteSDP1} = case OfferParty of
        local when AnswerParty==remote -> {OfferSDP, AnswerSDP};
        remote when AnswerParty==local -> {AnswerSDP, OfferSDP}
    end,
    case Started of
        true ->
            case 
                nksip_sdp:is_new(RemoteSDP1, RemoteSDP) orelse
                nksip_sdp:is_new(LocalSDP1, LocalSDP) 
            of
                true -> 
                    sip_session_update({update, LocalSDP1, RemoteSDP1}, Dialog, Call);
                false ->
                    ok
            end;
        _ ->
            sip_session_update({start, LocalSDP1, RemoteSDP1}, Dialog, Call)
    end,
    Invite1 = Invite#invite{
        local_sdp = LocalSDP1, 
        remote_sdp = RemoteSDP1, 
        media_started = true,
        sdp_offer = undefined,
        sdp_answer = undefined
    },
    Dialog#dialog{invite=Invite1};
            
session_update(Dialog, _Call) ->
    Dialog.


%% @private
-spec timer_update(nksip:request(), nksip:response(), uac|uas,
                   nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

timer_update(_Req, #sipmsg{class={resp, Code, _}}, _Class,
             #dialog{invite=#invite{status=confirmed}}=Dialog, Call) ->
    #dialog{id=DialogId, invite=Invite} = Dialog,
    #call{app_id=AppId} = Call,
    % class from #invite{} can only be used for INVITE, not UPDATE
    #invite{retrans_timer=RetransTimer, timeout_timer=TimeoutTimer} = Invite,
    cancel_timer(RetransTimer),
    case Code>=200 andalso Code<300 of
        true -> 
            cancel_timer(TimeoutTimer),
            Timeout = nksip_sipapp_srv:config(AppId, dialog_timeout),
            Invite1 = Invite#invite{
                retrans_timer = undefined,
                timeout_timer = start_timer(1000*Timeout, invite_timeout, DialogId)
            },
            Dialog#dialog{invite=Invite1};
        false ->
            % We are returning to confirmed after a non-2xx response
            Invite1 = Invite#invite{
                retrans_timer = undefined
            },
            Dialog#dialog{invite=Invite1}
    end;

timer_update(_Req, _Resp, _Class, 
             #dialog{invite=#invite{status=accepted_uas}}=Dialog, Call) ->
    #dialog{id=DialogId, invite=Invite} = Dialog,
    #invite{retrans_timer=RetransTimer, timeout_timer=TimeoutTimer} = Invite,
    #call{timers=#call_timers{t1=T1}} = Call,
    cancel_timer(RetransTimer),
    cancel_timer(TimeoutTimer),
    Invite1 = Invite#invite{
        retrans_timer = start_timer(T1, invite_retrans, DialogId),
        next_retrans = 2*T1,
        timeout_timer = start_timer(64*T1, invite_timeout, DialogId)
    },
    Dialog#dialog{invite=Invite1};

timer_update(_Req, _Resp, _Class, Dialog, Call) ->
    #dialog{id=DialogId, invite=Invite} = Dialog,
    #invite{retrans_timer=RetransTimer, timeout_timer=TimeoutTimer} = Invite,
    #call{timers=#call_timers{t1=T1}} = Call,
    cancel_timer(RetransTimer),
    cancel_timer(TimeoutTimer),
    Invite1 = Invite#invite{
        retrans_timer = undefined,
        timeout_timer = start_timer(64*T1, invite_timeout, DialogId)
    },
    Dialog#dialog{invite=Invite1}.


%% @private Fully stops a dialog
-spec stop(term(), nksip:dialog(), nksip_call:call()) ->
    nksip_call:call(). 

stop(Reason, #dialog{invite=Invite, subscriptions=Subs}=Dialog, Call) ->
    Dialog1 = lists:foldl(
        fun(Sub, Acc) -> nksip_call_event:stop(Sub, Acc, Call) end,
        Dialog,
        Subs),
    case Invite of
        #invite{} -> 
            update({invite, {stop, reason(Reason)}}, Dialog1, Call);
        undefined -> 
            update(none, Dialog1, Call)
    end.


%% @private Gets a value from dialog's meta, or call's meta if no dialog found
-spec get_meta(term(), nksip_dialog_lib:id(), nksip_call:call()) ->
    term() | undefined.

get_meta(Key, DialogId, Call) ->
    case find(DialogId, Call) of
        #dialog{meta=DlgMeta} -> nksip_lib:get_value(Key, DlgMeta);
        not_found -> nksip_lib:get_value(Key, Call#call.meta)
    end.


%% @private Stores a value in dialog's meta, or call's meta if no dialog found
-spec update_meta(term(), term(), nksip_dialog_lib:id(), nksip_call:call()) ->
    nksip_call:call().

update_meta(Key, Value, DialogId, Call) ->
    case find(DialogId, Call) of
        #dialog{meta=DialogMeta1} = Dialog1 ->
            DialogMeta2 = nksip_lib:store_value(Key, Value, DialogMeta1),
            Dialog2 = Dialog1#dialog{meta=DialogMeta2},
            ?call_debug("Meta {~p,~p} updated in dialog", [Key, Value]),
            store(Dialog2, Call);
        not_found ->
            #call{meta=CallMeta1} = Call,
            CallMeta2 = nksip_lib:store_value(Key, Value, CallMeta1),
            ?call_debug("Meta {~p,~p} updated in call", [Key, Value]),
            Call#call{meta=CallMeta2}
    end.


%% @private Called when a dialog timer is fired
-spec timer(invite_retrans|invite_timeout|invite_refresh, 
            nksip_dialog_lib:id(), nksip_call:call()) ->
    nksip_call:call().

   
timer(Tag, Id, Call) ->
    case find(Id, Call) of
        #dialog{} = Dialog ->
            do_timer(Tag, Dialog, Call);
        not_found ->
            ?call_warning("Call ignoring dialog timer (~p, ~p)", [Tag, Id]),
            Call
    end.


%% @private Called when a dialog timer is fired
-spec do_timer(invite_retrans|invite_timeout|invite_refresh, 
            nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().

do_timer(invite_retrans, #dialog{id=DialogId, invite=Invite}=Dialog, Call) ->
    case Invite of
        #invite{status=Status, response=Resp, next_retrans=Next} ->
            case Status of
                accepted_uas ->
                    case nksip_call_uas_transp:resend_response(Resp, []) of
                        {ok, _} ->
                            ?call_info("Dialog ~s resent response", [DialogId]),
                            #call{timers=#call_timers{t2=T2}} = Call,
                            Invite1 = Invite#invite{
                                retrans_timer = start_timer(Next, invite_retrans, DialogId),
                                next_retrans = min(2*Next, T2)
                            },
                            update(none, Dialog#dialog{invite=Invite1}, Call);
                        error ->
                            ?call_notice("Dialog ~s could not resend response", 
                                         [DialogId]),
                            update({invite, {stop, ack_timeout}}, Dialog, Call)
                    end;
                _ ->
                    ?call_notice("Dialog ~s retrans timer fired in ~p", 
                                [DialogId, Status]),
                    Call
            end;
        undefined ->
            ?call_notice("Dialog ~s retrans timer fired with no INVITE", 
                         [DialogId]),
            Call
    end;

do_timer(invite_refresh, #dialog{invite=Invite}=Dialog, Call) ->
    #invite{local_sdp=SDP} = Invite,
    sip_dialog_update({invite_refresh, SDP}, Dialog, Call),
    Call;

do_timer(invite_timeout, #dialog{id=DialogId, invite=Invite}=Dialog, Call) ->
    case Invite of
        #invite{class=Class, status=Status} ->
            ?call_notice("Dialog ~s (~p) timeout timer fired", [DialogId, Status]),
            case Class of
                proxy ->
                    update({invite, {stop, timeout}}, Dialog, Call);
                _ ->
                    ?call_notice("Dialog ~s sending BYE on timeout", [DialogId]),
                    case 
                        nksip_call_uac:dialog(DialogId, 'BYE', 
                            [async, {reason, {sip, 408, "Dialog Timeout"}}], Call) 
                    of
                        {ok, Call1} ->
                            sip_dialog_update(invite_timeout, Dialog, Call),
                            Call1;
                        {error, Error} ->
                            ?call_warning("Could not send timeout BYE: ~p", [Error]),
                            update({invite, {stop, timeout}}, Dialog, Call)
                    end
            end;
        _ ->
            ?call_notice("Dialog ~s unknown INVITE timeout timer", [DialogId]),
            Call
    end;

do_timer({event, Tag}, Dialog, Call) ->
    nksip_call_event:timer(Tag, Dialog, Call).



%% ===================================================================
%% Util
%% ===================================================================

%% @private
-spec find(nksip_dialog_lib:id(), nksip_call:call()) ->
    nksip:dialog() | not_found.

find(Id, #call{dialogs=Dialogs}) ->
    do_find(Id, Dialogs).


%% @private
-spec do_find(nksip_dialog_lib:id(), [nksip:dialog()]) ->
    nksip:dialog() | not_found.

do_find(_, []) -> not_found;
do_find(Id, [#dialog{id=Id}=Dialog|_]) -> Dialog;
do_find(Id, [_|Rest]) -> do_find(Id, Rest).


%% @private Updates a dialog into the call
-spec store(nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().

store(#dialog{}=Dialog, #call{dialogs=Dialogs}=Call) ->
    #dialog{id=Id, invite=Invite, subscriptions=Subs} = Dialog,
    case Dialogs of
        [] -> Rest = [], IsFirst = true;
        [#dialog{id=Id}|Rest] -> IsFirst = true;
        _ -> Rest=undefined, IsFirst = false
    end,
    case Invite==undefined andalso Subs==[] of
        true ->
            sip_dialog_update(stop, Dialog, Call),
            Dialogs1 = case IsFirst of
                true -> Rest;
                false -> lists:keydelete(Id, #dialog.id, Dialogs)
            end,
            Call#call{dialogs=Dialogs1, hibernate=dialog_stop};
        false ->
            Hibernate = case Invite of
                #invite{status=confirmed} -> 
                    dialog_confirmed;
                _ -> 
                    Call#call.hibernate
            end,
            Dialogs1 = case IsFirst of
                true -> 
                    [Dialog|Rest];
                false -> 
                    [Dialog|lists:keydelete(Id, #dialog.id, Dialogs)]
            end,
            Call#call{dialogs=Dialogs1, hibernate=Hibernate}
    end.


%% @private
-spec sip_dialog_update(term(), nksip:dialog(), nksip_call:call()) ->
    ok.

sip_dialog_update(Arg, Dialog, #call{app_id=AppId}=Call) ->
    AppId:nkcb_call(sip_dialog_update, [Arg, Dialog, Call], AppId),
    ok.


%% @private
-spec sip_session_update(term(), nksip:dialog(), nksip_call:call()) ->
    ok.

sip_session_update(Arg, Dialog, #call{app_id=AppId}=Call) ->
    AppId:nkcb_call(sip_session_update, [Arg, Dialog, Call], AppId),
    ok.



%% @private
reason(486) -> busy;
reason(487) -> cancelled;
reason(503) -> service_unavailable;
reason(603) -> declined;
reason(Other) -> Other.


%% @private
cancel_timer(Ref) ->
    nksip_lib:cancel_timer(Ref).


%% @private
-spec start_timer(integer(), atom(), nksip_dialog_lib:id()) ->
    reference().

start_timer(Time, Tag, Id) ->
    erlang:start_timer(round(Time) , self(), {dlg, Tag, Id}).

