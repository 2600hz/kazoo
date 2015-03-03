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

%% @private Call UAC Management: Response processing
-module(nksip_call_uac_resp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([response/3]).

-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec response(nksip:response(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

response(Resp, UAC, Call) ->
    #sipmsg{
        class = {resp, Code, _Reason}, 
        id = MsgId, 
        dialog_id = DialogId,
        transport = Transport
    } = Resp,
    #trans{
        id = TransId, 
        start = Start, 
        status = Status,
        opts = Opts,
        method = Method,
        request = Req, 
        from = From
    } = UAC,
    #call{
        msgs = Msgs, 
        timers = #call_timers{trans=TransTime}
    } = Call,
    Now = nksip_lib:timestamp(),
    case Now-Start < TransTime of
        true -> 
            Code1 = Code,
            Resp1 = Resp;
        false -> 
            Code1 = 408,
            Reply = {timeout, <<"Transaction Timeout">>},
            {Resp1, _} = nksip_reply:reply(Req, Reply)
    end,
    Call1 = case Code1>=200 andalso Code1<300 of
        true -> 
            nksip_call_lib:update_auth(DialogId, Resp1, Call);
        false -> 
            Call
    end,
    UAC1 = UAC#trans{response=Resp1, code=Code1},
    Call2 = update(UAC1, Call1),
    NoDialog = lists:member(no_dialog, Opts),
    case Transport of
        undefined -> 
            ok;    % It is own-generated
        _ -> 
            ?call_debug("UAC ~p ~p (~p) ~sreceived ~p", 
                        [TransId, Method, Status, 
                         if NoDialog -> "(no dialog) "; true -> "" end, Code1])
    end,
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    Call3 = case NoDialog of
        false when is_record(Req, sipmsg) -> 
            nksip_call_uac_dialog:response(Req, Resp1, IsProxy, Call2);
        _ -> 
            Call2
    end,
    Call4 = case 
        Code>=300 andalso (Method=='SUBSCRIBE' orelse Method=='REFER')
    of
        true -> 
            nksip_call_event:remove_prov_event(Req, Call3);
        false -> 
            Call3
    end,
    Msg = {MsgId, TransId, DialogId},
    Call5 = Call4#call{msgs=[Msg|Msgs]},
    response_status(Status, Resp1, UAC1, update(UAC1, Call5)).


%% @private
-spec response_status(nksip_call_uac:status(), nksip:response(), 
                      nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

response_status(invite_calling, Resp, UAC, Call) ->
    UAC1 = UAC#trans{status=invite_proceeding},
    UAC2 = nksip_call_lib:retrans_timer(cancel, UAC1, Call),
    response_status(invite_proceeding, Resp, UAC2, Call);

response_status(invite_proceeding, Resp, #trans{code=Code}=UAC, Call) when Code < 200 ->
    #trans{request=Req, cancel=Cancel} = UAC,
    #call{app_id=AppId} = Call,
    % Add another 3 minutes
    UAC1 = nksip_call_lib:timeout_timer(timer_c, UAC, Call),
    Call1 = update(UAC1, Call),
    Call2 = nksip_call_uac_reply:reply({resp, Resp}, UAC1, Call1),
    Call3 = case Cancel of
        to_cancel -> nksip_call_uac:cancel(UAC1, [], Call2);
        _ -> Call2
    end,
    case AppId:nkcb_uac_response(Req, Resp, UAC1, Call3) of
        {continue, [_, _, _, Call4]} ->
            Call4;
        {ok, Call4} ->
            Call4
    end;


% Final 2xx response received
% Enters new RFC6026 'invite_accepted' state, to absorb 2xx retransmissions
% and forked responses
response_status(invite_proceeding, Resp, #trans{code=Code, opts=Opts}=UAC, Call) 
                when Code < 300 ->
    #sipmsg{to={_, ToTag}, dialog_id=DialogId} = Resp,
    Call1 = nksip_call_uac_reply:reply({resp, Resp}, UAC, Call),
    UAC1 = UAC#trans{
        cancel = undefined,
        status = invite_accepted, 
        response = undefined,       % Leave the request in case a new 2xx 
        to_tags = [ToTag]           % response is received
    },
    UAC2 = nksip_call_lib:expire_timer(cancel, UAC1, Call1),
    UAC3 = nksip_call_lib:timeout_timer(timer_m, UAC2, Call),
    Call2 = update(UAC3, Call1),
    case lists:member(auto_2xx_ack, Opts) of
        true -> 
            send_2xx_ack(DialogId, Call2);
        false -> 
            Call2
    end;


% Final [3456]xx response received, own error response
response_status(invite_proceeding, #sipmsg{transport=undefined}=Resp, UAC, Call) ->
    Call1 = nksip_call_uac_reply:reply({resp, Resp}, UAC, Call),
    UAC1 = UAC#trans{status=finished, cancel=undefined},
    UAC2 = nksip_call_lib:timeout_timer(cancel, UAC1, Call),
    UAC3 = nksip_call_lib:expire_timer(cancel, UAC2, Call),
    update(UAC3, Call1);


% Final [3456]xx response received, real response
response_status(invite_proceeding, Resp, UAC, Call) ->
    #sipmsg{to={To, ToTag}} = Resp,
    #trans{request=Req, proto=Proto} = UAC,
    #call{app_id=AppId} = Call,
    UAC1 = UAC#trans{
        request = Req#sipmsg{to={To, ToTag}}, 
        response = undefined, 
        to_tags = [ToTag], 
        cancel = undefined
    },
    UAC2 = nksip_call_lib:timeout_timer(cancel, UAC1, Call),
    UAC3 = nksip_call_lib:expire_timer(cancel, UAC2, Call),
    send_ack(UAC3, Call),
    UAC5 = case Proto of
        udp -> 
            UAC4 = UAC3#trans{status=invite_completed},
            nksip_call_lib:timeout_timer(timer_d, UAC4, Call);
        _ -> 
            UAC3#trans{status=finished}
    end,
    Call1 = update(UAC5, Call),
    case AppId:nkcb_uac_response(Req, Resp, UAC5, Call1) of
        {continue, [_Req6, Resp6, UAC6, Call6]} ->
            nksip_call_uac_reply:reply({resp, Resp6}, UAC6, Call6);
        {ok, Call2} ->
            Call2
    end;

response_status(invite_accepted, _Resp, #trans{code=Code}, Call) 
                when Code < 200 ->
    Call;

response_status(invite_accepted, Resp, UAC, Call) ->
    #sipmsg{to={_, ToTag}} = Resp,
    #trans{id=TransId, code=Code, status=Status, to_tags=ToTags} = UAC,
    case ToTags of
        [ToTag|_] ->
            ?call_debug("UAC ~p (~p) received ~p retransmission", [TransId, Status, Code]),
            Call;
        _ ->
            do_received_hangup(Resp, UAC, Call)
    end;

response_status(invite_completed, Resp, UAC, Call) ->
    #sipmsg{class={resp, RespCode, _Reason}, to={_, ToTag}} = Resp,
    #trans{id=TransId, code=Code, to_tags=ToTags} = UAC,
    case ToTags of 
        [ToTag|_] ->
            case RespCode of
                Code ->
                    send_ack(UAC, Call);
                _ ->
                    ?call_info("UAC ~p (invite_completed) ignoring new ~p response "
                               "(previous was ~p)", [TransId, RespCode, Code])
            end,
            Call;
        _ ->  
            do_received_hangup(Resp, UAC, Call)
    end;

response_status(trying, Resp, UAC, Call) ->
    UAC1 = UAC#trans{status=proceeding},
    UAC2 = nksip_call_lib:retrans_timer(cancel, UAC1, Call),
    response_status(proceeding, Resp, UAC2, Call);

response_status(proceeding, #sipmsg{class={resp, Code, _Reason}}=Resp, UAC, Call) 
                when Code < 200 ->
    nksip_call_uac_reply:reply({resp, Resp}, UAC, Call);

% Final response received, own error response
response_status(proceeding, #sipmsg{transport=undefined}=Resp, UAC, Call) ->
    Call1 = nksip_call_uac_reply:reply({resp, Resp}, UAC, Call),
    UAC1 = UAC#trans{status=finished},
    UAC2 = nksip_call_lib:timeout_timer(cancel, UAC1, Call),
    update(UAC2, Call1);

% Final response received, real response
response_status(proceeding, Resp, UAC, Call) ->
    #sipmsg{to={_, ToTag}} = Resp,
    #trans{request=Req, proto=Proto} = UAC,
    #call{app_id=AppId} = Call,
    UAC2 = case Proto of
        udp -> 
            UAC1 = UAC#trans{
                status = completed, 
                request = undefined, 
                response = undefined,
                to_tags = [ToTag]
            },
            nksip_call_lib:timeout_timer(timer_k, UAC1, Call);
        _ -> 
            UAC1 = UAC#trans{status=finished},
            nksip_call_lib:timeout_timer(cancel, UAC1, Call)
    end,
    Call1 = update(UAC2, Call),
    case AppId:nkcb_uac_response(Req, Resp, UAC2, Call1) of
        {continue, [_Req6, Resp6, UAC6, Call6]} ->
            nksip_call_uac_reply:reply({resp, Resp6}, UAC6, Call6);
        {ok, Call2} ->
            Call2
    end;

response_status(completed, Resp, UAC, Call) ->
    #sipmsg{class={resp, Code, _Reason}, cseq={_, Method}, to={_, ToTag}} = Resp,
    #trans{id=TransId, to_tags=ToTags} = UAC,
    case ToTags of
        [ToTag|_] ->
            ?call_info("UAC ~p ~p (completed) received ~p retransmission", 
                       [TransId, Method, Code]),
            Call;
        _ ->
            ?call_info("UAC ~p ~p (completed) received new ~p response", 
                       [TransId, Method, Code]),
            UAC1 = case lists:member(ToTag, ToTags) of
                true -> UAC;
                false -> UAC#trans{to_tags=ToTags++[ToTag]}
            end,
            update(UAC1, Call)
    end.


%% @private
-spec do_received_hangup(nksip:response(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

do_received_hangup(Resp, UAC, Call) ->
    #sipmsg{to={_, ToTag}, dialog_id=DialogId} = Resp,
    #trans{id=TransId, code=Code, status=Status, to_tags=ToTags} = UAC,
    UAC1 = case lists:member(ToTag, ToTags) of
        true -> UAC;
        false -> UAC#trans{to_tags=ToTags++[ToTag]}
    end,
    case Code < 300 of
        true ->
            ?call_info("UAC ~p (~p) sending ACK and BYE to secondary response " 
                       "(dialog ~s)", [TransId, Status, DialogId]),
            spawn(
                fun() ->
                    Handle = nksip_dialog_lib:get_handle(Resp),
                    case nksip_uac:ack(Handle, []) of
                        ok ->
                            case nksip_uac:bye(Handle, []) of
                                {ok, 200, []} ->
                                    ok;
                                ByeErr ->
                                    ?call_notice("UAC ~p could not send BYE: ~p", 
                                                 [TransId, ByeErr])
                            end;
                        AckErr ->
                            ?call_notice("UAC ~p could not send ACK: ~p", 
                                         [TransId, AckErr])
                    end
                end);
        false ->       
            ?call_info("UAC ~p (~p) received new ~p response",
                        [TransId, Status, Code])
    end,
    update(UAC1, Call).


%% @private
-spec send_ack(nksip_call:trans(), nksip_call:call()) ->
    ok.

send_ack(#trans{request=Req, id=TransId}, _Call) ->
    Ack = nksip_call_uac_make:make_ack(Req),
    case nksip_call_uac_transp:resend_request(Ack, []) of
        {ok, _} -> 
            ok;
        error -> 
            ?call_notice("UAC ~p could not send non-2xx ACK", [TransId])
    end.


%% @private
-spec send_2xx_ack(nksip_dialog_lib:id(), nksip_call:call()) ->
    nksip_call:call().

send_2xx_ack(DialogId, Call) ->
    case nksip_call_uac:dialog(DialogId, 'ACK', [async], Call) of
        {ok, Call1} ->
            Call1;
        {error, Error} ->
            ?call_warning("Could not generate 2xx ACK: ~p", [Error]),
            Call
    end.

