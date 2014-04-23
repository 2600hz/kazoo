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

%% @doc Call UAC Management: Response processing
-module(nksip_call_uac_resp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([response/2]).

-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").

-define(MAX_AUTH_TRIES, 5).


%% ===================================================================
%% Private
%% ===================================================================


%% @doc Called when a new response is received.
-spec response(nksip:response(), nksip_call:call()) ->
    nksip_call:call().

response(Resp, #call{trans=Trans}=Call) ->
    #sipmsg{class={resp, Code, _Reason}, cseq={_, Method}} = Resp,
    TransId = nksip_call_uac:transaction_id(Resp),
    case lists:keyfind(TransId, #trans.trans_id, Trans) of
        #trans{class=uac, from=From, ruri=RUri}=UAC -> 
            IsProxy = case From of {fork, _} -> true; _ -> false end,
            DialogId = nksip_call_uac_dialog:uac_id(Resp, IsProxy, Call),
            Resp1 = Resp#sipmsg{ruri=RUri, dialog_id=DialogId},
            case is_prack_retrans(Resp1, UAC) of
                true ->
                    ?call_info("UAC received retransmission of reliable provisional "
                               "response", []),
                    Call;
                false ->
                    response(Resp1, UAC, Call)
            end;
        _ -> 
            ?call_info("UAC received ~p ~p response for unknown request", [Method, Code]),
            Call
    end.


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
        id = Id, 
        start = Start, 
        status = Status,
        opts = Opts,
        method = Method,
        request = Req, 
        from = From
    } = UAC,
    #call{msgs=Msgs} = Call,
    Now = nksip_lib:timestamp(),
    case Now-Start < ?MAX_TRANS_TIME of
        true -> 
            Code1 = Code,
            Resp1 = Resp;
        false -> 
            Code1 = 408,
            Reply = {timeout, <<"Transaction Timeout">>},
            {Resp1, _} = nksip_reply:reply(Req, Reply)
    end,
    Call1 = case Code1>=200 andalso Code1<300 of
        true -> nksip_call_lib:update_auth(DialogId, Resp1, Call);
        false -> Call
    end,
    UAC1 = UAC#trans{response=Resp1, code=Code1},
    Call2 = update(UAC1, Call1),
    NoDialog = lists:member(no_dialog, Opts),
    case Transport of
        undefined -> 
            ok;    % It is own-generated
        _ -> 
            ?call_debug("UAC ~p ~p (~p) ~sreceived ~p", 
                        [Id, Method, Status, 
                         if NoDialog -> "(no dialog) "; true -> "" end, Code1])
    end,
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    Resp2 = case IsProxy of
        true -> nksip_call_timer:proxy_response(Req, Resp1);
        false -> Resp1
    end,
    Call3 = case NoDialog of
        false when is_record(Req, sipmsg) -> 
            nksip_call_uac_dialog:response(Req, Resp2, IsProxy, Call2);
        _ -> 
            Call2
    end,
    Call4 = case 
        Code>=300 andalso (Method=='SUBSCRIBE' orelse Method=='REFER')
    of
        true -> nksip_call_event:remove_prov_event(Req, Call3);
        false -> Call3
    end,
    Msg = {MsgId, Id, DialogId},
    Call5 = Call4#call{msgs=[Msg|Msgs]},
    Call6 = response_status(Status, Resp2, UAC1, Call5),
    check_prack(Resp, UAC1, Call6).


%% @private
-spec response_status(nksip_call_uac:status(), nksip:response(), 
                      nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

response_status(invite_calling, Resp, UAC, Call) ->
    UAC1 = UAC#trans{status=invite_proceeding},
    UAC2 = nksip_call_lib:retrans_timer(cancel, UAC1, Call),
    response_status(invite_proceeding, Resp, UAC2, Call);

response_status(invite_proceeding, Resp, #trans{code=Code}=UAC, Call) 
                   when Code < 200 ->
    #trans{cancel=Cancel} = UAC,
    % Add another 3 minutes
    UAC1 = nksip_call_lib:timeout_timer(timer_c, UAC, Call),
    Call1 = nksip_call_uac_reply:reply({resp, Resp}, UAC1, Call),
    case Cancel of
        to_cancel -> nksip_call_uac:cancel(UAC1, undefined, update(UAC1, Call1));
        _ -> update(UAC1, Call1)
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
        true -> send_2xx_ack(DialogId, Call2);
        false -> Call2
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
    received_gruu(Req, Resp, UAC5, update(UAC5, Call));


response_status(invite_accepted, _Resp, #trans{code=Code}, Call) 
                   when Code < 200 ->
    Call;

response_status(invite_accepted, Resp, UAC, Call) ->
    #sipmsg{to={_, ToTag}} = Resp,
    #trans{id=Id, code=Code, status=Status, to_tags=ToTags} = UAC,
    case ToTags of
        [ToTag|_] ->
            ?call_debug("UAC ~p (~p) received ~p retransmission", [Id, Status, Code]),
            Call;
        _ ->
            do_received_hangup(Resp, UAC, Call)
    end;

response_status(invite_completed, Resp, UAC, Call) ->
    #sipmsg{class={resp, RespCode, _Reason}, to={_, ToTag}} = Resp,
    #trans{id=Id, code=Code, to_tags=ToTags} = UAC,
    case ToTags of 
        [ToTag|_] ->
            case RespCode of
                Code ->
                    send_ack(UAC, Call);
                _ ->
                    ?call_info("UAC ~p (invite_completed) ignoring new ~p response "
                               "(previous was ~p)", [Id, RespCode, Code])
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
    #trans{proto=Proto, request=Req} = UAC,
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
    received_gruu(Req, Resp, UAC2, update(UAC2, Call));

response_status(completed, Resp, UAC, Call) ->
    #sipmsg{class={resp, Code, _Reason}, cseq={_, Method}, to={_, ToTag}} = Resp,
    #trans{id=Id, to_tags=ToTags} = UAC,
    case ToTags of
        [ToTag|_] ->
            ?call_info("UAC ~p ~p (completed) received ~p retransmission", 
                       [Id, Method, Code]),
            Call;
        _ ->
            ?call_info("UAC ~p ~p (completed) received new ~p response", 
                       [Id, Method, Code]),
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
    #trans{id=Id, code=Code, status=Status, to_tags=ToTags} = UAC,
    UAC1 = case lists:member(ToTag, ToTags) of
        true -> UAC;
        false -> UAC#trans{to_tags=ToTags++[ToTag]}
    end,
    case Code < 300 of
        true ->
            ?call_info("UAC ~p (~p) sending ACK and BYE to secondary response " 
                       "(dialog ~s)", [Id, Status, DialogId]),
            spawn(
                fun() ->
                    case nksip_uac:ack(nksip_sipmsg:get_id(Resp), []) of
                        ok ->
                            case nksip_uac:bye(nksip_sipmsg:get_id(Resp), []) of
                                {ok, 200, []} ->
                                    ok;
                                ByeErr ->
                                    ?call_notice("UAC ~p could not send BYE: ~p", 
                                                 [Id, ByeErr])
                            end;
                        AckErr ->
                            ?call_notice("UAC ~p could not send ACK: ~p", 
                                         [Id, AckErr])
                    end
                end);
        false ->       
            ?call_info("UAC ~p (~p) received new ~p response",
                        [Id, Status, Code])
    end,
    update(UAC1, Call).


%% @private 
-spec received_gruu(nksip:request(), nksip:response(), 
                       nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

received_gruu(Req, Resp, UAC, Call) ->
    #trans{method = Method, code=Code} = UAC,
    #sipmsg{contacts=Contacts} = Resp,
    #call{app_id=AppId} = Call,
    case Method=='REGISTER' andalso Code>=200 andalso Code<300 of
        true -> find_gruus(AppId, Contacts);
        false -> ok
    end,
    received_auth(Req, Resp, UAC, Call).


%% @private
find_gruus(AppId, [#uri{ext_opts=Opts}|Rest]) ->
    HasPubGruu = case nksip_lib:get_value(<<"pub-gruu">>, Opts) of
        undefined -> 
            false;
        PubGruu ->
            case nksip_parse:ruris(nksip_lib:unquote(PubGruu)) of
                [PubUri] -> 
                    nksip_config:put({nksip_gruu_pub, AppId}, PubUri),
                    true;
                _ -> 
                    false
            end
    end,
    HasTmpGruu = case nksip_lib:get_value(<<"temp-gruu">>, Opts) of
        undefined -> 
            false;
        TempGruu ->
            case nksip_parse:ruris(nksip_lib:unquote(TempGruu)) of
                [TempUri] -> 
                    nksip_config:put({nksip_gruu_temp, AppId}, TempUri),
                    true;
                _ -> 
                    false
            end
    end,
    case HasPubGruu andalso HasTmpGruu of
        true -> ok;
        false -> find_gruus(AppId, Rest)
    end;

find_gruus(_, []) ->
    ok.


%% @private 
-spec received_auth(nksip:request(), nksip:response(), 
                       nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

received_auth(Req, Resp, UAC, Call) ->
     #trans{
        id = Id,
        opts = Opts,
        method = Method, 
        code = Code, 
        iter = Iter,
        from = From
    } = UAC,
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    case 
        (Code==401 orelse Code==407) andalso Iter < ?MAX_AUTH_TRIES
        andalso Method/='CANCEL' andalso (not IsProxy) andalso
        nksip_auth:make_request(Req, Resp, Opts) 
    of
        false ->
            received_422(Req, Resp, UAC, Call);
        {ok, Req1} ->
            nksip_call_uac_req:resend(Req1, UAC, Call);
        {error, Error} ->
            ?call_debug("UAC ~p could not generate new auth request: ~p", [Id, Error]),    
            received_422(Req, Resp, UAC, Call)
    end.


%% @private 
-spec received_422(nksip:request(), nksip:response(), 
                       nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

received_422(Req, Resp, UAC, Call) ->
    #trans{from=From} = UAC,
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    case 
        (not IsProxy) andalso 
        nksip_call_timer:uac_received_422(Req, Resp, UAC, Call) 
    of
        {resend, Req1, Call1} ->
            nksip_call_uac_req:resend(Req1, UAC, Call1);
        false ->
            received_reply(Resp, UAC, Call)
    end.


%% @private 
-spec received_reply(nksip:response(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

received_reply(Resp, UAC, Call) ->
    nksip_call_uac_reply:reply({resp, Resp}, UAC, Call) .


%% @private
-spec send_ack(nksip_call:trans(), nksip_call:call()) ->
    ok.

send_ack(#trans{request=Req, id=Id}, _Call) ->
    Ack = nksip_uac_lib:make_ack(Req),
    case nksip_transport_uac:resend_request(Ack, []) of
        {ok, _} -> 
            ok;
        error -> 
            ?call_notice("UAC ~p could not send non-2xx ACK", [Id])
    end.


%% @private
-spec send_2xx_ack(nksip_dialog:id(), nksip_call:call()) ->
    nksip_call:call().

send_2xx_ack(DialogId, Call) ->
    case nksip_call:sync_send_dialog(DialogId, 'ACK', [async], Call) of
        {ok, Call1} ->
            Call1;
        {error, Error} ->
            ?call_warning("Could not generate 2xx ACK: ~p", [Error]),
            Call
    end.

%% @private
-spec is_prack_retrans(nksip:response(), nksip_call:trans()) ->
    boolean().

is_prack_retrans(Resp, UAC) ->
    #sipmsg{dialog_id=DialogId, cseq={CSeq, Method}} = Resp,
    #trans{pracks=PRAcks} = UAC,
    case nksip_sipmsg:header(Resp, <<"rseq">>, integers) of
        [RSeq] when is_integer(RSeq) ->
            lists:member({RSeq, CSeq, Method, DialogId}, PRAcks);
        _ ->
            false
    end.


%% @private
-spec check_prack(nksip:response(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

check_prack(Resp, UAC, Call) ->
    #sipmsg{
        class = {resp, Code, _Reason}, 
        dialog_id = DialogId,
        require = Require
    } = Resp,
    #trans{
        id = Id, 
        from = From,
        method = Method
    } = UAC,
    case From of
        {fork, _} ->
            Call;
        _ ->
            case Method of
                'INVITE' when Code>100, Code<200 ->
                    case lists:member(<<"100rel">>, Require) of
                        true -> send_prack(Resp, Id, DialogId, Call);
                        false -> Call
                    end;
                _ ->
                    Call
            end
    end.


%% @private
-spec send_prack(nksip:response(), nksip_call_uac:id(), 
                 nksip_dialog:id(), nksip_call:call()) ->
    nksip_call:call().

send_prack(Resp, Id, DialogId, Call) ->
    #sipmsg{cseq={CSeq, _}} = Resp,
    #call{trans=Trans} = Call,
    try
        case nksip_sipmsg:header(Resp, <<"rseq">>, integers) of
            [RSeq] when RSeq > 0 -> ok;
            _ -> RSeq = throw(invalid_rseq)
        end,
        case lists:keyfind(Id, #trans.id, Trans) of
            #trans{} = UAC -> ok;
            _ -> UAC = throw(no_transaction)
        end,
        #trans{
            method = Method, 
            rseq = LastRSeq, 
            pracks = PRAcks,
            opts = UACOpts
        } = UAC,
        case LastRSeq of
            0 -> ok;
            _ when RSeq==LastRSeq+1 -> ok;
            _ -> throw(rseq_out_of_order)
        end,
        case nksip_call_dialog:find(DialogId, Call) of
            #dialog{invite=#invite{sdp_offer={remote, invite, RemoteSDP}}} -> ok;
            _ -> RemoteSDP = <<>>
        end,
        Body = case nksip_lib:get_value(prack_callback, UACOpts) of
            Fun when is_function(Fun, 2) -> 
                case catch Fun(RemoteSDP, Resp) of
                    Bin when is_binary(Bin) ->
                        Bin;
                    #sdp{} = LocalSDP -> 
                        LocalSDP;
                    Other ->
                        ?call_warning("error calling prack_sdp/2: ~p", [Other]),
                        <<>>
                end;
            _ ->
                <<>>
        end,
        RAck = list_to_binary([ 
            integer_to_list(RSeq),
            32,
            integer_to_list(CSeq),
            32,
            nksip_lib:to_list(Method)
        ]),
        Opts2 = [{add, "rack", RAck}, {body, Body}],
        case nksip_call:make_dialog(DialogId, 'PRACK', Opts2, Call) of
            {ok, Req, ReqOpts, Call1} -> 
                PRAcks1 = [{RSeq, CSeq, Method, DialogId}|PRAcks],
                UAC1 = UAC#trans{rseq=RSeq, pracks=PRAcks1},
                Call2 = update(UAC1, Call1),
                nksip_call_uac_req:request(Req, ReqOpts, none, Call2);
            {error, Error} ->
                throw(Error)
        end
    catch
        throw:TError ->
            ?call_warning("could not send PRACK: ~p", [TError]),
            Call
    end.



