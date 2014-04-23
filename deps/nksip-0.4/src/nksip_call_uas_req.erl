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

%% @doc Call UAS Management: Request processing
-module(nksip_call_uas_req).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/2]).

-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").



%% @doc Called when a new request is received.
-spec request(nksip:request(), nksip_call:call()) ->
    nksip_call:call().

request(Req, Call) -> 
    case is_trans_ack(Req, Call) of
        {true, UAS} -> 
            process_trans_ack(UAS, Call);
        false ->
            case is_retrans(Req, Call) of
                {true, UAS} ->
                    process_retrans(UAS, Call);
                {false, ReqTransId} ->
                    case nksip_uas_lib:preprocess(Req) of
                        own_ack -> Call;
                        Req1 -> process_request(Req1, ReqTransId, Call)
                    end
            end
    end.


%% @private Checks if `Req' is an ACK matching an existing transaction
%% (for a non 2xx response)
-spec is_trans_ack(nksip:request(), nksip_call:call()) ->
    {true, nksip_call:trans()} | false.

is_trans_ack(#sipmsg{class={req, 'ACK'}}=Req, #call{trans=Trans}) ->
    TransReq = Req#sipmsg{class={req, 'INVITE'}},
    ReqTransId = nksip_call_uas:transaction_id(TransReq),
    case lists:keyfind(ReqTransId, #trans.trans_id, Trans) of
        #trans{class=uas}=UAS -> 
            {true, UAS};
        false when ReqTransId < 0 ->
            % Pre-RFC3261 style
            case lists:keyfind(ReqTransId, #trans.ack_trans_id, Trans) of
                #trans{}=UAS -> {true, UAS};
                false -> false
            end;
        false ->
            false
    end;

is_trans_ack(_, _) ->
    false.


%% @private
-spec process_trans_ack(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

process_trans_ack(UAS, Call) ->
    #trans{id=Id, status=Status, proto=Proto} = UAS,
    case Status of
        invite_completed ->
            UAS1 = UAS#trans{response=undefined},
            UAS2 = nksip_call_lib:retrans_timer(cancel, UAS1, Call),
            UAS4 = case Proto of 
                udp -> 
                    UAS3 = UAS2#trans{status=invite_confirmed},
                    nksip_call_lib:timeout_timer(timer_i, UAS3, Call);
                _ ->
                    UAS3 = UAS2#trans{status=finished},
                    nksip_call_lib:timeout_timer(cancel, UAS3, Call)
            end,
            ?call_debug("UAS ~p received in-transaction ACK", [Id]),
            update(UAS4, Call);
        invite_confirmed ->
            ?call_debug("UAS ~p received non 2xx ACK in ~p", [Id, Status]),
            Call;
        _ ->
            ?call_notice("UAS ~p received non 2xx ACK in ~p", [Id, Status]),
            Call
    end.


%% @private Checks if request is a retransmission
-spec is_retrans(nksip:request(), nksip_call:call()) ->
    {true, nksip_call:trans()} | {false, integer()}.

is_retrans(Req, #call{trans=Trans}) ->
    ReqTransId = nksip_call_uas:transaction_id(Req),
    case lists:keyfind(ReqTransId, #trans.trans_id, Trans) of
        #trans{class=uas}=UAS -> {true, UAS};
        _ -> {false, ReqTransId}
    end.



%% @private
-spec process_retrans(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

process_retrans(UAS, Call) ->
    #trans{id=Id, status=Status, method=Method, response=Resp} = UAS,
    case 
        Status==invite_proceeding orelse Status==invite_completed
        orelse Status==proceeding orelse Status==completed
    of
        true when is_record(Resp, sipmsg) ->
            #sipmsg{class={resp, Code, _Reason}} = Resp,
            case nksip_transport_uas:resend_response(Resp, []) of
                {ok, _} ->
                    ?call_info("UAS ~p ~p (~p) sending ~p retransmission", 
                               [Id, Method, Status, Code]);
                error ->
                    ?call_notice("UAS ~p ~p (~p) could not send ~p retransmission", 
                                  [Id, Method, Status, Code])
            end;
        _ ->
            ?call_info("UAS ~p ~p received retransmission in ~p", 
                       [Id, Method, Status])
    end,
    Call.


%% @private
-spec process_request(nksip:request(), nksip_call_uas:id(), nksip_call:call()) ->
    nksip_call:call().

process_request(Req, TransId, Call) ->
    #sipmsg{
        class = {req, Method}, 
        id = MsgId, 
        ruri = RUri, 
        transport = Transp, 
        to = {_, ToTag}
    } = Req,
    #call{trans=Trans, next=Id, msgs=Msgs} = Call,
    ?call_debug("UAS ~p started for ~p (~s)", [Id, Method, MsgId]),
    LoopId = loop_id(Req),
    DialogId = nksip_dialog:make_id(uas, Req),
    UAS = #trans{
        id = Id,
        class = uas,
        status = authorize,
        opts = [],
        start = nksip_lib:timestamp(),
        from = undefined,
        trans_id = TransId, 
        request = Req#sipmsg{dialog_id=DialogId},
        method = Method,
        ruri = RUri,
        proto = Transp#transport.proto,
        stateless = true,
        response = undefined,
        code = 0,
        loop_id = LoopId
    },
     UAS1 = case Method of
        'INVITE' -> nksip_call_lib:timeout_timer(timer_c, UAS, Call);
        'ACK' -> UAS;
        _ -> nksip_call_lib:timeout_timer(noinvite, UAS, Call)
    end,
    Msg = {MsgId, Id, DialogId},
    Call1 = Call#call{trans=[UAS1|Trans], next=Id+1, msgs=[Msg|Msgs]},
    case ToTag==(<<>>) andalso lists:keymember(LoopId, #trans.loop_id, Trans) of
        true -> 
            {_, Call2} = nksip_call_uas_reply:reply(loop_detected, UAS1, Call1),
            Call2;
        false -> 
            nksip_call_uas_route:launch(UAS1, Call1)
    end.


%% @private
-spec loop_id(nksip:request()) ->
    integer().
    
loop_id(#sipmsg{from={_, FromTag}, cseq={CSeq, Method}}) ->
    erlang:phash2({FromTag, CSeq, Method}).

