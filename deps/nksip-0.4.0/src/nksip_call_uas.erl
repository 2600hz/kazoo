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

%% @doc Call UAS Management
-module(nksip_call_uas).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/2, reply/3, do_reply/3]).
-export_type([status/0, incoming/0]).

-import(nksip_call_lib, [update/2]).
-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-type status() ::  
    invite_proceeding | invite_accepted | invite_completed | invite_confirmed | 
    trying | proceeding | completed | 
    ack | finished.

-type incoming() :: 
    nksip:sipreply() | {nksip:response(), nksip:optslist()}.


%% ===================================================================
%% Private
%% ===================================================================

%% @doc Called when a new request is received.
-spec request(nksip:request(), nksip_call:call()) ->
    nksip_call:call().

request(Req, Call) -> 
    case is_trans_ack(Req, Call) of
        {true, InvUAS} -> 
            process_trans_ack(InvUAS, Call);
        false ->
            case is_retrans(Req, Call) of
                {true, UAS} ->
                    process_retrans(UAS, Call);
                {false, ReqTransId} ->
                    case is_own_ack(Req) of
                        true ->
                            Call;
                        false ->
                            process_request(Req, ReqTransId, Call)
                    end
            end
    end.


%% @private Checks if `Req' is an ACK matching an existing transaction
%% (for a non 2xx responses, ACK is in the same transaction as the INVITE)
-spec is_trans_ack(nksip:request(), nksip_call:call()) ->
    {true, nksip_call:trans()} | false.

is_trans_ack(#sipmsg{class={req, 'ACK'}}=Req, #call{trans=Trans}) ->
    TransReq = Req#sipmsg{class={req, 'INVITE'}},
    ReqTransId = nksip_call_lib:uas_transaction_id(TransReq),
    case lists:keyfind(ReqTransId, #trans.trans_id, Trans) of
        #trans{class=uas}=InvUAS -> 
            {true, InvUAS};
        false when ReqTransId < 0 ->
            % Pre-RFC3261 style
            case lists:keyfind(ReqTransId, #trans.ack_trans_id, Trans) of
                #trans{}=InvUAS -> 
                    {true, InvUAS};
                false -> 
                    false
            end;
        false ->
            false
    end;

is_trans_ack(_, _) ->
    false.


%% @private
-spec process_trans_ack(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

process_trans_ack(InvUAS, Call) ->
    #trans{id=Id, status=Status, proto=Proto} = InvUAS,
    case Status of
        invite_completed ->
            InvUAS1 = InvUAS#trans{response=undefined},
            InvUAS2 = nksip_call_lib:retrans_timer(cancel, InvUAS1, Call),
            InvUAS4 = case Proto of 
                udp -> 
                    InvUAS3 = InvUAS2#trans{status=invite_confirmed},
                    nksip_call_lib:timeout_timer(timer_i, InvUAS3, Call);
                _ ->
                    InvUAS3 = InvUAS2#trans{status=finished},
                    nksip_call_lib:timeout_timer(cancel, InvUAS3, Call)
            end,
            ?call_debug("InvUAS ~p received in-transaction ACK", [Id]),
            update(InvUAS4, Call);
        invite_confirmed ->
            ?call_debug("InvUAS ~p received non 2xx ACK in ~p", [Id, Status]),
            Call;
        _ ->
            ?call_notice("InvUAS ~p received non 2xx ACK in ~p", [Id, Status]),
            Call
    end.


%% @private Checks if request is a retransmission
-spec is_retrans(nksip:request(), nksip_call:call()) ->
    {true, nksip_call:trans()} | {false, integer()}.

is_retrans(Req, #call{trans=Trans}) ->
    ReqTransId = nksip_call_lib:uas_transaction_id(Req),
    case lists:keyfind(ReqTransId, #trans.trans_id, Trans) of
        #trans{class=uas}=UAS -> 
            {true, UAS};
        _ -> 
            {false, ReqTransId}
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
            case nksip_call_uas_transp:resend_response(Resp, []) of
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


%% @doc Returns true if the request is an ACK to a [3456]xx response generated
%% automatically by NkSIP
-spec is_own_ack(nksip:request()) ->
    boolean().

is_own_ack(#sipmsg{class={req, 'ACK'}}=Req) ->
    #sipmsg{
        to = {_, ToTag},
        vias = [#via{opts=ViaOpts}|_]
    } = Req,
    Branch = nksip_lib:get_binary(<<"branch">>, ViaOpts),
    GlobalId = nksip_config_cache:global_id(),
    nksip_lib:hash({GlobalId, Branch}) == ToTag;

is_own_ack(_) ->
    false.


%% @private
-spec process_request(nksip:request(), nksip_call:trans_id(), nksip_call:call()) ->
    nksip_call:call().

process_request(Req, UASTransId, Call) ->
    Req1 = preprocess(Req),
    #sipmsg{
        class = {req, Method}, 
        id = MsgId, 
        ruri = RUri, 
        transport = Transp, 
        to = {_, ToTag}
    } = Req1,
    #call{
        trans = Trans, 
        next = NextId, 
        msgs = Msgs
    } = Call,
    ?call_debug("UAS ~p started for ~p (~s)", [NextId, Method, MsgId]),
    LoopId = loop_id(Req1),
    DialogId = nksip_dialog_lib:make_id(uas, Req1),
    Status = case Method of
        'INVITE' -> invite_proceeding;
        'ACK' -> ack;
        _ -> trying
    end,
    UAS = #trans{
        id = NextId,
        class = uas,
        status = Status,
        start = nksip_lib:timestamp(),
        from = undefined,
        opts = [],
        trans_id = UASTransId, 
        request = Req1#sipmsg{dialog_id=DialogId},
        method = Method,
        ruri = RUri,
        proto = Transp#transport.proto,
        response = undefined,
        code = 0,
        to_tags = [],
        stateless = true,
        iter = 1,
        cancel = undefined,
        loop_id = LoopId,
        timeout_timer = undefined,
        retrans_timer = undefined,
        next_retrans = undefined,
        expire_timer = undefined,
        ack_trans_id = undefined,
        meta = []
    },
     UAS1 = case Method of
        'INVITE' -> 
            nksip_call_lib:timeout_timer(timer_c, UAS, Call);
        'ACK' -> 
            UAS;
        _ -> 
            nksip_call_lib:timeout_timer(noinvite, UAS, Call)
    end,
    Msg = {MsgId, NextId, DialogId},
    Call1 = Call#call{trans=[UAS1|Trans], next=NextId+1, msgs=[Msg|Msgs]},
    case ToTag==(<<>>) andalso lists:keymember(LoopId, #trans.loop_id, Trans) of
        true -> 
            do_reply(loop_detected, UAS1, Call1);
        false -> 
            nksip_call_uas_route:launch(UAS1, Call1)
    end.


%% @doc Sends a transaction reply
-spec reply(incoming(), nksip_call:trans(), nksip_call:call()) ->
    {ok | {error, term()}, nksip_call:call()}.

reply(Reply, Trans, Call) ->
    nksip_call_uas_reply:reply(Reply, Trans, Call).


%% @doc Sends a transaction reply
-spec do_reply(incoming(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

do_reply(Reply, Trans, Call) ->
    {_, Call1} = nksip_call_uas_reply:reply(Reply, Trans, Call),
    Call1.



%% ===================================================================
%% Utils
%% ===================================================================

%% @private
-spec loop_id(nksip:request()) ->
    integer().
    
loop_id(#sipmsg{from={_, FromTag}, cseq={CSeq, Method}}) ->
    erlang:phash2({FromTag, CSeq, Method}).


%% @doc Preprocess an incoming request.
%%  - Adds rport, received and transport options to Via.
%%  - Generates a To Tag candidate.
%%  - Performs strict routing processing.
%%  - Updates the request if maddr is present.
%%  - Removes first route if it is poiting to us.
-spec preprocess(nksip:request()) ->
    nksip:request().

preprocess(Req) ->
    #sipmsg{
        to = {_, ToTag},
        transport = #transport{proto=Proto, remote_ip=Ip, remote_port=Port}, 
        vias = [Via|ViaR]
    } = Req,
    Received = nksip_lib:to_host(Ip, false), 
    ViaOpts1 = [{<<"received">>, Received}|Via#via.opts],
    % For UDP, we honor the rport option
    % For connection transports, we force inclusion of remote port 
    % to reuse the same connection
    ViaOpts2 = case lists:member(<<"rport">>, ViaOpts1) of
        false when Proto==udp -> 
            ViaOpts1;
        _ -> 
            [{<<"rport">>, nksip_lib:to_binary(Port)} | ViaOpts1 -- [<<"rport">>]]
    end,
    Via1 = Via#via{opts=ViaOpts2},
    Branch = nksip_lib:get_binary(<<"branch">>, ViaOpts2),
    GlobalId = nksip_config_cache:global_id(),
    ToTag1 = case ToTag of
        <<>> -> 
            nksip_lib:hash({GlobalId, Branch});
        _ -> 
            ToTag
    end,
    Req1 = Req#sipmsg{
        vias = [Via1|ViaR], 
        to_tag_candidate = ToTag1
    },
    Req2 = strict_router(Req1),
    ruri_has_maddr(Req2).


%% @private If the Request-URI has a value we have placed on a Record-Route header, 
%% change it to the last Route header and remove it. This gets back the original 
%% RUri "stored" at the end of the Route header when proxing through a strict router
%% This could happen if
%% - in a previous request, we added a Record-Route header with our ip
%% - the response generated a dialog
%% - a new in-dialog request has arrived from a strict router, that copied our Record-Route
%%   in the ruri
%%
%% TODO: Is this working?
strict_router(#sipmsg{app_id=AppId, ruri=RUri, routes=Routes}=Request) ->
    case 
        nksip_lib:get_value(<<"nksip">>, RUri#uri.opts) /= undefined 
        andalso nksip_transport:is_local(AppId, RUri) of
    true ->
        case lists:reverse(Routes) of
            [] ->
                Request;
            [RUri1|RestRoutes] ->
                ?call_notice("recovering RURI from strict router request", []),
                Request#sipmsg{ruri=RUri1, routes=lists:reverse(RestRoutes)}
        end;
    false ->
        Request
    end.    


%% @private If RUri has a maddr address that corresponds to a local ip and has the 
% same transport class and local port than the transport, change the Ruri to
% this address, default port and no transport parameter
ruri_has_maddr(Request) ->
    #sipmsg{
        app_id = AppId, 
        ruri = RUri, 
        transport=#transport{proto=Proto, local_port=LPort}
    } = Request,
    case nksip_lib:get_binary(<<"maddr">>, RUri#uri.opts) of
        <<>> ->
            Request;
        MAddr -> 
            case nksip_transport:is_local(AppId, RUri#uri{domain=MAddr}) of
                true ->
                    case nksip_parse:transport(RUri) of
                        {Proto, _, LPort} ->
                            RUri1 = RUri#uri{
                                port = 0,
                                opts = nksip_lib:delete(RUri#uri.opts, 
                                                        [<<"maddr">>, <<"transport">>])
                            },
                            Request#sipmsg{ruri=RUri1};
                        _ ->
                            Request
                    end;
                false ->
                    Request
            end
    end.


