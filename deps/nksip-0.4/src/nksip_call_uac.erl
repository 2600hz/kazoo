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

%% @doc Call UAC Management
-module(nksip_call_uac).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/4, dialog/4, resend/3, cancel/3, cancel/4]).
-export([is_stateless/1, response/2]).
-export_type([status/0, uac_from/0]).
-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type status() :: 
    invite_calling | invite_proceeding | invite_accepted | invite_completed | 
    trying | proceeding | completed |
    finished | ack.

-type uac_from() :: 
    none | {srv, from()} | {fork, nksip_call_fork:id()}.


%% ===================================================================
%% Private
%% ===================================================================


%% @doc Starts a new UAC transaction.
-spec request(nksip:request(), nksip:optslist(), uac_from(), nksip_call:call()) ->
    nksip_call:call().

request(Req, Opts, From, Call) ->
    #sipmsg{class={req, Method}, id=MsgId} = Req,
    #call{app_id=AppId} = Call,
    {continue, [Req1, Opts1, From1, Call1]} = 
        AppId:nkcb_uac_pre_request(Req, Opts, From, Call),
    {#trans{id=TransId}=UAC, Call2} = make_trans(Req1, Opts1, From1, Call1),
    case lists:member(async, Opts1) andalso From1 of
        {srv, SrvFrom} when Method=='ACK' -> 
            gen_server:reply(SrvFrom, async);
        {srv, SrvFrom} ->
            Handle = nksip_sipmsg:get_handle(Req),
            gen_server:reply(SrvFrom, {async, Handle});
        _ ->
            ok
    end,
    case From1 of
        {fork, ForkId} ->
            ?call_debug("UAC ~p sending request ~p ~p (~s, fork: ~p)", 
                        [TransId, Method, Opts1, MsgId, ForkId]);
        _ ->
            ?call_debug("UAC ~p sending request ~p ~p (~s)", 
                        [TransId, Method, Opts1, MsgId])
    end,
    nksip_call_uac_send:send(UAC, Call2).


%% @doc Sends a new in-dialog request from inside the call process
-spec dialog(nksip_dialog_lib:id(), nksip:method(), nksip:optslist(), nksip_call:call()) ->
    {ok, nksip_call:call()} | {error, term()}.

dialog(DialogId, Method, Opts, Call) ->
    case make_dialog(DialogId, Method, Opts, Call) of
        {ok, Req, ReqOpts, Call1} ->
            {ok, request(Req, ReqOpts, none, Call1)};
        {error, Error} ->
            {error, Error}
    end.


%% @private Generates a new in-dialog request from inside the call process
-spec make_dialog(nksip_dialog_lib:id(), nksip:method(), nksip:optslist(), nksip_call:call()) ->
    {ok, nksip:request(), nksip:optslist(), nksip_call:call()} | {error, term()}.

make_dialog(DialogId, Method, Opts, Call) ->
    #call{app_id=AppId, call_id=CallId} = Call,
    case nksip_call_uac_dialog:make(DialogId, Method, Opts, Call) of
        {ok, RUri, Opts1, Call1} -> 
            Opts2 = [{call_id, CallId} | Opts1],
            case nksip_call_uac_make:make(AppId, Method, RUri, Opts2) of
                {ok, Req, ReqOpts} ->
                    {ok, Req, ReqOpts, Call1};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
%% Resend a requests using same Call-Id, incremented CSeq
-spec resend(nksip:request(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

resend(Req, UAC, Call) ->
     #trans{
        id = TransId,
        status = Status,
        opts = Opts,
        method = Method,
        iter = Iter,
        from = From
    } = UAC,
    #sipmsg{vias=[_|Vias], cseq={_, CSeqMethod}} = Req,
    ?call_info("UAC ~p ~p (~p) resending updated request", [TransId, Method, Status]),
    {CSeq, Call1} = nksip_call_uac_dialog:new_local_seq(Req, Call),
    Req1 = Req#sipmsg{vias=Vias, cseq={CSeq, CSeqMethod}},
    % Contact would be already generated
    Opts1 = Opts -- [contact],
    {NewUAC, Call2} = make_trans(Req1, Opts1, From, Call1),
    NewUAC1 = NewUAC#trans{iter=Iter+1},
    nksip_call_uac_send:send(NewUAC1, update(NewUAC1, Call2)).


%% @doc Tries to cancel an ongoing invite request with a reason
-spec cancel(nksip_call:trans_id(), nksip:optslist(), 
             {srv, from()} | undefined, nksip_call:call()) ->
    nksip_call:call().

cancel(TransId, Opts, From, #call{trans=Trans}=Call) when is_integer(TransId) ->
    case lists:keyfind(TransId, #trans.id, Trans) of
        #trans{class=uac, method='INVITE'} = UAC ->
            case From of
                {srv, SrvFrom} -> gen_server:reply(SrvFrom, ok);
                _ -> ok
            end,
            cancel(UAC, Opts, Call);
        _ -> 
            case From of
                {srv, SrvFrom} -> gen_server:reply(SrvFrom, {error, unknown_request});
                _ -> ok
            end,
            ?call_debug("UAC ~p not found to CANCEL", [TransId]),
            Call
    end.



%% @doc Tries to cancel an ongoing invite request with a reason
-spec cancel(nksip_call:trans(), nksip:optslist(), nksip_call:call()) ->
    nksip_call:call().

cancel(#trans{id=TransId, class=uac, cancel=Cancel, status=Status}=UAC, Opts, Call)
       when Cancel==undefined; Cancel==to_cancel ->
    case Status of
        invite_calling ->
            ?call_debug("UAC ~p (invite_calling) delaying CANCEL", [TransId]),
            UAC1 = UAC#trans{cancel=to_cancel},
            update(UAC1, Call);
        invite_proceeding ->
            ?call_debug("UAC ~p (invite_proceeding) generating CANCEL", [TransId]),
            CancelReq = nksip_call_uac_make:make_cancel(UAC#trans.request, Opts),
            UAC1 = UAC#trans{cancel=cancelled},
            request(CancelReq, [no_dialog], none, update(UAC1, Call))
    end;

cancel(#trans{id=TransId, cancel=Cancel, status=Status}, _Opts, Call) ->
    ?call_debug("UAC ~p (~p) cannot CANCEL request: (~p)", [TransId, Status, Cancel]),
    Call.


%% @doc Checks if a response is a stateless response
-spec is_stateless(nksip:response()) ->
    boolean().

is_stateless(Resp) ->
    #sipmsg{vias=[#via{opts=Opts}|_]} = Resp,
    case nksip_lib:get_binary(<<"branch">>, Opts) of
        <<"z9hG4bK", Branch/binary>> ->
            case binary:split(Branch, <<"-">>) of
                [BaseBranch, NkSIP] ->
                    GlobalId = nksip_config_cache:global_id(),
                    case nksip_lib:hash({BaseBranch, GlobalId, stateless}) of
                        NkSIP -> true;
                        _ -> false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.


%% @private
-spec make_trans(nksip:request(), nksip:optslist(), uac_from(), nksip_call:call()) ->
    {nksip_call:trans(), nksip_call:call()}.

make_trans(Req, Opts, From, Call) ->
    #sipmsg{class={req, Method}, id=MsgId, ruri=RUri} = Req, 
    #call{next=TransId, trans=Trans, msgs=Msgs} = Call,
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
    DialogId = nksip_call_uac_dialog:uac_dialog_id(Req, IsProxy, Call),
    UAC = #trans{
        id = TransId,
        class = uac,
        status = Status,
        start = nksip_lib:timestamp(),
        from = From,
        opts = Opts1,
        trans_id = undefined,
        request = Req#sipmsg{dialog_id=DialogId},
        method = Method,
        ruri = RUri,
        proto = undefined,
        response = undefined,
        code = 0,
        to_tags = [],
        stateless = undefined,
        iter = 1,
        cancel = undefined,
        loop_id = undefined,
        timeout_timer = undefined, 
        retrans_timer = undefined,
        next_retrans = undefined,
        expire_timer = undefined,
        ack_trans_id  = undefined,
        meta = []
    },
    Msg = {MsgId, TransId, DialogId},
    {UAC, Call#call{trans=[UAC|Trans], msgs=[Msg|Msgs], next=TransId+1}}.


%% @doc Called when a new response is received.
-spec response(nksip:response(), nksip_call:call()) ->
    nksip_call:call().

response(Resp, #call{app_id=AppId, trans=Trans}=Call) ->
    #sipmsg{class={resp, Code, _Reason}, cseq={_, Method}} = Resp,
    TransId = nksip_call_lib:uac_transaction_id(Resp),
    case lists:keyfind(TransId, #trans.trans_id, Trans) of
        #trans{class=uac, from=From, ruri=RUri}=UAC -> 
            IsProxy = case From of 
                {fork, _} -> true; 
                _ -> false 
            end,
            DialogId = nksip_call_uac_dialog:uac_dialog_id(Resp, IsProxy, Call),
            Resp1 = Resp#sipmsg{ruri=RUri, dialog_id=DialogId},
            case AppId:nkcb_uac_pre_response(Resp1, UAC, Call) of
                {continue, [Resp2, UAC2, Call2]} ->
                    nksip_call_uac_resp:response(Resp2, UAC2, Call2);
                {ok, Call2} ->
                    Call2
            end;
        _ -> 
            ?call_info("UAC received ~p ~p response for unknown transaction", [Method, Code]),
            Call
    end.


