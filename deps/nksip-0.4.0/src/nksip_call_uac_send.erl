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

%% @private Call UAC Management: Request sending
-module(nksip_call_uac_send).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([send/2]).
-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% @private
-spec send(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

send(#trans{method='ACK'}=UAC, Call) ->
    #trans{id=TransId, request=Req, opts=Opts} = UAC,
    case nksip_call_uac_transp:send_request(Req, Opts) of
        {ok, SentReq} ->
            sent_request(SentReq, UAC, Call);
        {error, Error} ->
            ?call_debug("UAC ~p error sending 'ACK' request: ~p", [TransId, Error]),
            UAC1 = UAC#trans{status=finished},
            Call1 = update(UAC1, Call),
            nksip_call_uac_reply:reply({error, Error}, UAC1, Call1)
    end;

send(UAC, Call) ->
    #trans{method=Method, id=TransId, request=Req, opts=Opts} = UAC,
    #sipmsg{to={_, ToTag}} = Req,
    NoDialog = lists:member(no_dialog, Opts),
    % For proxies sending SUBSCRIBE or NOTIFY, NoDialog will be true
    PreDialog = case NoDialog of
        true -> 
            ok;
        false when ToTag == <<>> ->
            ok;
        false -> 
            case nksip_call_uac_dialog:pre_request(Req, Call) of
                ok ->
                    ok;
                {error, DlgError} ->
                    {error, DlgError}
            end
    end,
    case PreDialog of
        ok ->
            Send = case Method of 
                'CANCEL' -> 
                    nksip_call_uac_transp:resend_request(Req, Opts);
                _ -> 
                    nksip_call_uac_transp:send_request(Req, Opts)
            end,
            case Send of
                {ok, SentReq} -> 
                    sent_request(SentReq, UAC, Call);
                {error, Error} ->
                    ?call_debug("UAC ~p error sending ~p request: ~p", 
                                [TransId, Method, Error]),
                    UAC1 = UAC#trans{status=finished},
                    Call1 = update(UAC1, Call),
                    nksip_call_uac_reply:reply({error, Error}, UAC1, Call1)
            end;
        {error, Error} ->
            ?call_info("UAC ~p dialog error: ~p", [TransId, Error]),
            UAC1 = UAC#trans{status=finished},
            Call1 = update(UAC1, Call),
            nksip_call_uac_reply:reply({error, Error}, UAC1, Call1)
    end.


%% @private
-spec sent_request(nksip:request(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

sent_request(#sipmsg{class={req, 'ACK'}}=Req, UAC, Call) ->
    #trans{id=TransId, opts=Opts} = UAC,
    ?call_debug("UAC ~p sent 'ACK' request", [TransId]),
    UAC1 = UAC#trans{
        status = finished, 
        request = Req,
        trans_id = nksip_call_lib:uac_transaction_id(Req)
    },
    Call1 = update(UAC1, Call),
    Call2 = nksip_call_uac_reply:reply({req, Req}, UAC1, Call1),
    case lists:member(no_dialog, Opts) of
        true -> Call1;
        false -> nksip_call_uac_dialog:request(Req, undefined, Call2)
    end;

sent_request(Req, UAC, Call) ->
    #sipmsg{
        class = {req, Method}, 
        to = {_, ToTag}, 
        transport = #transport{proto=Proto}
    } = Req,
    #trans{
        id = TransId, 
        opts = Opts, 
        from = From
    } = UAC,
    ?call_debug("UAC ~p sent ~p request", [TransId, Method]),
    UAC1 = UAC#trans{
        request = Req, 
        proto = Proto,
        trans_id = nksip_call_lib:uac_transaction_id(Req)
    },
    Call1 = update(UAC1, Call),
    Call2 = case lists:member(no_dialog, Opts) of
        true -> 
            Call1;
        false when ToTag == <<>> -> 
            Call1;
        false -> 
            IsProxy = case From of {fork, _} -> true; _ -> false end,
            nksip_call_uac_dialog:request(Req, IsProxy, Call1)
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
        udp -> 
            nksip_call_lib:retrans_timer(timer_a, UAC3, Call);
        _ -> 
            UAC3
    end,
    update(UAC4, Call);

sent_update(#sipmsg{class={req, Method}}=Req, #trans{proto=Proto}=UAC, Call) ->
    UAC1 = UAC#trans{status=trying},
    UAC2 = nksip_call_lib:timeout_timer(timer_f, UAC1, Call),
    UAC3 = case Proto of 
        udp -> 
            nksip_call_lib:retrans_timer(timer_e, UAC2, Call);
        _ -> 
            UAC2
    end,
    #sipmsg{to={_, ToTag}} = Req,
    Call1 = case 
        (Method=='SUBSCRIBE' orelse Method=='REFER') andalso ToTag == <<>> 
    of
        true -> nksip_call_event:create_prov_event(Req, Call);
        false -> Call
    end,
    update(UAC3, Call1).

    

