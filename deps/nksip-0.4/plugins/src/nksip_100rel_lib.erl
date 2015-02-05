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

%% @doc NkSIP Reliable Provisional Responses Plugin
-module(nksip_100rel_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").

-export([is_prack_retrans/2, send_prack/4]).
-export([uas_store_info/2, uas_method/3]).
-export([retrans_timer/2, timeout_timer/2]).

%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec is_prack_retrans(nksip:response(), nksip_call:trans()) ->
    boolean().

is_prack_retrans(Resp, UAC) ->
    #sipmsg{dialog_id=DialogId, cseq={CSeq, Method}} = Resp,
    #trans{meta=Meta} = UAC,
    case nksip_sipmsg:header(<<"rseq">>, Resp, integers) of
        [RSeq] when is_integer(RSeq) ->
            PRAcks = nksip_lib:get_value(nksip_100rel_pracks, Meta, []),
            lists:member({RSeq, CSeq, Method, DialogId}, PRAcks);
        _ ->
            false
    end.


%% @private
-spec send_prack(nksip:response(), nksip_call:trans_id(), 
                 nksip_dialog_lib:id(), nksip_call:call()) ->
    continue | {ok, nksip:call()}.

send_prack(Resp, Id, DialogId, Call) ->
    #sipmsg{class={resp, Code, _Phrase}, cseq={CSeq, _}} = Resp,
    #call{trans=Trans} = Call,
    try
        case nksip_sipmsg:header(<<"rseq">>, Resp, integers) of
            [RSeq] when RSeq > 0 -> ok;
            _ -> RSeq = throw(invalid_rseq)
        end,
        case lists:keyfind(Id, #trans.id, Trans) of
            #trans{} = UAC -> ok;
            _ -> UAC = throw(no_transaction)
        end,
        #trans{method=Method, meta=Meta, opts=UACOpts} = UAC,
        PRAcks = nksip_lib:get_value(nksip_100rel_pracks, Meta, []),
        LastRSeq = nksip_lib:get_value(nksip_100rel_rseq, Meta, 0),
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
                case catch Fun(RemoteSDP, {resp, Code, Resp, Call}) of
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
        % case nksip_call:make_dialog(DialogId, 'PRACK', Opts2, Call) of
        %     {ok, Req, ReqOpts, Call1} -> 
        %         PRAcks1 = [{RSeq, CSeq, Method, DialogId}|PRAcks],
        %         Meta1 = nksip_lib:store_value(nksip_100rel_pracks, PRAcks1, Meta),
        %         Meta2 = nksip_lib:store_value(nksip_100rel_rseq, RSeq, Meta1),
        %         UAC1 = UAC#trans{meta=Meta2},
        %         Call2 = nksip_call_lib:update(UAC1, Call1),
        %         {ok, nksip_call_uac:request(Req, ReqOpts, none, Call2)};
        %     {error, Error} ->
        %         throw(Error)
        % end
        case nksip_call_uac:dialog(DialogId, 'PRACK', Opts2, Call) of
            {ok, #call{trans=[UAC1|_]}=Call1} -> 
                PRAcks1 = [{RSeq, CSeq, Method, DialogId}|PRAcks],
                Meta1 = nksip_lib:store_value(nksip_100rel_pracks, PRAcks1, Meta),
                Meta2 = nksip_lib:store_value(nksip_100rel_rseq, RSeq, Meta1),
                UAC2 = UAC1#trans{meta=Meta2},
                {ok, nksip_call_lib:update(UAC2, Call1)};
            {error, Error} ->
                throw(Error)
        end
    catch
        throw:TError ->
            ?call_warning("could not send PRACK: ~p", [TError]),
            continue
    end.



%% @private
-spec uas_store_info(nksip:response(), nksip_call:trans()) ->
    {ok, nksip:response(), nksip_call:trans()} | {error, Error}
    when Error :: stateless_not_allowed | pending_prack.

uas_store_info(_, #trans{stateless=true}) ->
    {error, stateless_not_allowed};

uas_store_info(Resp, UAS) ->
    #sipmsg{dialog_id=DialogId, cseq={CSeq, Method}} = Resp,
    #trans{meta=Meta} = UAS,
    case nksip_lib:get_value(nksip_100rel_pracks, Meta, []) of
        [] ->
            RSeq = case nksip_lib:get_value(nksip_100rel_rseq, Meta, 0) of
                0 -> crypto:rand_uniform(1, 2147483647);
                LastRSeq -> LastRSeq+1
            end,
            Headers1 = nksip_headers:update(Resp, [{single, <<"rseq">>, RSeq}]),
            Resp1 = Resp#sipmsg{headers=Headers1},
            PRAcks = [{RSeq, CSeq, Method, DialogId}],
            Meta1 = nksip_lib:store_value(nksip_100rel_pracks, PRAcks, Meta),
            Meta2 = nksip_lib:store_value(nksip_100rel_rseq, RSeq, Meta1),
            UAS1 = UAS#trans{meta=Meta2},
            {ok, Resp1, UAS1};
        _ ->
            {error, pending_prack}
    end.


%% @private
-spec uas_method(nksip:request(), nksip_call:trans(), nksip_call:call()) ->
    {nksip_call:trans(), nksip_call:call()}.

uas_method(Req, UAS, Call) ->
    #sipmsg{dialog_id=DialogId} = Req,
    #call{trans=Trans} = Call,
    {RSeq, CSeq, Method} = case nksip_sipmsg:header(<<"rack">>, Req) of
        [RACK] ->
            case nksip_lib:tokens(RACK) of
                [RSeqB, CSeqB, MethodB] ->
                    {
                        nksip_lib:to_integer(RSeqB),
                        nksip_lib:to_integer(CSeqB),
                        nksip_parse:method(MethodB)
                    };
                _ ->
                    throw({reply, {invalid_request, <<"Invalid RAck">>}})
            end;
        _ ->
            throw({reply, {invalid_request, <<"Invalid RAck">>}})
    end,
    OrigUAS = find_prack_trans(RSeq, CSeq, Method, DialogId, Trans),
    Meta = lists:keydelete(nksip_100rel_pracks, 1, OrigUAS#trans.meta),
    OrigUAS1 = OrigUAS#trans{meta=Meta},
    OrigUAS2 = nksip_call_lib:retrans_timer(cancel, OrigUAS1, Call),
    OrigUAS3 = nksip_call_lib:timeout_timer(timer_c, OrigUAS2, Call),
    {UAS, nksip_call_lib:update(OrigUAS3, Call)}.


%% @private
find_prack_trans(_RSeq, _CSeq, _Method, _DialogId, []) ->
    throw({reply, no_transaction});

find_prack_trans(RSeq, CSeq, Method, Dialog, [Trans|Rest]) ->
    case 
        Trans#trans.status==invite_proceeding andalso 
        nksip_lib:get_value(nksip_100rel_pracks, Trans#trans.meta, []) 
    of
        [{RSeq, CSeq, Method, Dialog}] -> 
            Trans;
        _ -> 
            find_prack_trans(RSeq, CSeq, Method, Dialog, Rest)
    end.


%% @private
-spec timeout_timer(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:trans().

timeout_timer(UAS, Call) ->
    #trans{timeout_timer=Timeout0} = UAS,
    #call{timers=#call_timers{t1=T1}} = Call,
    nksip_call_lib:cancel_timer(Timeout0),
    Timeout1 = nksip_call_lib:start_timer(64*T1, nksip_100rel_prack_timeout, UAS),
    UAS#trans{timeout_timer=Timeout1}.


%% @private
-spec retrans_timer(nksip_call:trans(), nksip_call:call()) ->
    nksip_call:trans().

retrans_timer(UAS, Call) ->
    #trans{retrans_timer=Retrans0, next_retrans=Next} = UAS,
    #call{timers=#call_timers{t1=T1}} = Call,
    nksip_call_lib:cancel_timer(Retrans0),
    Time = case is_integer(Next) of
        true -> Next;
        false -> T1
    end, 
    Retrans1 = nksip_call_lib:start_timer(Time, nksip_100rel_prack_retrans, UAS),
    UAS#trans{retrans_timer=Retrans1, next_retrans = 2*Time}.





