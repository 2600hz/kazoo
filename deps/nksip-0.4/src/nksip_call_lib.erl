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

%% @private Call process utilities
-module(nksip_call_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([uac_transaction_id/1, uas_transaction_id/1, update/2]).
-export([update_auth/3, check_auth/2]).
-export([timeout_timer/3, retrans_timer/3, expire_timer/3]).
-export([cancel_timer/1, start_timer/3]).
-export_type([timeout_timer/0, retrans_timer/0, expire_timer/0, timer/0]).


-include("nksip.hrl").
-include("nksip_call.hrl").

-type timeout_timer() :: timer_b | timer_c | timer_d | timer_f | timer_h | 
                         timer_i | timer_j | timer_k | timer_l | timer_m | 
                         noinvite | cancel.

-type retrans_timer() :: timer_a | timer_e | timer_g | cancel.

-type expire_timer() ::  expire | cancel.

-type timer() :: timeout_timer() | retrans_timer() | expire_timer().

-type call() :: nksip_call:call().

-type trans() :: nksip_call:trans().




%% ===================================================================
%% Private
%% ===================================================================



%% @private
-spec uac_transaction_id(nksip:request()|nksip:response()) -> 
    integer().

uac_transaction_id(#sipmsg{cseq={_, Method}, vias=[Via|_]}) ->
    Branch = nksip_lib:get_value(<<"branch">>, Via#via.opts),
    erlang:phash2({Method, Branch}).


%% @private
-spec uas_transaction_id(nksip:request()) ->
    integer().
    
uas_transaction_id(Req) ->
        #sipmsg{
            class = {req, Method},
            ruri = RUri, 
            from = {_, FromTag}, 
            to = {_, ToTag}, 
            vias = [Via|_], 
            cseq = {CSeq, _}
        } = Req,
    {_Transp, ViaIp, ViaPort} = nksip_parse:transport(Via),
    case nksip_lib:get_value(<<"branch">>, Via#via.opts) of
        <<"z9hG4bK", Branch/binary>> when byte_size(Branch) > 0 ->
            erlang:phash2({Method, ViaIp, ViaPort, Branch});
        _ ->
            % pre-RFC3261 style
            {_, UriIp, UriPort} = nksip_parse:transport(RUri),
            -erlang:phash2({UriIp, UriPort, FromTag, ToTag, CSeq, 
                            Method, ViaIp, ViaPort})
    end.




%% @private
%% Updates a transaction on a call. New transaction will be the first.
-spec update(trans(), call()) ->
    call().

update(New, Call) ->
    #trans{
        id = TransId, 
        class = Class, 
        status = NewStatus, 
        method = Method
    } = New,
    #call{
        trans= Trans,
        hibernate = Hibernate
    } = Call,
    case Trans of
        [#trans{id=TransId, status=OldStatus}|Rest] -> 
            ok;
        _ -> 
            case lists:keytake(TransId, #trans.id, Trans) of 
                {value, #trans{status=OldStatus}, Rest} -> ok;
                false -> OldStatus=finished, Rest=Trans
            end
    end,
    CS = case Class of 
        uac -> <<"UAC">>; 
        uas -> <<"UAS">> 
    end,
    NewTrans = case NewStatus of
        finished ->
            ?call_debug("~s ~p ~p (~p) removed", 
                        [CS, TransId, Method, OldStatus]),
            Rest;
        _ when NewStatus==OldStatus -> 
            [New|Rest];
        _ -> 
            ?call_debug("~s ~p ~p ~p -> ~p", [CS, TransId, Method, OldStatus, NewStatus]),
            [New|Rest]
    end,
    NewHibernate = if
        NewStatus==invite_accepted; NewStatus==completed; NewStatus==finished ->
            NewStatus;
        NewStatus==invite_completed, Class==uac ->
            NewStatus;
        true ->
            Hibernate
    end,
    Call#call{trans=NewTrans, hibernate=NewHibernate}.


%% @private
-spec update_auth(nksip_dialog_lib:id(), nksip:request()|nksip:response(), call()) ->
    call().

update_auth(<<>>, _SipMsg, Call) ->
    Call;

update_auth(DialogId, SipMsg, #call{auths=Auths}=Call) ->
    case SipMsg of
        #sipmsg{transport=#transport{proto=Proto, remote_ip=Ip, remote_port=Port}} ->
            case lists:member({DialogId, Proto, Ip, Port}, Auths) of
                true ->
                    Call;
                false -> 
                    ?call_debug("Added cached auth for dialog ~s (~p:~p:~p)", 
                                [DialogId, Proto, Ip, Port]),
                    Call#call{auths=[{DialogId, Proto, Ip, Port}|Auths]}
            end;
        _ ->
            Call
    end.


%% @private
-spec check_auth(nksip:request()|nksip:response(), call()) ->
    boolean().

check_auth(#sipmsg{dialog_id = <<>>}, _Call) ->
    false;

check_auth(#sipmsg{dialog_id=DialogId, transport=#transport{}=Transp}, Call) ->
    #transport{proto=Proto, remote_ip=Ip, remote_port=Port} = Transp,
    #call{auths=Auths} = Call,
    case lists:member({DialogId, Proto, Ip, Port}, Auths) of
        true ->
            ?call_debug("Origin ~p:~p:~p is in dialog ~s authorized list", 
                        [Proto, Ip, Port, DialogId]),
            true;
        false ->
            AuthList = [{O, I, P} || {D, O, I, P}<-Auths, D==DialogId],
            ?call_debug("Origin ~p:~p:~p is NOT in dialog ~s "
                        "authorized list (~p)", 
                        [Proto, Ip, Port, DialogId, AuthList]),
            false
    end;

check_auth(_, _) ->
    false.


%% ===================================================================
%% Util - Timers
%% ===================================================================


%% @private
-spec timeout_timer(timeout_timer(), trans(), call()) ->
    trans().

timeout_timer(cancel, Trans, _Call) ->
    cancel_timer(Trans#trans.timeout_timer),
    Trans#trans{timeout_timer=undefined};

timeout_timer(Tag, Trans, Call) 
            when Tag==timer_b; Tag==timer_f; Tag==timer_m;
                 Tag==timer_h; Tag==timer_j; Tag==timer_l;
                 Tag==noinvite ->
    cancel_timer(Trans#trans.timeout_timer),
    #call{timers=#call_timers{t1=T1}} = Call,
    Trans#trans{timeout_timer=start_timer(64*T1, Tag, Trans)};

timeout_timer(timer_d, Trans, _) ->
    cancel_timer(Trans#trans.timeout_timer),
    Trans#trans{timeout_timer=start_timer(32000, timer_d, Trans)};

timeout_timer(Tag, Trans, Call) 
                when Tag==timer_k; Tag==timer_i ->
    cancel_timer(Trans#trans.timeout_timer),
    #call{timers=#call_timers{t4=T4}} = Call,
    Trans#trans{timeout_timer=start_timer(T4, Tag, Trans)};

timeout_timer(timer_c, Trans, Call) ->
    cancel_timer(Trans#trans.timeout_timer),
    #call{timers=#call_timers{tc=TC}} = Call,
    Trans#trans{timeout_timer=start_timer(1000*TC, timer_c, Trans)}.

% timeout_timer(sipapp_call, Trans, Call) ->
%     cancel_timer(Trans#trans.timeout_timer),
%     #call{timers={_, _, _, _, Time}} = Call,
%     Trans#trans{timeout_timer=start_timer(Time, sipapp_call, Trans)}.


%% @private
-spec retrans_timer(retrans_timer(), trans(), call()) ->
    trans().

retrans_timer(cancel, Trans, _Call) ->
    cancel_timer(Trans#trans.retrans_timer),
    Trans#trans{retrans_timer=undefined};

retrans_timer(timer_a, #trans{next_retrans=Next}=Trans, Call) -> 
    cancel_timer(Trans#trans.retrans_timer),
    Time = case is_integer(Next) of
        true -> 
            Next;
        false -> 
            #call{timers=#call_timers{t1=T1}} = Call,
            T1
    end,
    Trans#trans{
        retrans_timer = start_timer(Time, timer_a, Trans),
        next_retrans = 2*Time
    };

retrans_timer(Tag, #trans{next_retrans=Next}=Trans, Call) 
                when Tag==timer_e; Tag==timer_g ->
    cancel_timer(Trans#trans.retrans_timer),
    #call{timers=#call_timers{t1=T1, t2=T2}} = Call,
    Time = case is_integer(Next) of
        true -> Next;
        false -> T1
    end,
    Trans#trans{
        retrans_timer = start_timer(Time, Tag, Trans),
        next_retrans = min(2*Time, T2)
    }.


%% @private
-spec expire_timer(expire_timer(), trans(), call()) ->
    trans().

expire_timer(cancel, Trans, _Call) ->
    cancel_timer(Trans#trans.expire_timer),
    Trans#trans{expire_timer=undefined};

expire_timer(expire, Trans, _Call) ->
    #trans{id=TransId, class=Class, request=Req, opts=Opts} = Trans,
    cancel_timer(Trans#trans.expire_timer),
    Timer = case Req#sipmsg.expires of
        Expires when is_integer(Expires), Expires > 0 -> 
            case lists:member(no_auto_expire, Opts) of
                true -> 
                    ?call_debug("UAC ~p skipping INVITE expire", [TransId]),
                    undefined;
                _ -> 
                    Time = case Class of 
                        uac -> 1000*Expires;
                        uas -> 1000*Expires+100     % UAC fires first
                    end,
                    start_timer(Time, expire, Trans)
            end;
        _ ->
            undefined
    end,
    Trans#trans{expire_timer=Timer}.


%% @private
-spec start_timer(integer(), timer(), trans()) ->
    {timer(), reference()}.

start_timer(Time, Tag, #trans{class=Class, id=TransId}) ->
    {Tag, erlang:start_timer(round(Time), self(), {Class, Tag, TransId})}.


%% @private
-spec cancel_timer({timer(), reference()}|undefined) -> 
    ok.

cancel_timer(undefined) ->
    ok;

cancel_timer({_Tag, Ref}) when is_reference(Ref) -> 
    nksip_lib:cancel_timer(Ref),
    ok.


