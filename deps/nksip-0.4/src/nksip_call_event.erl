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

%% @private Call dialog event library module.
-module(nksip_call_event).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([uac_pre_request/3, uac_request/3, uac_response/4]).
-export([uas_request/3, uas_response/4]).
-export([stop/3, create_prov_event/2, remove_prov_event/2, is_prov_event/2, timer/3]).
-export([request_uac_opts/3]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% UAC
%% ===================================================================


%% @private
-spec uac_pre_request(nksip:request(), nksip:dialog(), nksip_call:call()) ->
    ok | {error, no_transaction}.

uac_pre_request(#sipmsg{class={req, 'NOTIFY'}}=Req, Dialog, _Call) ->
    case nksip_subscription:find(Req, Dialog) of
        not_found ->  

            lager:warning("PRE REQ: ~p, ~p", [nksip_subscription:make_id(Req), Dialog#dialog.subscriptions]),

            {error, no_transaction};
        #subscription{class=uas} -> ok;
        _ -> 

            {error, no_transaction}
    end;

uac_pre_request(_Req, _Dialog, _Call) ->
    ok.


%% @private
-spec uac_request(nksip:request(), nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

uac_request(_Req, Dialog, _Call) ->
    Dialog.


%% @private
-spec uac_response(nksip:request(), nksip:response(), 
                   nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

uac_response(#sipmsg{class={req, Method}}=Req, Resp, Dialog, Call)
             when Method=='SUBSCRIBE'; Method=='NOTIFY'; Method=='REFER' ->
    #sipmsg{class={resp, Code, _Reason}} = Resp,
    case nksip_subscription:find(Req, Dialog) of
        #subscription{class=Class, id=Id} = Subs
            when (Class==uac andalso Method=='SUBSCRIBE') orelse
                 (Class==uac andalso Method=='REFER') orelse
                 (Class==uas andalso Method=='NOTIFY') ->
            ?call_debug("Subscription ~s UAC response ~p ~p", [Id, Method, Code]),
            uac_do_response(Method, Code, Req, Resp, Subs, Dialog, Call);
        not_found when Code>=200 andalso Code<300 andalso
                       (Method=='SUBSCRIBE' orelse Method=='REFER') ->
            Subs = #subscription{id=Id} = create(uac, Req, Dialog, Call),
            ?call_debug("Subscription ~s UAC response ~p ~p", [Id, Method, Code]),
            uac_do_response(Method, Code, Req, Resp, Subs, Dialog, Call);
        _ ->
            case Code>=200 andalso Code<300 of
                true -> ?call_notice("UAC event ignoring ~p ~p", [Method, Code]);
                false -> ok
            end,
            Dialog
    end;

uac_response(_Req, _Resp, Dialog, _Call) ->
    Dialog.


%% @private
-spec uac_do_response(nksip:method(), nksip:response_code(), nksip:request(), 
                      nksip:response(), nksip:subscription(), nksip:dialog(), 
                      nksip_call:call()) ->
    nksip:dialog().

uac_do_response('SUBSCRIBE', Code, Req, Resp, Subs, Dialog, Call) 
                when Code>=200, Code<300 ->
    update({subscribe, Req, Resp}, Subs, Dialog, Call);

%% See RFC5070 for termination codes
uac_do_response('SUBSCRIBE', Code, _Req, _Resp, Subs, Dialog, Call) 
                when Code>=300 ->
    case Subs#subscription.answered of
        undefined ->
            update({active, 10}, Subs, Dialog, Call);
            % update({terminated, {code, Code}, undefined}, Subs, Dialog, Call);
        _ when Code==405; Code==408; Code==481; Code==501 ->
            update({terminated, {code, Code}, undefined}, Subs, Dialog, Call);
        _ ->
            update(none, Subs, Dialog, Call)
    end;

uac_do_response('NOTIFY', Code, Req, _Resp, Subs, Dialog, Call) 
                when Code>=200, Code<300 ->
    update({notify, Req}, Subs, Dialog, Call);
        
uac_do_response('NOTIFY', Code, _Req, _Resp, Subs, Dialog, Call)
                when Code==405; Code==408; Code==481; Code==501 ->
    update({terminated, {code, Code}, undefined}, Subs, Dialog, Call);

uac_do_response('REFER', Code, Req, Resp, Subs, Dialog, Call) ->
    uac_do_response('SUBSCRIBE', Code, Req, Resp, Subs, Dialog, Call);

uac_do_response(_, _Code, _Req, _Resp, _Subs, Dialog, _Call) ->
    Dialog.



%% ===================================================================
%% UAS
%% ===================================================================


%% @private
-spec uas_request(nksip:request(), nksip:dialog(), nksip_call:call()) ->
    {ok, nksip:dialog()} | {error, no_transaction}.

uas_request(#sipmsg{class={req, Method}}=Req, Dialog, Call)
            when Method=='SUBSCRIBE'; Method=='NOTIFY'; Method=='REFER' ->
    case nksip_subscription:find(Req, Dialog) of
        #subscription{class=Class, id=Id} when
            (Method=='SUBSCRIBE' andalso Class==uas) orelse
            (Method=='REFER' andalso Class==uas) orelse
            (Method=='NOTIFY' andalso Class==uac) ->
            ?call_debug("Subscription ~s UAS request ~p", [Id, Method]), 
            {ok, Dialog};
        not_found when Method=='SUBSCRIBE' andalso
                element(1, Req#sipmsg.event) == <<"refer">> ->
            {error, forbidden};
        not_found when Method=='SUBSCRIBE'; Method=='REFER' ->
            {ok, Dialog};
        not_found when Method=='NOTIFY' ->
            case is_prov_event(Req, Call) of
                true -> {ok, Dialog};
                false -> {error, no_transaction}
            end;
        _ ->
            ?call_notice("UAS event ignoring ~p", [Method]),
            {error, no_transaction}
    end;

uas_request(_Req, Dialog, _Call) ->
    Dialog.


%% @private
-spec uas_response(nksip:request(), nksip:response(), 
                   nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

uas_response(#sipmsg{class={req, Method}}=Req, Resp, Dialog, Call)
             when Method=='SUBSCRIBE'; Method=='NOTIFY'; Method=='REFER' ->
    #sipmsg{class={resp, Code, _Reason}} = Resp,
    case nksip_subscription:find(Req, Dialog) of
        #subscription{class=Class, id=Id} = Subs when
            (Method=='SUBSCRIBE' andalso Class==uas) orelse
            (Method=='REFER' andalso Class==uas) orelse
            (Method=='NOTIFY' andalso Class==uac) ->
            ?call_debug("Subscription ~s UAS response ~p ~p", [Id, Method, Code]),
            uas_do_response(Method, Code, Req, Resp, Subs, Dialog, Call);
        not_found when Code>=200, Code<300 ->
            Class = case Method of 
                'SUBSCRIBE' -> uas; 
                'REFER' -> uas; 
                'NOTIFY' -> uac 
            end,
            Subs = #subscription{id=Id} = create(Class, Req, Dialog, Call),
            ?call_debug("Subscription ~s UAS response ~p, ~p", [Id, Method, Code]), 
            uas_do_response(Method, Code, Req, Resp, Subs, Dialog, Call);
        _ ->
            case Code>=200 andalso Code<300 of
                true ->
                    ?call_notice("UAS event ignoring ~p ~p", [Method, Code]);
                false ->
                    ok
            end,
            Dialog
    end;

uas_response(_Req, _Resp, Dialog, _Call) ->
    Dialog.


%% @private
-spec uas_do_response(nksip:method(), nksip:response_code(), nksip:request(), 
                      nksip:response(), nksip:subscription(), nksip:dialog(), 
                      nksip_call:call()) ->
    nksip:dialog().

uas_do_response(_, Code, _Req, _Resp, _Subs, Dialog, _Call) when Code<200 ->
    Dialog;

uas_do_response('SUBSCRIBE', Code, Req, Resp, Subs, Dialog, Call) 
                when Code>=200, Code<300 ->
    update({subscribe, Req, Resp}, Subs, Dialog, Call);
        
uas_do_response('SUBSCRIBE', Code, _Req, _Resp, Subs, Dialog, Call) 
                when Code>=300 ->
    case Subs#subscription.answered of
        undefined ->
            update({terminated, {code, Code}, undefined}, Subs, Dialog, Call);
        _ when Code==405; Code==408; Code==481; Code==501 ->
            update({terminated, {code, Code}, undefined}, Subs, Dialog, Call);
        _ ->
            update(none, Subs, Dialog, Call)

    end;

uas_do_response('NOTIFY', Code, Req, _Resp, Subs, Dialog, Call) 
                when Code>=200, Code<300 ->
    update({notify, Req}, Subs, Dialog, Call);
        
uas_do_response('NOTIFY', Code, _Req, _Resp, Subs, Dialog, Call) 
                when Code==405; Code==408; Code==481; Code==501 ->
    update({terminated, {code, Code}, undefined}, Subs, Dialog, Call);

uas_do_response('REFER', Code, Req, Resp, Subs, Dialog, Call) ->
    uas_do_response('SUBSCRIBE', Code, Req, Resp, Subs, Dialog, Call);

uas_do_response(_, _Code, _Req, _Resp, _Subs, Dialog, _Call) ->
    Dialog.
    


%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec update(term(), nksip:subscription(), nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

update(none, Subs, Dialog, Call) ->
    store(Subs, Dialog, Call);

update({subscribe, #sipmsg{class={req, Method}}=Req, Resp}, Subs, Dialog, Call) ->
    #subscription{
        id = Id, 
        timer_n = TimerN,
        timer_expire = TimerExpire,
        timer_middle = TimerMiddle,
        last_notify_cseq = NotifyCSeq
    } = Subs,
    cancel_timer(TimerN),
    cancel_timer(TimerExpire),
    cancel_timer(TimerMiddle),
    #call{timers={T1, _, _, TC, _}} = Call,
    ReqExpires = case Req#sipmsg.expires of
        RE0 when is_integer(RE0), RE0>=0 -> RE0;
        _ when Method=='REFER' -> round(TC/1000);
        _ -> ?DEFAULT_EVENT_EXPIRES
    end,
    RespExpires = case Resp#sipmsg.expires of
        SE0 when is_integer(SE0), SE0>=0 -> SE0;
        _ when Method=='REFER' -> round(TC/1000);
        _ -> ?DEFAULT_EVENT_EXPIRES
    end,
    Expires = min(ReqExpires, RespExpires),
    ?call_debug("Event ~s expires updated to ~p", [Id, Expires]),
    TimerN1 = case NotifyCSeq > element(1, Req#sipmsg.cseq) of
        true when Expires>0 -> 
            undefined;     % NOTIFY already received
        _ -> 
            start_timer(64*T1, {timeout, Id}, Dialog)
    end,
    TimerExpire1 = case Expires of
        0 -> undefined;
        _ -> start_timer(1000*Expires, {timeout, Id}, Dialog)
    end,
    TimerMiddle1= case Expires of
        0 -> undefined;
        _ -> start_timer(500*Expires, {middle, Id}, Dialog)
    end,
    Subs1 = Subs#subscription{
        expires = Expires,
        timer_n = TimerN1,
        timer_expire = TimerExpire1,
        timer_middle = TimerMiddle1
    },
    store(Subs1, Dialog, Call);

update({notify, Req}, Subs, Dialog, Call) ->
    Subs1 = Subs#subscription{last_notify_cseq=element(1, Req#sipmsg.cseq)},
    Status = case nksip_subscription:subscription_state(Req) of
        invalid -> 
            ?call_notice("Invalid subscription state", []),
            {terminated, {code, 400}, undefined};
        SE -> 
            SE
    end,
    update(Status, Subs1, Dialog, Call);

update({Status, Expires}, Subs, Dialog, Call) 
        when Status==active; Status==pending ->
    #subscription{
        id = Id, 
        status = OldStatus,
        answered = Answered,
        timer_n = TimerN,
        timer_expire = TimerExpire,
        timer_middle = TimerMiddle
    } = Subs,
    case Status==OldStatus of
        true -> 
            ok;
        false -> 
            ?call_debug("Subscription ~s ~p -> ~p", [Id, OldStatus, Status]),
            cast(Status, Subs, Dialog, Call)
    end,
    cancel_timer(TimerN),
    cancel_timer(TimerExpire),
    cancel_timer(TimerMiddle),
    ?call_debug("Event ~s expires updated to ~p", [Id, Expires]),
    Answered1 = case Answered of
        undefined -> nksip_lib:timestamp();
        _ -> Answered
    end,
    Subs1 = Subs#subscription{
        status = Status,
        answered = Answered1,
        timer_n = undefined,
        timer_expire = start_timer(1000*Expires, {timeout, Id}, Dialog),
        timer_middle = start_timer(500*Expires, {middle, Id}, Dialog)
    },
    store(Subs1, Dialog, Call);

update({terminated, Reason, Retry}, Subs, Dialog, Call) ->
    #subscription{
        id = Id,
        status = OldStatus,
        timer_n = N, 
        timer_expire = Expire, 
        timer_middle = Middle
    } = Subs,
    cancel_timer(N),
    cancel_timer(Expire),
    cancel_timer(Middle),
    ?call_debug("Subscription ~s ~p -> {terminated, ~p}", [Id, OldStatus, Reason]),
    cast({terminated, Reason, Retry}, Subs, Dialog, Call),
    store(Subs#subscription{status={terminated, Reason}}, Dialog, Call).

% update(Status, Subs, Dialog, Call) ->
%     ?call_warning("Unknown event status: ~p", [Status]),
%     store(Subs, Dialog, Call).
%     % update({terminated, {code, 500}, undefined}, Subs, Dialog, Call).


%% @private. Create a provisional event and start timer N.
-spec create_prov_event(nksip:request(),  nksip_call:call()) ->
    nksip_call:call().

create_prov_event(#sipmsg{from={_, FromTag}}=Req, Call) ->
    Id = nksip_subscription:make_id(Req),
    ?call_debug("Provisional event ~s (~s) UAC created", [Id, FromTag]),
    #call{timers={T1, _, _, _, _}} = Call,
    Timer = erlang:start_timer(64*T1, self(), {remove_prov_event, {Id, FromTag}}),
    ProvEvent = #provisional_event{id={Id, FromTag}, timer_n=Timer},
    Call#call{events=[ProvEvent|Call#call.events]}.


%% @private Removes a stored provisional event.
-spec remove_prov_event(nksip:request() | {binary(), binary()}, nksip_call:call()) ->
    nksip_call:call().

remove_prov_event(#sipmsg{from={_, FromTag}}=Req, Call) ->
    Id = nksip_subscription:make_id(Req),
    remove_prov_event({Id, FromTag}, Call);

remove_prov_event({Id, FromTag}, #call{events=Events}=Call) ->
    case lists:keytake({Id, FromTag}, #provisional_event.id, Events) of
        {value, #provisional_event{timer_n=Timer}, Rest} ->
            cancel_timer(Timer),
            ?call_debug("Provisional event ~s (~s) destroyed", [Id, FromTag]),
            Call#call{events=Rest};
        false ->
            Call
    end.


%% @private
stop(#subscription{id=Id}, Dialog, Call) ->
    case nksip_subscription:find(Id, Dialog) of
        not_found -> Call;
        Subs -> update({terminated, forced, undefined}, Subs, Dialog, Call)
    end.


%% @private
-spec request_uac_opts(nksip:method(), nksip_lib:optslist(), 
                       nksip:dialog() | nksip:subscription()) ->
    {ok, nksip_lib:optslist()} | {error, unknown_subscription}.

request_uac_opts(Method, Opts, #dialog{}=Dialog) ->
    case lists:keytake(subscription_id, 1, Opts) of
        false ->
            {ok, Opts};
        {value, {_, Id}, Opts1} ->
            {_AppId, SubsId, _DialogId, _CallId} = nksip_subscription:parse_id(Id),
            case nksip_subscription:find(SubsId, Dialog) of
                #subscription{} = Subs ->
                    {ok, request_uac_opts(Method, Opts1, Subs)};
                not_found ->
                    {error, unknown_subscription}
            end
    end;

request_uac_opts('SUBSCRIBE', Opts, #subscription{event=Event, expires=Expires}) ->
    [{event, Event}, {expires, Expires} | Opts];

request_uac_opts('NOTIFY', Opts, #subscription{event=Event, timer_expire=Timer}) ->
    {value, {_, SS}, Opts1} = lists:keytake(subscription_state, 1, Opts),
    SS1 = case SS of
        {State, _Expire} when State==active; State==pending ->
            case is_reference(Timer) of
                true -> 
                    Expires = round(erlang:read_timer(Timer)/1000),
                    {State, Expires};
                false ->
                    {terminated, timeout, undefined}
            end;
        {terminated, Reason, Retry} ->
            {terminated, Reason, Retry}
    end,
    [{event, Event}, {subscription_state, SS1} | Opts1].


% %% @private
% maybe_add_refer_event(#sipmsg{class={req, 'REFER'}}=Req, Call) ->
%     #sipmsg{cseq={CSeq, _}} = Req,
%     #call{timers={_, _, _, TimerC, _}} = Call,
%     Req#sipmsg{
%         event = {<<"refer">>, [{<<"id">>, nksip_lib:to_binary(CSeq)}]},
%         expires = round(TimerC/1000)
%     };

% maybe_add_refer_event(Req, _Call) ->
%     Req.


%% @private Called when a dialog timer is fired
-spec timer({middle|timeout, nksip_subscription:id()}, nksip:dialog(), nksip_call:call()) ->
    nksip_call:call().

timer({Type, Id}, Dialog, Call) ->
    case nksip_subscription:find(Id, Dialog) of
        #subscription{} = Subs when Type==middle -> 
            cast(middle_timer, Subs, Dialog, Call),
            Call;
        #subscription{} = Subs when Type==timeout -> 
            Dialog1 = update({terminated, timeout, undefined}, Subs, Dialog, Call),
            nksip_call_dialog:update(none, Dialog1, Call);
        not_found -> 
            ?call_notice("Subscription ~s timer fired for unknown event", [Id]),
            Call
    end.



%% ===================================================================
%% Util
%% ===================================================================

%% @private Creates a new event
-spec create(uac|uas, nksip:request(), nksip:dialog(), nksip_call:call()) ->
    nksip:subscription().

create(Class, #sipmsg{class={req, Method}}=Req, Dialog, Call) ->
    Event = case Method of
        'REFER' -> 
            #sipmsg{cseq={CSeq, _}} = Req,
            {<<"refer">>, [{<<"id">>, nksip_lib:to_binary(CSeq)}]};
        _ ->
            Req#sipmsg.event
    end,        
    Id = nksip_subscription:make_id(Req),
    #call{timers={T1, _, _, _, _}} = Call,
    Subs = #subscription{
        id = Id,
        event = Event,
        status = init,
        class = Class,
        answered = undefined,
        timer_n = start_timer(64*T1, {timeout, Id}, Dialog)
    },
    cast(init, Subs, Dialog, Call),
    Subs.


%% @private Finds a provisional event
-spec is_prov_event(nksip:request(), nksip_call:call()) ->
    boolean().

is_prov_event(#sipmsg{class={req, 'NOTIFY'}, to={_, ToTag}}=Req, #call{events=Events}) ->
    Id = nksip_subscription:make_id(Req),
    do_is_prov_event({Id, ToTag}, Events);

is_prov_event(_, _) ->
    false.


%% @private.
do_is_prov_event(_, []) -> false;
do_is_prov_event(Id, [#provisional_event{id=Id}|_]) -> true;
do_is_prov_event(Id, [_|Rest]) -> do_is_prov_event(Id, Rest).


%% @private Updates an updated event into dialog
-spec store(nksip:subscription(), nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

store(Subs, Dialog, _Call) ->
    #subscription{id=Id, status=Status, class=Class} = Subs,
    #dialog{subscriptions=Subscriptions} = Dialog,
    case Subscriptions of
        [] -> Rest = [], IsFirst = true;
        [#subscription{id=Id}|Rest] -> IsFirst = true;
        _ -> Rest = [], IsFirst = false
    end,
    UA = case Class of uac -> "UAC"; uas -> "UAS" end,
    case Status of
        {terminated, _Reason} ->
            ?call_debug("~s removing event ~s", [UA, Id]),
            Subscriptions1 = case IsFirst of
                true -> Rest;
                false -> lists:keydelete(Id, #subscription.id, Subscriptions)
            end,
            Dialog#dialog{subscriptions=Subscriptions1};
        _ ->
            ?call_debug("~s storing event ~s: ~p", [UA, Id, Status]),
            Subscriptions1 = case IsFirst of
                true -> [Subs|Rest];
                false -> lists:keystore(Id, #subscription.id, Subscriptions, Subs)
            end,
            Dialog#dialog{subscriptions=Subscriptions1}
    end.



%% @private
-spec cast(term(), nksip:subscription(), nksip:dialog(), nksip_call:call()) ->
    ok.

cast(Arg, Subs, Dialog, Call) ->
    Arg1 = case Arg of
        {terminated, Reason, undefined} -> {terminated, Reason};
        _ -> Arg
    end,
    Id = nksip_subscription:get_id(Subs, Dialog),
    nksip_call_dialog:cast(dialog_update, {subscription_status, Id, Arg1}, Dialog, Call).


%% @private
cancel_timer(Ref) when is_reference(Ref) -> 
    case erlang:cancel_timer(Ref) of
        false -> receive {timeout, Ref, _} -> ok after 0 -> ok end;
        _ -> ok
    end;

cancel_timer(_) ->
    ok.


%% @private
-spec start_timer(integer(), {atom(), nksip_subscription:id()}, nksip:dialog()) ->
    reference().

start_timer(Time, Tag, #dialog{id=Id}) ->
    erlang:start_timer(Time , self(), {dlg, {event, Tag}, Id}).

