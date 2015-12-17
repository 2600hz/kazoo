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

%% @private nksip_uac_auto_register plugin callbacksuests and related functions.
-module(nksip_uac_auto_register_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([nkcb_handle_call/3, nkcb_handle_cast/2, nkcb_handle_info/2]).
-export([nkcb_uac_auto_register_launch_register/3, 
         nkcb_uac_auto_register_launch_unregister/3, 
         nkcb_uac_auto_register_update_register/4, 
         nkcb_uac_auto_register_launch_ping/2, 
         nkcb_uac_auto_register_update_ping/4]).

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").
-include("nksip_uac_auto_register.hrl").



%% ===================================================================
%% Plugin Callbacks
%% ===================================================================


%% @private 
nkcb_handle_call({'$nksip_uac_auto_register_start_register', RegId, Uri, Opts}, 
                 From, SipAppState) ->
    #state{regs=Regs} = State = get_state(SipAppState),
    case nksip_lib:get_value(call_id, Opts) of
        undefined -> 
            CallId = nksip_lib:luid(),
            Opts1 = [{call_id, CallId}|Opts];
        CallId -> 
            Opts1 = Opts
    end,
    case nksip_lib:get_value(expires, Opts) of
        undefined -> 
            Expires = 300,
            Opts2 = [{expires, Expires}|Opts1];
        Expires -> 
            Opts2 = Opts1
    end,
    Reg = #sipreg{
        id = RegId,
        ruri = Uri,
        opts = Opts2,
        call_id = CallId,
        interval = Expires,
        from = From,
        cseq = nksip_config:cseq(),
        next = 0,
        ok = undefined
    },
    Regs1 = lists:keystore(RegId, #sipreg.id, Regs, Reg),
    #sipapp_srv{app_id=AppId} = SipAppState,
    ?debug(AppId, CallId, "Started auto registration: ~p", [Reg]),
    gen_server:cast(self(), '$nksip_uac_auto_register_check'),
    State1 = State#state{regs=Regs1},
    {ok, set_state(State1, SipAppState)};

nkcb_handle_call({'$nksip_uac_auto_register_stop_register', RegId}, From, SipAppState) ->
    #state{regs=Regs} = State = get_state(SipAppState),
    case lists:keytake(RegId, #sipreg.id, Regs) of
        {value, Reg, Regs1} -> 
            gen_server:reply(From, ok),
            #sipapp_srv{app_id=AppId} = SipAppState,
            {ok, SipAppState1} = 
                AppId:nkcb_uac_auto_register_launch_unregister(Reg, false, SipAppState),
            {ok, set_state(State#state{regs=Regs1}, SipAppState1)};
        false -> 
            gen_server:reply(From, not_found),
            {ok, SipAppState}
    end;

nkcb_handle_call('$nksip_uac_auto_register_get_registers', From, SipAppState) ->
    #state{regs=Regs} = get_state(SipAppState),
    Now = nksip_lib:timestamp(),
    Info = [
        {RegId, Ok, Next-Now}
        ||  #sipreg{id=RegId, ok=Ok, next=Next} <- Regs
    ],
    gen_server:reply(From, Info),
    {ok, SipAppState};

nkcb_handle_call({'$nksip_uac_auto_register_start_ping', PingId, Uri, Opts}, 
                 From,  SipAppState) ->
    #state{pings=Pings} = State = get_state(SipAppState),
    case nksip_lib:get_value(call_id, Opts) of
        undefined -> 
            CallId = nksip_lib:luid(),
            Opts1 = [{call_id, CallId}|Opts];
        CallId -> 
            Opts1 = Opts
    end,
    case nksip_lib:get_value(expires, Opts) of
        undefined -> 
            Expires = 300,
            Opts2 = Opts1;
        Expires -> 
            Opts2 = nksip_lib:delete(Opts1, expires)
    end,
    Ping = #sipreg{
        id = PingId,
        ruri = Uri,
        opts = Opts2,
        call_id = CallId,
        interval = Expires,
        from = From,
        cseq = nksip_config:cseq(),
        next = 0,
        ok = undefined
    },
    #sipapp_srv{app_id=AppId} = SipAppState,
    ?info(AppId, CallId, "Started auto ping: ~p", [Ping]),
    Pinsg1 = lists:keystore(PingId, #sipreg.id, Pings, Ping),
    gen_server:cast(self(), '$nksip_uac_auto_register_check'),
    State1 = State#state{pings=Pinsg1},
    {ok, set_state(State1, SipAppState)};

nkcb_handle_call({'$nksip_uac_auto_register_stop_ping', PingId}, From, SipAppState) ->
    #state{pings=Pings} = State = get_state(SipAppState),
    case lists:keytake(PingId, #sipreg.id, Pings) of
        {value, _, Pings1} -> 
            gen_server:reply(From, ok),
            {ok, set_state(State#state{pings=Pings1}, SipAppState)};
        false -> 
            gen_server:reply(From, not_found),
            {ok, SipAppState}
    end;

nkcb_handle_call('$nksip_uac_auto_register_get_pings', From, SipAppState) ->
    #state{pings=Pings} = get_state(SipAppState),
    Now = nksip_lib:timestamp(),
    Info = [
        {PingId, Ok, Next-Now}
        ||  #sipreg{id=PingId, ok=Ok, next=Next} <- Pings
    ],
    gen_server:reply(From, Info),
    {ok, SipAppState};

nkcb_handle_call(_Msg, _From, _SipAppState) ->
    continue.


%% @private
nkcb_handle_cast({'$nksip_uac_auto_register_answer_register', RegId, Code, Meta}, 
                 SipAppState) ->
    #state{regs=Regs} = State = get_state(SipAppState),
    #sipapp_srv{app_id=AppId} = SipAppState,
    case lists:keytake(RegId, #sipreg.id, Regs) of
        {value, #sipreg{ok=OldOK}=Reg, Regs1} ->
            {ok, Reg1, SipAppState1} = 
                AppId:nkcb_uac_auto_register_update_register(Reg, Code, Meta, SipAppState),
            #sipreg{ok=Ok} = Reg1,
            case Ok of
                OldOK -> 
                    ok;
                _ -> 
                    AppId:nkcb_call(sip_uac_auto_register_updated_register, 
                                    [RegId, Ok, AppId], AppId)
            end,
            State1 = State#state{regs=[Reg1|Regs1]},
            {ok, set_state(State1, SipAppState1)};
        false ->
            {ok, SipAppState}
    end;

nkcb_handle_cast({'$nksip_uac_auto_register_answer_ping', PingId, Code, Meta}, SipAppState) ->
    #state{pings=Pings} = State = get_state(SipAppState),
    #sipapp_srv{app_id=AppId} = SipAppState,
    case lists:keytake(PingId, #sipreg.id, Pings) of
        {value, #sipreg{ok=OldOK}=Ping, Pings1} ->
            {ok, #sipreg{ok=OK}=Ping1, SipAppState1} = 
                AppId:nkcb_uac_auto_register_update_ping(Ping, Code, Meta, SipAppState),
            case OK of
                OldOK -> 
                    ok;
                _ -> 
                    AppId:nkcb_call(sip_uac_auto_register_updated_ping, [PingId, OK, AppId], AppId)
            end,
            State1 = State#state{pings=[Ping1|Pings1]},
            {ok, set_state(State1, SipAppState1)};
        false ->
            {ok, SipAppState}
    end;

nkcb_handle_cast('$nksip_uac_auto_register_force_regs', SipAppState) ->
    #state{regs=Regs} = State = get_state(SipAppState),
    Regs1 = lists:map(
        fun(#sipreg{next=Next}=SipReg) ->
            case is_integer(Next) of
                true -> SipReg#sipreg{next=0};
                false -> SipReg
            end
        end,
        Regs),
    {ok, set_state(State#state{regs=Regs1}, SipAppState)};

nkcb_handle_cast('$nksip_uac_auto_register_check', SipAppState) ->
    #state{pings=Pings, regs=Regs} = State = get_state(SipAppState),
    Now = nksip_lib:timestamp(),
    {Pings1, SipAppState1} = check_pings(Now, Pings, [], SipAppState),
    {Regs1, SipAppState2} = check_registers(Now, Regs, [], SipAppState1),
    State1 = State#state{pings=Pings1, regs=Regs1},
    {ok, set_state(State1, SipAppState2)};

nkcb_handle_cast(_Msg, _SipAppState) ->
    continue.


%% @private
nkcb_handle_info({timeout, _, '$nksip_uac_auto_register_timer'}, SipAppState) ->
    #sipapp_srv{app_id=AppId} = SipAppState,
    Timer = 1000 * nksip_sipapp_srv:config(AppId, nksip_uac_auto_register_timer),
    erlang:start_timer(Timer, self(), '$nksip_uac_auto_register_timer'),
    gen_server:cast(self(), '$nksip_uac_auto_register_check'),
    continue;

nkcb_handle_info(_Msg, _SipAppState) ->
    continue.


%% ===================================================================
%% Callbacks offered to second-level plugins
%% ===================================================================


%% @private
-spec nkcb_uac_auto_register_launch_register(#sipreg{}, boolean(), nksip_sipapp_srv:state()) -> 
    {ok, #sipreg{}, nksip_sipapp_srv:state()}.

nkcb_uac_auto_register_launch_register(Reg, Sync, SipAppState)->
    #sipreg{id=RegId, ruri=RUri, opts=Opts, cseq=CSeq} = Reg,    
    Opts1 = [contact, {cseq_num, CSeq}, {meta, [cseq_num, retry_after]}|Opts],
    Self = self(),
    #sipapp_srv{app_id=AppId} = SipAppState,
    Fun = fun() ->
        case nksip_uac:register(AppId, RUri, Opts1) of
            {ok, Code, Meta} -> ok;
            _ -> Code=500, Meta=[{cseq_num, CSeq}]
        end,
        gen_server:cast(Self, {'$nksip_uac_auto_register_answer_register', RegId, Code, Meta})
    end,
    case Sync of
        true -> Fun();
        false -> spawn_link(Fun)
    end,
    {ok, Reg#sipreg{next=undefined}, SipAppState}.
    

%% @private
-spec nkcb_uac_auto_register_launch_unregister(#sipreg{}, boolean(), nksip_sipapp_srv:state()) -> 
    {ok, nksip_sipapp_srv:state()}.

nkcb_uac_auto_register_launch_unregister(Reg, Sync, SipAppState)->
    #sipreg{ruri=RUri, opts=Opts, cseq=CSeq} = Reg,
    Opts1 = [contact, {cseq_num, CSeq}|nksip_lib:store_value(expires, 0, Opts)],
    #sipapp_srv{app_id=AppId} = SipAppState,
    Fun = fun() -> nksip_uac:register(AppId, RUri, Opts1) end,
    case Sync of
        true -> Fun();
        false -> spawn_link(Fun)
    end,
    {ok, SipAppState}.

   
%% @private
-spec nkcb_uac_auto_register_update_register(#sipreg{}, nksip:sip_code(), 
                                    nksip:optslist(), nksip_sipapp_srv:state()) ->
    {ok, #sipreg{}, nksip_sipapp_srv:state()}.

nkcb_uac_auto_register_update_register(Reg, Code, _Meta, SipAppState) when Code<200 ->
    {ok, Reg, SipAppState};

nkcb_uac_auto_register_update_register(Reg, Code, Meta, SipAppState) ->
    #sipreg{interval=Interval, from=From} = Reg,
    case From of
        undefined -> ok;
        _ -> gen_server:reply(From, {ok, Code<300})
    end,
    Time = case Code==503 andalso nksip_lib:get_value(retry_after, Meta) of
        false -> Interval;
        undefined -> Interval;
        Retry -> Retry
    end,
    Reg1 = Reg#sipreg{
        ok = Code < 300,
        cseq = nksip_lib:get_value(cseq_num, Meta) + 1,
        from = undefined,
        next = nksip_lib:timestamp() + Time
    },
    {ok, Reg1, SipAppState}.


%%%%%% Ping

%% @private
-spec nkcb_uac_auto_register_launch_ping(#sipreg{}, nksip_sipapp_srv:state()) -> 
    {ok, #sipreg{}, nksip_sipapp_srv:state()}.

nkcb_uac_auto_register_launch_ping(Ping, SipAppState)->
    #sipreg{id=PingId, ruri=RUri, opts=Opts, cseq=CSeq} = Ping,
    Opts1 = [{cseq_num, CSeq}, {meta, [cseq_num, retry_after]} | Opts],
    Self = self(),
    #sipapp_srv{app_id=AppId} = SipAppState,
    Fun = fun() ->
        case nksip_uac:options(AppId, RUri, Opts1) of
            {ok, Code, Meta} -> ok;
            _ -> Code=500, Meta=[{cseq_num, CSeq}]
        end,
        gen_server:cast(Self, {'$nksip_uac_auto_register_answer_ping', PingId, Code, Meta})
    end,
    spawn_link(Fun),
    {ok, Ping#sipreg{next=undefined}, SipAppState}.


   
%% @private
-spec nkcb_uac_auto_register_update_ping(#sipreg{}, nksip:sip_code(), 
                                nksip:optslist(), nksip_siapp_srv:state()) ->
    {ok, #sipreg{}, nksip_siapp_srv:state()}.

nkcb_uac_auto_register_update_ping(Ping, Code, _Meta, SipAppState) when Code<200 ->
    {ok, Ping, SipAppState};

nkcb_uac_auto_register_update_ping(Ping, Code, Meta, SipAppState) ->
    #sipreg{from=From, interval=Interval} = Ping,
    case From of
        undefined -> ok;
        _ -> gen_server:reply(From, {ok, Code<300})
    end,
    Time = case Code==503 andalso nksip_lib:get_value(retry_after, Meta) of
        false -> Interval;
        undefined -> Interval;
        Retry -> Retry
    end,
    Ping1 = Ping#sipreg{
        ok = Code < 300,
        cseq = nksip_lib:get_value(cseq_num, Meta) + 1,
        from = undefined,
        next = nksip_lib:timestamp() + Time
    },
    {ok, Ping1, SipAppState}.



%% ===================================================================
%% Private
%% ===================================================================

%% @private
check_pings(Now, [#sipreg{next=Next}=Ping|Rest], Acc, SipAppState) ->
    case is_integer(Next) andalso Now>=Next of 
        true -> 
            #sipapp_srv{app_id=AppId} = SipAppState,
            {ok, Ping1, SipAppState1} = 
                AppId:nkcb_uac_auto_register_launch_ping(Ping, SipAppState),
            check_pings(Now, Rest, [Ping1|Acc], SipAppState1);
        false ->
            check_pings(Now, Rest, [Ping|Acc], SipAppState)
    end;
    
check_pings(_, [], Acc, SipAppState) ->
    {Acc, SipAppState}.


%% @private Only one register in each cycle
check_registers(Now, [#sipreg{next=Next}=Reg|Rest], Acc, SipAppState) ->
    #sipapp_srv{app_id=AppId} = SipAppState,
    case Now>=Next of
        true -> 
            {ok, Reg1, SipAppState1} = 
                AppId:nkcb_uac_auto_register_launch_register(Reg, false, SipAppState),
            check_registers(-1, Rest, [Reg1|Acc], SipAppState1);
        false ->
            check_registers(Now, Rest, [Reg|Acc], SipAppState)
    end;

check_registers(_, [], Acc, SipAppState) ->
    {Acc, SipAppState}.


%% @private
get_state(SipAppState) ->
    nksip_sipapp_srv:get_meta(nksip_uac_auto_register, SipAppState).


%% @private
set_state(State, SipAppState) ->
    nksip_sipapp_srv:set_meta(nksip_uac_auto_register, State, SipAppState).

