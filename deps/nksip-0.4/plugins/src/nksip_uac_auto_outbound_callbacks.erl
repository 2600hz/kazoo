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

%% @private
-module(nksip_uac_auto_outbound_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([nkcb_handle_call/3, nkcb_handle_info/2]).
-export([nkcb_uac_auto_register_launch_register/3, 
         nkcb_uac_auto_register_launch_unregister/3, 
         nkcb_uac_auto_register_update_register/4]).

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").
-include("nksip_uac_auto_register.hrl").
-include("nksip_uac_auto_outbound.hrl").



%% ===================================================================
%% Plugin Callbacks
%% ===================================================================


%% @private
nkcb_handle_call('$nksip_uac_auto_outbound_get_registers', From, SipAppState) ->
    #state{regs=Regs} = nksip_sipapp_srv:get_meta(nksip_uac_auto_register, SipAppState),
    #state_ob{regs=RegsOb} = get_state(SipAppState),
    Now = nksip_lib:timestamp(),
    Info = [
        {RegId, Ok, Next-Now, Fails} 
        ||
        #sipreg{id=RegId, ok=Ok, next=Next} <- Regs,
            case lists:keyfind(RegId, #sipreg_ob.id, RegsOb) of
                false -> Fails=0, false;
                #sipreg_ob{fails=Fails} -> true
            end
    ],
    gen_server:reply(From, Info),
    {ok, SipAppState};

nkcb_handle_call(_Msg, _From, _SipAppState) ->
    continue.


%% @private
-spec nkcb_handle_info(term(), nksip_sipapp_srv:state()) ->
    {ok, nksip_sipapp_srv:state()} | continue | {continue, list()}.

nkcb_handle_info({'DOWN', Mon, process, _Pid, _}, SipAppState) ->
    #state_ob{regs=RegsOb} = get_state(SipAppState),
    case lists:keyfind(Mon, #sipreg_ob.conn_monitor, RegsOb) of
        #sipreg_ob{id=RegId, cseq=CSeq} ->
            #sipapp_srv{app_id=_AppId} = SipAppState,
            % ?info(AppId, <<>>, "register outbound flow ~p has failed", [RegId]),
            Meta = [{cseq_num, CSeq}],
            Msg = {'$nksip_uac_auto_register_answer_register', RegId, 503, Meta},
            gen_server:cast(self(), Msg),
            {ok, SipAppState};
        false ->
            continue
    end;

nkcb_handle_info({'$nksip_uac_auto_outbound_notify', RegId}, SipAppState) ->
    #state_ob{regs=RegsOb} = StateOb = get_state(SipAppState),
    #sipapp_srv{app_id=AppId} = SipAppState,
    case lists:keytake(RegId, #sipreg_ob.id, RegsOb) of
        {value, RegOb, RegsOb1} -> 
            ?debug(AppId, <<>>, "connection for register ~p successfully refreshed",
                   [RegId]),
            StateOb1 = StateOb#state_ob{regs=[RegOb#sipreg_ob{fails=0}|RegsOb1]},
            {ok, set_state(StateOb1, SipAppState)};
        false -> 
            continue
    end;

nkcb_handle_info(_Msg, _SipAppState) ->
    continue.


%% @private
-spec nkcb_uac_auto_register_launch_register(#sipreg{}, boolean(), nksip_sipapp_srv:state()) -> 
    {ok, #sipreg{}, nksip_sipapp_srv:state()} | continue.

nkcb_uac_auto_register_launch_register(Reg, Sync, SipAppState)->
    #sipreg{id=RegId, ruri=RUri, opts=Opts, cseq=CSeq} = Reg,   
    #state_ob{outbound=Ob, pos=NextPos, regs=RegsOb} = StateOb = get_state(SipAppState),
    #sipapp_srv{app_id=AppId} = SipAppState,
    User = nksip_lib:get_value(user, Opts, []),
    case Ob andalso lists:member('$nksip_uac_auto_outbound', User) of
        true ->
            RegOb1 = case lists:keyfind(RegId, #sipreg_ob.id, RegsOb) of
                false ->
                    Pos = case nksip_lib:get_value(reg_id, Opts) of
                        UserPos when is_integer(UserPos), UserPos>0 -> UserPos;
                        _ -> NextPos
                    end,
                    #sipreg_ob{id=RegId, cseq=CSeq, pos=Pos, fails=0};
                #sipreg_ob{pos=Pos} = RegOb0 ->
                    RegOb0#sipreg_ob{cseq=CSeq}
            end,
            RegsOb1 = lists:keystore(RegId, #sipreg_ob.id, RegsOb, RegOb1),
            Meta = [cseq_num, retry_after, remote, require, <<"flow-timer">>],
            Opts1 = [contact, {cseq_num, CSeq}, {meta, Meta}, {reg_id, Pos} | Opts],
            Self = self(),
            Fun = fun() ->
                case nksip_uac:register(AppId, RUri, Opts1) of
                    {ok, Code, Meta1} -> ok;
                    _ -> Code=500, Meta1=[{cseq_num, CSeq}]
                end,
                Msg1 = {'$nksip_uac_auto_register_answer_register', RegId, Code, Meta1},
                gen_server:cast(Self, Msg1)
            end,
            case Sync of
                true -> Fun();
                false -> spawn_link(Fun)
            end,
            StateOb1 = StateOb#state_ob{pos=max(NextPos, Pos+1), regs=RegsOb1},
            ?debug(AppId, <<>>, "Started auto registration outbound: ~p", [RegId]),
            Reg1 = Reg#sipreg{next=undefined},
            {ok, Reg1, set_state(StateOb1, SipAppState)};
        false ->
            continue
    end.


%% @private
-spec nkcb_uac_auto_register_launch_unregister(#sipreg{}, boolean(), nksip_sipapp_srv:state()) -> 
    {ok, nksip_sipapp_srv:state()} | continue | {continue, list()}.

nkcb_uac_auto_register_launch_unregister(Reg, Sync, SipAppState)->
    #sipreg{id=RegId, ruri=RUri, opts=Opts, cseq=CSeq} = Reg,
    StateOb = get_state(SipAppState),
    % This plugin could have already stopped
    case 
        is_record(StateOb, state_ob) andalso 
        lists:keytake(RegId, #sipreg_ob.id, StateOb#state_ob.regs) 
    of
        {value, #sipreg_ob{pos=Pos, conn_monitor=Monitor, conn_pid=Pid}, RegsOb1} -> 
            case is_reference(Monitor) of
                true -> erlang:demonitor(Monitor);
                false -> ok
            end, 
            case is_pid(Pid) of
                true -> nksip_connection:stop_refresh(Pid);
                false -> ok
            end,
            Opts1 = [contact, {cseq_num, CSeq}, {reg_id, Pos} |
                     nksip_lib:store_value(expires, 0, Opts)],
            #sipapp_srv{app_id=AppId} = SipAppState,
            Fun = fun() -> nksip_uac:register(AppId, RUri, Opts1) end,
            case Sync of
                true -> Fun();
                false -> spawn_link(Fun)
            end,
            SipAppState1 = set_state(StateOb#state_ob{regs=RegsOb1}, SipAppState),
            {ok, SipAppState1};
        false ->
            continue 
    end.


  
%% @private
-spec nkcb_uac_auto_register_update_register(#sipreg{}, nksip:sip_code(), 
                                    nksip:optslist(), nksip_sipapp_srv:state()) ->
    {ok, #sipreg{}, nksip_sipapp_srv:state()}.

nkcb_uac_auto_register_update_register(Reg, Code, _Meta, SipAppState) when Code<200 ->
    {ok, Reg, SipAppState};

nkcb_uac_auto_register_update_register(Reg, Code, Meta, SipAppState) when Code<300 ->
    #sipreg{id=RegId, call_id=CallId, opts=Opts} = Reg,
    #sipapp_srv{app_id=AppId} = SipAppState,
    #state_ob{regs=RegsOb} = StateOb = get_state(SipAppState),
    case lists:keytake(RegId, #sipreg_ob.id, RegsOb) of
        {value, #sipreg_ob{conn_monitor=Monitor}=RegOb1, RegsOb1} ->
            case is_reference(Monitor) of
                true -> erlang:demonitor(Monitor);
                false -> ok
            end,
            {Proto, Ip, Port, Res} = nksip_lib:get_value(remote, Meta),
            Require = nksip_lib:get_value(require, Meta),
            % 'fails' is not updated until the connection confirmation arrives
            % (or process down)
            RegOb2 = case lists:member(<<"outbound">>, Require) of
                true ->
                    case nksip_transport:get_connected(AppId, Proto, Ip, Port, Res) of
                        [{_, Pid}|_] -> 
                            case start_refresh(AppId, Meta, Proto, Pid, Reg) of
                                ok -> 
                                    Mon = erlang:monitor(process, Pid),
                                    RegOb1#sipreg_ob{conn_monitor=Mon, conn_pid=Pid};
                                error -> 
                                    RegOb1
                            end;
                        [] -> 
                            ?notice(AppId, CallId, 
                                    "could not start outbound keep-alive", []),
                            RegOb1
                    end;
                false ->
                    RegOb1
            end,
            StateOb2 = StateOb#state_ob{regs=[RegOb2|RegsOb1]},
            Time = nksip_lib:get_value(expires, Opts),
            Reg1 = Reg#sipreg{interval=Time},
            {continue, [Reg1, Code, Meta, set_state(StateOb2, SipAppState)]};
        false ->
            continue
    end;

nkcb_uac_auto_register_update_register(Reg, Code, Meta, SipAppState) ->
    #sipreg{id=RegId, call_id=CallId} = Reg,
    #state_ob{regs=RegsOb} = StateOb = get_state(SipAppState),
    case lists:keytake(RegId, #sipreg_ob.id, RegsOb) of
        {value, RegOb1, RegsOb1} ->
            #sipreg_ob{conn_monitor=Monitor, conn_pid=Pid, fails=Fails} = RegOb1,
            case is_reference(Monitor) of
                true -> erlang:demonitor(Monitor);
                false -> ok
            end,
            case is_pid(Pid) of
                true -> nksip_connection:stop_refresh(Pid);
                false -> ok
            end,
            #sipapp_srv{app_id=AppId} = SipAppState,
            Config = nksip_sipapp_srv:config(AppId),
            MaxTime = nksip_lib:get_value(nksip_uac_auto_outbound_max_time, Config),
            BaseTime = case [true || #sipreg_ob{fails=0} <- RegsOb] of
                [] -> nksip_lib:get_value(nksip_uac_auto_outbound_all_fail, Config);
                _ -> nksip_lib:get_value(nksip_uac_auto_outbound_any_ok, Config)
            end,
            Upper = min(MaxTime, BaseTime*math:pow(2, Fails+1)),
            Time = round(crypto:rand_uniform(50, 101) * Upper / 100),
            ?notice(AppId, CallId, "Outbound registration failed "
                         "(basetime: ~p, fails: ~p, upper: ~p, time: ~p)",
                         [BaseTime, Fails+1, Upper, Time]),
            RegOb2 = RegOb1#sipreg_ob{
                conn_monitor = undefined,
                conn_pid = undefined,
                fails = Fails+1
            },
            StateOb1 = StateOb#state_ob{regs=[RegOb2|RegsOb1]},
            SipAppState1 = set_state(StateOb1, SipAppState),
            Reg1 = Reg#sipreg{interval=Time},
            {continue, [Reg1, Code, Meta, SipAppState1]};
        false ->
            continue
    end.

    
%% ===================================================================
%% Private
%% ===================================================================


%% @private
get_state(SipAppState) ->
    nksip_sipapp_srv:get_meta(nksip_uac_auto_outbound, SipAppState).


%% @private
set_state(State, SipAppState) ->
    nksip_sipapp_srv:set_meta(nksip_uac_auto_outbound, State, SipAppState).


%% @private
start_refresh(AppId, Meta, Proto, Pid, Reg) ->
    #sipreg{id=RegId, call_id=CallId} = Reg,
    FlowTimer = case nksip_lib:get_value(<<"flow-timer">>, Meta) of
        [FlowTimer0] -> nksip_lib:to_integer(FlowTimer0);
        _ -> undefined
    end,
    Secs = case FlowTimer of
        FT when is_integer(FT), FT > 5 -> 
            FT;
        _ when Proto==udp -> 
            nksip_sipapp_srv:config(AppId,
                                    nksip_uac_auto_outbound_default_udp_ttl);
        _ -> 
            nksip_sipapp_srv:config(AppId,
                                    nksip_uac_auto_outbound_default_tcp_ttl)
    end,
    Ref = {'$nksip_uac_auto_outbound_notify', RegId},
    Rand = crypto:rand_uniform(80, 101),
    Time = (Rand*Secs) div 100,
    case nksip_connection:start_refresh(Pid, Time, Ref) of
        ok -> 
            ?info(AppId, CallId, "successfully set outbound keep-alive: ~p secs", 
                  [Secs]),
            ok;
        error -> 
            ?notice(AppId, CallId, "could not start outbound keep-alive", []),
            error
    end.



