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

%% @doc Automatic registrations (with Outbound support) and pings support for SipApps.
%% This module allows a SipApp to program automatic periodic sending of
%% <i>OPTION</i> or <i>REGISTER</i> requests and related functions.

-module(nksip_sipapp_auto).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start_ping/5, stop_ping/2, get_pings/1]).
-export([start_register/5, stop_register/2, get_registers/1]).
-export([init/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([timer/1]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Programs the SipApp to start a series of <i>registrations</i>
%% to the registrar at `Uri', at `Time' (in seconds) intervals.
%% `RegId' indentifies this request to stop it later.
%% Use {@link get_regs/1} to know about registration status, or the 
%% callback function {@link nksip_sipapp:register_update/3}.
%%
%% If the SipApp is configured to support outbound (RFC5626),  
%% there is a 'reg_id' option in `Opts' with a numeric value,
%% and the remote party replies indicating it has outbound support also,
%% NkSIP will keep the 'flow' opened sending keep-alive packets. If the flow
%% goes down, NkSIP will try to re-send the registration at specific intervals.
-spec start_register(term()|nksip:app_id(), term(), nksip:user_uri(), pos_integer(),
                        nksip_lib:optslist()) -> 
    {ok, boolean()} | {error, invalid_uri|sipapp_not_found}.

start_register(App, RegId, Uri, Time, Opts) 
                when is_integer(Time), Time > 0, is_list(Opts) ->
    case nksip_parse:uris(Uri) of
        [ValidUri] -> 
            Msg = {'$nksip_start_register', RegId, ValidUri, Time, Opts},
            case catch nksip:call(App, Msg) of
                {ok, Reply} -> {ok, Reply};
                {'EXIT', _} -> {error, sipapp_not_found}
            end;
        _ -> 
            {error, invalid_uri}
    end.


%% @doc Stops a previously started registration series.
%% For outbound-supported requests, it will also stop the keep-alive messages
%% on the flow.
-spec stop_register(term()|nksip:app_id(), term()) -> 
    ok | not_found.

stop_register(App, RegId) ->
    nksip:call(App, {'$nksip_stop_register', RegId}).
    

%% @doc Get current registration status, including if last registration was successful 
%% and time remaining to next one.
-spec get_registers(term()|nksip:app_id()) -> 
    [{RegId::term(), OK::boolean(), Time::non_neg_integer()}].
 
get_registers(App) ->
    nksip:call(App, '$nksip_get_registers').



%% @doc Programs the SipApp to start a series of <i>pings</i> (OPTION requests) 
%% to the SIP element at `Uri', at `Time' (in seconds) intervals.
%% `PingId' indentifies this request to stop it later.
%% Use {@link get_pings/1} to know about ping status, or the callback function
%% {@link nksip_sipapp:register_update/3}.
-spec start_ping(term()|nksip:app_id(), term(), nksip:user_uri(), pos_integer(),
                    nksip_lib:optslist()) -> 
    {ok, boolean()} | {error, invalid_uri|sipapp_not_found}.

start_ping(App, PingId, Uri, Time, Opts) 
            when is_integer(Time), Time > 0, is_list(Opts) ->
    case nksip_parse:uris(Uri) of
        [ValidUri] -> 
            Msg = {'$nksip_start_ping', PingId, ValidUri, Time, Opts},
            case catch nksip:call(App, Msg) of
                {ok, Reply} -> {ok, Reply};
                {'EXIT', _} -> {error, sipapp_not_found}
            end;
        _ -> 
            {error, invalid_uri}
    end.


%% @doc Stops a previously started ping request.
-spec stop_ping(term()|nksip:app_id(), term()) -> 
    ok | not_found.

stop_ping(App, PingId) ->
    nksip:call(App, {'$nksip_stop_ping', PingId}).
    

%% @doc Get current ping status, including if last ping was successful and time 
%% remaining to next one.
-spec get_pings(term()|nksip:app_id()) -> 
    [{PingId::term(), OK::boolean(), Time::non_neg_integer()}].
 
get_pings(App) ->
    nksip:call(App, '$nksip_get_pings').


%% ===================================================================
%% Private
%% ===================================================================

-record(sipreg, {
    id :: term(),
    pos :: integer(),
    ruri :: nksip:uri(),
    opts :: nksip_lib:optslist(),
    call_id :: nksip:call_id(),
    interval :: non_neg_integer(),
    from :: any(),
    cseq :: nksip:cseq(),
    next :: nksip_lib:timestamp(),
    ok :: boolean(),
    req_pid :: pid(),
    conn_monitor :: reference(),
    conn_pid :: pid(),
    fails :: non_neg_integer()
}).


-record(state, {
    app_id :: nksip:app_id(),
    outbound :: boolean(),
    ob_base_time :: pos_integer(),     % For outbound support
    pos :: integer(),
    pings :: [#sipreg{}],
    regs :: [#sipreg{}]
}).


%% @private 
init(AppId, _Args) ->
    Config = nksip_sipapp_srv:config(AppId),
    RegTime = nksip_lib:get_integer(register_expires, Config, 300),
    case nksip_lib:get_value(register, Config) of
        undefined ->
            ok;
        RegUris ->
            Regs = lists:zip(lists:seq(1, length(RegUris)), RegUris),
            lists:foreach(
                fun({Pos, Uri}) ->
                    Name = <<"auto-", (nksip_lib:to_binary(Pos))/binary>>,
                    spawn_link(
                        fun() -> 
                            start_register(AppId, Name, Uri, RegTime, Config) 
                        end)
                end,
                Regs)
    end,
    Supported = nksip_lib:get_value(supported, Config, ?SUPPORTED),
    #state{
        app_id = AppId, 
        outbound = lists:member(<<"outbound">>, Supported),
        ob_base_time = nksip_lib:get_value(outbound_time_any_ok, Config),
        pos = 1,
        pings = [], 
        regs = []
    }.


%% @private
handle_call({'$nksip_start_register', RegId, Uri, Time, Opts}, From, 
            #state{app_id=AppId, outbound=Outbound, pos=Pos, regs=Regs}=State) ->
    Opts1 = case lists:keymember(reg_id, 1, Opts) of
        false when Outbound -> [{reg_id, Pos}];
        _ -> Opts
    end,
    CallId = nksip_lib:luid(),
    Reg = #sipreg{
        id = RegId,
        pos = Pos,
        ruri = Uri,
        opts = Opts1,
        call_id = CallId,
        interval = Time,
        from = From,
        cseq = nksip_config:cseq(),
        next = 0,
        ok = undefined,
        fails = 0
    },
    Regs1 = lists:keystore(RegId, #sipreg.id, Regs, Reg),
    ?debug(AppId, CallId, "Started auto registration: ~p", [Reg]),
    timer(State#state{pos=Pos+1, regs=Regs1});

handle_call({'$nksip_stop_register', RegId}, From, State) ->
    #state{app_id=AppId, regs=Regs} = State,
    case lists:keytake(RegId, #sipreg.id, Regs) of
        {value, #sipreg{conn_monitor=Monitor}=Reg, Regs1} -> 
            gen_server:reply(From, ok),
            case is_reference(Monitor) of
                true -> erlang:demonitor(Monitor);
                false -> ok
            end, 
            spawn(fun() -> launch_unregister(AppId, Reg) end),
            State#state{regs=Regs1};
        false -> 
            gen_server:reply(From, not_found),
            State
    end;

handle_call('$nksip_get_registers', From, #state{regs=Regs}=State) ->
    % Info = Regs,
    Now = nksip_lib:timestamp(),
    Info = [
        {RegId, Ok, Next-Now}
        ||  #sipreg{id=RegId, ok=Ok, next=Next} <- Regs
    ],
    gen_server:reply(From, Info),
    State;

handle_call({'$nksip_start_ping', PingId, Uri, Time, Opts}, From, 
            #state{app_id=AppId, pings=Pings}=State) ->
    CallId = nksip_lib:luid(),
    Ping = #sipreg{
        id = PingId,
        ruri = Uri,
        opts = Opts,
        call_id = CallId,
        interval = Time,
        from = From,
        cseq = nksip_config:cseq(),
        next = 0,
        ok = undefined
    },
    ?debug(AppId, CallId, "Started auto ping: ~p", [Ping]),
    Pinsg1 = lists:keystore(PingId, #sipreg.id, Pings, Ping),
    timer(State#state{pings=Pinsg1});

handle_call({'$nksip_stop_ping', PingId}, From, #state{pings=Pings}=State) ->
    case lists:keytake(PingId, #sipreg.id, Pings) of
        {value, _, Pings1} -> 
            gen_server:reply(From, ok),
            State#state{pings=Pings1};
        false -> 
            gen_server:reply(From, not_found),
            State
    end;

handle_call('$nksip_get_pings', From, #state{pings=Pings}=State) ->
    Now = nksip_lib:timestamp(),
    Info = [
        {PingId, Ok, Next-Now}
        ||  #sipreg{id=PingId, ok=Ok, next=Next} <- Pings
    ],
    gen_server:reply(From, Info),
    State;

handle_call(_, _From, _State) ->
    error.


%% @private
handle_cast({'$nksip_register_answer', RegId, Code, Meta}, 
            #state{app_id=AppId, regs=Regs}=State) -> 
    case lists:keytake(RegId, #sipreg.id, Regs) of
        {value, #sipreg{ok=OldOK}=Reg, Regs1} ->
            #sipreg{ok=OK} = Reg1 = update_register(Reg, Code, Meta, State),
            case OK of
                OldOK ->
                    ok;
                _ -> 
                    Args = [RegId, OK],
                    nksip_sipapp_srv:sipapp_cast(AppId, register_update, Args, Args)
            end,
            update_basetime(State#state{regs=[Reg1|Regs1]});
        false ->
            State
    end;

handle_cast({'$nksip_ping_answer', PingId, Code, Meta}, 
            #state{app_id=AppId, pings=Pings}=State) -> 
    case lists:keytake(PingId, #sipreg.id, Pings) of
        {value, #sipreg{ok=OldOK}=Ping, Pings1} ->
            #sipreg{ok=OK} = Ping1 = update_ping(Ping, Code, Meta, State),
            case OK of
                OldOK -> 
                    ok;
                _ -> 
                    Args = [PingId, OK],
                    nksip_sipapp_srv:sipapp_cast(AppId, ping_update, Args, Args)
            end,
            State#state{pings=[Ping1|Pings1]};
        false ->
            State
    end;

handle_cast('$nksip_force_regs', #state{regs=Regs}=State) ->
    Regs1 = lists:map(
        fun(#sipreg{next=Next}=SipReg) ->
            case is_integer(Next) of
                true -> SipReg#sipreg{next=0};
                false -> SipReg
            end
        end,
        Regs),
    State#state{regs=Regs1};

handle_cast(_, _) ->
    error.


%% @private
handle_info({'DOWN', Mon, process, _Pid, _}, #state{app_id=AppId, regs=Regs}=State) ->
    case lists:keyfind(Mon, #sipreg.conn_monitor, Regs) of
        #sipreg{id=RegId, cseq=CSeq, call_id=CallId} ->
            ?info(AppId, CallId, "register outbound flow ~p has failed", [RegId]),
            Meta = [{cseq_num, CSeq}],
            gen_server:cast(self(), {'$nksip_register_answer', RegId, 503, Meta});
        false ->
            ok
    end,
    State;

handle_info({'$nksip_register_notify', RegId}, #state{regs=Regs}=State) ->
    case lists:keytake(RegId, #sipreg.id, Regs) of
        {value, Reg, Regs1} -> 
            State1 = State#state{regs=[Reg#sipreg{fails=0}|Regs1]},
            update_basetime(State1);
        false -> 
            State
    end;

handle_info(_, _) ->
    error.


%% @private
terminate(_Reason, #state{app_id=AppId, regs=Regs}) ->  
    lists:foreach(
        fun(#sipreg{ok=Ok}=Reg) -> 
            case Ok of
                true -> launch_unregister(AppId, Reg);
                _ -> ok
            end
        end,
        Regs).



%% ===================================================================
%% Private
%% ===================================================================


%% @private
timer(#state{app_id=AppId, pings=Pings, regs=Regs}=State) ->
    Now = nksip_lib:timestamp(),
    Pings1 = lists:map(
        fun(#sipreg{next=Next}=Ping) ->
            case is_integer(Next) andalso Now>=Next of 
                true -> launch_ping(AppId, Ping);
                false -> Ping
            end
        end,
        Pings),
    Regs1 = timer_register(AppId, Now, Regs, []),
    State#state{pings=Pings1, regs=Regs1}.


%% @private Only one register in each cycle
timer_register(AppId, Now, [#sipreg{next=Next}=Reg|Rest], Acc) ->
    case Now>=Next of
        true -> 
            Reg1 = launch_register(AppId, Reg),
            timer_register(AppId, -1, Rest, [Reg1|Acc]);
        false ->
            timer_register(AppId, Now, Rest, [Reg|Acc])
    end;

timer_register(_, _, [], Acc) ->
    Acc.


%%%%%% Register

%% @private
-spec launch_register(nksip:app_id(), #sipreg{}) -> 
    #sipreg{}.

launch_register(AppId, Reg)->
    #sipreg{
        id = RegId, 
        ruri = RUri,
        opts = Opts, 
        interval = Interval, 
        cseq = CSeq,
        call_id = CallId
    } = Reg,
    Opts1 = [
        contact, {call_id, CallId}, {cseq_num, CSeq}, {expires, Interval}, 
        {meta, [cseq_num, remote, parsed_require, <<"retry-after">>, <<"flow-timer">>]} 
        | Opts
    ],   
    Self = self(),
    Fun = fun() ->
        case nksip_uac:register(AppId, RUri, Opts1) of
            {ok, Code, Meta} -> ok;
            _ -> Code=500, Meta=[{cseq_num, CSeq}]
        end,
        gen_server:cast(Self, {'$nksip_register_answer', RegId, Code, Meta})
    end,
    Pid = spawn_link(Fun),
    Reg#sipreg{next=undefined, req_pid=Pid}.
    

%% @private
-spec launch_unregister(nksip:app_id(), #sipreg{}) -> 
    ok.

launch_unregister(AppId, Reg)->
    #sipreg{
        ruri = RUri,
        opts = Opts, 
        cseq = CSeq,
        call_id = CallId,
        conn_pid = Pid
    } = Reg,
    Opts1 = [
        contact, {call_id, CallId}, {cseq_num, CSeq}, {expires, 0}
        | Opts
    ],
    nksip_uac:register(AppId, RUri, Opts1),
    case is_pid(Pid) of
        true -> nksip_connection:stop_refresh(Pid);
        false -> ok
    end.

   
%% @private
-spec update_register(#sipreg{}, nksip:response_code(), nksip_lib:optslist(), #state{}) ->
    #sipreg{}.

update_register(Reg, Code, Meta, #state{app_id=AppId}) when Code>=200, Code<300 ->
    #sipreg{id=RegId, conn_monitor=Monitor, interval=Interval, 
            from=From, call_id=CallId} = Reg,
    case From of
        undefined -> ok;
        _ -> gen_server:reply(From, {ok, true})
    end,
    case is_reference(Monitor) of
        true -> erlang:demonitor(Monitor);
        false -> ok
    end,
    {Proto, Ip, Port, Res} = nksip_lib:get_value(remote, Meta),
    Require = nksip_lib:get_value(parsed_require, Meta),
    % 'fails' is not updated until the connection confirmation arrives
    % (or process down)
    Reg1 = Reg#sipreg{
        ok = true,
        cseq = nksip_lib:get_value(cseq_num, Meta) + 1,
        from = undefined,
        next = nksip_lib:timestamp() + Interval
    },
    case lists:member(<<"outbound">>, Require) of
        true ->
            case nksip_transport:get_connected(AppId, Proto, Ip, Port, Res) of
                [{_, Pid}|_] -> 
                    Secs = case nksip_lib:get_integer(<<"flow-timer">>, Meta) of
                        FT when is_integer(FT), FT > 5 -> FT;
                        _ when Proto==udp -> ?DEFAULT_UDP_KEEPALIVE;
                        _ -> ?DEFAULT_TCP_KEEPALIVE
                    end,
                    Ref = {'$nksip_register_notify', RegId},
                    case nksip_connection:start_refresh(Pid, Secs, Ref) of
                        ok -> 
                            Mon = erlang:monitor(process, Pid),
                            Reg1#sipreg{conn_monitor=Mon, conn_pid=Pid};
                        error -> 
                            ?notice(AppId, CallId, 
                                    "could not start outbound keep-alive", []),
                            Reg1
                    end;
                [] -> 
                    Reg1
            end;
        false ->
            Reg1
    end;

update_register(Reg, Code, Meta, State) ->
    #sipreg{conn_monitor=Monitor, fails=Fails, from=From, call_id=CallId} = Reg,
    #state{app_id=AppId, ob_base_time=BaseTime} = State,
    case From of
        undefined -> ok;
        _ -> gen_server:reply(From, {ok, false})
    end,
    case is_reference(Monitor) of
        true -> erlang:demonitor(Monitor);
        false -> ok
    end,
    Config = nksip_sipapp_srv:config(AppId),
    MaxTime = nksip_lib:get_value(outbound_max_time, Config),
    Upper = min(MaxTime, BaseTime*math:pow(2, Fails+1)),
    Elap = round(crypto:rand_uniform(50, 101) * Upper / 100),
    Add = case Code==503 andalso nksip_lib:get_value(<<"retry-after">>, Meta) of
        [Retry1] ->
            case nksip_lib:to_integer(Retry1) of
                Retry2 when Retry2 > 0 -> Retry2;
                _ -> 0
            end;
        _ -> 
            0
    end,
    ?notice(AppId, CallId, "Outbound registration failed "
                 "Basetime: ~p, fails: ~p, upper: ~p, time: ~p",
                 [BaseTime, Fails+1, Upper, Elap]),
    Reg#sipreg{
        ok = false,
        cseq = nksip_lib:get_value(cseq_num, Meta) + 1,
        from = undefined,
        conn_monitor = undefined,
        conn_pid = undefined,
        next = nksip_lib:timestamp() + Elap + Add,
        fails = Fails+1
    }.

%% @private
update_basetime(#state{app_id=AppId, regs=Regs}=State) ->
    Key = case [true || #sipreg{fails=0} <- Regs] of
        [] -> 
            ?notice(AppId, <<>>, "all outbound flows have failed", []),
            outbound_time_all_fail;
        _ -> 
            outbound_time_any_ok
    end,
    Config = nksip_sipapp_srv:config(AppId),
    State#state{ob_base_time=nksip_lib:get_value(Key, Config)}.



%%%%%% Ping

%% @private
-spec launch_ping(nksip:app_id(), #sipreg{}) -> 
    #sipreg{}.

launch_ping(AppId, Ping)->
    #sipreg{
        id = PingId,
        ruri = RUri, 
        opts = Opts, 
        cseq = CSeq,
        call_id = CallId
    } = Ping,
    Opts1 = [{call_id, CallId}, {cseq_num, CSeq}, {meta, [cseq_num]} | Opts],
    Self = self(),
    Fun = fun() ->
        case nksip_uac:options(AppId, RUri, Opts1) of
            {ok, Code, Meta} -> ok;
            _ -> Code=500, Meta=[{cseq_num, CSeq}]
        end,
        gen_server:cast(Self, {'$nksip_ping_answer', PingId, Code, Meta})
    end,
    Pid = spawn_link(Fun),
    Ping#sipreg{next=undefined, req_pid=Pid}.


   
%% @private
-spec update_ping(#sipreg{}, nksip:response_code(), nksip_lib:optslist(), #state{}) ->
    #sipreg{}.

update_ping(Ping, Code, Meta, _State) ->
    Ok = Code>=200 andalso Code<300,
    #sipreg{from=From, interval=Interval} = Ping,
    case From of
        undefined -> ok;
        _ -> gen_server:reply(From, {ok, Ok})
    end,
    Add = case Code==503 andalso nksip_lib:get_value(<<"retry-after">>, Meta) of
        [Retry1] ->
            case nksip_lib:to_integer(Retry1) of
                Retry2 when Retry2 > 0 -> Retry2;
                _ -> 0
            end;
        _ -> 
            0
    end,
    Ping#sipreg{
        ok = Ok,
        cseq = nksip_lib:get_value(cseq_num, Meta) + 1,
        from = undefined,
        next = nksip_lib:timestamp() + Interval + Add
    }.






