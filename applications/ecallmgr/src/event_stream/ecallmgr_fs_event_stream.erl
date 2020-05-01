%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_event_stream).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type profile_name() :: atom() | kz_term:ne_binary().
-type binding() :: atom() | kz_term:ne_binary().
-type bindings() :: [binding()].
-type profile() :: {profile_name(), bindings()}.
-type event_packet_type() :: 1 | 2 | 4.

-export_type([profile/0
             ,event_packet_type/0
             ]).

-record(state, {node :: atom()
               ,bindings :: bindings() | 'undefined'
               ,profile_name :: profile_name()
               ,ip :: inet:ip_address() | 'undefined'
               ,port :: inet:port_number() | 'undefined'
               ,socket :: inet:socket() | 'undefined'
               ,idle_alert = 'infinity' :: timeout()
               ,packet :: event_packet_type()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), profile(), event_packet_type()) -> kz_types:startlink_ret().
start_link(Node, {_Name, _Bindings}=Profile, Packet) ->
    gen_server:start_link(?SERVER, [Node, Profile, Packet], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | profile() | event_packet_type()]) -> {'ok', state()} | {'stop', any()}.
init([Node, {ProfileName, Bindings}, Packet]) ->
    init(Node, ProfileName, Bindings, Packet).

-spec init(atom(), profile_name(), bindings(), event_packet_type()) -> {'ok', state()} | {'stop', any()}.
init(Node, ProfileName, Bindings, Packet) ->
    process_flag('trap_exit', 'true'),
    kz_log:put_callid(list_to_binary([kz_term:to_binary(Node), <<"-eventstream-">>, kz_term:to_binary(ProfileName)])),
    request_event_stream(#state{node=Node
                               ,profile_name=ProfileName
                               ,bindings=Bindings
                               ,idle_alert=idle_alert_timeout()
                               ,packet=Packet
                               }).

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('connect', #state{ip=IP, port=Port, packet=Packet, idle_alert=Timeout}=State) ->
    case gen_tcp:connect(IP, Port, [{'mode', 'binary'}
                                   ,{'packet', Packet}
                                   ])
    of
        {'ok', Socket} ->
            _ = kz_amqp_channel:requisition(),
            lager:debug("opened event stream socket to ~p:~p for ~p"
                       ,[IP, Port, get_event_bindings(State)]
                       ),
            {'noreply', State#state{socket=Socket}, Timeout};
        {'error', Reason} ->
            {'stop', Reason, State}
    end;
handle_cast(_Msg, #state{socket='undefined'}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State};
handle_cast(_Msg, #state{idle_alert=Timeout}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, Timeout}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'tcp', Socket, Data}, #state{socket=Socket
                                         ,node=Node
                                         ,idle_alert=Timeout
                                         }=State) ->
    case handle_data(Node, Data) of
        {'error', {'not_handled', _EvtData}} ->
            lager:warning("data from event stream socket not processed => ~p", [_EvtData]),
            {'noreply', State, Timeout};
        {'error', 'decode_error'} ->
            lager:warning("failed to decode packet from ~s (~p b) for ~p: ~p"
                         ,[Node, byte_size(Data), get_event_bindings(State), Data]
                         ),
            {'stop', 'decode_error', State};
        {'error', _Error} ->
            lager:error("failed to decode packet from ~s (~p b) for ~p: ~p => ~p"
                       ,[Node, byte_size(Data), get_event_bindings(State), _Error, Data]
                       ),
            {'stop', 'decode_error', State};
        _Pid when is_pid(_Pid) ->
            {'noreply', State, Timeout}
    end;
handle_info({'tcp_closed', Socket}, #state{socket=Socket, node=Node}=State) ->
    lager:info("event stream for ~p on node ~p closed"
              ,[get_event_bindings(State), Node]
              ),
    timer:sleep(3 * ?MILLISECONDS_IN_SECOND),
    {'stop', 'normal', State#state{socket='undefined'}};
handle_info({'tcp_error', Socket, _Reason}, #state{socket=Socket}=State) ->
    lager:warning("event stream tcp error: ~p", [_Reason]),
    gen_tcp:close(Socket),
    timer:sleep(3 * ?MILLISECONDS_IN_SECOND),
    {'stop', 'tcp_error', State#state{socket='undefined'}};
handle_info('timeout', #state{node=Node, idle_alert=Timeout}=State) ->
    lager:warning("event stream for ~p on node ~p is unexpectedly idle",
                  [get_event_bindings(State), Node]
                 ),
    {'noreply', State, Timeout};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {'stop', {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {'stop', Reason, State};
handle_info(_Msg, #state{socket='undefined'}=State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State};

handle_info({'kz_amqp_assignment', {'new_channel', _, Channel}}, State) ->
    lager:debug("channel acquired ~p", [Channel]),
    _ = kz_amqp_channel:consumer_channel(Channel),
    {'noreply', State};

handle_info({'kz_amqp_assignment', 'lost_channel'}, State) ->
    lager:debug("channel lost"),
    _ = kz_amqp_channel:remove_consumer_channel(),
    {'noreply', State};

handle_info(_Msg, #state{idle_alert=Timeout}=State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State, Timeout}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{socket='undefined', node=Node}=State) ->
    catch(kz_amqp_channel:release()),
    lager:debug("event stream for ~p on node ~p terminating: ~p"
               ,[get_event_bindings(State), Node, _Reason]
               );
terminate(_Reason, #state{socket=Socket, node=Node}=State) ->
    catch(gen_tcp:close(Socket)),
    catch(kz_amqp_channel:release()),
    lager:debug("event stream for ~p on node ~p terminating: ~p"
               ,[get_event_bindings(State), Node, _Reason]
               ).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec request_event_stream(state()) -> {'ok', state()} | {'stop', any()}.
request_event_stream(#state{node=Node}=State) ->
    Bindings = get_event_bindings(State),
    case maybe_bind(Node, Bindings) of
        {'ok', {IP, Port}} ->
            {'ok', IPAddress} = inet_parse:address(IP),
            gen_server:cast(self(), 'connect'),
            kz_log:put_callid(list_to_binary([kz_term:to_binary(Node)
                                             ,$-, kz_term:to_binary(IP)
                                             ,$:, kz_term:to_binary(Port)
                                             ])),
            {'ok', State#state{ip=IPAddress, port=kz_term:to_integer(Port)}};
        {'EXIT', ExitReason} ->
            {'stop', {'shutdown', ExitReason}};
        {'error', ErrorReason} ->
            lager:warning("unable to establish event stream to ~p for ~p: ~p", [Node, Bindings, ErrorReason]),
            {'stop', ErrorReason}
    end.

-spec get_event_bindings(state()) -> kz_term:atoms().
get_event_bindings(State) ->
    get_event_bindings(State, []).

-spec get_event_bindings(state(), kz_term:atoms()) -> kz_term:atoms().
get_event_bindings(#state{bindings='undefined'
                         ,idle_alert='infinity'
                         }
                  ,Acc
                  ) ->
    Acc;
get_event_bindings(#state{bindings='undefined'}, Acc) ->
    ['HEARTBEAT' | Acc];
get_event_bindings(#state{bindings=Bindings}=State, Acc) when is_list(Bindings) ->
    get_event_bindings(State#state{bindings='undefined'}
                      ,[kz_term:to_atom(Binding, 'true') || Binding <- Bindings] ++ Acc
                      ).

-spec maybe_bind(atom(), kz_term:atoms()) ->
          {'ok', {kz_term:text(), inet:port_number()}} |
          {'error', any()} |
          {'EXIT', any()}.
maybe_bind(Node, Bindings) ->
    maybe_bind(Node, Bindings, 0).

-spec maybe_bind(atom(), kz_term:atoms(), non_neg_integer()) ->
          {'ok', {kz_term:text(), inet:port_number()}} |
          {'error', any()} |
          {'EXIT', any()}.
maybe_bind(Node, Bindings, 2) ->
    case catch gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2 * ?MILLISECONDS_IN_SECOND) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'EXIT', {'timeout',_}} -> {'error', 'timeout'};
        {'EXIT', {{'nodedown',_}, _}} -> {'error', 'nodedown'};
        {'EXIT', _} = Exit -> Exit;
        {'error', _Reason}=E -> E
    end;
maybe_bind(Node, Bindings, Attempts) ->
    case catch gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2 * ?MILLISECONDS_IN_SECOND) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'EXIT', {'timeout',_}} ->
            lager:debug("timeout on attempt ~b to bind: ~p", [Attempts, Bindings]),
            maybe_bind(Node, Bindings, Attempts+1);
        {'EXIT', {{'nodedown',_}, _}} -> {'error', 'nodedown'};
        {'error', _Reason} ->
            lager:debug("failed on attempt ~b to bind: ~p", [Attempts, _Reason]),
            maybe_bind(Node, Bindings, Attempts+1)
    end.

-spec idle_alert_timeout() -> timeout().
idle_alert_timeout() ->
    case kapps_config:get_integer(?APP_NAME, <<"event_stream_idle_alert">>, 0) of
        Timeout when Timeout =< 30 -> 'infinity';
        Else -> Else * ?MILLISECONDS_IN_SECOND
    end.

-spec handle_data(atom(), binary()) -> any().
handle_data(Node, Data) ->
    try binary_to_term(Data) of
        {'event', 'json', JObj} ->
            Channel = kz_amqp_channel:consumer_channel(),
            kz_process:spawn(fun handle_event/3, [Node, JObj, Channel]);
        Else -> {'error', {'not_handled', Else}}
    catch
        'error':'badarg':_ -> {'error', 'decode_error'};
        _:_E:_ -> {'error', _E}
    end.

-spec handle_event(atom(), kz_json:object(), pid()) -> any().
handle_event(Node, JObj, Channel) ->
    kz_amqp_channel:consumer_channel(Channel),
    kz_log:put_callid(JObj),
    log_json_event(Node, kz_api:event_name(JObj), JObj),
    UUID = kz_api:call_id(JObj),
    Category = kz_api:event_category(JObj),
    Event = kz_api:event_name(JObj),
    Ctx = #{node => Node
           ,call_id => UUID
           ,category => Category
           ,event => Event
           ,payload => JObj
           },
    run_bindings(Ctx).

-spec log_json_event(atom(), kz_term:api_ne_binary(), kz_json:object()) -> any().
log_json_event(Node, 'undefined', JObj) ->
    lager:debug_unsafe("received unknown fs event from ~s : ~s", [Node, kz_json:encode(JObj, ['pretty'])]);
log_json_event(Node, Event, JObj) ->
    lager:debug_unsafe("received fs event from ~s => ~s : ~s", [Node, kz_api:event_category(JObj), Event]).

run_bindings(Ctx) ->
    Stages = [fun run_event/1
             ,fun run_process/1
             ,fun run_notify/1
             ,fun run_publish/1
             ],
    Fun = fun(StageFun) -> StageFun(Ctx) end,
    lists:foreach(Fun, Stages).

run_event(Ctx) ->
    Routing = create_routing(<<"event">>, Ctx),
    kazoo_bindings:map(Routing, Ctx).

run_process(Ctx) ->
    Routing = create_routing(<<"process">>, Ctx),
    kazoo_bindings:map(Routing, Ctx).

run_notify(Ctx) ->
    Routing = create_routing(<<"registered">>, Ctx),
    kazoo_bindings:map(Routing, Ctx).

run_publish(Ctx) ->
    Routing = create_routing(<<"publish">>, Ctx),
    kazoo_bindings:map(Routing, Ctx, run_publish_options()).

create_routing(Name, #{category := undefined} = Ctx) ->
    create_routing(Name, Ctx#{category => <<"invalid">>});
create_routing(Name, #{category := Category, event := Event}) ->
    <<"event_stream.", Name/binary, ".", Category/binary, ".", Event/binary>>.

-spec run_publish_options() -> kazoo_bindings:rt_options().
run_publish_options() ->
    [{'candidates', fun run_publish_candidates/1}
    ].

-spec run_publish_candidates(kz_term:ne_binary()) -> kazoo_bindings:kz_bindings().
run_publish_candidates(Routing) ->
    case lists:sort(kazoo_bindings:bindings(Routing)) of
        [] -> [];
        Bindings -> Bindings
    end.
