%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Manage a FreeSWITCH node and its resources
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_node).

-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([show_channels/1]).
-export([fs_node/1]).
-export([uuid_exists/2]).
-export([uuid_dump/2]).
-export([hostname/1]).
-export([reloadacl/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-record(state, {node = 'undefined' :: atom()
                ,stats = #node_stats{} :: #node_stats{}
                ,options = [] :: proplist()
               }).

-define(SERVER, ?MODULE).

-define(YR_TO_MICRO(Y), wh_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), wh_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), wh_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), wh_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), wh_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), wh_util:to_integer(Mil)*1000).

-define(FS_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/1 :: (atom()) -> {'ok', pid()} | {'error', term()}.
start_link(Node) ->
    gen_server:start_link(?SERVER, [Node, []], []).

-spec start_link/2 :: (atom(), proplist()) -> {'ok', pid()} | {'error', term()}.
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

-spec show_channels/1 :: (pid()) -> [proplist(),...] | [].
show_channels(Srv) ->
    Node = fs_node(Srv),
    case freeswitch:api(Node, show, "channels") of
        {ok, Rows} -> convert_rows(Node, Rows);
        _ -> []
    end.

-spec hostname/1 :: (pid()) -> fs_api_ret().
hostname(Srv) ->
    case catch(gen_server:call(Srv, hostname, ?FS_TIMEOUT)) of
        {'EXIT', _} -> timeout;
        Else -> Else
    end.

-spec reloadacl/1 ::(pid()) -> 'ok'.
reloadacl(Srv) ->
    Node = fs_node(Srv),
    lager:debug("reloadacl command sent to FS ~s", [Node]),
    {ok, <<"+OK acl reloaded\n">>} = freeswitch:api(Node, reloadacl),
    ok.

-spec fs_node/1 :: (pid()) -> atom().
fs_node(Srv) ->
    case catch(gen_server:call(Srv, fs_node, ?FS_TIMEOUT)) of
        {'EXIT', _} -> undefined;
        Else -> Else
    end.

-spec uuid_exists/2 :: (pid(), ne_binary()) -> boolean() | 'error'.
uuid_exists(Srv, UUID) ->
    case freeswitch:api(fs_node(Srv), uuid_exists, wh_util:to_list(UUID)) of
        {'ok', Result} ->
            lager:debug("result of uuid_exists(~s): ~s", [UUID, Result]),
            wh_util:is_true(Result);
        _ ->
            lager:debug("failed to get result from uuid_exists(~s)", [UUID]),
            error
    end.

-spec uuid_dump/2 :: (pid(), ne_binary()) -> {'ok', proplist()} | {'error', ne_binary()} | 'timeout'.
uuid_dump(Srv, UUID) ->
    case freeswitch:api(fs_node(Srv), uuid_dump, wh_util:to_list(UUID)) of
        {'ok', Result} ->
            Props = ecallmgr_util:eventstr_to_proplist(Result),
            {ok, Props};
        Error ->
            lager:debug("failed to get result from uuid_dump(~s)", [UUID]),
            Error
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    lager:debug("starting new fs node ~s", [Node]),

    process_flag(trap_exit, true),
    put(callid, wh_util:to_binary(Node)),

    erlang:monitor_node(Node, true),
    case freeswitch:register_event_handler(Node) of
        ok ->
            Stats = #node_stats{started = erlang:now()},
            lager:debug("event handler registered on node ~s", [Node]),            
            run_start_cmds(Node),
            NodeData = extract_node_data(Node),
            Active = get_active_channels(Node),
            ok = freeswitch:event(Node, ['CHANNEL_CREATE', 'CHANNEL_DESTROY', 'HEARTBEAT', 'CHANNEL_HANGUP_COMPLETE'
                                         ,'CUSTOM', 'sofia::register', 'sofia::transfer'
                                        ]),
            lager:debug("bound to switch events on node ~s", [Node]),
            {ok, #state{stats=(Stats#node_stats{
                                 created_channels = Active
                                 ,fs_uptime = props:get_value(uptime, NodeData, 0)
                                })
                        ,node=Node
                        ,options=Options}};
        {error, Reason} ->
            lager:warning("error when trying to register event handler on node ~s: ~p", [Node, Reason]),
            {stop, Reason};
        timeout ->
            lager:warning("timeout when trying to register event handler on node ~s", [Node]),
            {stop, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call/3 :: ('hostname', call_from(), #state{}) -> {'reply', {'ok', ne_binary()}, #state{}};
                       ('fs_node', call_from(), #state{}) -> {'reply', atom(), #state{}}.
handle_call(hostname, _From, #state{node=Node}=State) ->
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    {reply, {ok, Hostname}, State};
handle_call(fs_node, _From, #state{node=Node}=State) ->
    {reply, Node, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast/2 :: (term(), #state{}) -> {'noreply', #state{}}.
handle_cast(_Req, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Msg, #state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=S) when De > Cr ->
    %% If we start up while there are active channels, we'll have negative active_channels in our stats.
    %% The first clause fixes that situation
    handle_info(Msg, S#state{stats=Stats#node_stats{created_channels=De, destroyed_channels=De}});
handle_info({event, [undefined | Data]}, #state{stats=Stats}=State) ->
    case props:get_value(<<"Event-Name">>, Data) of
        <<"HEARTBEAT">> ->
            {noreply, State#state{stats=Stats#node_stats{last_heartbeat=erlang:now()}}, hibernate};
        <<"CUSTOM">> ->
            spawn(fun() -> 
                          process_custom_data(Data) 
                  end),
            {noreply, State, hibernate};
        _ ->
            {noreply, State, hibernate}
    end;
handle_info({event, [UUID | Data]}, #state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=State) ->
    case props:get_value(<<"Event-Name">>, Data) of
        <<"CHANNEL_CREATE">> ->
            spawn(fun() -> 
                          lager:debug("received channel create event: ~s", [UUID]),
                          ecallmgr_call_control:add_leg(Data) 
                  end),
            {noreply, State#state{stats=Stats#node_stats{created_channels=Cr+1}}, hibernate};
        <<"CHANNEL_DESTROY">> ->
            case props:get_value(<<"Channel-State">>, Data) of
                <<"CS_NEW">> -> % ignore
                    lager:debug("ignoring channel destroy because of CS_NEW: ~s", [UUID]),
                    {noreply, State, hibernate};
                <<"CS_DESTROY">> ->
                    spawn(fun() ->
                                  lager:debug("received channel destroyed: ~s", [UUID]),
                                  _ = ecallmgr_call_control:rm_leg(Data),
                                  ecallmgr_call_events:publish_channel_destroy(Data)
                          end),
                    {noreply, State#state{stats=Stats#node_stats{destroyed_channels=De+1}}, hibernate}
            end;
        <<"CHANNEL_HANGUP_COMPLETE">> ->
            spawn(fun() -> 
                          put(callid, UUID), 
                          ecallmgr_call_cdr:new_cdr(UUID, Data) 
                  end),
            {noreply, State};
        <<"CUSTOM">> ->
            spawn(fun() -> 
                          process_custom_data(Data) 
                  end),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_info({update_options, NewOptions}, State) ->
    {noreply, State#state{options=NewOptions}, hibernate};
handle_info({diagnostics, Pid}, #state{stats=Stats}=State) ->
    spawn(fun() -> diagnostics(Pid, Stats) end),
    {noreply, State};
handle_info({'EXIT', _Pid, noconnection}, State) ->
    lager:debug("noconnection received for node, pid: ~p", [_Pid]),
    {stop, normal, State};
handle_info({nodedown, Node}, #state{node=Node}=State) ->
    lager:debug("nodedown received from node ~s", [Node]),
    {stop, normal, State};
handle_info(nodedown, #state{node=Node}=State) ->
    lager:debug("nodedown received from node ~s", [Node]),
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs node ~s termination: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec diagnostics/2 :: (pid(), tuple()) -> proplist().
diagnostics(Pid, Stats) ->
    Resp = ecallmgr_diagnostics:get_diagnostics(Stats),
    Pid ! Resp.

-spec extract_node_data/1 :: (atom()) -> [{'cpu',string()} |
                                          {'sessions_max',integer()} |
                                          {'sessions_per_thirty',integer()} |
                                          {'sessions_since_startup',integer()} |
                                          {'uptime',number()}
                                          ,...].
extract_node_data(Node) ->
    Lines = case freeswitch:api(Node, status) of
                {ok, Status} ->
                    string:tokens(wh_util:to_list(Status), [$\n]);
                _Else ->
                    lager:info("failed to get initial status of node '~s': ~p", [Node, _Else]),
                    ["", "", "", "", ""]
            end,
    process_status(Lines, Node).

-spec process_status/2 :: ([nonempty_string(),...], atom()) -> [{'cpu',string()} |
                                                                {'sessions_max',integer()} |
                                                                {'sessions_per_thirty',integer()} |
                                                                {'sessions_since_startup',integer()} |
                                                                {'uptime',number()}
                                                                ,...].
process_status([Uptime, _, SessSince, Sess30, SessMax, CPU], Node) ->
    process_status([Uptime, SessSince, Sess30, SessMax, CPU], Node);
process_status(["UP " ++ Uptime, SessSince, Sess30, SessMax, CPU], Node) ->
    Parsers = [fun(P) ->
                       case re:run(Uptime, "([\\d]+)", [{capture, [1], list}, global]) of
                           {match, [[Y],[D],[Hour],[Min],[Sec],[Milli],[Micro]]} ->
                               UpMicro = ?YR_TO_MICRO(Y) + ?DAY_TO_MICRO(D) + ?HR_TO_MICRO(Hour) + ?MIN_TO_MICRO(Min)
                                   + ?SEC_TO_MICRO(Sec) + ?MILLI_TO_MICRO(Milli) + wh_util:to_integer(Micro),
                               [{uptime, UpMicro}|P];
                           _Else ->
                               lager:info("failed to determine uptime of node '~s', statistics may not be accurate", [Node]),
                               [{uptime, 0}|P]
                       end
               end
               ,fun(P) ->  
                        case re:run(SessSince, "([\\d]+)", [{capture, [1], list}]) of
                            {match, SessSinceNum} ->
                                [{sessions_since_startup, wh_util:to_integer(lists:flatten(SessSinceNum))} |P];
                            _Else ->
                                lager:info("failed to determine session since startup of node '~s', statistics may not be accurate", [Node]),
                                [{sessions_since_startup, 0}|P]
                        end
                end
               ,fun(P) ->
                        case re:run(Sess30, "([\\d]+)", [{capture, [1], list}]) of
                            {match, Sess30Num} ->
                                [{sessions_per_thirty, wh_util:to_integer(lists:flatten(Sess30Num))}|P];
                            _Else ->
                                lager:info("failed to determine session per thirty of node '~s', statistics may not be accurate", [Node]),
                                [{sessions_per_thirty, 0}|P]
                        end
                end
               ,fun(P) ->
                        case re:run(SessMax, "([\\d]+)", [{capture, [1], list}]) of
                            {match, SessMaxNum} ->
                                [{sessions_max, wh_util:to_integer(lists:flatten(SessMaxNum))}|P];
                            _Else ->
                                lager:info("failed to determine max sessions of node '~s', statistics may not be accurate", [Node]),
                                [{sessions_max, 5000}|P]
                        end
                end
               ,fun(P) ->
                        case re:run(CPU, "([\\d\.]+)", [{capture, [1], list}]) of
                            {match, CPUNum} ->
                                [{cpu, lists:flatten(CPUNum)}|P];
                            _Else ->
                                lager:info("failed to determine cpu info of node '~s', statistics may not be accurate", [Node]),
                                [{cpu, "0.00"}|P]
                        end
                end
              ],
    lists:foldr(fun(F, P) -> F(P) end, [], Parsers).

process_custom_data(Data) ->
    put(callid, props:get_value(<<"call-id">>, Data)),
    Subclass = props:get_value(<<"Event-Subclass">>, Data),
    case Subclass of
        <<"sofia::register">> ->
            lager:debug("received registration event"),
            publish_register_event(Data);
        <<"sofia::transfer">> ->
            lager:debug("received transfer event"),
            process_transfer_event(props:get_value(<<"Type">>, Data), Data);
        _ ->
            ok
    end.

publish_register_event(Data) ->
    ApiProp = lists:foldl(fun(K, Api) ->
                                  case props:get_value(wh_util:to_lower_binary(K), Data) of
                                      undefined ->
                                          case props:get_value(K, Data) of
                                              undefined -> Api;
                                              V -> [{K, V} | Api]
                                          end;
                                      V -> [{K, V} | Api]
                                  end
                          end
                          ,[{<<"Event-Timestamp">>, round(wh_util:current_tstamp())}
                            ,{<<"Call-ID">>, get(callid)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)]
                          ,wapi_registration:success_keys()),
    lager:debug("sending successful registration"),
    wapi_registration:publish_success(ApiProp).

-spec process_transfer_event/2 :: (ne_binary(), proplist()) -> ok.
process_transfer_event(<<"BLIND_TRANSFER">>, Data) ->
    lager:debug("recieved blind transfer notice"),
    TransfererCtrlUUId = case props:get_value(<<"Transferor-Direction">>, Data) of
                             <<"inbound">> ->
                                 props:get_value(<<"Transferor-UUID">>, Data);
                             _ ->
                                 props:get_value(<<"Transferee-UUID">>, Data)
                         end,

    case ecallmgr_call_control_sup:find_workers(TransfererCtrlUUId) of
        {ok, Pids} ->
            [begin
                 _ = ecallmgr_call_control:transferer(Pid, Data),
                 lager:debug("sending transferer notice for ~s to ecallmgr_call_control ~p", [TransfererCtrlUUId, Pid])
             end
             || Pid <- Pids
            ];
        {error, not_found} ->
            lager:debug(TransfererCtrlUUId, "no ecallmgr_call_control processes exist locally for reception of transferer notice"),
            ok
    end;
process_transfer_event(_Type, Data) ->
    lager:debug("recieved ~s transfer notice", [_Type]),
    TransfererCtrlUUId = case props:get_value(<<"Transferor-Direction">>, Data) of
                             <<"inbound">> ->
                                 props:get_value(<<"Transferor-UUID">>, Data);
                             _ ->
                                 props:get_value(<<"Transferee-UUID">>, Data)
                         end,

    _ = case ecallmgr_call_control_sup:find_workers(TransfererCtrlUUId) of
            {ok, TransfererPids} ->
                [begin
                     _ = ecallmgr_call_control:transferer(Pid, Data),
                     lager:debug("sending transferer notice for ~s to ecallmgr_call_control ~p", [TransfererCtrlUUId, Pid])
                 end
                 || Pid <- TransfererPids
                ];
            {error, not_found} ->
                lager:debug(TransfererCtrlUUId, "no ecallmgr_call_control processes exist for reception of transferer notice"),
                ok
        end,
    TransfereeCtrlUUId = props:get_value(<<"Replaces">>, Data),

    case ecallmgr_call_control_sup:find_workers(TransfereeCtrlUUId) of
        {ok, ReplacesPids} ->
            [begin
                 ecallmgr_call_control:transferee(Pid, Data),
                 lager:debug("sending transferee notice for ~s to ecallmgr_call_control ~p", [TransfereeCtrlUUId, Pid])
             end
             || Pid <- ReplacesPids
            ];
        {error, not_found} ->
            lager:debug(TransfererCtrlUUId, "no ecallmgr_call_control processes exist locally for reception of transferee notice"),
            ok
    end.

-type cmd_result() :: {'ok', {atom(), nonempty_string()}, ne_binary()} |
                      {'error', {atom(), nonempty_string()}, ne_binary()} |
                      {'timeout', {atom(), ne_binary()}}.
-type cmd_results() :: [cmd_result(),...] | [].

-spec run_start_cmds/1 :: (atom()) -> pid().
run_start_cmds(Node) ->
    spawn_link(fun() ->
                       Cmds = ecallmgr_config:get(<<"fs_cmds">>, [], Node),
                       Res = process_cmds(Node, Cmds),
                       case lists:filter(fun was_not_successful_cmd/1, Res) of
                           [] -> ok;
                           Errs ->
                               print_api_responses(Errs)
                       end
               end).

-spec process_cmds/2 :: (atom(), wh_json:json_object() | [] | [ne_binary(),...]) -> cmd_results().
process_cmds(_, []) ->
    lager:info("no freeswitch commands to run, seems suspect. Is your ecallmgr connected to the same AMQP as the whapps running sysconf?"),
    [];
process_cmds(Node, Cmds) when is_list(Cmds) ->
    lists:foldl(fun(Cmd, Acc) -> process_cmd(Node, Cmd, Acc) end, [], Cmds);
process_cmds(Node, Cmds) ->
    case wh_json:is_json_object(Cmds) of
        true ->
            process_cmd(Node, Cmds, []);
        false ->
            lager:debug("recv something other than a list for fs_cmds: ~p", [Cmds]),
            timer:sleep(5000),
            run_start_cmds(Node)
    end.

-spec process_cmd/3 :: (atom(), wh_json:json_object(), cmd_results()) -> cmd_results().
-spec process_cmd/4 :: (atom(), ne_binary(), ne_binary(), cmd_results()) -> cmd_results().
process_cmd(Node, JObj, Acc0) ->
    lists:foldl(fun({ApiCmd, ApiArg}, Acc) ->
                        process_cmd(Node, ApiCmd, ApiArg, Acc)
                end, Acc0, wh_json:to_proplist(JObj)).

process_cmd(Node, ApiCmd0, ApiArg0, Acc) ->
    ApiCmd = wh_util:to_atom(wh_util:to_binary(ApiCmd0), ?FS_CMD_SAFELIST),
    ApiArg = wh_util:to_list(ApiArg0),
    case freeswitch:api(Node, ApiCmd, wh_util:to_list(ApiArg)) of
        {ok, FSResp} ->
            process_resp(ApiCmd, ApiArg, binary:split(FSResp, <<"\n">>, [global]), Acc);
        {error, _}=E -> [E|Acc];
        timeout -> [{timeout, {ApiCmd, ApiArg}} | Acc]
    end.

process_resp(ApiCmd, ApiArg, [<<>>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, Acc);
process_resp(ApiCmd, ApiArg, [<<"+OK Reloading XML">>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, Acc);
process_resp(ApiCmd, ApiArg, [<<"+OK acl reloaded">>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, Acc);
process_resp(ApiCmd, ApiArg, [<<"+OK ", Resp/binary>>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, [{ok, {ApiCmd, ApiArg}, Resp} | Acc]);
process_resp(ApiCmd, ApiArg, [<<"-ERR ", Err/binary>>|Resps], Acc) ->
    case was_bad_error(Err, ApiCmd, ApiArg) of
        true -> process_resp(ApiCmd, ApiArg, Resps, [{error, {ApiCmd, ApiArg}, Err} | Acc]);
        false -> process_resp(ApiCmd, ApiArg, Resps, Acc)
    end;
process_resp(_, _, [], Acc) ->
    Acc.

was_bad_error(<<"[Module already loaded]">>, load, _) ->
    false;
was_bad_error(_E, _, _) ->
    lager:debug("bad error: ~s", [_E]),
    true.

was_not_successful_cmd({ok, _}) ->
    false;
was_not_successful_cmd({ok, _, _}) ->
    false;
was_not_successful_cmd(_) ->
    true.

-spec get_active_channels/1 :: (atom()) -> integer().
get_active_channels(Node) ->
    case freeswitch:api(Node, show, "channels") of
        {ok, Chans} ->
            case re:run(Chans, "([\\d+])", [{capture, [1], list}]) of
                {match, Match} ->
                    wh_util:to_integer(lists:flatten(Match));
                _Else ->
                    lager:info("failed to parse active channel count on node '~s', statistics may not be accurate", [Node]),
                    0
            end;
        _ ->
            lager:info("failed to get active channel count from node '~s', statistics may not be accurate", [Node]),
            0
    end.

-spec convert_rows/2 :: (atom(), binary()) -> [proplist(),...] | [].
convert_rows(Node, <<"\n0 total.\n">>) ->
    lager:debug("no channels up on node ~s", [Node]),
    [];
convert_rows(Node, RowsBin) ->
    [_|Rows] = binary:split(RowsBin, <<"\n">>, [global]),
    return_rows(Node, Rows, []).

-spec return_rows/3 :: (atom(), [binary(),...] | [], [proplist(),...] | []) -> [proplist(),...] | [].
return_rows(Node, [<<>>|Rs], Acc) ->
    return_rows(Node, Rs, Acc);
return_rows(Node, [R|Rs], Acc) ->
    case binary:split(R, <<",">>) of
        [_Total] ->
            lager:debug("found ~s calls on node ~s", [_Total, Node]),
            return_rows(Node, Rs, Acc);
        [UUID|_] ->
            case freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)) of
                {'ok', Result} ->
                    Props = ecallmgr_util:eventstr_to_proplist(Result),
                    ApplicationName = props:get_value(<<"variable_current_application">>, Props),
                    JObj = wh_json:from_list([{<<"Switch-Hostname">>, Node}
                                              ,{<<"Answer-State">>, props:get_value(<<"Answer-State">>, Props)}
                                              | ecallmgr_call_events:create_event_props(<<>>, ApplicationName, Props)
                                             ]),
                    return_rows(Node, Rs, [JObj|Acc]);
                Error ->
                    lager:debug("failed to get result from uuid_dump(~s): ~p", [UUID, Error]),
                    return_rows(Node, Rs, Acc)
            end
    end;
return_rows(_Node, [], Acc) -> Acc.

-spec print_api_responses/1 :: (cmd_results()) -> 'ok'.
print_api_responses(Res) ->
    lager:debug("start cmd results:"),
    _ = [ print_api_response(ApiRes) || ApiRes <- lists:flatten(Res)],
    lager:debug("end cmd results").

-spec print_api_response/1 :: (cmd_result()) -> 'ok'.
print_api_response({ok, {Cmd, Args}, Res}) ->
    lager:debug("ok: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({error, {Cmd, Args}, Res}) ->
    lager:debug("error: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({timeout, {Cmd, Arg}}) ->
    lager:debug("timeout: ~s(~s)", [Cmd, Arg]).
