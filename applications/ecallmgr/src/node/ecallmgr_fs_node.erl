%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Manage a FreeSWITCH node and its resources
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_node).
-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([handle_reload_acls/2]).
-export([handle_reload_gateways/2]).
-export([sync_channels/1
        ,sync_info/1
        ,sync_capabilities/1
        ]).
-export([info/1]).
-export([sip_url/1, sip_url/2]).
-export([sip_external_ip/1, sip_external_ip/2]).
-export([fs_node/1]).
-export([hostname/1]).
-export([interface/1, interface/2]).
-export([interfaces/1]).
-export([instance_uuid/1]).
-export([fetch_timeout/0, fetch_timeout/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(UPTIME_S, kapps_config:get_integer(?APP_NAME, <<"fs_node_uptime_s">>, 600)).

%% -type interface() :: {kz_term:ne_binary(), kz_term:proplist()}.
%% -type interfaces() :: [interface()].

-define(DEFAULT_FS_COMMANDS, [kz_json:from_list([{<<"load">>, <<"mod_sofia">>}])
                             ,kz_json:from_list([{<<"reloadacl">>, <<>>}])
                             ]).
-define(FS_CMDS(Node), kapps_config:get_jsons(?APP_NAME, <<"fs_cmds">>, ?DEFAULT_FS_COMMANDS, Node)).

-define(DEFAULT_CAPABILITIES, [kz_json:from_list([{<<"module">>, <<"mod_conference">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"conference">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_channel_move">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"channel_move">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_http_cache">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"http_cache">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_dptools">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"dialplan">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_sofia">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"sip">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_spandsp">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"fax">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_flite">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"tts">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_freetdm">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"freetdm">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_skypopen">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"skype">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_dingaling">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"xmpp">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_skinny">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"skinny">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_sms">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"sms">>}
                                                 ])
                              ]).

-record(state, {node               :: atom()
                                                %               ,instance_uuid      :: kz_term:api_ne_binary()
               ,options = []       :: kz_term:proplist()
                                                %               ,interfaces = []    :: interfaces()
               ,info          :: kz_term:api_object()
               ,start_cmds_pid_ref :: kz_term:api_pid_ref()
               }).
-type state() :: #state{}.

-define(RESPONDERS, [{{?MODULE, 'handle_reload_acls'}
                     ,[{<<"switch_event">>, <<"reload_acls">>}]
                     }
                    ,{{?MODULE, 'handle_reload_gateways'}
                     ,[{<<"switch_event">>, <<"reload_gateways">>}]
                     }
                    ,{'ecallmgr_fs_node_command'
                     ,[{<<"switch_event">>, <<"command">>}]
                     }
                    ]).
-define(BINDINGS(Node), [{'switch', [{'node', Node}
                                    ,{'restrict_to', ['reload_acls'
                                                     ,'reload_gateways'
                                                     ,'command'
                                                     ]
                                     }
                                    ,'federate'
                                    ]
                         }
                        ]).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(FS_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-type fs_node() :: atom() | kz_term:ne_binary() | pid().

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) when is_atom(Node) ->
    QueueName = list_to_binary([kz_term:to_binary(Node)
                               ,"-"
                               ,kz_term:to_binary(?MODULE)
                               ]),

    case freeswitch:json_api(Node, <<"node.info">>) of
        {'ok', Info} ->
            gen_listener:start_link(?SERVER
                                   ,[{'responders', ?RESPONDERS}
                                    ,{'bindings', ?BINDINGS(Node)}
                                    ,{'queue_name', QueueName}
                                    ,{'queue_options', ?QUEUE_OPTIONS}
                                    ,{'consume_options', ?CONSUME_OPTIONS}
                                    ]
                                   ,[Node, Info, Options]
                                   );
        _Err -> _Err
    end.

-spec sync_channels(fs_node()) -> 'ok'.
sync_channels(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_channels').

-spec sync_conferences(fs_node()) -> 'ok'.
sync_conferences(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_conferences').

-spec sync_info(fs_node()) -> 'ok'.
sync_info(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_info').

-spec sync_capabilities(fs_node()) -> 'ok'.
sync_capabilities(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_capabilities').

-spec hostname(fs_node()) -> kz_term:api_binary().
hostname(Srv) ->
    case fs_node(Srv) of
        'undefined' -> 'undefined';
        Node ->
            [_, Hostname] = binary:split(kz_term:to_binary(Node), <<"@">>),
            Hostname
    end.

-spec sip_url(fs_node()) -> kz_term:api_binary().
sip_url(Srv) ->
    sip_url(Srv, <<?DEFAULT_FS_PROFILE>>).

-spec sip_url(fs_node(), kz_term:ne_binary()) -> kz_term:api_binary().
sip_url(Srv, Profile) ->
    gen_server:call(find_srv(Srv), {'sip_url', Profile}).

-spec sip_external_ip(fs_node()) -> kz_term:api_binary().
sip_external_ip(Srv) ->
    sip_external_ip(Srv, <<?DEFAULT_FS_PROFILE>>).

-spec sip_external_ip(fs_node(), kz_term:ne_binary()) -> kz_term:api_binary().
sip_external_ip(Srv, Profile) ->
    gen_server:call(find_srv(Srv), {'sip_external_ip', Profile}).

-spec handle_reload_acls(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reload_acls(JObj, Props) ->
    'true' = kapi_switch:reload_acls_v(JObj),

    Node = props:get_value('node', Props),
    case freeswitch:bgapi(Node, 'reloadacl', "") of
        {'ok', Job} -> lager:debug("reloadacl command sent to ~s: JobID: ~s", [Node, Job]);
        {'error', _E} -> lager:debug("reloadacl failed with error: ~p", [_E])
    end.

-spec handle_reload_gateways(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reload_gateways(JObj, Props) ->
    'true' = kapi_switch:reload_gateways_v(JObj),

    Node = props:get_value('node', Props),
    Args = ["profile "
           ,?DEFAULT_FS_PROFILE
           ," rescan"
           ],
    case kapps_config:get_boolean(?APP_NAME, <<"process_gateways">>, 'false')
        andalso freeswitch:bgapi(Node, 'sofia', lists:flatten(Args))
    of
        'false' -> 'ok';
        {'ok', Job} -> lager:debug("sofia ~s command sent to ~s: JobID: ~s", [Args, Node, Job]);
        {'error', _E} -> lager:debug("sofia ~s failed with error: ~p", [Args, _E])
    end.

-spec fs_node(fs_node()) -> atom().
fs_node(Srv) ->
    case catch(gen_server:call(find_srv(Srv), 'node', ?FS_TIMEOUT)) of
        {'EXIT', _} -> 'undefined';
        Else -> Else
    end.

-spec find_srv(fs_node()) -> kz_term:api_pid().
find_srv(Pid) when is_pid(Pid) -> Pid;
find_srv(Node) when is_binary(Node) -> find_srv(kz_term:to_atom(Node));
find_srv(Node) when is_atom(Node) ->
    ecallmgr_fs_node_sup:node_srv(ecallmgr_fs_sup:find_node(Node)).

-spec fetch_timeout() -> pos_integer().
fetch_timeout() ->
    kapps_config:get_integer(?APP_NAME, <<"fetch_timeout">>, ?DEFAULT_FETCH_TIMEOUT).

-spec fetch_timeout(fs_node()) -> pos_integer().
fetch_timeout(_Node) ->
    %% TODO: eventually expose this timeout via mod_kazoo and decrement a bit.
    fetch_timeout().

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_json:object() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Info, Options]) ->
    init(Node, Info, Options).

-spec init(atom(), kz_json:object(), kz_term:proplist()) -> {'ok', state()}.
init(Node, Info, Options) ->
    process_flag('trap_exit', 'true'),
    kz_log:put_callid(Node),
    process_flag('priority', 'high'), %% Living dangerously!
    lager:info("starting new fs node listener for ~s", [Node]),
    gproc:reg({'p', 'l', 'fs_node'}),
    sync_channels(self()),
    sync_conferences(self()),
    PidRef = run_start_cmds(Node, Info, Options),
    lager:debug("running start commands in ~p", [PidRef]),
    {'ok', #state{node=Node
                 ,options=Options
                 ,start_cmds_pid_ref=PidRef
                 ,info=Info
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'sip_external_ip', Profile}, _, #state{info=Info}=State) ->
    ExternalIP = kz_json:get_ne_binary_value([<<"Roles">>, <<"Media">>, <<"profiles">>, Profile, <<"info">>, <<"ext-sip-ip">>], Info),
    {'reply', ExternalIP, State};
handle_call({'sip_url', Profile}, _, #state{info=Info}=State) ->
    SIPUrl = kz_json:get_ne_binary_value([<<"Roles">>, <<"Media">>, <<"profiles">>, Profile, <<"info">>, <<"url">>], Info),
    {'reply', SIPUrl, State};
handle_call('interfaces', _, #state{info=Info}=State) ->
    Resp = kz_json:get_json_value([<<"Roles">>, <<"Media">>, <<"profiles">>], Info, kz_json:new()),
    {'reply', Resp, State};
handle_call({'interface', Interface}, _, #state{info=Info}=State) ->
    Resp = kz_json:get_json_value([<<"Roles">>, <<"Media">>, <<"profiles">>, Interface], Info, kz_json:new()),
    {'reply', Resp, State};
handle_call('instance_uuid', _, #state{info=Info}=State) ->
    {'reply', kz_json:get_ne_binary_value([<<"Runtime-Info">>, <<"Core-UUID">>], Info), State};
handle_call('info', _, #state{info=Info}=State) ->
    {'reply', Info, State};
handle_call('node', _, #state{node=Node}=State) ->
    {'reply', Node, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('sync_info', #state{node=Node}=State) ->
    case freeswitch:json_api(Node, <<"node.info">>) of
        {'ok', JObj} -> {'noreply', State#state{info=JObj}};
        _ -> {'noreply', State}
    end;
handle_cast('sync_capabilities', #state{node=Node, info=Info}=State) ->
    _Pid = kz_process:spawn(fun probe_capabilities/2, [Node, Info]),
    lager:debug("syncing capabilities in ~p", [_Pid]),
    {'noreply', State};
handle_cast('sync_channels', #state{node=Node}=State) ->
    Channels = [kz_json:get_value(<<"uuid">>, J)
                || J <- channels_as_json(Node)
               ],
    _ = ecallmgr_fs_channels:sync(Node, Channels),
    {'noreply', State};
handle_cast('sync_conferences', #state{node=Node}=State) ->
    _ = ecallmgr_fs_conferences:sync_node(Node),
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'bgok', _Job, _Result}, State) ->
    lager:debug("job ~s finished successfully: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'bgerror', _Job, _Result}, State) ->
    lager:debug("job ~s finished with an error: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{node=_Node, start_cmds_pid_ref={Pid, Ref}}=State) ->
    lager:debug("fs sync complete"),
    {'noreply', State#state{start_cmds_pid_ref='undefined'}};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node
                          ,options=Options
                          }) ->
    {'reply', [{'node', Node}
              ,{'node_options', Options}
              ]
    }.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("node listener for ~s terminating: ~p", [Node, _Reason]).

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

-type cmd_result() :: {'ok', {atom(), nonempty_string()}, kz_term:ne_binary()} |
                      {'error', {atom(), nonempty_string()}, kz_term:ne_binary()} |
                      {'timeout', {atom(), kz_term:ne_binary()}}.
-type cmd_results() :: [cmd_result()] |
                       {'error', 'retry'}.

-spec run_start_cmds(atom(), kz_json:object(), kz_term:proplist()) -> kz_term:pid_ref().
run_start_cmds(Node, Info, Options) when is_atom(Node) ->
    kz_process:spawn_monitor(fun run_start_cmds/4, [Node, Info, Options, self()]).

-spec run_start_cmds(atom(), kz_json:object(), kz_term:proplist(), pid()) -> any().
run_start_cmds(Node, Info, Options, Parent) when is_atom(Node) ->
    kz_log:put_callid(Node),
    timer:sleep(kapps_config:get_integer(?APP_NAME, <<"fs_cmds_wait_ms">>, 5 * ?MILLISECONDS_IN_SECOND, Node)),
    run_start_cmds(Node, Info, Options, Parent, is_restarting(Info)).

-spec is_restarting(kz_json:object()) -> boolean().
is_restarting(Info) ->
    Uptime = (kz_json:get_integer_value([<<"Runtime-Info">>, <<"uptime">>, <<"years">>], Info, 0) * ?SECONDS_IN_YEAR)
        + (kz_json:get_integer_value([<<"Runtime-Info">>, <<"uptime">>, <<"days">>], Info, 0) * ?SECONDS_IN_DAY)
        + (kz_json:get_integer_value([<<"Runtime-Info">>, <<"uptime">>, <<"hours">>], Info, 0) * ?SECONDS_IN_HOUR)
        + (kz_json:get_integer_value([<<"Runtime-Info">>, <<"uptime">>, <<"minutes">>], Info, 0) * ?SECONDS_IN_MINUTE)
        + kz_json:get_integer_value([<<"Runtime-Info">>, <<"uptime">>, <<"seconds">>], Info, 0),
    lager:debug("node has been up for ~b s (considered restarting: ~s)", [Uptime, Uptime < ?UPTIME_S]),
    Uptime < ?UPTIME_S.

-spec run_start_cmds(atom(), kz_json:object(), kz_term:proplist(), pid(), boolean() | kz_json:objects()) -> 'ok'.
run_start_cmds(Node, Info, Options, Parent, 'true') when is_atom(Node) ->
    lager:debug("node ~s is considered restarting", [Node]),
    run_start_cmds(Node, Info, Options, Parent, ?FS_CMDS(Node));
run_start_cmds(Node, Info, Options, Parent, 'false') when is_atom(Node) ->
    lager:debug("node ~s is not considered restarting, trying reconnect cmds first", [Node]),
    Cmds = case kapps_config:get_jsons(?APP_NAME, <<"fs_reconnect_cmds">>, 'undefined') of
               'undefined' -> ?FS_CMDS(Node);
               ReconCmds -> ReconCmds
           end,
    run_start_cmds(Node, Info, Options, Parent, Cmds);
run_start_cmds(Node, Info, Options, Parent, Cmds) when is_atom(Node) ->
    Res = process_cmds(Node, Info, Options, Cmds),

    case is_list(Res)
        andalso [R || R <- Res, was_not_successful_cmd(R)]
    of
        [] -> sync(Parent);
        'false' ->
            lager:debug("failed to run start commands, retrying"),
            run_start_cmds(Node, Info, Options, Parent);
        Errs ->
            print_api_responses(Errs),
            sync(Parent)
    end.

-spec sync(pid()) -> any().
sync(Parent) ->
    sync_info(Parent),
    sync_capabilities(Parent).

-spec process_cmds(atom(), kz_json:object(), kz_term:proplist(), kz_json:objects()) -> cmd_results().
process_cmds(_Node, _Info, _Options, []) ->
    lager:info("no freeswitch commands to run, seems suspect. Is your ecallmgr connected to the same AMQP as the kapps running sysconf?"),
    [];
process_cmds(Node, Info, Options, Cmds) when is_list(Cmds) ->
    lists:foldl(fun(Cmd, Acc) -> process_cmd(Node, Info, Options, Cmd, Acc) end, [], Cmds).

-spec process_cmd(atom(), kz_json:object(), kz_term:proplist(), kz_json:object(), cmd_results()) -> cmd_results().
process_cmd(Node, Info, Options, JObj, Acc0) ->
    kz_json:foldl(fun(ApiCmd, ApiArg, Acc) ->
                          lager:debug("process ~s: ~s: ~s", [Node, ApiCmd, ApiArg]),
                          process_cmd(Node, Info, Options, ApiCmd, ApiArg, Acc)
                  end
                 ,Acc0
                 ,JObj
                 ).

-spec process_cmd(atom(), kz_json:object(), kz_term:proplist(), kz_term:ne_binary(), kz_json:json_term(), cmd_results()) -> cmd_results().
process_cmd(Node, Info, Options, ApiCmd0, ApiArg, Acc) ->
    execute_command(Node, Info, Options, ApiCmd0, ApiArg, Acc).

-spec execute_command(atom(), kz_json:object(), kz_term:proplist(), kz_term:ne_binary(), kz_json:json_term(), cmd_results()) -> cmd_results().
execute_command(Node, _Info, _Options, ApiCmd0, ApiArg, Acc) ->
    ApiCmd = kz_term:to_atom(ApiCmd0, ?FS_CMD_SAFELIST),
    lager:debug("exec ~s on ~s", [ApiCmd, Node]),
    case freeswitch:bgapi(Node, ApiCmd, ApiArg) of
        {'ok', BGApiID} ->
            receive
                {'bgok', BGApiID} ->
                    [{'ok', {ApiCmd, ApiArg}, <<"OK">>} | Acc];
                {'bgok', BGApiID, FSResp} ->
                    [{'ok', {ApiCmd, ApiArg}, FSResp} | Acc];
                {'bgerror', BGApiID, Error} ->
                    [{'error', {ApiCmd, ApiArg}, Error} | Acc]
            after 120 * ?MILLISECONDS_IN_SECOND ->
                    [{'timeout', {ApiCmd, ApiArg}} | Acc]
            end;
        {'error', Error} ->
            [{'error', {ApiCmd, ApiArg}, Error} | Acc]
    end.

-spec was_not_successful_cmd(cmd_result()) -> boolean().
was_not_successful_cmd({'ok', _, _}) -> 'false';
was_not_successful_cmd(_) -> 'true'.

-spec print_api_responses(cmd_results()) -> 'ok'.
print_api_responses(Res) ->
    lager:debug("start cmd results:"),
    lists:foreach(fun print_api_response/1, lists:flatten(Res)),
    lager:debug("end cmd results").

-spec print_api_response(cmd_result()) -> 'ok'.
print_api_response({'ok', {Cmd, Args}, Res}) ->
    lager:info("ok: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({'error', {Cmd, Args}, Res}) ->
    lager:info("error: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({'timeout', {Cmd, Arg}}) ->
    lager:info("timeout: ~s(~s)", [Cmd, Arg]).

-spec channels_as_json(atom()) -> kz_json:objects().
channels_as_json(Node) ->
    case freeswitch:api(Node, 'show', "channels as json") of
        {'ok', Bin} -> kz_json:get_list_value(<<"rows">>, kz_json:decode(Bin), []);
        {'error', _} -> []
    end.

-spec probe_capabilities(atom(), kz_json:object()) -> 'ok'.
probe_capabilities(Node, Info) ->
    probe_capabilities(Node, Info, kapps_config:get_jsons(?APP_NAME, <<"capabilities">>, ?DEFAULT_CAPABILITIES)).

-spec probe_capabilities(atom(), kz_json:object(), kz_json:objects()) -> 'ok'.
probe_capabilities(Node, Info, PossibleCapabilities) ->
    Loaded = kz_json:get_list_value([<<"Modules">>, <<"loaded">>], Info, []),
    Existing = kz_json:get_list_value([<<"Modules">>, <<"available">>], Info, []),
    F2 = fun(Capability) -> {lists:member(Capability, Existing), lists:member(Capability, Loaded)} end,
    F = fun(Capability) -> maybe_add_capability(Node, Capability, F2) end,
    lists:foreach(F, PossibleCapabilities),
    lager:notice("capabilities sync complete").

-spec maybe_add_capability(atom(), kz_json:object(), fun()) -> any().
maybe_add_capability(Node, Capability, Fun) ->
    Module = kz_json:get_value(<<"module">>, Capability),
    lager:debug("probing ~s about ~s", [Node, Module]),
    case Fun(Module) of
        {'true', 'true'} ->
            lager:debug("adding capability of ~s", [Module]),
            ecallmgr_fs_nodes:add_capability(Node, kz_json:set_value(<<"is_loaded">>, 'true', Capability));
        {'true', 'false'} ->
            ecallmgr_fs_nodes:add_capability(Node, kz_json:set_value(<<"is_loaded">>, 'false', Capability));
        {'false', _} ->
            catch(ecallmgr_fs_nodes:remove_capability(Node, Module))
    end.

-spec interfaces(atom() | binary()) -> kz_term:api_object().
interfaces(Node) ->
    gen_server:call(find_srv(Node), 'interfaces').

-spec interface(atom() | binary()) -> kz_term:api_object().
interface(Node) ->
    interface(Node, <<?DEFAULT_FS_PROFILE>>).

-spec interface(atom() | binary(), kz_term:ne_binary()) -> kz_term:api_object().
interface(Node, Profile) ->
    gen_server:call(find_srv(Node), {'interface', Profile}).

-spec instance_uuid(atom() | binary()) -> kz_term:api_ne_binary().
instance_uuid(Node) ->
    gen_server:call(find_srv(Node), 'instance_uuid').

-spec info(fs_node()) -> kz_term:api_object().
info(Srv) ->
    case find_srv(Srv) of
        Pid when is_pid(Pid) ->
            try
                gen_server:call(Pid, 'info')
            catch
                _:_:_ -> 'undefined'
            end;
        _ -> 'undefined'
    end.
