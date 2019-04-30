%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Receive route(dialplan) requests from FS, request routes and respond
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_router_call).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo_sip/include/kzsip_uri.hrl").
-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(FETCH_SECTION, 'dialplan').
-define(BINDINGS_CFG_KEY, <<"call_routing_bindings">>).
-define(DEFAULT_BINDINGS, [?DEFAULT_FREESWITCH_CONTEXT]).

-record(state, {node = 'undefined' :: atom()
               ,options = [] :: kz_term:proplist()
               }).
-type state() :: #state{}.

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
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs route listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_dialplan'),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_dialplan', #state{node=Node}=State) ->
    Bindings = kapps_config:get_ne_binaries(?APP_NAME, ?BINDINGS_CFG_KEY, ?DEFAULT_BINDINGS, Node),
    case ecallmgr_fs_router_util:register_bindings(Node, ?FETCH_SECTION, Bindings) of
        'true' -> {'noreply', State};
        'false' ->
            lager:critical("unable to establish route bindings : ~p", [Bindings]),
            {'stop', 'no_binding', State}
    end;
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'route', Section, _EventName, _SubClass, _Context, Id, 'undefined', _FSData}
           ,#state{node=Node}=State
           ) ->
    lager:warning("fetch unknown callid from ~s: Ev: ~p Sc: ~p, Ctx: ~p Id: ~s"
                 ,[Node, _EventName, _SubClass, _Context, Id]
                 ),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, Id, Section, Resp),
    {'noreply', State};
handle_info({'route', Section, <<"REQUEST_PARAMS">>, _SubClass, _Context, FSId, CallId, FSData}
           ,#state{node=Node}=State
           ) ->
    lager:info("process route request for fetch id ~s (uuid ~s)", [FSId, CallId]),

    Props = interaction_props(Node, CallId, FSData),
    _ = kz_util:spawn(fun process_route_req/5, [Section, Node, FSId, CallId, FSData ++ Props]),
    {'noreply', State};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {'stop', {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {'stop', Reason, State};
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

-spec interaction_props(atom(), kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
interaction_props(Node, CallId, FSData) ->
    case props:get_value(?GET_CCV(<<?CALL_INTERACTION_ID>>), FSData) of
        'undefined' ->
            InterActionId = props:get_value(?GET_CUSTOM_HEADER(<<"Call-Interaction-ID">>), FSData, ?CALL_INTERACTION_DEFAULT),
            kz_util:spawn(fun ecallmgr_fs_command:set/3, [Node, CallId, [{<<?CALL_INTERACTION_ID>>, InterActionId}]]),
            [{?GET_CCV(<<?CALL_INTERACTION_ID>>), InterActionId}];
        _InterActionId -> []
    end.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
                                                % terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("route listener for ~s terminating: ~p", [Node, _Reason]).

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
-spec process_route_req(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kzd_freeswitch:data()) -> 'ok'.
process_route_req(Section, Node, FetchId, CallId, Props) ->
    kz_util:put_callid(CallId),

    case kz_term:is_true(props:get_value(<<"variable_recovered">>, Props)) of
        'false' ->
            maybe_process_route_req(Section, Node, FetchId, CallId, Props);
        'true' ->
            lager:debug("recovered channel already exists on ~s, park it", [Node]),
            JObj = kz_json:from_list([{<<"Routes">>, []}
                                     ,{<<"Method">>, <<"park">>}
                                     ]),
            ecallmgr_fs_router_util:reply_affirmative(Section, Node, FetchId, CallId, JObj, Props)
    end.

maybe_process_route_req(Section, Node, FetchId, CallId, Props) ->
    kz_amqp_worker:worker_pool(ecallmgr_call_sup:pool_name()),
    maybe_process_route_req(Section, Node, FetchId, CallId, Props
                           ,kz_amqp_worker:checkout_worker()
                           ).

maybe_process_route_req(Section, Node, FetchId, _CallId, _Props, {'error', _E}) ->
    lager:warning("unable to process dialplan fetch ~s: no workers: ~p", [FetchId, _E]),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, FetchId, Section, Resp);
maybe_process_route_req(Section, Node, FetchId, CallId, Props, {'ok', AMQPWorker}) ->
    do_process_route_req(Section, Node, FetchId, CallId, Props, AMQPWorker).

-spec do_process_route_req(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kzd_freeswitch:data(), pid()) -> 'ok'.
do_process_route_req(Section, Node, FetchId, CallId, Props, AMQPWorker) ->
    Start = kz_time:now(),

    Filtered = ecallmgr_fs_loopback:filter(Node, CallId, Props),
    case ecallmgr_fs_router_util:search_for_route(Section, Node, FetchId, CallId, Filtered, AMQPWorker) of
        'ok' ->
            lager:debug("xml fetch dialplan ~s finished without success", [FetchId]);
        {'ok', JObj} ->
            lager:debug("route response recv, attempting to start call handling"),
            ecallmgr_fs_channels:update(CallId, #channel.handling_locally, 'true'),
            maybe_start_call_handling(Node, FetchId, CallId, JObj, AMQPWorker, Start)
    end.

-spec maybe_start_call_handling(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), pid(), kz_time:now()) -> 'ok'.
maybe_start_call_handling(Node, FetchId, CallId, JObj, AMQPWorker, Start) ->
    case kz_json:get_value(<<"Method">>, JObj) of
        <<"error">> -> lager:debug("sent error response to ~s, not starting call handling", [Node]);
        _Else -> start_call_handling(Node, FetchId, CallId, JObj, AMQPWorker, Start)
    end.

-spec start_call_handling(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), pid(), kz_time:now()) -> 'ok'.
start_call_handling(Node, FetchId, CallId, JObj, AMQPWorker, Start) ->
    ServerQ = kz_api:server_id(JObj),
    CCVs =
        kz_json:set_values([{<<"Application-Name">>, kz_api:app_name(JObj)}
                           ,{<<"Application-Node">>, kz_api:node(JObj)}
                           ]
                          ,kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())
                          ),
    _Evt = ecallmgr_call_sup:start_event_process(Node, CallId),
    _Ctl = ecallmgr_call_sup:start_control_process(Node, CallId, FetchId, ServerQ, CCVs, AMQPWorker),

    lager:debug("started event ~p and control ~p(~p) processes", [_Evt, _Ctl, AMQPWorker]),

    _ = ecallmgr_fs_command:set(Node, CallId, kz_json:to_proplist(CCVs)),
    lager:debug("xml fetch dialplan ~s finished with success after ~pms", [FetchId, kz_time:elapsed_ms(Start)]).
