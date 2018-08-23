%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Receive route(dialplan) requests from FS, request routes and respond
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_router_text).
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
-define(FETCH_SECTION, 'chatplan').
-define(BINDINGS_CFG_KEY, <<"text_routing_bindings">>).
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
    lager:info("starting new fs route text listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_chatplan'),
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
handle_cast('bind_to_chatplan', #state{node=Node}=State) ->
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
handle_info({'route', Section, EventName, SubClass, Context, FSId, 'undefined', FSData}, State) ->
    MsgId = kz_binary:rand_hex(16),
    handle_info({'route', Section, EventName, SubClass, Context, FSId, MsgId, [{<<"Unique-ID">>, MsgId} | FSData]}, State);
handle_info({'route', Section, <<"REQUEST_PARAMS">>, _SubClass, _Context, FSId, MsgId, FSData}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun process_route_req/5, [Section, Node, FSId, MsgId, FSData]),
    {'noreply', State, 'hibernate'};
handle_info({'route', Section, <<"MESSAGE">>, _SubClass, _Context, FSId, MsgId, FSData}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun process_route_req/5, [Section, Node, FSId, MsgId, FSData]),
    {'noreply', State, 'hibernate'};
handle_info({'route', Section, <<"CUSTOM">>, <<"KZ::", _/binary>>, _Context, FSId, MsgId, FSData}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun process_route_req/5, [Section, Node, FSId, MsgId, FSData]),
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

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
-spec should_expand_var(any()) -> boolean().
should_expand_var({<<?CHANNEL_VAR_PREFIX, _/binary>>, _}) -> 'true';
should_expand_var({<<"sip_", _/binary>>, _}) -> 'true';
should_expand_var(_) -> 'false'.

-spec init_message_props(kz_term:proplist()) -> kz_term:proplist().
init_message_props(Props) ->
    Routines = [fun add_message_missing_props/1
               ,fun expand_message_vars/1
               ],
    lists:foldl(fun(F,P) -> F(P) end, Props, Routines).

-spec add_message_missing_props(kz_term:proplist()) -> kz_term:proplist().
add_message_missing_props(Props) ->
    props:insert_values([{<<"Call-Direction">>, <<"outbound">>}
                        ,{<<"Resource-Type">>,<<"sms">>}
                        ,{<<"Message-ID">>, kz_binary:rand_hex(16)}
                        ,{<<"Caller-Caller-ID-Number">>, props:get_value(<<"from_user">>, Props)}
                        ,{<<"Caller-Destination-Number">>, props:get_value(<<"to_user">>, Props)}
                        ]
                       ,Props
                       ).

-spec expand_message_vars(kz_term:proplist()) -> kz_term:proplist().
expand_message_vars(Props) ->
    lists:foldl(fun expand_message_var/2
               ,Props
               ,props:filter(fun should_expand_var/1, Props)
               ).

-spec expand_message_var({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) ->
                                kz_term:proplist().
expand_message_var({K,V}, Ac) ->
    case props:get_value(<<"variable_", K/binary>>, Ac) of
        'undefined' -> props:set_value(<<"variable_", K/binary>>, V, Ac);
        _ -> Ac
    end.

-spec process_route_req(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
process_route_req(Section, Node, FetchId, MsgId, Props) ->
    do_process_route_req(Section, Node, FetchId, MsgId, init_message_props(Props)).

-spec do_process_route_req(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
do_process_route_req(Section, Node, FetchId, MsgId, Props) ->
    case ecallmgr_fs_router_util:search_for_route(Section, Node, FetchId, MsgId, Props) of
        'ok' ->
            lager:debug("xml fetch chatplan ~s finished without success", [FetchId]);
        {'ok', JObj} ->
            start_message_handling(Node, FetchId, MsgId, JObj)
    end.

-spec start_message_handling(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
start_message_handling(_Node, FetchId, MsgId, JObj) ->
    ServerQ = kz_api:server_id(JObj),
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    Win = [{<<"Msg-ID">>, FetchId}
          ,{<<"Call-ID">>, MsgId}
          ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ServerQ]),
    kz_amqp_worker:cast(Win, fun(Payload)-> kapi_route:publish_win(ServerQ, Payload) end).
