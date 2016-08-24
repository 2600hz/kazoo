%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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
               ,options = [] :: kz_proplist()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

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
    kz_util:put_callid(Node),
    lager:info("starting new fs route text listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_chatplan'),
    {'ok', #state{node=Node, options=Options}}.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('bind_to_chatplan', #state{node=Node}=State) ->
    Bindings = ecallmgr_config:get(?BINDINGS_CFG_KEY, ?DEFAULT_BINDINGS, Node),
    case ecallmgr_fs_router_util:register_bindings(Node, ?FETCH_SECTION, Bindings) of
        'true' -> {'noreply', State};
        'false' ->
            lager:critical("unable to establish route bindings : ~p", [Bindings]),
            {'stop', 'no_binding', State}
    end;
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'route', Section, EventName, SubClass, Context, FSId, 'undefined', FSData}, State) ->
    MsgId = kz_util:rand_hex_binary(16),
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
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
                                                % terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("route listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec should_expand_var(any()) -> boolean().
should_expand_var({<<?CHANNEL_VAR_PREFIX, _/binary>>, _}) -> 'true';
should_expand_var({<<"sip_", _/binary>>, _}) -> 'true';
should_expand_var(_) -> 'false'.

-spec init_message_props(kz_proplist()) -> kz_proplist().
init_message_props(Props) ->
    Routines = [fun add_message_missing_props/1
               ,fun expand_message_vars/1
               ],
    lists:foldl(fun(F,P) -> F(P) end, Props, Routines).

-spec add_message_missing_props(kz_proplist()) -> kz_proplist().
add_message_missing_props(Props) ->
    props:insert_values(
      [{<<"Call-Direction">>, <<"outbound">>}
      ,{<<"Resource-Type">>,<<"sms">>}
      ,{<<"Message-ID">>, kz_util:rand_hex_binary(16)}
      ,{<<"Caller-Caller-ID-Number">>, props:get_value(<<"from_user">>, Props)}
      ,{<<"Caller-Destination-Number">>, props:get_value(<<"to_user">>, Props)}
      ]
                       ,Props
     ).

-spec expand_message_vars(kz_proplist()) -> kz_proplist().
expand_message_vars(Props) ->
    lists:foldl(fun expand_message_var/2
               ,Props
               ,props:filter(fun should_expand_var/1, Props)
               ).

-spec expand_message_var({ne_binary(), ne_binary()}, kz_proplist()) ->
                                kz_proplist().
expand_message_var({K,V}, Ac) ->
    case props:get_value(<<"variable_", K/binary>>, Ac) of
        'undefined' -> props:set_value(<<"variable_", K/binary>>, V, Ac);
        _ -> Ac
    end.

-spec process_route_req(atom(), atom(), ne_binary(), ne_binary(), kz_proplist()) -> 'ok'.
process_route_req(Section, Node, FetchId, MsgId, Props) ->
    do_process_route_req(Section, Node, FetchId, MsgId, init_message_props(Props)).

-spec do_process_route_req(atom(), atom(), ne_binary(), ne_binary(), kz_proplist()) -> 'ok'.
do_process_route_req(Section, Node, FetchId, MsgId, Props) ->
    case ecallmgr_fs_router_util:search_for_route(Section, Node, FetchId, MsgId, Props) of
        'ok' ->
            lager:debug("xml fetch chatplan ~s finished without success", [FetchId]);
        {'ok', JObj} ->
            start_message_handling(Node, FetchId, MsgId, JObj)
    end.

-spec start_message_handling(atom(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
start_message_handling(_Node, _FetchId, MsgId, JObj) ->
    ServerQ = kz_json:get_value(<<"Server-ID">>, JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    Win = [{<<"Msg-ID">>, MsgId}
          ,{<<"Call-ID">>, MsgId}
          ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ServerQ]),
    kz_amqp_worker:cast(Win, fun(Payload)-> kapi_route:publish_win(ServerQ, Payload) end).
