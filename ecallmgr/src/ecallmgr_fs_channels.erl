%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_channels).

-behaviour(gen_listener).

-export([start_link/0]).
-export([sync/2]).
-export([show_all/0]).
-export([flush_node/1]).
-export([new/1]).
-export([destroy/1]).
-export([update/3
         ,updates/2
        ]).
-export([account_summary/1]).
-export([match_presence/1]).
-export([handle_channel_status/2]).
-export([handle_query_auth_id/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(RESPONDERS, [{{?MODULE, handle_channel_status}, [{<<"call_event">>, <<"channel_status_req">>}]}
                     ,{{?MODULE, handle_query_auth_id}, [{<<"call_event">>, <<"query_auth_id_req">>}]}
                    ]).
-define(BINDINGS, [{call, [{restrict_to, [status_req]}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {}).
-type state() :: #state{}.

-record(astats, {bridge_ids =           sets:new() :: set()
                 ,outbound_flat_rate =  sets:new() :: set()
                 ,inbound_flat_rate =   sets:new() :: set()
                 ,outbound_per_minute = sets:new() :: set()
                 ,inbound_per_minute =  sets:new() :: set()
                 ,resource_consumers =  sets:new() :: set()
                }).
-type astats() :: #astats{}.

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({local, ?MODULE}, ?MODULE, [{responders, ?RESPONDERS}
                                                        ,{bindings, ?BINDINGS}
                                                        ,{queue_name, ?QUEUE_NAME}
                                                        ,{queue_options, ?QUEUE_OPTIONS}
                                                        ,{consume_options, ?CONSUME_OPTIONS}
                                                       ], []).

-spec sync(atom(), ne_binaries()) -> 'ok'.
sync(Node, Channels) ->
    gen_server:cast(?MODULE, {'sync_channels', Node, Channels}).

-spec show_all() -> wh_json:objects().
show_all() ->
    ets:foldl(fun(Channel, Acc) ->
                      [ecallmgr_fs_channel:to_json(Channel) | Acc]
              end, [], ?CHANNELS_TBL).

-spec flush_node(string() | binary() | atom()) -> 'ok'.
flush_node(Node) ->
    gen_server:cast(?MODULE, {'flush_node', wh_util:to_atom(Node, 'true')}).

-spec new(channel()) -> 'ok'.
new(#channel{}=Channel) ->
    gen_server:call(?MODULE, {'new_channel', Channel}).

-spec destroy(channel()) -> 'ok'.
destroy(#channel{uuid=UUID, node=Node}) ->
    gen_server:cast(?MODULE, {'destroy_channel', UUID, Node}).

-spec update(ne_binary(), pos_integer(), _) -> 'ok'.
update(UUID, Key, Value) ->
    updates(UUID, [{Key, Value}]).
    
-spec updates(ne_binary(), wh_proplist()) -> 'ok'.
updates(UUID, Updates) ->
    gen_server:cast(?MODULE, {'channel_update', UUID, Updates}).

-spec account_summary(ne_binary()) -> channels().
account_summary(AccountId) ->
    MatchSpec = [{#channel{direction = '$1', account_id = '$2', account_billing = '$7'
                           ,authorizing_id = '$3', resource_id = '$4', bridge_id = '$5'
                           , _ = '_'
                          }
                  ,[{'=:=', '$2', {'const', AccountId}}]
                  ,['$_']}
                ],
    summarize_account_usage(ets:select(?CHANNELS_TBL, MatchSpec)).

-spec match_presence(ne_binary()) -> wh_proplist_kv(ne_binary(), atom()).
match_presence(PresenceId) ->
    MatchSpec = [{#channel{uuid = '$1', presence_id = '$2', node = '$3',  _ = '_'}
                  ,[{'=:=', '$2', {'const', PresenceId}}]
                  ,[{{'$1', '$3'}}]}
                ],
    ets:select(?CHANNELS_TBL, MatchSpec).

-spec handle_query_auth_id(wh_json:object(), proplist()) -> 'ok'.
handle_query_auth_id(JObj, _Props) ->
    'true' = wapi_call:query_auth_id_req_v(JObj),
    AuthId = wh_json:get_value(<<"Auth-ID">>, JObj),
    Channels = case find_by_auth_id(AuthId) of
                   {'error', 'not_found'} -> [];
                   {'ok', C} -> C
               end,
    Resp = [{<<"Channels">>, Channels}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    wapi_call:publish_query_auth_id_resp(ServerId, Resp).

-spec handle_channel_status(wh_json:json_object(), proplist()) -> 'ok'.
handle_channel_status(JObj, _Props) ->
    'true' = wapi_call:channel_status_req_v(JObj),
    _ = wh_util:put_callid(JObj),

    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("channel status request received"),

    AllNodesConnected = ecallmgr_fs_nodes:all_nodes_connected(),
    case ecallmgr_fs_channel:fetch(CallId) of
        {'error', 'not_found'} when AllNodesConnected ->
            lager:debug("no node found with channel ~s", [CallId]),
            Resp = [{<<"Call-ID">>, CallId}
                    ,{<<"Status">>, <<"terminated">>}
                    ,{<<"Error-Msg">>, <<"no node found with channel">>}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        {'error', 'not_found'} ->
            lager:debug("no node found with channel ~s, but we are not authoritative", [CallId]);
        {'ok', Channel} ->
            channel_status_resp(CallId, Channel, JObj)
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
init([]) ->
    put('callid', ?LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:debug("starting new fs channels"),
    _ = ets:new(?CHANNELS_TBL, ['set', 'protected', 'named_table', {'keypos', #channel.uuid}]),
    {'ok', #state{}}.

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
handle_call({'new_channel', Channel}, _, State) ->
    ets:insert(?CHANNELS_TBL, Channel),
    {'reply', 'ok', State};
handle_call(_, _, State) ->
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
-spec handle_cast(term(), state()) -> {'noreply', state()}.
handle_cast({'channel_update', UUID, Update}, State) ->
    ets:update_element(?CHANNELS_TBL, UUID, Update),
    {'noreply', State, 'hibernate'};
handle_cast({'destroy_channel', UUID, Node}, State) ->
    MatchSpec = [{#channel{uuid='$1', node='$2', _ = '_'}
                  ,[{'andalso', {'=:=', '$2', {'const', Node}}
                     ,{'=:=', '$1', UUID}}
                   ],
                  ['true']
                 }],
    N = ets:select_delete(?CHANNELS_TBL, MatchSpec),
    lager:debug("removed ~p channel(s) with id ~s on ~s", [N, UUID, Node]),
    {'noreply', State, 'hibernate'};
handle_cast({'sync_channels', Node, Channels}, State) ->
    lager:debug("ensuring channel cache is in sync with ~s", [Node]),
    MatchSpec = [{#channel{uuid = '$1', node = '$2', _ = '_'}
                  ,[{'=:=', '$2', {'const', Node}}]
                  ,['$1']}
                ],
    CachedChannels = sets:from_list(ets:select(?CHANNELS_TBL, MatchSpec)),
    SyncChannels = sets:from_list(Channels),
    Remove = sets:subtract(CachedChannels, SyncChannels),
    Add = sets:subtract(SyncChannels, CachedChannels),
    _ = [begin
             lager:debug("removed channel ~s from cache during sync with ~s", [UUID, Node]),
             ets:delete(?CHANNELS_TBL, UUID)
         end
         || UUID <- sets:to_list(Remove)
        ],
    _ = [begin
             lager:debug("added channel ~s to cache during sync with ~s", [UUID, Node]),
             case ecallmgr_fs_channel:renew(Node, UUID) of
                 {'ok', C} -> ets:insert(?CHANNELS_TBL, C);
                 {'error', _R} -> lager:warning("failed to sync channel ~s: ~p", [UUID, _R])
             end
         end
         || UUID <- sets:to_list(Add)
        ],
    {'noreply', State, 'hibernate'};
handle_cast({'flush_node', Node}, State) ->
    lager:debug("flushing all channels in cache associated to node ~s", [Node]),
    MatchSpec = [{#channel{node = '$1', _ = '_'}
                  ,[{'=:=', '$1', {'const', Node}}]
                  ,['true']}
                ],
    ets:select_delete(?CHANNELS_TBL, MatchSpec),
    {'noreply', State};
handle_cast(_Req, State) ->
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
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{}) ->
    {'reply', []}.

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
terminate(_Reason, #state{}) ->
    lager:info("fs channels terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec summarize_account_usage(channels()) -> wh_json:object().
summarize_account_usage(Channels) ->
    AStats = lists:foldr(fun classify_channel/2, #astats{}, Channels),
    wh_json:from_list(
      [{<<"Calls">>, sets:size(AStats#astats.bridge_ids)}
       ,{<<"Channels">>,  length(Channels)}
       ,{<<"Outbound-Flat-Rate">>, sets:size(AStats#astats.outbound_flat_rate)}
       ,{<<"Inbound-Flat-Rate">>, sets:size(AStats#astats.inbound_flat_rate)}
       ,{<<"Outbound-Per-Minute">>, sets:size(AStats#astats.outbound_per_minute)}
       ,{<<"Inbound-Per-Minute">>, sets:size(AStats#astats.inbound_per_minute)}
       ,{<<"Resource-Consuming-Calls">>, sets:size(AStats#astats.resource_consumers)}
      ]).

-spec classify_channel(channel(), astats()) -> astats().
classify_channel(#channel{bridge_id='undefined', uuid=UUID}=Channel, AStats) ->
    classify_channel(Channel#channel{bridge_id=UUID}
                     ,AStats
                    );
classify_channel(#channel{direction = <<"outbound">>
                          ,account_billing = <<"flat_rate">>
                          ,bridge_id=BridgeId
                          ,uuid=UUID
                         }
                 ,#astats{outbound_flat_rate=OutboundFlatRates
                          ,resource_consumers=ResourceConsumers
                          ,bridge_ids=BridgeIds
                         }=AStats) ->
    AStats#astats{outbound_flat_rate=sets:add_element(UUID, OutboundFlatRates)
                  ,resource_consumers=sets:add_element(BridgeId, ResourceConsumers)
                  ,bridge_ids=sets:add_element(BridgeId, BridgeIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,account_billing = <<"flat_rate">>
                          ,bridge_id=BridgeId
                          ,uuid=UUID
                         }
                 ,#astats{inbound_flat_rate=InboundFlatRates
                          ,resource_consumers=ResourceConsumers
                          ,bridge_ids=BridgeIds
                         }=AStats) ->
    AStats#astats{inbound_flat_rate=sets:add_element(UUID, InboundFlatRates)
                  ,resource_consumers=sets:add_element(BridgeId, ResourceConsumers)
                  ,bridge_ids=sets:add_element(BridgeId, BridgeIds)
                 };
classify_channel(#channel{direction = <<"outbound">>
                          ,account_billing = <<"per_minute">>
                          ,bridge_id=BridgeId
                          ,uuid=UUID
                         }
                 ,#astats{outbound_per_minute=OutboundPerMinute
                          ,resource_consumers=ResourceConsumers
                          ,bridge_ids=BridgeIds
                         }=AStats) ->
    AStats#astats{outbound_per_minute=sets:add_element(UUID, OutboundPerMinute)
                  ,resource_consumers=sets:add_element(BridgeId, ResourceConsumers)
                  ,bridge_ids=sets:add_element(BridgeId, BridgeIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,account_billing = <<"per_minute">>
                          ,bridge_id=BridgeId
                          ,uuid=UUID
                         }
                 ,#astats{inbound_per_minute=InboundPerMinute
                          ,resource_consumers=ResourceConsumers
                          ,bridge_ids=BridgeIds
                         }=AStats) ->
    AStats#astats{inbound_per_minute=sets:add_element(UUID, InboundPerMinute)
                  ,resource_consumers=sets:add_element(BridgeId, ResourceConsumers)
                  ,bridge_ids=sets:add_element(BridgeId, BridgeIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,authorizing_id='undefined'
                          ,bridge_id=BridgeId
                         }
                 ,#astats{resource_consumers=ResourceConsumers
                          ,bridge_ids=BridgeIds
                         }=AStats) ->
    AStats#astats{resource_consumers=sets:add_element(BridgeId, ResourceConsumers)
                  ,bridge_ids=sets:add_element(BridgeId, BridgeIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,bridge_id=BridgeId
                         }
                 ,#astats{bridge_ids=BridgeIds}=AStats) ->
    AStats#astats{bridge_ids=sets:add_element(BridgeId, BridgeIds)};
classify_channel(#channel{direction = <<"outbound">>
                          ,resource_id='undefined'
                          ,bridge_id=BridgeId
                         }
                 ,#astats{bridge_ids=BridgeIds}=AStats) ->
    AStats#astats{bridge_ids=sets:add_element(BridgeId, BridgeIds)};
classify_channel(#channel{direction = <<"outbound">>
                          ,bridge_id=BridgeId
                         }
                 ,#astats{resource_consumers=ResourceConsumers
                          ,bridge_ids=BridgeIds
                         }=AStats) ->
    AStats#astats{resource_consumers=sets:add_element(BridgeId, ResourceConsumers)
                  ,bridge_ids=sets:add_element(BridgeId, BridgeIds)
                 }.

-spec channel_status_resp(ne_binary(), wh_json:object(), wh_json:object()) -> 'ok'.
channel_status_resp(CallId, Channel, JObj) ->
    Node = wh_json:get_binary_value(<<"node">>, Channel),
    [_, Hostname] = binary:split(Node, <<"@">>),
    lager:debug("channel is on ~s", [Hostname]),
    Resp = [{<<"Call-ID">>, CallId}
            ,{<<"Status">>, <<"active">>}
            ,{<<"Switch-Hostname">>, Hostname}
            ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec find_by_auth_id(ne_binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', 'not_found'}.
find_by_auth_id(AuthorizingId) ->
    MatchSpec = [{#channel{authorizing_id = '$1', _ = '_'}
                  ,[{'=:=', '$1', {'const', AuthorizingId}}]
                  ,['$_']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [] -> {'error', 'not_found'};
        Channels -> {'ok', [ecallmgr_fs_channel:to_json(Channel)
                            || Channel <- Channels
                           ]}
    end.
