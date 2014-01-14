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
-export([summary/0
         ,summary/1
        ]).
-export([details/0
         ,details/1
        ]).
-export([authz_summary/0]).
-export([show_all/0]).
-export([flush_node/1]).
-export([new/1]).
-export([destroy/2]).
-export([update/3
         ,updates/2
        ]).
-export([account_summary/1]).
-export([match_presence/1]).

-export([handle_query_auth_id/2]).
-export([handle_query_user_channels/2]).
-export([handle_query_account_channels/2]).
-export([handle_channel_status/2]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(RESPONDERS, [{{?MODULE, 'handle_query_auth_id'}
                      ,[{<<"call_event">>, <<"query_auth_id_req">>}]
                     }
                     ,{{?MODULE, 'handle_query_user_channels'}
                       ,[{<<"call_event">>, <<"query_user_channels_req">>}]
                      }
                     ,{{?MODULE, 'handle_query_account_channels'}
                       ,[{<<"call_event">>, <<"query_account_channels_req">>}]
                      }
                     ,{{?MODULE, 'handle_channel_status'}
                       ,[{<<"call_event">>, <<"channel_status_req">>}]
                      }
                    ]).
-define(BINDINGS, [{'call', [{'restrict_to', ['status_req']}]}]).
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
    gen_listener:start_link({'local', ?MODULE}, ?MODULE, [{'responders', ?RESPONDERS}
                                                          ,{'bindings', ?BINDINGS}
                                                          ,{'queue_name', ?QUEUE_NAME}
                                                          ,{'queue_options', ?QUEUE_OPTIONS}
                                                          ,{'consume_options', ?CONSUME_OPTIONS}
                                                         ], []).

-spec sync(atom(), ne_binaries()) -> 'ok'.
sync(Node, Channels) ->
    gen_server:cast(?MODULE, {'sync_channels', Node, Channels}).

-spec summary() -> 'ok'.
summary() ->
    MatchSpec = [{#channel{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_summary(ets:select(?CHANNELS_TBL, MatchSpec, 1)).

-spec summary(text()) -> 'ok'.
summary(Node) when not is_atom(Node) ->
    summary(wh_util:to_atom(Node, 'true'));
summary(Node) ->
    MatchSpec = [{#channel{node='$1', _ = '_'}
                  ,[{'=:=', '$1', {'const', Node}}]
                  ,['$_']
                 }],
    print_summary(ets:select(?CHANNELS_TBL, MatchSpec, 1)).

-spec details() -> 'ok'.
details() ->
    MatchSpec = [{#channel{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_details(ets:select(?CHANNELS_TBL, MatchSpec, 1)).

-spec details(text()) -> 'ok'.
details(UUID) when not is_binary(UUID) ->
    details(wh_util:to_binary(UUID));
details(UUID) ->
    MatchSpec = [{#channel{uuid='$1', _ = '_'}
                  ,[{'=:=', '$1', {'const', UUID}}]
                  ,['$_']
                 }],
    print_details(ets:select(?CHANNELS_TBL, MatchSpec, 1)).

-spec authz_summary() -> 'ok'.
authz_summary() ->
    MatchSpec = [{#channel{account_id='$1', _ = '_'}
                  ,[]
                  ,['$1']
                 }],
    AccountIds = sets:from_list(ets:select(?CHANNELS_TBL, MatchSpec)),
    print_authz_summary(sets:to_list(AccountIds)).

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

-spec destroy(ne_binary(), atom()) -> 'ok'.
destroy(UUID, Node) ->
    gen_server:cast(?MODULE, {'destroy_channel', UUID, Node}).

-spec update(ne_binary(), pos_integer(), _) -> 'ok'.
update(UUID, Key, Value) ->
    updates(UUID, [{Key, Value}]).

-spec updates(ne_binary(), wh_proplist()) -> 'ok'.
updates(UUID, Updates) ->
    gen_server:call(?MODULE, {'channel_updates', UUID, Updates}).

-spec account_summary(ne_binary()) -> wh_json:object().
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

-spec handle_query_auth_id(wh_json:object(), wh_proplist()) -> 'ok'.
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

-spec handle_query_user_channels(wh_json:object(), wh_proplist()) -> 'ok'.
handle_query_user_channels(JObj, _Props) ->
    'true' = wapi_call:query_user_channels_req_v(JObj),

    Realm = wh_json:get_value(<<"Realm">>, JObj),

    case wh_json:get_value(<<"Username">>, JObj) of
        'undefined' -> handle_query_users_channels(JObj, Realm);
        Username -> handle_query_user_channels(JObj, Username, Realm)
    end.

-spec handle_query_users_channels(wh_json:object(), ne_binary()) -> 'ok'.
handle_query_users_channels(JObj, Realm) ->
    case find_users_channels(wh_json:get_value(<<"Usernames">>, JObj), Realm) of
        {'error', 'not_found'} -> send_user_query_resp(JObj, []);
        {'error', _E} -> lager:debug("failed to lookup channels in realm ~s", [Realm]);
        {'ok', Cs} -> send_user_query_resp(JObj, Cs)
    end.

-spec handle_query_user_channels(wh_json:object(), ne_binary(), ne_binary()) -> 'ok'.
handle_query_user_channels(JObj, Username, Realm) ->
    case find_by_user_realm(Username, Realm) of
        {'error', 'not_found'} -> send_user_query_resp(JObj, []);
        {'error', _E} -> lager:debug("failed to lookup channels for ~s:~s", [Username, Realm]);
        {'ok', Cs} -> send_user_query_resp(JObj, Cs)
    end.

-spec send_user_query_resp(wh_json:object(), wh_json:objects()) -> 'ok'.
send_user_query_resp(JObj, Cs) ->
    Resp = [{<<"Channels">>, Cs}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    lager:debug("sending back channel data to ~s", [ServerId]),
    wapi_call:publish_query_user_channels_resp(ServerId, Resp).

-spec handle_query_account_channels(wh_json:object(), ne_binary()) -> 'ok'.
handle_query_account_channels(JObj, _) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    case find_account_channels(AccountId) of
        {'error', 'not_found'} -> send_account_query_resp(JObj, []);
        {'error', _E} -> lager:debug("failed to lookup channels for account ~s", [AccountId]);
        {'ok', Cs} -> send_account_query_resp(JObj, Cs)
    end.

-spec send_account_query_resp(wh_json:object(), wh_json:objects()) -> 'ok'.
send_account_query_resp(JObj, Cs) ->
    Resp = [{<<"Channels">>, Cs}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    lager:debug("sending back channel data to ~s", [ServerId]),
    wapi_call:publish_query_account_channels_resp(ServerId, Resp).

-spec handle_channel_status(wh_json:object(), wh_proplist()) -> 'ok'.
handle_channel_status(JObj, _Props) ->
    'true' = wapi_call:channel_status_req_v(JObj),
    _ = wh_util:put_callid(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("channel status request received"),
    case ecallmgr_fs_channel:fetch(CallId) of
        {'error', 'not_found'} ->
            lager:debug("no node found with channel ~s", [CallId]),
            Resp = [{<<"Call-ID">>, CallId}
                    ,{<<"Status">>, <<"terminated">>}
                    ,{<<"Error-Msg">>, <<"no node found with channel">>}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        {'ok', Channel} ->
            Node = wh_json:get_binary_value(<<"node">>, Channel),
            [_, Hostname] = binary:split(Node, <<"@">>),
            lager:debug("channel is on ~s", [Hostname]),
            Resp = [{<<"Call-ID">>, CallId}
                    ,{<<"Status">>, <<"active">>}
                    ,{<<"Switch-Hostname">>, Hostname}
                    ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
                    ,{<<"Switch-URL">>, ecallmgr_fs_nodes:sip_url(Node)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
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
handle_call({'channel_updates', UUID, Update}, _, State) ->
    ets:update_element(?CHANNELS_TBL, UUID, Update),
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
    lager:debug("unhandled cast: ~p", [_Req]),
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
    ets:delete(?CHANNELS_TBL),
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

-spec find_by_user_realm(ne_binary(), ne_binary()) ->
                                {'ok', wh_json:objects()} |
                                {'error', 'not_found'}.
find_by_user_realm(Username, Realm) ->
    MatchSpec = [{#channel{username = '$1', realm='$2', _ = '_'}
                  ,[{'=:=', '$1', {'const', Username}}
                    ,{'=:=', '$2', {'const', Realm}}
                   ]
                  ,['$_']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [] -> {'error', 'not_found'};
        Channels ->
            {'ok', [ecallmgr_fs_channel:to_json(Channel)
                    || Channel <- Channels
                   ]}
    end.

-spec find_users_channels(ne_binaries(), ne_binary()) ->
                                 {'ok', wh_json:objects()} |
                                 {'error', 'not_found'}.
find_users_channels(Usernames, Realm) ->
    ETSUsernames = build_matchspec_ors(Usernames),
    MatchSpec = [{#channel{username='$1', realm='$2', _ = '_'}
                  ,[ETSUsernames
                    ,{'=:=', '$2', {'const', Realm}}
                   ]
                  ,['$_']
                 }],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [] -> {'error', 'not_found'};
        Channels ->
            {'ok', [ecallmgr_fs_channel:to_json(Channel)
                    || Channel <- Channels
                   ]}
    end.

-spec find_account_channels(ne_binaries()) ->
                                 {'ok', wh_json:objects()} |
                                 {'error', 'not_found'}.
find_account_channels(AccountId) ->
    case ets:match_object(?CHANNELS_TBL, #channel{account_id=AccountId, _='_'}) of
        [] -> {'error', 'not_found'};
        Channels ->
            {'ok', [ecallmgr_fs_channel:to_json(Channel)
                    || Channel <- Channels
                   ]}
    end.

build_matchspec_ors(L) ->
    lists:foldl(fun(El, Acc) -> {'or', {'=:=', '$1', El}, Acc} end, 'false', L).

print_summary('$end_of_table') ->
    io:format("No channels found!~n", []);
print_summary(Match) ->
    io:format("+----------------------------------------------------+------------------------------------------+-----------+-----------------+----------------------------------+~n"),
    io:format("| UUID                                               | Node                                     | Direction | Destination     | Account-ID                       |~n"),
    io:format("+====================================================+==========================================+===========+=================+==================================+~n"),
    print_summary(Match, 0).

print_summary('$end_of_table', Count) ->
    io:format("+----------------------------------------------------+------------------------------------------+-----------+-----------------+----------------------------------+~n"),
    io:format("Found ~p channels~n", [Count]);
print_summary({[#channel{uuid=UUID
                         ,node=Node
                         ,direction=Direction
                         ,destination=Destination
                         ,account_id=AccountId
                        }]
               ,Continuation}
              ,Count) ->
    io:format("| ~-50s | ~-40s | ~-9s | ~-15s | ~-32s |~n"
              ,[UUID, Node, Direction, Destination, AccountId]),
    print_summary(ets:select(Continuation), Count + 1).

print_details('$end_of_table') ->
    io:format("No channels found!~n", []);
print_details(Match) ->
    print_details(Match, 0).

print_details('$end_of_table', Count) ->
    io:format("~nFound ~p channels~n", [Count]);
print_details({[#channel{}=Channel]
               ,Continuation}
              ,Count) ->
    io:format("~n"),
    _ = [io:format("~-19s: ~s~n", [K, wh_util:to_binary(V)])
         || {K, V} <- ecallmgr_fs_channel:to_props(Channel)
        ],
    print_details(ets:select(Continuation), Count + 1).

-spec print_authz_summary(ne_binaries()) -> 'ok'.
-spec print_authz_summary(ne_binaries(), non_neg_integer()) -> 'ok'.

print_authz_summary([]) ->
    io:format("No accounts found!~n");
print_authz_summary(AccountIds) ->
    io:format("+----------------------------------+------------+------------+-------------------------+-------------------------+------------+~n"),
    io:format("| Account ID                       | Calls      | Channels   |        Flat-Rate        |        Per-Minute       | Resources  |~n"),
    io:format("|                                  |            |            | Inbound    | Outbound   | Inbound    | Outbound   |            |~n"),
    io:format("+==================================+============+============+============+============+============+============+============+"),
    print_authz_summary(AccountIds, 0).

print_authz_summary([], Count) ->
    io:format("~n+----------------------------------+------------+------------+------------+------------+------------+------------+------------+~n"),
    io:format("Found ~p accounts~n", [Count]);
print_authz_summary([AccountId|AccountIds], Count) ->
    JObj = account_summary(AccountId),
    io:format("~n| ~-32s | ~-10s | ~-10s | ~-10s | ~-10s | ~-10s | ~-10s | ~-10s |"
              ,[AccountId
                ,wh_json:get_binary_value(<<"Calls">>, JObj, 0)
                ,wh_json:get_binary_value(<<"Channels">>, JObj, 0)
                ,wh_json:get_binary_value(<<"Inbound-Flat-Rate">>, JObj, 0)
                ,wh_json:get_binary_value(<<"Outbound-Flat-Rate">>, JObj, 0)
                ,wh_json:get_binary_value(<<"Inbound-Per-Minute">>, JObj, 0)
                ,wh_json:get_binary_value(<<"Outbound-Per-Minute">>, JObj, 0)
                ,wh_json:get_binary_value(<<"Resource-Consuming-Calls">>, JObj, 0)
               ]),
    print_authz_summary(AccountIds, Count + 1).
