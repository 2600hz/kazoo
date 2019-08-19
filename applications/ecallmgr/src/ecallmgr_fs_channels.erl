%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
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
-export([show_all/0]).
-export([per_minute_accounts/0]).
-export([per_minute_channels/1]).
-export([flush_node/1]).
-export([new/1]).
-export([destroy/2]).
-export([update/3
        ,updates/2
        ,cleanup_old_channels/0, cleanup_old_channels/1
        ,max_channel_uptime/0
        ,set_max_channel_uptime/1, set_max_channel_uptime/2
        ]).
-export([match_presence/1]).
-export([count/0]).

-export([handle_query_auth_id/2]).
-export([handle_query_user_channels/2]).
-export([handle_query_account_channels/2]).
-export([handle_query_channels/2]).
-export([handle_channel_status/2]).

-export([has_channels_for_owner/1]).

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

-define(RESPONDERS, [{{?MODULE, 'handle_query_auth_id'}
                     ,[{<<"call_event">>, <<"query_auth_id_req">>}]
                     }
                    ,{{?MODULE, 'handle_query_user_channels'}
                     ,[{<<"call_event">>, <<"query_user_channels_req">>}]
                     }
                    ,{{?MODULE, 'handle_query_account_channels'}
                     ,[{<<"call_event">>, <<"query_account_channels_req">>}]
                     }
                    ,{{?MODULE, 'handle_query_channels'}
                     ,[{<<"call_event">>, <<"query_channels_req">>}]
                     }
                    ,{{?MODULE, 'handle_channel_status'}
                     ,[{<<"call_event">>, <<"channel_status_req">>}]
                     }
                    ]).
-define(BINDINGS, [{'call', [{'restrict_to', ['status_req']}
                            ,'federate'
                            ]}
                  ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(CALL_PARK_FEATURE, "*3").
-record(state, {max_channel_cleanup_ref :: reference()}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'responders', ?RESPONDERS}
                                                         ,{'bindings', ?BINDINGS}
                                                         ,{'queue_name', ?QUEUE_NAME}
                                                         ,{'queue_options', ?QUEUE_OPTIONS}
                                                         ,{'consume_options', ?CONSUME_OPTIONS}
                                                         ], []).

-spec sync(atom(), kz_term:ne_binaries()) -> 'ok'.
sync(Node, Channels) ->
    gen_server:cast(?SERVER, {'sync_channels', Node, Channels}).

-spec summary() -> 'ok'.
summary() ->
    MatchSpec = [{#channel{_ = '_'}
                 ,[]
                 ,['$_']
                 }],
    print_summary(ets:select(?CHANNELS_TBL, MatchSpec, 1)).

-spec summary(kz_term:text()) -> 'ok'.
summary(Node) when not is_atom(Node) ->
    summary(kz_term:to_atom(Node, 'true'));
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

-spec details(kz_term:text()) -> 'ok'.
details(UUID) when not is_binary(UUID) ->
    details(kz_term:to_binary(UUID));
details(UUID) ->
    MatchSpec = [{#channel{uuid='$1', _ = '_'}
                 ,[{'=:=', '$1', {'const', UUID}}]
                 ,['$_']
                 }],
    print_details(ets:select(?CHANNELS_TBL, MatchSpec, 1)).

-spec show_all() -> kz_json:objects().
show_all() ->
    ets:foldl(fun(Channel, Acc) ->
                      [ecallmgr_fs_channel:to_json(Channel) | Acc]
              end, [], ?CHANNELS_TBL).

-spec per_minute_accounts() -> kz_term:ne_binaries().
per_minute_accounts() ->
    MatchSpec = [{#channel{account_id = '$1'
                          ,account_billing = <<"per_minute">>
                          ,reseller_id = '$2'
                          ,reseller_billing = <<"per_minute">>
                          ,_ = '_'}
                 ,[{'andalso', {'=/=', '$1', 'undefined'}, {'=/=', '$2', 'undefined'}}]
                 ,['$$']
                 }
                ,{#channel{reseller_id = '$1', reseller_billing = <<"per_minute">>, _ = '_'}
                 ,[{'=/=', '$1', 'undefined'}]
                 ,['$$']
                 }
                ,{#channel{account_id = '$1', account_billing = <<"per_minute">>, _ = '_'}
                 ,[{'=/=', '$1', 'undefined'}]
                 ,['$$']
                 }
                ],
    lists:usort(lists:flatten(ets:select(?CHANNELS_TBL, MatchSpec))).

-spec per_minute_channels(kz_term:ne_binary()) -> [{atom(), kz_term:ne_binary()}].
per_minute_channels(AccountId) ->
    MatchSpec = [{#channel{node = '$1'
                          ,uuid = '$2'
                          ,reseller_id = AccountId
                          ,reseller_billing = <<"per_minute">>
                          ,_ = '_'
                          }
                 ,[]
                 ,[{{'$1', '$2'}}]
                 }
                ,{#channel{node = '$1'
                          ,uuid = '$2'
                          ,account_id = AccountId
                          ,account_billing = <<"per_minute">>
                          ,_ = '_'
                          }
                 ,[]
                 ,[{{'$1', '$2'}}]
                 }
                ],
    ets:select(?CHANNELS_TBL, MatchSpec).

-spec flush_node(string() | binary() | atom()) -> 'ok'.
flush_node(Node) ->
    gen_server:cast(?SERVER, {'flush_node', kz_term:to_atom(Node, 'true')}).

-spec new(channel()) -> 'ok'.
new(#channel{}=Channel) ->
    gen_server:call(?SERVER, {'new_channel', Channel}).

-spec destroy(kz_term:ne_binary(), atom()) -> 'ok'.
destroy(UUID, Node) ->
    gen_server:cast(?SERVER, {'destroy_channel', UUID, Node}).

-spec update(kz_term:ne_binary(), pos_integer(), any()) -> 'ok'.
update(UUID, Key, Value) ->
    updates(UUID, [{Key, Value}]).

-spec updates(kz_term:ne_binary(), channel_updates()) -> 'ok'.
updates(UUID, Updates) ->
    lager:debug("updating channel properties: ~p", [Updates]),
    gen_server:cast(?SERVER, {'channel_updates', UUID, Updates}).

-spec count() -> non_neg_integer().
count() -> ets:info(?CHANNELS_TBL, 'size').

-spec match_presence(kz_term:ne_binary()) -> kz_term:proplist_kv(kz_term:ne_binary(), atom()).
match_presence(PresenceId) ->
    MatchSpec = [{#channel{uuid = '$1'
                          ,presence_id = '$2'
                          ,node = '$3'
                          , _ = '_'}
                 ,[{'=:=', '$2', {'const', PresenceId}}]
                 ,[{{'$1', '$3'}}]}
                ],
    ets:select(?CHANNELS_TBL, MatchSpec).

-spec handle_query_auth_id(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_query_auth_id(JObj, _Props) ->
    'true' = kapi_call:query_auth_id_req_v(JObj),
    AuthId = kz_json:get_ne_binary_value(<<"Auth-ID">>, JObj),
    Channels = case find_by_auth_id(AuthId) of
                   {'error', 'not_found'} -> [];
                   {'ok', C} -> C
               end,
    Resp = [{<<"Channels">>, Channels}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    kapi_call:publish_query_auth_id_resp(ServerId, Resp).

-spec handle_query_user_channels(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_query_user_channels(JObj, _Props) ->
    'true' = kapi_call:query_user_channels_req_v(JObj),
    UserChannels0 = case kz_json:get_value(<<"Realm">>, JObj) of
                        'undefined' -> [];
                        Realm ->
                            Usernames = kz_json:get_first_defined([<<"Username">>
                                                                  ,<<"Usernames">>
                                                                  ], JObj),
                            find_by_user_realm(Usernames, Realm)
                    end,
    UserChannels1 = case kz_json:get_value(<<"Authorizing-IDs">>, JObj) of
                        'undefined' -> [];
                        AuthIds -> find_by_authorizing_id(AuthIds)
                    end,
    UserChannels2 = lists:keymerge(1, UserChannels0, UserChannels1),
    handle_query_users_channels(JObj, UserChannels2).

-spec handle_query_users_channels(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_query_users_channels(JObj, Cs) ->
    Channels = [Channel || {_, Channel} <- Cs],
    send_user_query_resp(JObj, Channels).

-spec send_user_query_resp(kz_json:object(), kz_json:objects()) -> 'ok'.
send_user_query_resp(JObj, []) ->
    case kz_json:is_true(<<"Active-Only">>, JObj, 'true') of
        'true' -> lager:debug("no channels, not sending response");
        'false' ->
            lager:debug("no channels, sending empty response"),
            Resp = [{<<"Channels">>, []}
                   ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
            lager:debug("sending back channel data to ~s", [ServerId]),
            kapi_call:publish_query_user_channels_resp(ServerId, Resp)
    end;
send_user_query_resp(JObj, Cs) ->
    Resp = [{<<"Channels">>, Cs}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    lager:debug("sending back channel data to ~s", [ServerId]),
    kapi_call:publish_query_user_channels_resp(ServerId, Resp).

-spec handle_query_account_channels(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_query_account_channels(JObj, _) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    case find_account_channels(AccountId) of
        {'error', 'not_found'} -> send_account_query_resp(JObj, []);
        {'ok', Cs} -> send_account_query_resp(JObj, Cs)
    end.

-spec send_account_query_resp(kz_json:object(), kz_json:objects()) -> 'ok'.
send_account_query_resp(JObj, Cs) ->
    Resp = [{<<"Channels">>, Cs}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    lager:debug("sending back channel data to ~s", [ServerId]),
    kapi_call:publish_query_account_channels_resp(ServerId, Resp).

-spec handle_query_channels(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_query_channels(JObj, _Props) ->
    'true' = kapi_call:query_channels_req_v(JObj),
    Fields = kz_json:get_value(<<"Fields">>, JObj, []),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    Channels = query_channels(Fields, CallId),
    case kz_term:is_empty(Channels) and
        kz_json:is_true(<<"Active-Only">>, JObj, 'false')
    of
        'true' ->
            lager:debug("not sending query_channels resp due to active-only=true");
        'false' ->
            Resp = [{<<"Channels">>, Channels}
                   ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_call:publish_query_channels_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec handle_channel_status(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_channel_status(JObj, _Props) ->
    'true' = kapi_call:channel_status_req_v(JObj),
    _ = kz_util:put_callid(JObj),
    CallId = kz_api:call_id(JObj),
    lager:debug("channel status request received"),
    case ecallmgr_fs_channel:fetch(CallId) of
        {'error', 'not_found'} ->
            maybe_send_empty_channel_resp(CallId, JObj);
        {'ok', Channel} ->
            Node = kz_json:get_binary_value(<<"node">>, Channel),
            [_, Hostname] = binary:split(Node, <<"@">>),
            lager:debug("channel is on ~s", [Hostname]),
            Resp =
                props:filter_undefined(
                  [{<<"Call-ID">>, CallId}
                  ,{<<"Status">>, <<"active">>}
                  ,{<<"Switch-Hostname">>, Hostname}
                  ,{<<"Switch-Nodename">>, kz_term:to_binary(Node)}
                  ,{<<"Switch-URL">>, ecallmgr_fs_nodes:sip_url(Node)}
                  ,{<<"Other-Leg-Call-ID">>, kz_json:get_value(<<"other_leg">>, Channel)}
                  ,{<<"Realm">>, kz_json:get_value(<<"realm">>, Channel)}
                  ,{<<"Username">>, kz_json:get_value(<<"username">>, Channel)}
                  ,{<<"Custom-Channel-Vars">>, kz_json:from_list(ecallmgr_fs_channel:channel_ccvs(Channel))}
                  ,{<<"Custom-Application-Vars">>, kz_json:from_list(ecallmgr_fs_channel:channel_cavs(Channel))}
                  ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                  ]
                 ),
            kapi_call:publish_channel_status_resp(kz_api:server_id(JObj), Resp)
    end.

-spec maybe_send_empty_channel_resp(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_empty_channel_resp(CallId, JObj) ->
    case kz_json:is_true(<<"Active-Only">>, JObj) of
        'true' -> 'ok';
        'false' -> send_empty_channel_resp(CallId, JObj)
    end.

-spec send_empty_channel_resp(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_empty_channel_resp(CallId, JObj) ->
    Resp = [{<<"Call-ID">>, CallId}
           ,{<<"Status">>, <<"terminated">>}
           ,{<<"Error-Msg">>, <<"no node found with channel">>}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_call:publish_channel_status_resp(kz_api:server_id(JObj), Resp).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:debug("starting new fs channels"),
    _ = ets:new(?CHANNELS_TBL, ['set'
                               ,'protected'
                               ,'named_table'
                               ,{'keypos', #channel.uuid}
                               ]),
    {'ok', #state{max_channel_cleanup_ref=start_cleanup_ref()}}.

-define(CLEANUP_TIMEOUT
       ,kapps_config:get_integer(?APP_NAME, <<"max_channel_cleanup_timeout_ms">>, ?MILLISECONDS_IN_MINUTE)
       ).

-spec start_cleanup_ref() -> reference().
start_cleanup_ref() ->
    erlang:start_timer(?CLEANUP_TIMEOUT, self(), 'ok').

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'new_channel', Channel}, _, State) ->
    ets:insert(?CHANNELS_TBL, Channel),
    {'reply', 'ok', State};
handle_call({'channel_updates', UUID, Update}, _, State) ->
    ets:update_element(?CHANNELS_TBL, UUID, Update),
    {'reply', 'ok', State};
handle_call(_, _, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'channel_updates', UUID, Update}, State) ->
    ets:update_element(?CHANNELS_TBL, UUID, Update),
    {'noreply', State};
handle_cast({'destroy_channel', UUID, Node}, State) ->
    kz_util:put_callid(UUID),
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
             case ets:lookup(?CHANNELS_TBL, UUID) of
                 [#channel{handling_locally='true'}=Channel] ->
                     lager:debug("emitting channel disconnect ~s during sync with ~s", [UUID, Node]),
                     handle_channel_disconnected(Channel),
                     ets:delete(?CHANNELS_TBL, UUID);
                 [_Channel] ->
                     lager:debug("removed channel ~s from cache during sync with ~s", [UUID, Node]),
                     ets:delete(?CHANNELS_TBL, UUID);
                 [] ->
                     lager:debug("channel ~s not found during sync delete with ~s", [UUID, Node])
             end
         end
         || UUID <- sets:to_list(Remove)
        ],
    _ = [begin
             lager:debug("trying to add channel ~s to cache during sync with ~s", [UUID, Node]),
             case ecallmgr_fs_channel:renew(Node, UUID) of
                 {'error', _R} -> lager:warning("failed to sync channel ~s: ~p", [UUID, _R]);
                 {'ok', C} ->
                     lager:debug("added channel ~s to cache during sync with ~s", [UUID, Node]),
                     ets:insert(?CHANNELS_TBL, C),
                     PublishReconect = kapps_config:get_boolean(?APP_NAME, <<"publish_channel_reconnect">>, 'false'),
                     handle_channel_reconnected(C, PublishReconect)
             end
         end
         || UUID <- sets:to_list(Add)
        ],
    {'noreply', State, 'hibernate'};
handle_cast({'flush_node', Node}, State) ->
    lager:debug("flushing all channels in cache associated to node ~s", [Node]),

    LocalChannelsMS = [{#channel{node = '$1'
                                ,handling_locally='true'
                                ,_ = '_'
                                }
                       ,[{'=:=', '$1', {'const', Node}}]
                       ,['$_']}
                      ],
    case ets:select(?CHANNELS_TBL, LocalChannelsMS) of
        [] ->
            lager:debug("no locally handled channels");
        LocalChannels ->
            _P = kz_util:spawn(fun handle_channels_disconnected/1, [LocalChannels]),
            lager:debug("sending channel disconnects for local channels: ~p", [LocalChannels])
    end,

    MatchSpec = [{#channel{node = '$1', _ = '_'}
                 ,[{'=:=', '$1', {'const', Node}}]
                 ,['true']}
                ],
    ets:select_delete(?CHANNELS_TBL, MatchSpec),
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue', _QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', Ref, _Msg}, #state{max_channel_cleanup_ref=Ref}=State) ->
    maybe_cleanup_old_channels(),
    {'noreply', State#state{max_channel_cleanup_ref=start_cleanup_ref()}};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{}) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{}) ->
    ets:delete(?CHANNELS_TBL),
    lager:info("fs channels terminating: ~p", [_Reason]).

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
-spec find_by_auth_id(kz_term:ne_binary()) ->
                             {'ok', kz_json:objects()} |
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

-spec has_channels_for_owner(kz_term:ne_binary()) -> boolean().
has_channels_for_owner(OwnerId) ->
    MatchSpec = [{#channel{owner_id = '$1'
                          ,_ = '_'
                          }
                 ,[]
                 ,[{'=:=', '$1', {const, OwnerId}}]
                 }
                ],
    Count = ets:select_count(?CHANNELS_TBL, MatchSpec),
    lager:info("Found ~p channels", [Count]),
    Count > 0.

-spec find_by_authorizing_id(kz_term:ne_binaries()) -> [] | kz_term:proplist().
find_by_authorizing_id(AuthIds) ->
    find_by_authorizing_id(AuthIds, []).

-spec find_by_authorizing_id(kz_term:ne_binaries(), kz_term:proplist()) -> [] | kz_term:proplist().
find_by_authorizing_id([], Acc) -> Acc;
find_by_authorizing_id([AuthId|AuthIds], Acc) ->
    Pattern = #channel{authorizing_id=AuthId
                      ,_='_'},
    case ets:match_object(?CHANNELS_TBL, Pattern) of
        [] -> find_by_authorizing_id(AuthIds, Acc);
        Channels ->
            Cs = [{Channel#channel.uuid, ecallmgr_fs_channel:to_json(Channel)}
                  || Channel <- Channels
                 ],
            find_by_authorizing_id(AuthIds, lists:keymerge(1, Acc, Cs))
    end.

-spec find_by_user_realm(kz_term:api_binary() | kz_term:ne_binaries(), kz_term:ne_binary()) -> [] | kz_term:proplist().
find_by_user_realm('undefined', Realm) ->
    Pattern = #channel{realm=kz_term:to_lower_binary(Realm)
                      ,_='_'},
    case ets:match_object(?CHANNELS_TBL, Pattern) of
        [] -> [];
        Channels ->
            [{Channel#channel.uuid, ecallmgr_fs_channel:to_json(Channel)}
             || Channel <- Channels
            ]
    end;
find_by_user_realm(<<?CALL_PARK_FEATURE, _/binary>>=Username, Realm) ->
    Pattern = #channel{destination=kz_term:to_lower_binary(Username)
                      ,realm=kz_term:to_lower_binary(Realm)
                      ,other_leg='undefined'
                      ,_='_'},
    case ets:match_object(?CHANNELS_TBL, Pattern) of
        [] -> [];
        Channels ->
            [{Channel#channel.uuid, ecallmgr_fs_channel:to_json(Channel)}
             || Channel <- Channels
            ]
    end;
find_by_user_realm(Usernames, Realm) when is_list(Usernames) ->
    ETSUsernames = build_matchspec_ors(Usernames),
    MatchSpec = [{#channel{username='$1'
                          ,realm=kz_term:to_lower_binary(Realm)
                          ,_ = '_'}
                 ,[ETSUsernames]
                 ,['$_']
                 }],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [] -> [];
        Channels ->
            [{Channel#channel.uuid, ecallmgr_fs_channel:to_json(Channel)}
             || Channel <- Channels
            ]
    end;
find_by_user_realm(Username, Realm) ->
    Pattern = #channel{username=kz_term:to_lower_binary(Username)
                      ,realm=kz_term:to_lower_binary(Realm)
                      ,_='_'},
    case ets:match_object(?CHANNELS_TBL, Pattern) of
        [] -> [];
        Channels ->
            [{Channel#channel.uuid, ecallmgr_fs_channel:to_json(Channel)}
             || Channel <- Channels
            ]
    end.

-spec find_account_channels(kz_term:ne_binary()) ->
                                   {'ok', kz_json:objects()} |
                                   {'error', 'not_found'}.
find_account_channels(<<"all">>) ->
    case ets:match_object(?CHANNELS_TBL, #channel{_='_'}) of
        [] -> {'error', 'not_found'};
        Channels ->
            {'ok', [ecallmgr_fs_channel:to_json(Channel)
                    || Channel <- Channels
                   ]}
    end;
find_account_channels(AccountId) ->
    case ets:match_object(?CHANNELS_TBL, #channel{account_id=AccountId, _='_'}) of
        [] -> {'error', 'not_found'};
        Channels ->
            {'ok', [ecallmgr_fs_channel:to_json(Channel)
                    || Channel <- Channels
                   ]}
    end.

-spec build_matchspec_ors(kz_term:ne_binaries()) -> tuple() | 'false'.
build_matchspec_ors(Usernames) ->
    lists:foldl(fun build_matchspec_ors_fold/2
               ,'false'
               ,Usernames
               ).

-spec build_matchspec_ors_fold(kz_term:ne_binary(), tuple() | 'false') -> tuple().
build_matchspec_ors_fold(Username, Acc) ->
    {'or', {'=:=', '$1', kz_term:to_lower_binary(Username)}, Acc}.

-spec query_channels(kz_term:ne_binaries(), kz_term:api_binary()) -> kz_json:object().
query_channels(Fields, 'undefined') ->
    query_channels(ets:match_object(?CHANNELS_TBL, #channel{_='_'}, 1)
                  ,Fields
                  ,kz_json:new()
                  );
query_channels(Fields, CallId) ->
    query_channels(ets:match_object(?CHANNELS_TBL, #channel{uuid=CallId, _='_'}, 1)
                  ,Fields
                  ,kz_json:new()
                  ).

-spec query_channels({[channel()], ets:continuation()} | '$end_of_table', kz_term:ne_binary() | kz_term:ne_binaries(), kz_json:object()) ->
                            kz_json:object().
query_channels('$end_of_table', _, Channels) -> Channels;
query_channels({[#channel{uuid=CallId}=Channel], Continuation}
              ,<<"all">>, Channels) ->
    JObj = ecallmgr_fs_channel:to_api_json(Channel),
    query_channels(ets:match_object(Continuation)
                  ,<<"all">>
                  ,kz_json:set_value(CallId, JObj, Channels)
                  );
query_channels({[#channel{uuid=CallId}=Channel], Continuation}
              ,Fields, Channels) ->
    ChannelProps = ecallmgr_fs_channel:to_api_props(Channel),
    JObj = kz_json:from_list(
             [{Field, props:get_value(Field, ChannelProps)}
              || Field <- Fields
             ]),
    query_channels(ets:match_object(Continuation)
                  ,Fields
                  ,kz_json:set_value(CallId, JObj, Channels)
                  ).

-define(SUMMARY_HEADER, "| ~-50s | ~-40s | ~-9s | ~-15s | ~-32s |~n").

print_summary('$end_of_table') ->
    io:format("No channels found!~n", []);
print_summary(Match) ->
    io:format("+----------------------------------------------------+------------------------------------------+-----------+-----------------+----------------------------------+~n"),
    io:format(?SUMMARY_HEADER
             ,[ <<"UUID">>, <<"Node">>, <<"Direction">>, <<"Destination">>, <<"Account-ID">>]
             ),
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
    io:format(?SUMMARY_HEADER
             ,[UUID, Node, Direction, Destination, AccountId]
             ),
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
    _ = [io:format("~-19s: ~s~n", [K, kz_term:to_binary(V)])
         || {K, V} <- ecallmgr_fs_channel:to_props(Channel)
        ],
    print_details(ets:select(Continuation), Count + 1).

-spec handle_channel_reconnected(channel(), boolean()) -> 'ok'.
handle_channel_reconnected(#channel{handling_locally='true'
                                   ,uuid=_UUID
                                   }=Channel
                          ,'true') ->
    lager:debug("channel ~s connected, publishing update", [_UUID]),
    publish_channel_connection_event(Channel, [{<<"Event-Name">>, <<"CHANNEL_CONNECTED">>}]);
handle_channel_reconnected(_Channel, _ShouldPublish) ->
    'ok'.

-spec handle_channels_disconnected(channels()) -> 'ok'.
handle_channels_disconnected(LocalChannels) ->
    _ = [catch handle_channel_disconnected(LocalChannel) || LocalChannel <- LocalChannels],
    'ok'.

-spec handle_channel_disconnected(channel()) -> 'ok'.
handle_channel_disconnected(Channel) ->
    publish_channel_connection_event(Channel, [{<<"Event-Name">>, <<"CHANNEL_DISCONNECTED">>}]).

-spec publish_channel_connection_event(channel(), kz_term:proplist()) -> 'ok'.
publish_channel_connection_event(#channel{uuid=UUID
                                         ,direction=Direction
                                         ,node=Node
                                         ,destination=Destination
                                         ,username=Username
                                         ,realm=Realm
                                         ,presence_id=PresenceId
                                         ,answered=IsAnswered
                                         }=Channel
                                ,ChannelSpecific) ->
    Event = [{<<"Timestamp">>, kz_time:now_s()}
            ,{<<"Call-ID">>, UUID}
            ,{<<"Call-Direction">>, Direction}
            ,{<<"Media-Server">>, Node}
            ,{<<"Custom-Channel-Vars">>, connection_ccvs(Channel)}
            ,{<<"Custom-Application-Vars">>, connection_cavs(Channel)}
            ,{<<"To">>, safe_uri(Destination, Realm)}
            ,{<<"From">>, safe_uri(Username, Realm)}
            ,{<<"Presence-ID">>, PresenceId}
            ,{<<"Channel-Call-State">>, channel_call_state(IsAnswered)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++ ChannelSpecific
            ],
    _ = kz_amqp_worker:cast(Event, fun kapi_call:publish_event/1),
    lager:debug("published channel connection event (~s) for ~s", [kz_api:event_name(Event), UUID]).

-spec safe_uri(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
safe_uri(User, Realm)
  when is_binary(User)
       andalso is_binary(Realm) ->
    <<User/binary, "@", Realm/binary>>;
safe_uri(_, _) -> 'undefined'.

-spec channel_call_state(boolean()) -> kz_term:api_binary().
channel_call_state('true') ->
    <<"ANSWERED">>;
channel_call_state('false') ->
    'undefined'.

-spec connection_ccvs(channel()) -> kz_json:object().
connection_ccvs(#channel{account_id=AccountId
                        ,authorizing_id=AuthorizingId
                        ,authorizing_type=AuthorizingType
                        ,resource_id=ResourceId
                        ,fetch_id=FetchId
                        ,bridge_id=BridgeId
                        ,owner_id=OwnerId
                        }) ->
    kz_json:from_list(
      [{<<"Account-ID">>, AccountId}
      ,{<<"Authorizing-ID">>, AuthorizingId}
      ,{<<"Authorizing-Type">>, AuthorizingType}
      ,{<<"Resource-ID">>, ResourceId}
      ,{<<"Fetch-ID">>, FetchId}
      ,{<<"Bridge-ID">>, BridgeId}
      ,{<<"Owner-ID">>, OwnerId}
      ]).

-spec connection_cavs(channel()) -> kz_term:api_object().
connection_cavs(#channel{cavs=CAVs}) when is_list(CAVs) ->
    kz_json:from_list(CAVs);
connection_cavs(#channel{}) -> 'undefined'.


-define(MAX_CHANNEL_UPTIME_KEY, <<"max_channel_uptime_s">>).

-spec max_channel_uptime() -> non_neg_integer().
max_channel_uptime() ->
    kapps_config:get_integer(?APP_NAME, ?MAX_CHANNEL_UPTIME_KEY, 0).

-spec set_max_channel_uptime(non_neg_integer()) -> 'ok'.
set_max_channel_uptime(MaxAge) ->
    set_max_channel_uptime(MaxAge, 'true').

-spec set_max_channel_uptime(non_neg_integer(), boolean()) -> 'ok'.
set_max_channel_uptime(MaxAge, 'true') ->
    kapps_config:set_default(?APP_NAME, ?MAX_CHANNEL_UPTIME_KEY, kz_term:to_integer(MaxAge));
set_max_channel_uptime(MaxAge, 'false') ->
    kapps_config:set(?APP_NAME, ?MAX_CHANNEL_UPTIME_KEY, kz_term:to_integer(MaxAge)).

-spec maybe_cleanup_old_channels() -> 'ok'.
maybe_cleanup_old_channels() ->
    case max_channel_uptime() of
        N when N =< 0 -> 'ok';
        MaxAge ->
            _P = kz_util:spawn(fun cleanup_old_channels/1, [MaxAge]),
            'ok'
    end.

-spec cleanup_old_channels() -> non_neg_integer().
cleanup_old_channels() ->
    cleanup_old_channels(max_channel_uptime()).

-spec cleanup_old_channels(non_neg_integer()) -> non_neg_integer().
cleanup_old_channels(MaxAge) ->
    NoOlderThan = kz_time:now_s() - MaxAge,

    MatchSpec = [{#channel{uuid='$1'
                          ,node='$2'
                          ,timestamp='$3'
                          ,handling_locally='true'
                          ,_ = '_'
                          }
                 ,[{'<', '$3', NoOlderThan}]
                 ,[['$1', '$2', '$3']]
                 }],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [] -> 0;
        OldChannels ->
            N = length(OldChannels),
            lager:debug("~p channels over ~p seconds old", [N, MaxAge]),
            hangup_old_channels(OldChannels),
            N
    end.

-type old_channel() :: [kz_term:ne_binary() | atom() | kz_time:gregorian_seconds()].
-type old_channels() :: [old_channel(),...].

-spec hangup_old_channels(old_channels()) -> 'ok'.
hangup_old_channels(OldChannels) ->
    lists:foreach(fun hangup_old_channel/1, OldChannels).

-spec hangup_old_channel(old_channel()) -> 'ok'.
hangup_old_channel([UUID, Node, Started]) ->
    lager:debug("killing channel ~s on ~s, started ~s"
               ,[UUID, Node, kz_time:pretty_print_datetime(Started)]),
    freeswitch:api(Node, 'uuid_kill', UUID).
