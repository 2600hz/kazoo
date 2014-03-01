%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_channels).

-behaviour(gen_listener).

-export([start_link/0]).
-export([sync/0]).
-export([total_calls/1]).
-export([resource_consuming/1]).
-export([inbound_flat_rate/1]).
-export([outbound_flat_rate/1]).
-export([allotments/1]).
-export([allotment_consumed/4]).
-export([per_minute/1]).
-export([per_minute_cost/1]).
-export([accounts/0]).
-export([account/1]).
-export([to_props/1]).
-export([authorized/1]).
-export([remove/1]).
-export([handle_authz_resp/2]).
-export([handle_rate_resp/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("jonny5.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-record(state, {sync_ref :: api_reference()
                ,sync_timer:: api_reference()
               }).
-type state() :: #state{}.

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(SYNC_PERIOD, 900000). %% 15 minutes

-record(channel, {call_id :: api_binary() | '$1' | '$2' | '_'
                  ,other_leg_call_id :: api_binary() | '$2' | '$3' | '_'
                  ,direction :: api_binary() | '_'
                  ,account_id :: api_binary() | '$1' | '_'
                  ,account_billing :: api_binary() | '$1' | '_'
                  ,account_allotment = 'false' :: boolean() | '_'
                  ,reseller_id :: api_binary() | '$1' | '_'
                  ,reseller_billing :: api_binary() | '$1' | '_'
                  ,reseller_allotment = 'false' :: boolean() | '_'
                  ,soft_limit = 'false' :: boolean() | '_'
                  ,timestamp = wh_util:current_tstamp() :: pos_integer() | '_'
                  ,answered_timestamp :: 'undefined' | pos_integer() | '$1' | '_'
                  ,rate :: api_binary() | '_'
                  ,rate_increment :: api_binary() | '_'
                  ,rate_minimum :: api_binary() | '_'
                  ,discount_percentage :: api_binary() | '_'
                  ,surcharge :: api_binary() | '_'
                  ,rate_name :: api_binary() | '_'
                  ,rate_id :: api_binary() | '_'
                  ,base_cost :: api_binary() | '_'
                 }).
-type channel() :: #channel{}.
-type channels() :: [channel(),...] | [].
-export_type([channel/0
              ,channels/0
             ]).

-define(BINDINGS, [{'authz', [{'restrict_to', ['broadcast']}
                              ,'federate'
                             ]}
                   ,{'rate', [{'restrict_to', ['broadcast']}
                              ,'federate'
                             ]}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_authz_resp'}
                      ,[{<<"authz">>, <<"authz_resp">>}]}
                     ,{{?MODULE, 'handle_rate_resp'}
                       ,[{<<"rate">>, <<"resp">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).


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
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'bindings', ?BINDINGS}
                                                          ,{'responders', ?RESPONDERS}
                                                          ,{'queue_name', ?QUEUE_NAME}
                                                          ,{'queue_options', ?QUEUE_OPTIONS}
                                                          ,{'consume_options', ?CONSUME_OPTIONS}
                                                         ], []).

-spec sync() -> 'ok'.
sync() -> gen_server:cast(?SERVER, 'synchronize_channels').

-spec total_calls(ne_binary()) -> non_neg_integer().
total_calls(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,call_id = '$1'
                           ,other_leg_call_id = '$2'
                           ,_='_'
                          }
                  ,[]
                  ,[{{'$1', '$2'}}]
                 }
                 ,{#channel{reseller_id = AccountId
                            ,call_id = '$1'
                            ,other_leg_call_id = '$2'
                            ,_='_'
                           }
                   ,[]
                   ,[{{'$1', '$2'}}]
                  }
                ],
    count_unique_calls(ets:select(?TAB, MatchSpec)).

-spec resource_consuming(ne_binary()) -> non_neg_integer().
resource_consuming(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_billing = '$1'
                           ,call_id = '$2'
                           ,other_leg_call_id = '$3'
                           ,_='_'
                          }
                  ,[{'=/=', '$1', 'undefined'}
                    ,{'=/=', '$1', <<"limits_disabled">>}
                   ]
                  ,[{{'$2', '$3'}}]
                 }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = '$1'
                            ,call_id = '$2'
                            ,other_leg_call_id = '$3'
                            ,_='_'
                           }
                   ,[{'=/=', '$1', 'undefined'}
                     ,{'=/=', '$1', <<"limits_disabled">>}
                    ]
                   ,[{{'$2', '$3'}}]
                  }
                ],
    count_unique_calls(ets:select(?TAB, MatchSpec)).

-spec inbound_flat_rate(ne_binary()) -> non_neg_integer().
inbound_flat_rate(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_billing = <<"flat_rate">>
                           ,direction = <<"inbound">>
                           ,_='_'
                          }
                  ,[]
                  ,['true']
                 }
                 ,{#channel{account_id = AccountId
                            ,account_billing = <<"flat_rate_burst">>
                            ,direction = <<"inbound">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"flat_rate">>
                            ,direction = <<"inbound">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"flat_rate_burst">>
                            ,direction = <<"inbound">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                ],
    ets:select_count(?TAB, MatchSpec).

-spec outbound_flat_rate(ne_binary()) -> non_neg_integer().
outbound_flat_rate(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_billing = <<"flat_rate">>
                           ,direction = <<"outbound">>
                           ,_='_'
                          }
                  ,[]
                  ,['true']
                 }
                 ,{#channel{account_id = AccountId
                            ,account_billing = <<"flat_rate_burst">>
                            ,direction = <<"outbound">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"flat_rate">>
                            ,direction = <<"outbound">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"flat_rate_burst">>
                            ,direction = <<"outbound">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                ],
    ets:select_count(?TAB, MatchSpec).

-spec allotments(ne_binary()) -> non_neg_integer().
allotments(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_allotment = 'true'
                           ,_='_'
                          }
                  ,[]
                  ,['true']
                 }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_allotment = 'true'
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                ],
    ets:select_count(?TAB, MatchSpec).


-spec allotment_consumed(non_neg_integer(), non_neg_integer(), ne_binary(), ne_binary() | j5_limits:limits()) -> non_neg_integer().
allotment_consumed(CycleStart, Span, Classification, AccountId) when is_binary(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_billing = <<"allotment_", Classification/binary>>
                           ,answered_timestamp = '$1'
                           ,_='_'
                          }
                  ,[]
                  ,['$1']
                 }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"allotment_", Classification/binary>>
                            ,answered_timestamp = '$1'
                            ,_='_'
                           }
                   ,[]
                   ,['$1']
                  }
                ],
    sum_allotment_consumed(CycleStart, Span, ets:select(?TAB, MatchSpec));
allotment_consumed(CycleStart, Span, Classification, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    allotment_consumed(CycleStart, Span, Classification, AccountId).

-spec per_minute(ne_binary()) -> non_neg_integer().
per_minute(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_billing = <<"per_minute">>
                           ,_='_'
                          }
                  ,[]
                  ,['true']
                 }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"per_minute">>
                            ,_='_'
                           }
                   ,[]
                   ,['true']
                  }
                ],
    ets:select_count(?TAB, MatchSpec).

-spec per_minute_cost(ne_binary()) -> non_neg_integer().
per_minute_cost(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,account_billing = <<"per_minute">>
                           ,_='_'
                          }
                  ,[]
                  ,['$_']
                 }
                 ,{#channel{reseller_id = AccountId
                            ,reseller_billing = <<"per_minute">>
                            ,_='_'
                           }
                   ,[]
                   ,['$_']
                  }
                ],
    lists:foldl(fun(Channel, Cost) ->
                        call_cost(Channel) + Cost
                end, 0, ets:select(?TAB, MatchSpec)).

-spec accounts() -> ne_binaries().
accounts() ->
    MatchSpec = [{#channel{account_id = '$1'
                           ,reseller_id = '$2'
                           ,_='_'
                          }
                  ,[]
                  ,['$$']
                 }
                ],
    accounts(ets:select(?TAB, MatchSpec), sets:new()).

-spec accounts(_, set()) -> ne_binaries().
accounts([], Accounts) ->
    lists:reverse(sets:to_list(Accounts));
accounts([['undefined', 'undefined']|Ids], Accounts) ->
    accounts(Ids, Accounts);
accounts([[AccountId, 'undefined']|Ids], Accounts) ->
    accounts(Ids, sets:add_element(AccountId, Accounts));
accounts([['undefined', ResellerId]|Ids], Accounts) ->
    accounts(Ids, sets:add_element(ResellerId, Accounts));
accounts([[AccountId, ResellerId]|Ids], Accounts) ->
    accounts(Ids
             ,sets:add_element(
                 AccountId
                 ,sets:add_element(ResellerId, Accounts)
                )
             ).

-spec account(ne_binary()) -> channels().
account(AccountId) ->
    MatchSpec = [{#channel{account_id = AccountId
                           ,_='_'
                          }
                  ,[]
                  ,['$_']
                 }
                 ,{#channel{reseller_id = AccountId
                            ,_='_'
                           }
                   ,[]
                   ,['$_']
                  }
                ],
    ets:select(?TAB, MatchSpec).

-spec to_props(channel()) -> wh_proplist().
to_props(#channel{call_id=CallId
                  ,other_leg_call_id=OtherLeg
                  ,direction=Direction
                  ,account_id=AccountId
                  ,account_billing=AccountBilling
                  ,reseller_id=ResellerId
                  ,reseller_billing=ResellerBilling
                  ,soft_limit=SoftLimit
                  ,timestamp=Timestamp
                  ,answered_timestamp=AnsweredTimestamp
                  ,rate=Rate
                  ,rate_increment=RateIncrement
                  ,rate_minimum=RateMinimum
                  ,discount_percentage=DiscountPercentage
                  ,surcharge=Surcharge
                  ,rate_name=RateName
                  ,rate_id=RateId
                  ,base_cost=BaseCost
                 }) ->
    props:filter_undefined(
      [{<<"Call-ID">>, CallId}
       ,{<<"Other-Leg-Call-ID">>, OtherLeg}
       ,{<<"Direction">>, Direction}
       ,{<<"Account-ID">>, AccountId}
       ,{<<"Account-Billing">>, AccountBilling}
       ,{<<"Reseller-ID">>, ResellerId}
       ,{<<"Reseller-Billing">>, ResellerBilling}
       ,{<<"Soft-Limit">>, SoftLimit}
       ,{<<"Timestamp">>, Timestamp}
       ,{<<"Answered-Timestamp">>, AnsweredTimestamp}
       ,{<<"Rate">>, Rate}
       ,{<<"Rate-Increment">>, RateIncrement}
       ,{<<"Rate-Minimum">>, RateMinimum}
       ,{<<"Discount-Percentage">>, DiscountPercentage}
       ,{<<"Surcharge">>, Surcharge}
       ,{<<"Rate-Name">>, RateName}
       ,{<<"Rate-ID">>, RateId}
       ,{<<"Base-Cost">>, BaseCost}
      ]
     ).

-spec authorized(wh_json:object()) -> 'ok'.
authorized(JObj) -> gen_server:cast(?SERVER, {'authorized', JObj}).

-spec remove(ne_binary()) -> 'ok'.
remove(CallId) -> gen_server:cast(?SERVER, {'remove', CallId}).

-spec handle_authz_resp(wh_json:object(), wh_proplist()) -> 'ok'.
handle_authz_resp(JObj, _Props) ->
    'true' = wapi_authz:authz_resp_v(JObj),
    case wh_json:is_true(<<"Is-Authorized">>, JObj) of
        'true' -> authorized(JObj);
        'false' -> 'ok'
    end.

-spec handle_rate_resp(wh_json:object(), wh_proplist()) -> 'ok'.
handle_rate_resp(JObj, Props) ->
    'true' = wapi_rate:resp_v(JObj),
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'rate_resp', JObj}).

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
    wh_hooks:register(),
    wh_nodes:notify_expire(),
    _ = ets:new(?TAB, ['set'
                       ,'protected'
                       ,'named_table'
                       ,{'keypos', #channel.call_id}
                      ]),
    {'ok', start_channel_sync_timer(#state{})}.

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
handle_cast({'rate_resp', JObj}, State) ->
    Props = props:filter_undefined(
              [{#channel.rate, wh_json:get_value(<<"Rate">>, JObj)}
               ,{#channel.rate_increment, wh_json:get_value(<<"Rate-Increment">>, JObj)}
               ,{#channel.rate_minimum, wh_json:get_value(<<"Rate-Minimum">>, JObj)}
               ,{#channel.discount_percentage, wh_json:get_value(<<"Discount-Percentage">>, JObj)}
               ,{#channel.surcharge, wh_json:get_value(<<"Surcharge">>, JObj)}
               ,{#channel.rate_name, wh_json:get_value(<<"Rate-Name">>, JObj)}
               ,{#channel.rate_id, wh_json:get_value(<<"Rate-ID">>, JObj)}
               ,{#channel.base_cost, wh_json:get_value(<<"Base-Cost">>, JObj)}
              ]
             ),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    _ = ets:update_element(?TAB, CallId, Props),
    {'noreply', State};
handle_cast('synchronize_channels', #state{sync_ref=SyncRef}=State) ->
    self() ! {'synchronize_channels', SyncRef},
    {'noreply', State};
handle_cast({'wh_nodes', {'expire', _Node}}, #state{sync_ref=SyncRef}=State) ->
    lager:debug("notifed that node ~s is no longer reachable, synchronizing channels", [_Node]),
    self() ! {'synchronize_channels', SyncRef},
    {'noreply', State};
handle_cast({'authorized', JObj}, State) ->
    _ = ets:insert(?TAB, from_jobj(JObj)),
    {'noreply', State};
handle_cast({'remove', CallId}, State) ->
    _ = ets:delete(?TAB, CallId),
    {'noreply', State};
handle_cast(_Msg, State) ->
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
handle_info({'synchronize_channels', SyncRef}, #state{sync_ref=SyncRef}=State) ->
    Req = wh_api:default_headers(?APP_NAME, ?APP_VERSION),
    _ = case whapps_util:amqp_pool_collect(Req
                                           ,fun wapi_call:publish_query_channels_req/1
                                           ,{'ecallmgr', 'true'})
        of
            {'error', _R} ->
                lager:error("could not reach ecallmgr channels: ~p", [_R]);
            {_, JObjs} ->
                EcallmgrChannelIds = ecallmgr_channel_ids(JObjs),
                LocalChannelIds = j5_channel_ids(),
                fix_channel_disparity(LocalChannelIds, EcallmgrChannelIds)
        end,
    {'noreply', start_channel_sync_timer(State)};
handle_info({'synchronize_channels', _}, State) ->
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_CREATE">>, JObj), State) ->
    %% insert_new keeps a CHANNEL_CREATE from overriding an entry from
    %% an auth_resp BUT an auth_resp CAN override a CHANNEL_CREATE
    _ = ets:insert_new(?TAB, from_jobj(JObj)),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_ANSWER">>, JObj), State) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Props = [{#channel.answered_timestamp, wh_util:current_tstamp()}],
    _ = ets:update_element(?TAB, CallId, Props),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_DESTROY">>, JObj), State) ->
    _ = ets:delete(?TAB, wh_json:get_value(<<"Call-ID">>, JObj)),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
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
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

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
-spec from_jobj(wh_json:object()) -> channel().
from_jobj(JObj) ->
    %% CHANNEL_CREATE has bunch of stuff in CCVs where as auth_resp
    %%  is root level, so if no CCVs then just use the JObj as is...
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, JObj),
    AccountBilling = wh_json:get_value(<<"Account-Billing">>, CCVs),
    ResellerBilling = wh_json:get_value(<<"Reseller-Billing">>, CCVs),
    #channel{call_id = wh_json:get_value(<<"Call-ID">>, JObj)
             ,other_leg_call_id = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)
             ,direction = wh_json:get_value(<<"Call-Direction">>, JObj)
             ,account_id = wh_json:get_value(<<"Account-ID">>, CCVs)
             ,account_billing = AccountBilling
             ,account_allotment = is_alloment(AccountBilling)
             ,reseller_id = wh_json:get_value(<<"Reseller-ID">>, CCVs)
             ,reseller_billing = ResellerBilling
             ,reseller_allotment = is_alloment(ResellerBilling)
             ,soft_limit = wh_json:is_true(<<"Soft-Limit">>, JObj)
            }.

-spec is_alloment(ne_binary()) -> boolean().
is_alloment(<<"allotment_", _/binary>>) -> 'true';
is_alloment(_) -> 'false'.

-type unique_channel() :: {ne_binary(), api_binary()}.
-type unique_channels() :: [unique_channel(),...] | [].

-spec count_unique_calls(unique_channels()) -> non_neg_integer().
count_unique_calls(Channels) ->
    sets:size(count_unique_calls(Channels, sets:new())).

-spec count_unique_calls(unique_channels(), set()) -> set().
count_unique_calls([], Set) -> Set;
count_unique_calls([{CallId, 'undefined'}|Channels], Set) ->
    count_unique_calls(Channels, sets:add_element(CallId, Set));
count_unique_calls([{_, CallId}|Channels], Set) ->
    count_unique_calls(Channels, sets:add_element(CallId, Set)).

-spec j5_channel_ids() -> set().
j5_channel_ids() ->
    sets:from_list(
      ets:select(?TAB, [{#channel{call_id='$1', _='_'}, [], ['$1']}])
     ).

-spec ecallmgr_channel_ids(wh_json:objects()) -> set().
ecallmgr_channel_ids(JObjs) ->
    ecallmgr_channel_ids(JObjs, sets:new()).

-spec ecallmgr_channel_ids(wh_json:objects(), set()) -> set().
ecallmgr_channel_ids([], ChannelIds) -> ChannelIds;
ecallmgr_channel_ids([JObj|JObjs], ChannelIds) ->
    Channels = wh_json:get_value(<<"Channels">>, JObj),
    ecallmgr_channel_ids(
      JObjs
      ,lists:foldl(fun(ChannelId, Ids) ->
                           sets:add_element(ChannelId, Ids)
                   end, ChannelIds, wh_json:get_keys(Channels))
     ).

-spec fix_channel_disparity(set(), set()) -> 'ok'.
fix_channel_disparity(LocalChannelIds, EcallmgrChannelIds) ->
    Disparity = sets:to_list(sets:subtract(LocalChannelIds, EcallmgrChannelIds)),
    fix_channel_disparity(Disparity).

-spec fix_channel_disparity(ne_binaries()) -> 'ok'.
fix_channel_disparity([]) -> 'ok';
fix_channel_disparity([ChannelId|ChannelIds]) ->
    lager:debug("channel disparity with ecallmgr, removing ~s"
                ,[ChannelId]),
    _ = ets:delete(?TAB, ChannelId),
    fix_channel_disparity(ChannelIds).

-spec start_channel_sync_timer(state()) -> state().
start_channel_sync_timer(State) ->
    SyncRef = make_ref(),
    TRef = erlang:send_after(?SYNC_PERIOD, self(), {'synchronize_channels', SyncRef}),
    State#state{sync_ref=SyncRef
                ,sync_timer=TRef}.

-type non_neg_integers() :: [non_neg_integer(),...] | [].

-spec sum_allotment_consumed(non_neg_integer(), non_neg_integer(), non_neg_integers()) -> non_neg_integer().
sum_allotment_consumed(CycleStart, Span, Matches) ->
    sum_allotment_consumed(CycleStart, Span, wh_util:current_tstamp(), 0, Matches).

-spec sum_allotment_consumed(non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integers()) -> non_neg_integer().
sum_allotment_consumed(_, _, _, Seconds, []) -> Seconds;
sum_allotment_consumed(CycleStart, Span, CurrentTimestamp, Seconds, ['undefined'|Matches]) ->
    sum_allotment_consumed(CycleStart, Span, CurrentTimestamp, Seconds + 60, Matches);
sum_allotment_consumed(CycleStart, Span, CurrentTimestamp, Seconds, [Timestamp|Matches]) ->
    S = calculate_consumed(CycleStart, Span, CurrentTimestamp, Timestamp) + Seconds,
    sum_allotment_consumed(CycleStart, Span, CurrentTimestamp, S, Matches).

-spec calculate_consumed(non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
calculate_consumed(CycleStart, Span, CurrentTimestamp, Timestamp) ->
    Consumed = case Timestamp < CycleStart of
                   'true' -> CurrentTimestamp - CycleStart + 60;
                   'false' -> CurrentTimestamp - Timestamp + 60
               end,
    case Consumed > Span of
        'true' -> Span;
        'false' -> Consumed
    end.

-spec call_cost(channel()) -> non_neg_integer().
call_cost(#channel{answered_timestamp='undefined'}=Channel) ->
    wht_util:call_cost(billing_jobj(60, Channel));
call_cost(#channel{answered_timestamp=Timestamp}=Channel) ->
    BillingSeconds = wh_util:current_tstamp() - Timestamp + 60,
    wht_util:call_cost(billing_jobj(BillingSeconds, Channel)).

-spec billing_jobj(non_neg_integer(), channel()) -> wh_json:object().
billing_jobj(BillingSeconds, Channel) ->
    wh_json:from_list([{<<"Billing-Seconds">>, BillingSeconds} | to_props(Channel)]).
