%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_channels).

-behaviour(gen_listener).

-export([start_link/0]).
-export([sync/0]).
-export([flush/0]).
-export([total_calls/1]).
-export([total_inbound_channels_per_did_rules/2]).
-export([resource_consuming/1]).
-export([inbound_flat_rate/1]).
-export([outbound_flat_rate/1]).
-export([allotments/1]).
-export([allotment_consumed/4]).
-export([per_minute/1]).
-export([per_minute_cost/1, real_per_minute_cost/1]).
-export([accounts/0]).
-export([account/1]).
-export([to_props/1]).
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
-include_lib("kazoo_events/include/kz_hooks.hrl").

-record(state, {sync_ref :: kz_term:api_reference()
               ,sync_timer :: kz_term:api_reference()
               ,cleanup_timer :: kz_term:api_reference()
               }).
-type state() :: #state{}.

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(SYNC_PERIOD, 900000). %% 15 minutes
-define(CLEANUP_PERIOD, ?MILLISECONDS_IN_MINUTE).

-record(channel, {call_id :: kz_term:api_binary() | '$1' | '$2' | '_'
                 ,other_leg_call_id :: kz_term:api_binary() | '$2' | '$3' | '_'
                 ,direction :: kz_term:api_binary() | '_'
                 ,account_id :: kz_term:api_binary() | '$1' | '_'
                 ,account_billing :: kz_term:api_binary() | '$1' | '_'
                 ,account_allotment = 'false' :: boolean() | '_'
                 ,reseller_id :: kz_term:api_binary() | '$1' | '$2' | '_'
                 ,reseller_billing :: kz_term:api_binary() | '$1' | '_'
                 ,reseller_allotment = 'false' :: boolean() | '_'
                 ,soft_limit = 'false' :: boolean() | '_'
                 ,timestamp = kz_time:now_s() :: pos_integer() | '_' | '$2'
                 ,answered_timestamp :: kz_term:api_pos_integer() | '$1' | '_'
                 ,rate :: kz_term:api_binary() | '_'
                 ,rate_increment :: kz_term:api_binary() | '_'
                 ,rate_minimum :: kz_term:api_binary() | '_'
                 ,rate_nocharge_time :: kz_term:api_binary() | '_'
                 ,discount_percentage :: kz_term:api_binary() | '_'
                 ,surcharge :: kz_term:api_binary() | '_'
                 ,rate_name :: kz_term:api_binary() | '_'
                 ,rate_description :: kz_term:api_binary() | '_'
                 ,rate_id :: kz_term:api_binary() | '_'
                 ,base_cost :: kz_term:api_binary() | '_'
                 ,to_did :: kz_term:api_binary() | '_'
                 ,destroyed = 'false' :: boolean() | '_' | '$1'
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel()].
-export_type([channel/0
             ,channels/0
             ]).

-define(BINDINGS, [{'authz', [{'restrict_to', ['broadcast']}
                             ,'federate'
                             ]
                   }
                  ,{'rate', [{'restrict_to', ['broadcast']}
                            ,'federate'
                            ]
                   }
                  ]).

-define(RESPONDERS, [{{?MODULE, 'handle_authz_resp'}
                     ,[{<<"authz">>, <<"authz_resp">>}]
                     }
                    ,{{?MODULE, 'handle_rate_resp'}
                     ,[{<<"rate">>, <<"resp">>}]
                     }
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

-spec sync() -> 'ok'.
sync() ->
    gen_server:cast(?SERVER, 'synchronize_channels').

-spec flush() -> 'ok'.
flush() -> gen_server:cast(?SERVER, 'flush_channels').

-spec total_calls(kz_term:ne_binary()) -> non_neg_integer().
total_calls(<<AccountId/binary>>) ->
    MatchSpec = [{#channel{account_id = AccountId
                          ,call_id = '$1'
                          ,other_leg_call_id = '$2'
                           %% ,destroyed = 'false'
                          ,_='_'
                          }
                 ,[]
                 ,[{{'$1', '$2'}}]
                 }
                ,{#channel{reseller_id = AccountId
                          ,call_id = '$1'
                          ,other_leg_call_id = '$2'
                           %% ,destroyed = 'false'
                          ,_='_'
                          }
                 ,[]
                 ,[{{'$1', '$2'}}]
                 }
                ],
    count_unique_calls(ets:select(?TAB, MatchSpec)).

-spec total_inbound_channels_per_did_rules(kz_term:ne_binary() ,kz_term:ne_binary()) -> non_neg_integer().
total_inbound_channels_per_did_rules(Number, AccountId) ->
    ToDID = knm_converters:normalize(Number),
    MatchSpec = [{#channel{account_id = AccountId
                          ,direction = <<"inbound">>
                          ,to_did = ToDID
                          ,call_id = '$1'
                          ,_='_'
                          }
                 ,[]
                 ,[{{ToDID, '$1'}}]
                 }
                ,{#channel{reseller_id = AccountId
                          ,direction = <<"inbound">>
                          ,to_did = ToDID
                          ,call_id = '$1'
                          ,_='_'
                          }
                 ,[]
                 ,[{{ToDID, '$1'}}]
                 }
                ],
    count_unique_calls(ets:select(?TAB, MatchSpec)).

-spec resource_consuming(kz_term:ne_binary()) -> non_neg_integer().
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

-spec inbound_flat_rate(kz_term:ne_binary()) -> non_neg_integer().
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

-spec outbound_flat_rate(kz_term:ne_binary()) -> non_neg_integer().
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

-spec allotments(kz_term:ne_binary()) -> non_neg_integer().
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


-spec allotment_consumed(non_neg_integer(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary() | j5_limits:limits()) -> non_neg_integer().
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

-spec per_minute(kz_term:ne_binary()) -> non_neg_integer().
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

-spec per_minute_cost(kz_term:ne_binary()) -> non_neg_integer().
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

-spec real_per_minute_cost(kz_term:ne_binary()) -> non_neg_integer().
real_per_minute_cost(AccountId) ->
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
                        call_cost(Channel, 0) + Cost
                end, 0, ets:select(?TAB, MatchSpec)).

-spec accounts() -> kz_term:ne_binaries().
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

-spec accounts(any(), sets:set()) -> kz_term:ne_binaries().
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
            ,sets:add_element(AccountId
                             ,sets:add_element(ResellerId, Accounts)
                             )
            ).

-spec account(kz_term:ne_binary()) -> channels().
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

-spec to_props(channel()) -> kz_term:proplist().
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
                 ,rate_nocharge_time=RateNoChargeTime
                 ,discount_percentage=DiscountPercentage
                 ,surcharge=Surcharge
                 ,rate_name=RateName
                 ,rate_description=RateDescription
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
      ,{<<"Rate-NoCharge-Time">>, RateNoChargeTime}
      ,{<<"Discount-Percentage">>, DiscountPercentage}
      ,{<<"Surcharge">>, Surcharge}
      ,{<<"Rate-Name">>, RateName}
      ,{<<"Rate-Description">>, RateDescription}
      ,{<<"Rate-ID">>, RateId}
      ,{<<"Base-Cost">>, BaseCost}
      ]
     ).

-spec rated(kz_json:object()) -> 'ok'.
rated(JObj) ->
    CallId = kz_call_event:call_id(JObj),
    Props = props:filter_undefined(
              [{#channel.rate, kz_json:get_value(<<"Rate">>, JObj)}
              ,{#channel.rate_increment, kz_json:get_value(<<"Rate-Increment">>, JObj)}
              ,{#channel.rate_minimum, kz_json:get_value(<<"Rate-Minimum">>, JObj)}
              ,{#channel.rate_nocharge_time, kz_json:get_value(<<"Rate-NoCharge-Time">>, JObj)}
              ,{#channel.discount_percentage, kz_json:get_value(<<"Discount-Percentage">>, JObj)}
              ,{#channel.surcharge, kz_json:get_value(<<"Surcharge">>, JObj)}
              ,{#channel.rate_name, kz_json:get_value(<<"Rate-Name">>, JObj)}
              ,{#channel.rate_description, kz_json:get_value(<<"Rate-Description">>, JObj)}
              ,{#channel.rate_id, kz_json:get_value(<<"Rate-ID">>, JObj)}
              ,{#channel.base_cost, kz_json:get_value(<<"Base-Cost">>, JObj)}
              ]
             ),
    _ = ets:update_element(?TAB, CallId, Props),
    lager:debug("channel rated").

-spec authorized(kz_json:object()) -> 'ok'.
authorized(JObj) ->
    Channel = #channel{call_id=CallId}=from_jobj(JObj),
    Props = props:filter_undefined(
              [{#channel.account_billing, Channel#channel.account_billing}
              ,{#channel.account_allotment, Channel#channel.account_allotment}
              ,{#channel.reseller_billing, Channel#channel.reseller_billing}
              ,{#channel.reseller_allotment, Channel#channel.reseller_allotment}
              ,{#channel.soft_limit, Channel#channel.soft_limit}
              ]
             ),
    _ = ets:update_element(?TAB, CallId, Props),
    lager:debug("channel authorized => ~s", [format_updates(Props)]).

-spec format_updates(kz_term:proplist()) -> kz_term:ne_binary().
format_updates(Updates) ->
    Fields = record_info('fields', 'channel'),
    Out = [io_lib:format("~s=~p", [lists:nth(Field - 1, Fields), V]) || {Field, V} <- Updates],
    kz_binary:join(Out, <<",">>).

-spec handle_authz_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_authz_resp(JObj, Props) ->
    'true' = kapi_authz:authz_resp_v(JObj),
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'authz_resp', JObj}).

-spec handle_rate_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_rate_resp(JObj, Props) ->
    'true' = kapi_rate:resp_v(JObj),
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'rate_resp', JObj}).

-spec handle_channel_destroy(kz_term:ne_binary()) -> 'ok'.
handle_channel_destroy(<<CallId/binary>>) ->
    _ = ets:delete(?TAB, CallId),
    lager:debug("removed channel ~s from ~p", [CallId, ?TAB]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_hooks:register(),
    kz_nodes:notify_expire(),
    _ = ets:new(?TAB, ['set'
                      ,'public'
                      ,'named_table'
                      ,{'keypos', #channel.call_id}
                      ]),
    State = start_cleanup_timer(#state{}),
    {'ok', start_channel_sync_timer(State)}.

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
handle_cast({'rate_resp', JObj}, State) ->
    kz_log:put_callid(JObj),
    rated(JObj),
    {'noreply', State};
handle_cast({'authz_resp', JObj}, State) ->
    kz_log:put_callid(JObj),
    authorized(JObj),
    {'noreply', State};
handle_cast('synchronize_channels', #state{sync_ref=SyncRef}=State) ->
    self() ! {'synchronize_channels', SyncRef},
    {'noreply', State};
handle_cast('flush_channels', State) ->
    lager:info("flushing all channels from cache"),
    _ = ets:delete_all_objects(?TAB),
    {'noreply', State};
handle_cast({'kz_nodes', {'expire', #kz_node{node=NodeName, kapps=Whapps}}}
           ,#state{sync_ref=SyncRef}=State
           ) ->
    case props:get_value(<<"ecallmgr">>, Whapps) of
        'undefined' -> {'noreply', State};
        _WhappInfo ->
            lager:debug("ecallmgr node ~s is no longer reachable, synchronizing channels", [NodeName]),
            self() ! {'synchronize_channels', SyncRef},
            {'noreply', State}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'synchronize_channels', SyncRef}, #state{sync_ref=SyncRef}=State) ->
    kz_process:spawn(fun synchronize/0),
    {'noreply', start_channel_sync_timer(State)};
handle_info({'synchronize_channels', _}, State) ->
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_CREATE">>, JObj), State) ->
    kz_log:put_callid(JObj),
    %% insert_new keeps a CHANNEL_CREATE from overriding an entry from
    %% an auth_resp BUT an auth_resp CAN override a CHANNEL_CREATE
    Channel = #channel{call_id=CallId}=from_jobj(JObj),
    lager:debug("inserting new channel ~s", [CallId]),
    _ = ets:insert_new(?TAB, Channel),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_ANSWER">>, JObj), State) ->
    CallId = kz_call_event:call_id(JObj),
    Props = [{#channel.answered_timestamp, kz_time:now_s()}],
    lager:info("updating ~s with answered timestamp", [CallId]),
    _ = ets:update_element(?TAB, CallId, Props),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_DESTROY">>, JObj), State) ->
    handle_channel_destroy(kz_api:call_id(JObj)),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_BRIDGE">>, JObj), State) ->
    channel_bridge(JObj),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_UNBRIDGE">>, JObj), State) ->
    channel_unbridge(JObj),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_DISCONNECTED">>, JObj), State) ->
    %% TODO
    %% create a timer that when fired
    %% will call handle_channel_destroy
    %% if the channel hasn't reconnected
    %% same should happen in jonny5_listener
    %% for this event
    handle_channel_destroy(kz_api:call_id(JObj)),
    {'noreply', State};
handle_info(?HOOK_EVT(_, <<"CHANNEL_CONNECTED">>, _JObj), State) ->
    {'noreply', State};
handle_info('cleanup', State) ->
    _P = kz_process:spawn(fun delete_destroyed_channels/0),
    {'noreply', start_cleanup_timer(State)};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
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
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

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
-spec from_jobj(kz_json:object()) -> channel().
from_jobj(JObj) ->
    AccountId = kz_json:get_first_defined(
                  [<<"Account-ID">>
                  ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                  ], JObj
                 ),
    AccountBilling = kz_json:get_first_defined(
                       [<<"Account-Billing">>
                       ,[<<"Custom-Channel-Vars">>, <<"Account-Billing">>]
                       ], JObj
                      ),
    ResellerId = kz_json:get_first_defined(
                   [<<"Reseller-ID">>
                   ,[<<"Custom-Channel-Vars">>, <<"Reseller-ID">>]
                   ], JObj
                  ),
    ResellerBilling = kz_json:get_first_defined(
                        [<<"Reseller-Billing">>
                        ,[<<"Custom-Channel-Vars">>, <<"Reseller-Billing">>]
                        ], JObj
                       ),
    SoftLimit = kz_json:get_first_defined(
                  [<<"Soft-Limit">>
                  ,[<<"Custom-Channel-Vars">>, <<"Soft-Limit">>]
                  ], JObj
                 ),
    #channel{call_id = kz_call_event:call_id(JObj)
            ,other_leg_call_id = kz_call_event:other_leg_call_id(JObj)
            ,direction = kz_call_event:call_direction(JObj)
            ,account_id = AccountId
            ,account_billing = AccountBilling
            ,account_allotment = is_allotment(AccountBilling)
            ,reseller_id = ResellerId
            ,reseller_billing = ResellerBilling
            ,reseller_allotment = is_allotment(ResellerBilling)
            ,soft_limit = kz_term:is_true(SoftLimit)
            ,to_did = to_did_lookup(JObj)
            }.

-spec is_allotment(kz_term:ne_binary()) -> boolean().
is_allotment(<<"allotment_", _/binary>>) -> 'true';
is_allotment(_) -> 'false'.

-type unique_channel() :: {kz_term:ne_binary(), kz_term:api_binary()}.
-type unique_channels() :: [unique_channel()].

-spec count_unique_calls(unique_channels()) -> non_neg_integer().
count_unique_calls(Channels) ->
    sets:size(count_unique_calls(Channels, sets:new())).

-spec count_unique_calls(unique_channels(), sets:set()) -> sets:set().
count_unique_calls([], Set) -> Set;
count_unique_calls([{CallId, 'undefined'}|Channels], Set) ->
    count_unique_calls(Channels, sets:add_element(CallId, Set));
count_unique_calls([{CallId, OtherId}|Channels], Set) ->
    case sets:is_element(OtherId, Set) of
        true -> count_unique_calls(Channels, Set);
        false -> count_unique_calls(Channels, sets:add_element(CallId, Set))
    end.

-spec j5_channel_ids() -> sets:set().
j5_channel_ids() ->
    sets:from_list(
      ets:select(?TAB, [{#channel{call_id='$1', _='_'}, [], ['$1']}])
     ).

-spec ecallmgr_channel_ids(kz_json:objects()) -> sets:set().
ecallmgr_channel_ids(JObjs) ->
    ecallmgr_channel_ids(JObjs, sets:new()).

-spec ecallmgr_channel_ids(kz_json:objects(), sets:set()) -> sets:set().
ecallmgr_channel_ids([], ChannelIds) -> ChannelIds;
ecallmgr_channel_ids([JObj|JObjs], ChannelIds) ->
    UpdatedChannelIds = lists:foldl(fun(ChannelId, Ids) ->
                                            sets:add_element(ChannelId, Ids)
                                    end
                                   ,ChannelIds
                                   ,kz_json:get_keys(<<"Channels">>, JObj)
                                   ),
    ecallmgr_channel_ids(JObjs, UpdatedChannelIds).

-spec fix_channel_disparity(sets:set(), sets:set()) -> 'ok'.
fix_channel_disparity(LocalChannelIds, EcallmgrChannelIds) ->
    Disparity = sets:to_list(sets:subtract(LocalChannelIds, EcallmgrChannelIds)),
    fix_channel_disparity(Disparity).

-spec fix_channel_disparity(kz_term:ne_binaries()) -> 'ok'.
fix_channel_disparity([]) -> 'ok';
fix_channel_disparity([ChannelId|ChannelIds]) ->
    lager:debug("channel disparity with ecallmgr, removing ~s", [ChannelId]),
    _ = ets:delete(?TAB, ChannelId),
    fix_channel_disparity(ChannelIds).

-spec delete_destroyed_channels() -> 'ok'.
delete_destroyed_channels() ->
    ThenS = kz_time:now_s() - 3, % anything prior to 3s ago

    Deleted = ets:select_delete(?TAB, [{#channel{destroyed='true', timestamp='$2', _='_'}
                                       ,[{'<', '$2', {'const', ThenS}}]
                                       ,['true']
                                       }
                                      ]),
    maybe_log_deleted(Deleted, ThenS).

-spec maybe_log_deleted(non_neg_integer(), kz_time:gregorian_seconds()) -> 'ok'.
maybe_log_deleted(0, _ThenS) -> 'ok';
maybe_log_deleted(Deleted, ThenS) ->
    lager:debug("deleted ~p destroyed channels from before ~p", [Deleted, ThenS]).

-spec start_cleanup_timer(state()) -> state().
start_cleanup_timer(State) ->
    TRef = erlang:send_after(?CLEANUP_PERIOD, self(), 'cleanup'),
    State#state{cleanup_timer=TRef}.

-spec start_channel_sync_timer(state()) -> state().
start_channel_sync_timer(State) ->
    SyncRef = make_ref(),
    TRef = erlang:send_after(?SYNC_PERIOD, self(), {'synchronize_channels', SyncRef}),
    State#state{sync_ref=SyncRef
               ,sync_timer=TRef
               }.

-type non_neg_integers() :: [non_neg_integer()].

-spec sum_allotment_consumed(non_neg_integer(), non_neg_integer(), non_neg_integers()) -> non_neg_integer().
sum_allotment_consumed(CycleStart, Span, Matches) ->
    sum_allotment_consumed(CycleStart, Span, kz_time:now_s(), 0, Matches).

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
call_cost(Channel) -> call_cost(Channel, 60).

-spec call_cost(channel(), integer()) -> non_neg_integer().
call_cost(#channel{answered_timestamp='undefined'}=Channel, Seconds) ->
    kapps_call_util:call_cost(billing_jobj(Seconds, Channel));
call_cost(#channel{answered_timestamp=Timestamp}=Channel, Seconds) ->
    BillingSeconds = kz_time:now_s() - Timestamp + Seconds,
    kapps_call_util:call_cost(billing_jobj(BillingSeconds, Channel)).

-spec billing_jobj(non_neg_integer(), channel()) -> kz_json:object().
billing_jobj(BillingSeconds, Channel) ->
    kz_json:from_list([{<<"Billing-Seconds">>, BillingSeconds} | to_props(Channel)]).

-spec to_did_lookup(kz_json:object()) -> kz_term:ne_binary() | 'undefined'.
to_did_lookup(JObj) ->
    case kz_json:get_first_defined(
           [<<"To">>
           ,<<"To-Uri">>
           ,<<"Request">>
           ,[<<"Custom-Channel-Vars">>,<<"To">>]
           ]
          ,JObj
          )
    of
        'undefined' -> 'undefined';
        ToUri ->
            [H|_] = binary:split(ToUri, <<"@">>),
            knm_converters:normalize(H)
    end.

-spec synchronize() -> 'ok'.
synchronize() ->
    Req = kz_api:default_headers(?APP_NAME, ?APP_VERSION),
    _ = case kz_amqp_worker:call_collect(Req
                                        ,fun kapi_call:publish_query_channels_req/1
                                        ,{'ecallmgr', 'true'}
                                        )
        of
            {'error', _R} ->
                lager:error("could not reach ecallmgr channels: ~p", [_R]);
            {_, JObjs} ->
                EcallmgrChannelIds = ecallmgr_channel_ids(JObjs),
                LocalChannelIds = j5_channel_ids(),
                fix_channel_disparity(LocalChannelIds, EcallmgrChannelIds)
        end.

-spec channel_bridge(kz_json:object()) -> 'ok'.
channel_bridge(JObj) ->
    UUID = kz_call_event:call_id(JObj),
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    _ = ets:update_element(?TAB, UUID, [{#channel.other_leg_call_id, OtherLeg}
                                       ,{#channel.timestamp, kz_time:now_s()}
                                       ]),
    _ = ets:update_element(?TAB, OtherLeg, [{#channel.other_leg_call_id, UUID}
                                           ,{#channel.timestamp, kz_time:now_s()}
                                           ]),
    'ok'.

-spec channel_unbridge(kz_json:object()) -> any().
channel_unbridge(JObj) ->
    UUID = kz_call_event:call_id(JObj),
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    _ = ets:update_element(?TAB, UUID, [{#channel.other_leg_call_id, 'undefined'}
                                       ,{#channel.timestamp, kz_time:now_s()}
                                       ]),
    _ = ets:update_element(?TAB, OtherLeg, [{#channel.other_leg_call_id, 'undefined'}
                                           ,{#channel.timestamp, kz_time:now_s()}
                                           ]),
    'ok'.
