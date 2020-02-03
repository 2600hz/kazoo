%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_im_offnet).
-behaviour(gen_listener).

-export([start_link/1]).

-export([route/1, route/2]).

-export([handle_message/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_im.hrl").

-define(SERVER, ?MODULE).

-record(state, {connection :: amqp_listener_connection()
               ,confirms = #{count => 1, pids => #{}} :: map()
               }).
-type state() :: #state{}.

-define(BINDINGS(Ex), [{'im', [{'exchange', Ex}
                              ,{'restrict_to', ['inbound']}
                              ]}
                      ]).
-define(RESPONDERS, [{{?MODULE, 'handle_message'}
                     ,[{<<"*">>, <<"inbound">>}]
                     }
                    ]).

-define(QUEUE_OPTIONS, [{'exclusive', 'false'}
                       ,{'durable', 'true'}
                       ,{'auto_delete', 'false'}
                       ,{'arguments', [{<<"x-message-ttl">>, 'infinity'}
                                      ,{<<"x-max-length">>, 'infinity'}
                                      ]}
                       ]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}
                         ,{'no_ack', 'false'}
                         ]).

-define(AMQP_PUBLISH_OPTIONS, [{'mandatory', 'true'}
                              ,{'delivery_mode', 2}
                              ]).

-define(ROUTE_TIMEOUT, 'infinity').

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(amqp_listener_connection()) -> kz_types:startlink_ret().
start_link(#amqp_listener_connection{broker=Broker
                                    ,exchange=Exchange
                                    ,type=Type
                                    ,queue=Queue
                                    ,options=Options
                                    }=C) ->
    Exchanges = [{Exchange, Type, Options}],
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(Exchange)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', Queue}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ,{'declare_exchanges', Exchanges}
                            ,{'broker', Broker}
                            ,{'server_confirms', 'true'}
                            ]
                           ,[C]
                           ,[]
                           ).

-spec handle_message(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_message(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Deliver = props:get_value('deliver', Props),
    case kapi_im:inbound_v(JObj) of
        'true' ->
            handle_inbound(JObj, Srv, Deliver);
        'false' ->
            lager:debug("error validating inbound message : ~p", [JObj]),
            gen_listener:ack(Srv, Deliver)
    end.

-spec route(kz_term:api_terms()) -> 'ok' | {'error', 'no_connections'}.
route(Payload) ->
    case kz_im_offnet_sup:worker() of
        {'error', 'no_connections'} = E -> E;
        Pid -> route(Pid, Payload)
    end.

-spec route(pid(), kz_term:api_terms()) -> 'ok'.
route(Pid, Payload) ->
    gen_listener:call(Pid, {'route', Payload}, ?ROUTE_TIMEOUT).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([amqp_listener_connection()]) -> {'ok', state()}.
init([#amqp_listener_connection{}=Connection]) ->
    {'ok', #state{connection=Connection}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'route', Payload}, From, #state{connection = Connection, confirms = Confirms} = State) ->
    #amqp_listener_connection{exchange = Exchange} = Connection,
    Values = [{<<"Exchange-ID">>, Exchange}],
    JObj = kz_json:set_values(Values, Payload),
    kapi_im:publish_outbound(JObj, ?AMQP_PUBLISH_OPTIONS),
    #{count := Count, pids := Pids} = Confirms,
    Info = #{payload => kz_json:delete_key(<<"Body">>, Payload), from => From},
    {'noreply', State#state{confirms = Confirms#{count => Count + 1, pids => Pids#{Count => Info}}}};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'server_confirms', _Confirms}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'confirm', Confirm}}, State) ->
    {'noreply', handle_confirm(Confirm, State)};
handle_cast(_Msg, State) ->
    lager:debug("external listener unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("external listener unhandled info: ~p", [_Info]),
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
    lager:debug("external listener terminating : ~s", [_Reason]).

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
-spec handle_inbound(kz_json:object(), pid(), gen_listener:basic_deliver()) -> 'ok'.
handle_inbound(JObj, Srv, Deliver) ->
    case maybe_relay_request(JObj) of
        'ack' -> gen_listener:ack(Srv, Deliver);
        'nack' -> gen_listener:ack(Srv, Deliver)
    end.

-spec maybe_relay_request(kz_json:object()) -> 'ack' | 'nack'.
maybe_relay_request(JObj) ->
    Number = knm_converters:normalize(kz_im:to(JObj)),
    IM = kapps_im:from_payload(JObj),
    Map = #{number => Number
           ,inception => <<"offnet">>
           ,request => JObj
           ,route_id => kz_im:route_id(JObj)
           ,route_type => kz_im:route_type(JObj)
           ,im => IM
           },
    Routines = [fun lookup_number/1
               ,fun number_has_im_enabled/1
               ,fun account_from_number/1
               ,fun account_fetch/1
               ,fun reseller_fetch/1
               ,fun account_enabled/1
               ,fun reseller_enabled/1
               ,fun account_has_im_enabled/1
               ,fun reseller_has_im_enabled/1
               ,fun account_standing_is_acceptable/1
               ,fun reseller_standing_is_acceptable/1
               ,fun set_rate/1
               ],
    case kz_maps:exec(Routines, Map) of
        #{account_id := AccountId, error := Error} ->
            lager:warning("external ~s request for number ~s validation failed  in account ~s : ~p", [kapps_im:type(IM), Number, AccountId, Error]),
            'nack';
        #{error := Error} ->
            lager:warning("validation failed in external ~s request for number ~s : ~p", [kapps_im:type(IM), Number, Error]),
            'nack';
        #{account_id := AccountId, request := Payload, rate := Rate} ->
            lager:info("accepted external ~s request ~s for account ~s", [kapps_im:type(IM), Number, AccountId]),
            Values = [{<<"Account-ID">>, AccountId}
                     ,{<<"Charges">>, Rate}
                     ],
            API = kz_json:set_values(Values, Payload),
            case kz_im_onnet:route(API) of
                'ok' -> 'ack';
                {'error', _Error} ->
                    lager:warning("onnet rejected request for account ~s : ~p", [AccountId, _Error]),
                    'ack'
            end;
        M ->
            lager:debug("unable to determine account for ~s => ~p", [Number, M]),
            'nack'
    end.

lookup_number(#{account_id := _AccountId} = Map) -> Map;
lookup_number(#{number := Number} = Map) ->
    case knm_phone_number:fetch(Number) of
        {'error', _R} -> Map;
        {'ok', KNumber} -> Map#{phone_number => KNumber}
    end;
lookup_number(Map) -> Map.

number_has_im_enabled(#{phone_number := PN, im := IM} = Map) ->
    case knm_im:enabled(PN, kapps_im:type(IM)) of
        'true' -> Map;
        'false' -> maps:without([account_id, account, phone_number]
                               ,Map#{error => io_lib:format("number does not have ~s enabled", [kapps_im:type(IM)])}
                               )
    end;
number_has_im_enabled(Map) -> Map.

account_from_number(#{account_id := _AccountId} = Map) -> Map;
account_from_number(#{phone_number := PN, im := IM} = Map) ->
    case knm_phone_number:assigned_to(PN) of
        'undefined' -> Map;
        AccountId -> Map#{account_id => AccountId
                         ,reseller_id => kz_services_reseller:get_id(AccountId)
                         ,im => kapps_im:set_account_id(AccountId, IM)
                         }
    end;
account_from_number(Map) -> Map.

account_fetch(#{account_id := AccountId} = Map) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', Error} -> maps:without([account_id], Map#{error => Error});
        {'ok', Account} -> Map#{account => Account}
    end;
account_fetch(Map) -> Map.

reseller_fetch(#{reseller_id := ResellerId} = Map) ->
    case kzd_accounts:fetch(ResellerId) of
        {'error', Error} -> maps:without([reseller_id], Map#{error => Error});
        {'ok', Reseller} -> Map#{reseller => Reseller}
    end;
reseller_fetch(Map) -> Map.

account_enabled(#{account := Account} = Map) ->
    case kzd_accounts:enabled(Account) of
        'true' -> Map;
        'false' -> maps:without([account_id, account]
                               ,Map#{error => <<"account is disabled">>}
                               )
    end;
account_enabled(Map) -> Map.

reseller_enabled(#{reseller := Reseller} = Map) ->
    case kzd_accounts:enabled(Reseller) of
        'true' -> Map;
        'false' -> maps:without([account_id, account, reseller_id, reseller]
                               ,Map#{error => <<"reseller is disabled">>}
                               )
    end;
reseller_enabled(Map) -> Map.

account_has_im_enabled(#{account_id := AccountId, im := IM} = Map) ->
    case kz_services_im:is_enabled(AccountId, kapps_im:type(IM)) of
        'true' -> Map;
        'false' -> maps:without([account_id, account]
                               ,Map#{error => io_lib:format("account does not have ~s enabled", [kapps_im:type(IM)])}
                               )
    end;
account_has_im_enabled(Map) -> Map.

reseller_has_im_enabled(#{reseller_id := ResellerId, im := IM} = Map) ->
    case kz_services_im:is_enabled(ResellerId, kapps_im:type(IM)) of
        'true' -> Map;
        'false' -> maps:without([account_id, account, reseller_id, reseller]
                               ,Map#{error => kz_term:to_binary(io_lib:format("reseller ~s does not have ~s enabled", [ResellerId, kapps_im:type(IM)]))}
                               )
    end;
reseller_has_im_enabled(Map) -> Map.

account_standing_is_acceptable(#{account_id := AccountId} = Map) ->
    case kz_services_standing:acceptable(AccountId) of
        {'true', _} -> Map;
        {'false', Error} -> maps:without([account_id, account]
                                        ,Map#{error => maps:get(reason, Error)}
                                        )
    end;
account_standing_is_acceptable(Map) -> Map.

reseller_standing_is_acceptable(#{reseller_id := ResellerId} = Map) ->
    case kz_services_standing:acceptable(ResellerId) of
        {'true', _} -> Map;
        {'false', Error} -> maps:without([account_id, account, reseller_id, reseller]
                                        ,Map#{error => maps:get(reason, Error)}
                                        )
    end;
reseller_standing_is_acceptable(Map) -> Map.

set_rate(#{account_id := AccountId
          ,im := IM
          } = Map) ->
    Rate = kz_services_im:flat_rate(AccountId, kapps_im:type(IM), kapps_im:direction(IM)),
    Map#{rate => Rate};
set_rate(Map) -> Map.

handle_confirm(#'basic.ack'{delivery_tag = Idx, multiple = 'true'}
              ,#state{confirms = #{pids := Pids} = Confirms} = State
              ) ->
    Keys = maps:fold(fun reply_ok/3, [], maps:filter(fun(K, _) -> K =< Idx end, Pids)),
    State#state{confirms = Confirms#{pids => maps:without(Keys, Pids)}};
handle_confirm(#'basic.ack'{delivery_tag = Idx, multiple = 'false'}
              ,#state{confirms = #{pids := Pids} = Confirms} = State
              ) ->
    Keys = maps:fold(fun reply_ok/3, [], maps:with([Idx], Pids)),
    State#state{confirms = Confirms#{pids => maps:without(Keys, Pids)}};
handle_confirm(#'basic.nack'{delivery_tag = Idx, multiple = 'true'}
              ,#state{confirms = #{pids := Pids} = Confirms} = State
              ) ->
    Keys = maps:fold(fun reply_error/3, [], maps:filter(fun(K, _) -> K =< Idx end, Pids)),
    State#state{confirms = Confirms#{pids => maps:without(Keys, Pids)}};
handle_confirm(#'basic.nack'{delivery_tag = Idx, multiple = 'false'}
              ,#state{confirms = #{pids := Pids} = Confirms} = State
              ) ->
    Keys = maps:fold(fun reply_error/3, [], maps:with([Idx], Pids)),
    State#state{confirms = Confirms#{pids => maps:without(Keys, Pids)}}.

reply_ok(Key, #{payload := Payload, from := Pid}, Acc) ->
    gen_server:reply(Pid, 'ok'),
    _ = kz_process:spawn(fun create_ledger/2, ['outbound', Payload]),
    [Key | Acc].

reply_error(Key, Pid, Acc) ->
    gen_server:reply(Pid, {'error', <<"server declined message">>}),
    [Key | Acc].

-spec create_ledger(kapps_im:direction(), kz_json:object()) -> 'ok'.
create_ledger(Type, Payload) ->
    IM = kapps_im:from_payload(Payload),
    kapps_im:put_message_id(IM),
    lager:debug("creating ~s ledger", [Type]),
    kz_im_flat_rate:debit(IM).
