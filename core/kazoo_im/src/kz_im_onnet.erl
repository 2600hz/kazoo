%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_im_onnet).
-behaviour(gen_listener).

-export([start_link/0]).

-export([route/1, route/2]).

%% Responders
-export([handle_outbound/2
        ]).

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

-type state() :: map().

-define(BINDINGS, [{'im', [{'restrict_to', ['outbound']}]}]).

-define(QUEUE_NAME, <<"im">>).

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

-define(RESPONDERS, [{{?MODULE, 'handle_outbound'}, [{<<"*">>, <<"outbound">>}]}]).

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
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ,{'server_confirms', 'true'}
                            ]
                           ,[]
                           ).

%%------------------------------------------------------------------------------
%% @doc Route Internally.
%% @end
%%------------------------------------------------------------------------------
-spec route(kz_term:api_terms()) -> 'ok' | {'error', 'no_connections'}.
route(Payload) ->
    case kz_im_onnet_sup:worker() of
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
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #{count => 1, pids => #{}}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'route', Payload}, From, State) ->
    kapi_im:publish_inbound(Payload, ?AMQP_PUBLISH_OPTIONS),
    #{count := Count, pids := Pids} = State,
    Info = #{payload => kz_json:delete_key(<<"Body">>, Payload), from => From},
    {'noreply', State#{count => Count + 1, pids => Pids#{Count => Info}}};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', ?QUEUE_NAME}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'server_confirms', _Confirms}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'confirm', Confirm}}, State) ->
    {'noreply', handle_confirm(Confirm, State)};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
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

%%%=============================================================================
%%% Handle Outbound
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_outbound(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_outbound(JObj, Props) ->
    _ = kz_log:put_callid(JObj),
    Srv = props:get_value('server', Props),
    Deliver = props:get_value('deliver', Props),
    case kapi_im:outbound_v(JObj)
        andalso handle_outbound_route(JObj, Props)
    of
        'false' -> gen_listener:ack(Srv, Deliver);
        'ack' -> gen_listener:ack(Srv, Deliver);
        'nack' -> gen_listener:ack(Srv, Deliver)
    end.


-spec handle_outbound_route(kz_json:object(), kz_term:proplist()) -> 'ack' | 'nack'.
handle_outbound_route(JObj, Props) ->
    IM = kapps_im:from_payload(JObj),
    Funs = [fun account/1
           ,fun trusted_application/1
           ,fun account_fetch/1
           ,fun reseller_fetch/1
           ,fun account_is_enabled/1
           ,fun reseller_is_enabled/1
           ,fun account_has_im/1
           ,fun reseller_has_im/1
           ,fun account_standing_is_acceptable/1
           ,fun reseller_standing_is_acceptable/1
           ,fun number/1
           ],
    route_offnet(kz_maps:exec(Funs
                             ,#{payload => JObj
                               ,route => kz_im:route_id(JObj)
                               ,props => Props
                               ,im => IM
                               }
                             )).

-spec route_offnet(map()) -> 'ack' | 'nack'.
route_offnet(#{enabled := 'true'
              ,payload := Payload
              ,account_id := AccountId
              ,route := RouteId
              }) ->
    case kz_im_offnet:route(kz_im:set_route_id(Payload, RouteId)) of
        'ok' -> 'ack';
        {'error', _Error} ->
            lager:warning("offnet rejected request for account ~s : ~p", [AccountId, _Error]),
            'ack'
    end;
route_offnet(_Map) ->
    lager:debug_unsafe("not routing ~p", [_Map]),
    'nack'.

account(#{payload := JObj} = Map) ->
    case kz_im:account_id(JObj) of
        'undefined' -> Map;
        AccountId -> Map#{account_id => AccountId
                         ,reseller_id => kz_services_reseller:get_id(AccountId)
                         ,enabled => 'true'
                         }
    end.

trusted_application(#{payload := JObj} = Map) ->
    case kz_im:application_id(JObj) of
        'undefined' -> Map;
        AppId ->
            Trusted = kz_json:get_list_value([<<"outbound">>, <<"trusted_apps">>], config(), []),
            case lists:member(AppId, Trusted)  of
                'false' -> Map;
                'true' -> Map#{enabled => 'true'}
            end
    end.

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

account_is_enabled(#{account := Account} = Map) ->
    case kzd_accounts:enabled(Account) of
        'true' -> Map;
        'false' -> Map#{enabled => 'false'}
    end;
account_is_enabled(Map) -> Map.


reseller_is_enabled(#{reseller := Reseller} = Map) ->
    case kzd_accounts:enabled(Reseller) of
        'true' -> Map;
        'false' -> Map#{enabled => 'false'}
    end;
reseller_is_enabled(Map) -> Map.

account_has_im(#{account_id := AccountId, im := IM} = Map) ->
    case kz_services_im:is_enabled(AccountId, kapps_im:type(IM)) of
        'true' -> Map;
        'false' -> Map#{enabled => 'false'}
    end;
account_has_im(Map) -> Map.

reseller_has_im(#{reseller_id := ResellerId, im := IM} = Map) ->
    case kz_services_im:is_enabled(ResellerId, kapps_im:type(IM)) of
        'true' -> Map;
        'false' -> Map#{enabled => 'false'}
    end;
reseller_has_im(Map) -> Map.

-define(STANDING_ACCEPTABLE_OPTIONS, #{cache_acceptable => true}).

account_standing_is_acceptable(#{account_id := AccountId} = Map) ->
    case kz_services_standing:acceptable(AccountId, ?STANDING_ACCEPTABLE_OPTIONS) of
        {'true', _} -> Map;
        _Else -> Map#{enabled => 'false'}
    end;
account_standing_is_acceptable(Map) -> Map.

reseller_standing_is_acceptable(#{reseller_id := ResellerId} = Map) ->
    case kz_services_standing:acceptable(ResellerId, ?STANDING_ACCEPTABLE_OPTIONS) of
        {'true', _} -> Map;
        _Else -> Map#{enabled => 'false'}
    end;
reseller_standing_is_acceptable(Map) -> Map.

number(#{enabled := 'false'} = Map) -> Map;
number(#{payload := JObj, im := IM} = Map) ->
    case knm_phone_number:fetch(kz_im:from(JObj)) of
        {'ok', Num} ->
            case knm_im:enabled(Num, kapps_im:type(IM))
                andalso number_provider(Num)
            of
                'false' ->
                    lager:debug("number does not have ~s enabled", [kapps_im:type(IM)]),
                    Map#{enabled => 'false'};
                'undefined' ->
                    Map#{number => Num};
                Provider ->
                    Setters = [{fun kz_im:set_originator_property/3, <<"Number-Provider">>, Provider}
                              ,{fun kz_im:set_originator_flag/2, Provider}
                              ],
                    Map#{number => Num
                        ,module => Provider
                        ,route => Provider
                        ,payload => kz_json:exec_first(Setters, JObj)
                        }
            end;
        _ -> Map
    end.

number_provider(Num) ->
    Mod = knm_phone_number:module_name(Num),
    kz_json:get_ne_binary_value([<<"outbound">>, <<"knm">>, Mod], config(), Mod).

config() ->
    case kapps_config:get_category(?APP_NAME) of
        {'ok', JObj} -> kz_json:get_json_value([<<"default">>, <<"connector">>], JObj, kz_json:new());
        _ -> kz_json:new()
    end.

%%%=============================================================================
%%% Handle Confirms
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
handle_confirm(#'basic.ack'{delivery_tag = Idx, multiple = 'true'}
              ,#{pids := Pids} = State
              ) ->
    Keys = maps:fold(fun reply_ok/3, [], maps:filter(fun(K, _) -> K =< Idx end, Pids)),
    State#{pids => maps:without(Keys, Pids)};
handle_confirm(#'basic.ack'{delivery_tag = Idx, multiple = 'false'}
              ,#{pids := Pids} = State
              ) ->
    Keys = maps:fold(fun reply_ok/3, [], maps:with([Idx], Pids)),
    State#{pids => maps:without(Keys, Pids)};
handle_confirm(#'basic.nack'{delivery_tag = Idx, multiple = 'true'}
              ,#{pids := Pids} = State
              ) ->
    Keys = maps:fold(fun reply_error/3, [], maps:filter(fun(K, _) -> K =< Idx end, Pids)),
    State#{pids => maps:without(Keys, Pids)};
handle_confirm(#'basic.nack'{delivery_tag = Idx, multiple = 'false'}
              ,#{pids := Pids} = State
              ) ->
    Keys = maps:fold(fun reply_error/3, [], maps:with([Idx], Pids)),
    State#{pids => maps:without(Keys, Pids)}.

reply_ok(Key, #{payload := Payload, from := Pid}, Acc) ->
    gen_server:reply(Pid, 'ok'),
    _ = kz_process:spawn(fun create_ledger/2, ['inbound', Payload]),
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
