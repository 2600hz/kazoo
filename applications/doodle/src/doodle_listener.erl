%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_listener).
-behaviour(gen_listener).

-export([start_link/0]).

%% Responders
-export([handle_message/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("doodle.hrl").

-define(SERVER, ?MODULE).

-type state() :: map().
-type context() :: map().

-define(BINDINGS, [{'im', [{'restrict_to', ['inbound']}
                          ,{'im_types', ['sms']}
                          ]
                   }
                  ]).

-define(QUEUE_NAME, <<"doodle">>).

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

-define(RESPONDERS, [{{?MODULE, 'handle_message'}, [{<<"sms">>, <<"inbound">>}]}]).

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
                            ]
                           ,[]
                           ).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #{}}.

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
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
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
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
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
%%% Handle Inbound
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_message(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_message(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Deliver = props:get_value('deliver', Props),
    case kapi_im:inbound_v(JObj) of
        'true' ->
            Context = inbound_context(JObj, Props),
            kz_log:put_callid(kz_im:message_id(JObj)),
            handle_inbound(Context);
        'false' ->
            lager:debug("error validating inbound message : ~p", [JObj]),
            gen_listener:nack(Srv, Deliver)
    end.

-spec inbound_context(kz_json:object(), kz_term:proplist()) -> context().
inbound_context(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Deliver = props:get_value('deliver', Props),
    Basic = props:get_value('basic', Props),
    {Number, Inception} = doodle_util:get_inbound_destination(JObj),
    #{number => Number
     ,inception => Inception
     ,request => JObj
     ,route_id => kz_im:route_id(JObj)
     ,route_type => kz_im:route_type(JObj)
     ,basic => Basic
     ,deliver => Deliver
     ,server => Srv
     }.

-spec handle_inbound(context()) -> 'ok'.
handle_inbound(#{number := Number} = Context0) ->
    Routines = [fun custom_vars/1
               ,fun custom_header_token/1
               ,fun lookup_number/1
               ,fun account_from_number/1
               ,fun set_inception/1
               ,fun lookup_mdn/1
               ,fun create_im/1
               ],
    case kz_maps:exec(Routines, Context0) of
        #{account_id := _AccountId} = Context ->
            lager:info("processing inbound sms request ~s in account ~s", [Number, _AccountId]),
            route_message(Context);
        Context ->
            lager:info("unable to determine account for ~s => ~p", [Number, Context]),
            %% TODO send system notify ?
            ack(Context)
    end.

ack(#{server := Server
     ,deliver := Deliver
     }) -> gen_listener:ack(Server, Deliver).

%% nack(#{server := Server
%%        ,deliver := Deliver
%%        }) -> gen_listener:nack(Server, Deliver).


custom_vars(#{authorizing_id := _} = Map) -> Map;
custom_vars(#{request := JObj} = Map) ->
    CCVsFilter = [<<"Account-ID">>, <<"Authorizing-ID">>],
    CCVs = kz_json:get_json_value(<<"Custom-Vars">>, JObj, kz_json:new()),
    Filtered = [{CCV, V} || CCV <- CCVsFilter, (V = kz_json:get_value(CCV, CCVs)) =/= 'undefined'],
    lists:foldl(fun custom_var/2, Map, Filtered).

custom_var({K,V}, Map) ->
    maps:put(kz_term:to_atom(kz_json:normalize_key(K), 'true'), V, Map).

custom_header_token(#{authorizing_id := _} = Map) -> Map;
custom_header_token(#{request := JObj} = Map) ->
    case kz_json:get_ne_binary_value([<<"Custom-SIP-Headers">>, <<"X-AUTH-Token">>], JObj) of
        'undefined' -> Map;
        Token ->
            custom_header_token(Map, JObj, Token)
    end.

custom_header_token(Map, JObj, Token) ->
    case binary:split(Token, <<"@">>, ['global']) of
        [AuthorizingId, AccountId | _] ->
            AccountRealm = kzd_accounts:fetch_realm(AccountId),
            AccountDb = kzs_util:format_account_db(AccountId),
            case kz_datamgr:open_cache_doc(AccountDb, AuthorizingId) of
                {'ok', Doc} ->
                    Props = props:filter_undefined([{?CV(<<"Authorizing-Type">>), kz_doc:type(Doc)}
                                                   ,{?CV(<<"Authorizing-ID">>), AuthorizingId}
                                                   ,{?CV(<<"Owner-ID">>), kzd_devices:owner_id(Doc)}
                                                   ,{?CV(<<"Account-ID">>), AccountId}
                                                   ,{?CV(<<"Realm">>), AccountRealm}
                                                   ]),
                    Map#{authorizing_id => AuthorizingId
                        ,account_id => AccountId
                        ,request => kz_json:set_values(Props, JObj)
                        };
                _Else ->
                    lager:warning("unexpected result reading doc ~s/~s => ~p", [AuthorizingId, AccountId, _Else]),
                    Map
            end;
        _Else ->
            lager:warning("unexpected result spliting Token => ~p", [_Else]),
            Map
    end.

lookup_number(#{account_id := _AccountId} = Map) -> Map;
lookup_number(#{number := Number} = Map) ->
    case knm_phone_number:fetch(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for ~s: ~p", [Number, _R]),
            Map;
        {'ok', KNumber} ->
            Map#{phone_number => KNumber, used_by => knm_phone_number:used_by(KNumber)}
    end;
lookup_number(Map) ->
    Map.

account_from_number(#{account_id := _AccountId} = Map) -> Map;
account_from_number(#{phone_number := KNumber, request := JObj} = Map) ->
    case knm_phone_number:assigned_to(KNumber) of
        'undefined' -> Map;
        AccountId ->
            Props = [{<<"Account-ID">>, AccountId}
                    ,{?CV(<<"Account-ID">>), AccountId}
                    ,{?CV(<<"Authorizing-Type">>), <<"resource">>}
                    ],
            Map#{account_id => AccountId
                ,request => kz_json:set_values(Props, JObj)
                }
    end;
account_from_number(Map) ->
    Map.

-spec set_inception(context()) -> context().
set_inception(#{inception := <<"offnet">>, request := JObj} = Map) ->
    Request = kz_json:get_value(<<"From">>, JObj),
    Map#{request => kz_json:set_value(?CV(<<"Inception">>), Request, JObj)};
set_inception(#{request := JObj} = Map) ->
    Map#{request => kz_json:delete_keys([<<"Inception">>, ?CV(<<"Inception">>)], JObj)}.

-spec lookup_mdn(context()) -> context().
lookup_mdn(#{authorizing_id := _AuthorizingId} = Map) -> Map;
lookup_mdn(#{phone_number := KNumber, request := JObj} = Map) ->
    Number = knm_phone_number:number(KNumber),
    case doodle_util:lookup_mdn(Number) of
        {'ok', Id, OwnerId} ->
            Props = props:filter_undefined([{?CV(<<"Authorizing-Type">>), <<"device">>}
                                           ,{?CV(<<"Authorizing-ID">>), Id}
                                           ,{?CV(<<"Owner-ID">>), OwnerId}
                                           ]),
            JObj1 = kz_json:delete_keys([?CV(<<"Authorizing-Type">>)
                                        ,?CV(<<"Authorizing-ID">>)
                                        ], JObj),
            Map#{request => kz_json:set_values(Props, JObj1)};
        {'error', _} -> Map
    end;
lookup_mdn(Map) -> Map.

-spec create_im(context()) -> context().
create_im(#{request:= SmsReq, account_id := AccountId} = Map) ->
    Funs = [{fun kapps_im:set_application_name/2, ?APP_NAME}
           ,{fun kapps_im:set_application_version/2, ?APP_VERSION}
           ,{fun kapps_im:set_account_id/2, AccountId}
           ,fun maybe_update_sender/1
           ],
    IM = kapps_im:exec(Funs, kapps_im:from_payload(SmsReq)),
    Map#{fetch_id => kapps_im:message_id(IM)
        ,message_id => kapps_im:message_id(IM)
        ,im => IM
        };
create_im(Map) -> Map.

-spec maybe_update_sender(kapps_im:im()) -> kapps_im:im().
maybe_update_sender(Im) ->
    case kapps_im:endpoint(Im) of
        'undefined' -> Im;
        EP ->
            CID = kzd_devices:caller_id(EP, kz_json:new()),
            From = kzd_caller_id:internal_number(CID, kapps_im:from(Im)),
            kapps_im:set_from(From, Im)
    end.

%%%=============================================================================
%%% Route Message
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec route_message(context()) -> 'ok'.
route_message(#{im := Im} = Context) ->
    AllowNoMatch = allow_no_match(Im),
    case kz_flow:lookup(Im) of
        %% if NoMatch is false then allow the textflow or if it is true and we are able allowed
        %% to use it for this message
        {'ok', Flow, NoMatch} when (not NoMatch)
                                   orelse AllowNoMatch ->
            route_message(Context, Flow, NoMatch);
        {'ok', _, 'true'} ->
            lager:info("only available flow is a nomatch for a unauthorized message"),
            ack(Context);
        {'error', R} ->
            lager:info("unable to find flow ~p", [R]),
            ack(Context)
    end.

-spec allow_no_match(kapps_im:im()) -> boolean().
allow_no_match(Im) ->
    allow_no_match_type(Im).

-spec allow_no_match_type(kapps_im:im()) -> boolean().
allow_no_match_type(Im) ->
    case kapps_im:authorizing_type(Im) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _ -> 'true'
    end.

-spec route_message(context(), kz_json:object(), boolean()) -> 'ok'.
route_message(#{im := Im} = Context, Flow, NoMatch) ->
    lager:info("flow ~s in ~s satisfies request"
              ,[kz_doc:id(Flow), kapps_im:account_id(Im)]),
    Updaters = [{fun kapps_im:kvs_store_proplist/2
                ,[{'tf_flow_id', kz_doc:id(Flow)}
                 ,{'tf_flow', kz_json:get_value(<<"flow">>, Flow)}
                 ,{'tf_no_match', NoMatch}
                 ]
                }
               ],
    {'ok', Pid} = tf_exe_sup:new(kapps_im:exec(Updaters, Im)),
    MonitorRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonitorRef, 'process', Pid, {shutdown, Reason}} ->
            lager:info("textflow result : ~p", [Reason]),
            ack(Context);
        {'DOWN', MonitorRef, 'process', Pid, Reason} ->
            lager:info("textflow result : ~p", [Reason]),
            ack(Context)
    end.
