%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Make a request for authorization, and answer queries about the CallID
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_authz).

-export([authorize/3]).
-export([rate_channel/2]).
-export([kill_channel/2]).

-include("ecallmgr.hrl").

-define(RATE_VARS, [<<"Base-Cost">>
                   ,<<"Discount-Percentage">>
                   ,<<"Pvt-Cost">>
                   ,<<"Rate">>
                   ,<<"Rate-Description">>
                   ,<<"Rate-Increment">>
                   ,<<"Rate-Minimum">>
                   ,<<"Rate-Name">>
                   ,<<"Rate-NoCharge-Time">>
                   ,<<"Surcharge">>
                   ]).

-type authz_reply() :: boolean() | {'true', kz_json:object()}.

-export_type([authz_reply/0]).

-spec authorize(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
authorize(Data, CallId, Node) ->
    kz_log:put_callid(CallId),
    AuthorizeReply = is_emergency_number(Data)
        orelse is_mobile_device(Data)
        orelse maybe_authorized_channel(Data, Node),
    lager:info("channel is~s authorized", [authorized_log(AuthorizeReply)]),
    _ = ecallmgr_fs_channel:set_authorized(CallId, was_authorized(AuthorizeReply)),
    AuthorizeReply.

-spec was_authorized(authz_reply()) -> boolean().
was_authorized({'true', _}) -> 'true';
was_authorized('true') -> 'true';
was_authorized('false') -> 'false'.

-spec authorized_log(boolean()) -> string().
authorized_log({'true', _}) -> "";
authorized_log('true') -> "";
authorized_log('false') -> " not".

-spec kill_channel(kzd_freeswitch:data(), atom()) -> 'ok'.
kill_channel(Data, Node) ->
    Direction = kzd_freeswitch:call_direction(Data),
    ResourceType = kzd_freeswitch:resource_type(Data, <<"audio">>),
    CallId = kzd_freeswitch:call_id(Data),
    lager:debug("killing unauthorized channel"),
    kill_channel(Direction, ResourceType, CallId, Node).

-spec kill_channel(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), atom()) -> 'ok'.
kill_channel(_, <<"sms">>, _CallId, _Node) -> 'ok';
kill_channel(<<"inbound">>, _, CallId, Node) ->
    %% Give any pending route requests a chance to cleanly terminate this call,
    %% if it has not been processed yet.  Then chop its head off....
    _ = freeswitch:api(Node, 'uuid_kill', kz_term:to_list(<<CallId/binary, " USER_BUSY">>)),
    'ok';
kill_channel(<<"outbound">>, _, CallId, Node) ->
    _ = freeswitch:api(Node, 'uuid_kill', kz_term:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    'ok'.

-spec is_mobile_device(kzd_freeswitch:data()) -> authz_reply().
is_mobile_device(Data) ->
    <<"mobile">> =:=  kzd_freeswitch:authorizing_type(Data).

-spec maybe_authorized_channel(kzd_freeswitch:data(), atom()) -> authz_reply().
maybe_authorized_channel(Data, Node) ->
    case kzd_freeswitch:channel_authorized(Data) of
        <<"true">> ->
            lager:debug("channel is already authorized"),
            'true';
        <<"false">> ->
            lager:debug("channel is already denied authorization"),
            'false';
        _Else ->
            maybe_authorize_conference_number(Data)
                orelse maybe_channel_recovering(Data, kzd_freeswitch:call_id(Data), Node)
    end.

-spec maybe_authorize_conference_number(kzd_freeswitch:data()) -> authz_reply().
maybe_authorize_conference_number(Data) ->
    lager:debug("is destination number 'conference': ~s"
               ,[kzd_freeswitch:hunt_destination_number(Data)]
               ),
    <<"conference">> =:= kzd_freeswitch:hunt_destination_number(Data).

-spec maybe_channel_recovering(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
maybe_channel_recovering(Data, CallId, Node) ->
    case kzd_freeswitch:is_channel_recovering(Data, 'false') of
        'false' -> is_authz_enabled(Data, CallId, Node);
        'true' ->
            lager:info("channel is authorized because it is recovering"),
            allow_call(Data, CallId, Node)
    end.

-spec is_authz_enabled(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_authz_enabled(Data, CallId, Node) ->
    case kapps_config:is_true(?APP_NAME, <<"authz_enabled">>, 'false') of
        'true' -> is_global_resource(Data, CallId, Node);
        'false' ->
            lager:info("channel is authorized because config ecallmgr.authz is disabled"),
            allow_call(Data, CallId, Node)
    end.

-spec is_global_resource(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_global_resource(Data, CallId, Node) ->
    case kzd_freeswitch:is_consuming_global_resource(Data, 'true')
        orelse kapps_config:is_true(?APP_NAME, <<"authz_local_resources">>, 'false')
    of
        'true' -> is_consuming_resource(Data, CallId, Node);
        'false' ->
            lager:debug("channel is authorized because it is a local resource"),
            allow_call(Data, CallId, Node)
    end.

-spec is_consuming_resource(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_consuming_resource(Data, CallId, Node) ->
    case kzd_freeswitch:call_direction(Data) of
        <<"outbound">> ->
            is_consuming_outbound_resource(Data, CallId, Node);
        <<"inbound">> ->
            is_consuming_inbound_resource(Data, CallId, Node)
    end.

-spec is_consuming_outbound_resource(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_consuming_outbound_resource(Data, CallId, Node) ->
    case kzd_freeswitch:resource_id(Data) of
        'undefined' ->
            lager:debug("outbound channel is authorized because it is not consuming a resource"),
            allow_call(Data, CallId, Node);
        _ResourceId -> request_channel_authorization(Data, CallId, Node)
    end.

-spec is_consuming_inbound_resource(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_consuming_inbound_resource(Data, CallId, Node) ->
    case kzd_freeswitch:authorizing_id(Data) =:= 'undefined'
        orelse kzd_freeswitch:authorizing_type(Data) =:= <<"resource">>
    of
        'true' -> request_channel_authorization(Data, CallId, Node);
        'false' ->
            lager:debug("inbound channel is authorized because it is not consuming a resource"),
            allow_call(Data, CallId, Node)
    end.

-spec request_channel_authorization(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                                           authz_reply().
request_channel_authorization(Data, CallId, Node) ->
    lager:debug("channel authorization request started"),
    ReqResp = kz_amqp_worker:call(authz_req(Data)
                                 ,fun kapi_authz:publish_authz_req/1
                                 ,fun kapi_authz:authz_resp_v/1
                                 ,ecallmgr_fs_node:fetch_timeout(Node)
                                 ),
    case ReqResp of
        {'ok', JObj} -> authz_response(JObj, Data, CallId, Node);
        {'error', _R} ->
            lager:notice("authz request lookup failed: ~p", [_R]),
            authz_default(Data, CallId, Node)
    end.

-spec authz_response(kz_json:object(), kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
authz_response(JObj, Data, CallId, Node) ->
    case kz_json:is_true(<<"Is-Authorized">>, JObj)
        orelse kz_json:is_true(<<"Soft-Limit">>, JObj)
    of
        'true' -> authorize_account(JObj, Data, CallId, Node);
        'false' ->
            AccountBilling = kz_json:get_value(<<"Account-Billing">>, JObj),
            ResellerBilling = kz_json:get_value(<<"Reseller-Billing">>, JObj),
            lager:info("channel is unauthorized: ~s/~s" ,[AccountBilling, ResellerBilling]),
            case kapps_config:get_boolean(?APP_NAME, <<"authz_dry_run">>, 'false') of
                'true' -> authorize_account(JObj, Data, CallId, Node);
                'false' ->
                    %% Set the following CCVs so that we can see why the call was barred in CDRs

                    %% set Account-ID and Reseller-ID so CDRs can be saved for this call
                    %% (in case the call is inbound from carrier)
                    AccountId = kz_json:get_value(<<"Account-ID">>
                                                 ,JObj
                                                 ,kzd_freeswitch:account_id(Data)
                                                 ),
                    ResellerId = kz_json:get_value(<<"Reseller-ID">>
                                                  ,JObj
                                                  ,kzd_freeswitch:reseller_id(Data)
                                                  ),
                    _ = ecallmgr_fs_command:set(Node, CallId, [{<<"Account-Billing">>, AccountBilling}
                                                              ,{<<"Account-ID">>, AccountId}
                                                              ,{<<"Reseller-Billing">>, ResellerBilling}
                                                              ,{<<"Reseller-ID">>, ResellerId}
                                                              ]),
                    _ = kz_process:spawn(fun kill_channel/2, [Data, Node]),
                    'false'
            end
    end.

-spec authorize_account(kz_json:object(), kzd_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                               authz_reply().
authorize_account(JObj, Data, CallId, Node) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    Type      = kz_json:get_value(<<"Account-Billing">>, JObj),
    ChanVars  = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),

    lager:debug("channel is authorized by account ~s as ~s", [AccountId, Type]),
    P = kzd_freeswitch:set_ccvs(Data, [{<<"Account-ID">>, AccountId}
                                      ,{<<"Account-Billing">>, Type}
                                       | maybe_add_outbound_flags(ChanVars)
                                      ]),

    authorize_reseller(JObj, P, CallId, Node).

-spec maybe_add_outbound_flags(kz_json:object()) -> kz_term:proplist().
maybe_add_outbound_flags(JObj) ->
    case kz_json:get_value(<<"Outbound-Flags">>, JObj) of
        'undefined' -> [];
        Flags -> [{<<"Outbound-Flags">>, Flags}]
    end.

-spec authorize_reseller(kz_json:object(), kzd_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                                authz_reply().
authorize_reseller(JObj, Data, CallId, Node) ->
    AccountId = kzd_freeswitch:account_id(Data),
    case kz_json:get_value(<<"Reseller-ID">>, JObj, AccountId) of
        AccountId -> set_ccv_trunk_usage(JObj, Data, CallId, Node);
        ResellerId ->
            Type = kz_json:get_value(<<"Reseller-Billing">>, JObj),
            lager:debug("channel is authorized by reseller ~s as ~s", [ResellerId, Type]),
            P = kzd_freeswitch:set_ccvs(Data, [{<<"Reseller-ID">>, ResellerId}
                                              ,{<<"Reseller-Billing">>, Type}
                                              ]),
            set_ccv_trunk_usage(JObj, P, CallId, Node)
    end.

-spec set_ccv_trunk_usage(kz_json:object(), kzd_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                                 authz_reply().
set_ccv_trunk_usage(JObj, Data, CallId, Node) ->
    Usage = [{Key, TrunkUsage}
             || Key <- [<<"Account-Trunk-Usage">>
                       ,<<"Reseller-Trunk-Usage">>
                       ],
                'undefined' =/= (TrunkUsage = kz_call_event:custom_channel_var(JObj, Key))
            ],
    P = kzd_freeswitch:set_ccvs(Data, props:filter_undefined(Usage)),
    rate_call(P, CallId, Node).

-spec rate_call(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
rate_call(Data, CallId, Node) ->
    _P = kz_process:spawn(fun rate_channel/2, [Data, Node]),
    lager:debug("rating call in ~p", [_P]),
    allow_call(Data, CallId, Node).

-spec allow_call(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
allow_call(Data, _CallId, _Node) ->
    lager:debug("channel authorization succeeded, allowing call"),
    Vars = props:filter_undefined(
             [{<<"Account-ID">>, kzd_freeswitch:account_id(Data)}
             ,{<<"Account-Billing">>, kzd_freeswitch:account_billing(Data)}
             ,{<<"Account-Trunk-Usage">>, kzd_freeswitch:account_trunk_usage(Data)}
             ,{<<"Reseller-ID">>, kzd_freeswitch:reseller_id(Data)}
             ,{<<"Reseller-Billing">>, kzd_freeswitch:reseller_billing(Data)}
             ,{<<"Reseller-Trunk-Usage">>, kzd_freeswitch:reseller_trunk_usage(Data)}
             ,{<<"Global-Resource">>, kzd_freeswitch:is_consuming_global_resource(Data)}
             ,{<<"Channel-Authorized">>, <<"true">>}
             ]),
    case kzd_freeswitch:is_call_setup(Data) of
        'false' ->
            lager:info("channel is authorized (with channel vars)"),
            {'true', kz_json:from_list(Vars)};
        'true' ->
            lager:info("channel is authorized"),
            'true'
    end.

-spec rate_channel(kzd_freeswitch:data(), atom()) -> 'ok'.
rate_channel(Data, Node) ->
    CallId = kzd_freeswitch:call_id(Data),
    kz_log:put_callid(CallId),
    Direction = kzd_freeswitch:call_direction(Data),
    ReqResp = kz_amqp_worker:call(rating_req(CallId, Data)
                                 ,fun kapi_rate:publish_req/1
                                 ,fun kapi_rate:resp_v/1
                                  %% get inbound_rate_resp_timeout or outbound_rate_resp_timeout
                                 ,kapps_config:get_integer(?APP_NAME, <<Direction/binary, "_rate_resp_timeout">>, 10 * ?MILLISECONDS_IN_SECOND)
                                 ),
    rate_channel_resp(Data, Node, ReqResp).

-spec rate_channel_resp(kzd_freeswitch:data(), atom(), kz_amqp_worker:request_return()) -> 'ok'.
rate_channel_resp(Data, Node, {'ok', RespJObj}) ->
    maybe_set_rating_ccvs(Data, RespJObj, Node);
rate_channel_resp(Data, Node, {'error', _R}) ->
    lager:debug("rate request lookup failed: ~p", [_R]),

    %% disconnect only per_minute channels
    case <<"per_minute">> =:= kzd_freeswitch:account_billing(Data)
        orelse <<"per_minute">> =:= kzd_freeswitch:reseller_billing(Data)
    of
        'true' -> maybe_kill_unrated_channel(Data, Node);
        'false' -> 'ok'
    end.

-spec maybe_kill_unrated_channel(kzd_freeswitch:data(), atom()) -> 'ok'.
maybe_kill_unrated_channel(Data, Node) ->
    Direction = kzd_freeswitch:call_direction(Data),

    case kapps_config:is_true(?APP_NAME, <<Direction/binary, "_rate_required">>, 'false') of
        'false' -> 'ok';
        'true' ->
            lager:debug("no rate returned for ~s call, killing this channel", [Direction]),
            kill_channel(Data, Node)
    end.

-spec authz_default(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> {'ok', kz_term:ne_binary()} | boolean().
%% TODO: fix use of authz_default
authz_default(Data, CallId, Node) ->
    case kapps_config:get_ne_binary(?APP_NAME, <<"authz_default_action">>, <<"deny">>) =:= <<"deny">>
        andalso kapps_config:get_boolean(?APP_NAME, <<"authz_dry_run">>, 'false') =/= 'false'
    of
        'false' -> rate_call(Data, CallId, Node);
        'true' ->
            _ = kz_process:spawn(fun kill_channel/2, [Data, Node]),
            'false'
    end.

-spec maybe_set_rating_ccvs(kzd_freeswitch:data(), kz_json:object(), atom()) -> 'ok'.
maybe_set_rating_ccvs(Data, JObj, Node) ->
    case kz_json:get_value(<<"Rate">>, JObj) of
        'undefined' -> maybe_kill_unrated_channel(Data, Node);
        _Rate -> set_rating_ccvs(JObj, Node)
    end.

-spec set_rating_ccvs(kz_json:object(), atom()) -> 'ok'.
set_rating_ccvs(JObj, Node) ->
    lager:debug("setting rating information"),
    ecallmgr_fs_command:set(Node
                           ,kz_json:get_value(<<"Call-ID">>, JObj)
                           ,get_rating_ccvs(JObj)
                           ).

-spec get_rating_ccvs(kz_json:object()) -> kz_term:proplist().
get_rating_ccvs(JObj) ->
    lists:foldl(fun(Key, Acc) ->
                        rating_ccv(Key, Acc, JObj)
                end
               ,[]
               ,?RATE_VARS
               ).

-spec rating_ccv(kz_term:ne_binary(), kz_term:proplist(), kz_json:object()) ->
                        kz_term:proplist().
rating_ccv(<<"Rate">>, Acc, JObj) ->
    maybe_update_callee_id(JObj, Acc);
rating_ccv(Key, Acc, JObj) ->
    case kz_json:get_binary_value(Key, JObj) of
        'undefined' -> Acc;
        Value -> [{Key, Value}|Acc]
    end.

-spec maybe_update_callee_id(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
maybe_update_callee_id(JObj, Acc) ->
    Rate = kz_json:get_binary_value(<<"Rate">>, JObj, <<"0.00">>),

    case kz_json:is_true(<<"Update-Callee-ID">>, JObj, 'false') of
        'true' ->
            ConvertedRate = kz_term:to_binary(kz_currency:units_to_dollars(kz_term:to_number(Rate))),
            [{<<"ignore_display_updates">>, <<"false">>}
            ,{<<"effective_callee_id_name">>, <<"$", ConvertedRate/binary
                                                ," per min ${effective_callee_id_name}"
                                              >>
             }
            ,{<<"Rate">>, Rate}
             | Acc
            ];
        'false' -> [{<<"Rate">>, Rate}|Acc]
    end.

-spec authz_req(kzd_freeswitch:data()) -> kz_term:proplist().
authz_req(Data) ->
    AccountId = kzd_freeswitch:account_id(Data),
    props:filter_undefined(
      [{<<"To">>, kzd_freeswitch:to(Data)}
      ,{<<"From">>, kzd_freeswitch:from(Data)}
      ,{<<"Request">>, kzd_freeswitch:request(Data)}
      ,{<<"Call-ID">>, kzd_freeswitch:call_id(Data)}
      ,{<<"Call-Direction">>, kzd_freeswitch:call_direction(Data)}
      ,{<<"Other-Leg-Call-ID">>, kzd_freeswitch:other_leg_call_id(Data)}
      ,{<<"Caller-ID-Name">>
       ,kzd_freeswitch:caller_id_name(Data, kapps_call:unknown_caller_id_name(AccountId))
       }
      ,{<<"Caller-ID-Number">>
       ,kzd_freeswitch:caller_id_number(Data, kz_privacy:anonymous_caller_id_number(AccountId))
       }
      ,{<<"From-Network-Addr">>, kzd_freeswitch:from_network_ip(Data)}
      ,{<<"From-Network-Port">>, kzd_freeswitch:from_network_port(Data)}
      ,{<<"Custom-Channel-Vars">>, kzd_freeswitch:ccvs(Data)}
      ,{<<"Custom-Application-Vars">>, kzd_freeswitch:cavs(Data)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec outbound_flags(kzd_freeswitch:data()) -> [binary()] | 'undefined'.
outbound_flags(Data) ->
    case kzd_freeswitch:outbound_flags(Data) of
        'undefined' -> 'undefined';
        <<_/binary>> = Flags -> binary:split(Flags, <<"|">>);
        Flags when is_list(Flags) -> Flags
    end.

-spec rating_req(kz_term:ne_binary(), kzd_freeswitch:data()) -> kz_term:proplist().
rating_req(CallId, Data) ->
    props:filter_undefined([{<<"To-DID">>, kzd_freeswitch:to_did(Data)}
                           ,{<<"From-DID">>, kzd_freeswitch:caller_id_number(Data)}
                           ,{<<"Call-ID">>, CallId}
                           ,{<<"Account-ID">>, kzd_freeswitch:account_id(Data)}
                           ,{<<"Direction">>, kzd_freeswitch:call_direction(Data)}
                           ,{<<"Send-Empty">>, 'true'}
                           ,{<<"Outbound-Flags">>, outbound_flags(Data)}
                           ,{<<"Resource-ID">>, kzd_freeswitch:ccv(Data, <<"Resource-ID">>)}
                           ,{<<"Authorizing-Type">>, kzd_freeswitch:authorizing_type(Data)}
                            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ]).

-spec is_emergency_number(kzd_freeswitch:data()) -> authz_reply().
is_emergency_number(Data) ->
    <<"emergency">> =:= knm_converters:classify(kzd_freeswitch:to_did(Data))
        andalso <<"outbound">> =:= kzd_freeswitch:call_direction(Data).
