%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Calls coming from known clients, getting settings for caller-id and
%%% what not, and sending the calls offnet.
%%%
%%% If the call is destined for a known client, the offnet whapp (probably
%%% stepswitch) will redirect the request back in, to be picked up by
%%% ts_from_offnet.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_from_onnet).

-export([start_link/1, init/2]).

-include("ts.hrl").

-define(SERVER, ?MODULE).

-spec start_link(kz_json:object()) -> startlink_ret().
start_link(RouteReqJObj) ->
    proc_lib:start_link(?SERVER, 'init', [self(), RouteReqJObj]).

-spec init(pid(), kz_json:object()) -> 'ok'.
init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {'ok', self()}),
    Funs = [fun maybe_referred_call/1
           ,fun maybe_redirected_call/1
           ],
    JObj = kz_json:exec(Funs, RouteReqJObj),
    start_amqp(ts_callflow:init(JObj, <<"sys_info">>)).

start_amqp({'error', 'not_ts_account'}) -> 'ok';
start_amqp(State) ->
    maybe_onnet_data(ts_callflow:start_amqp(State)).

maybe_onnet_data(State) ->
    JObj = ts_callflow:get_request_data(State),
    CallID = ts_callflow:get_aleg_id(State),
    AccountId = ts_callflow:get_account_id(State),
    {ToUser, _} = kapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
    ToDID = knm_converters:normalize(ToUser, AccountId),
    FromUser = kz_json:get_value(<<"Caller-ID-Name">>, JObj),

    lager:info("on-net request from ~s(~s) to ~s", [FromUser, AccountId, ToDID]),
    Options =
        case ts_util:lookup_did(FromUser, AccountId) of
            {'ok', Opts} -> Opts;
            _ ->
                Username = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Username">>], JObj, <<>>),
                Realm = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Realm">>], JObj, <<>>),

                case ts_util:lookup_user_flags(Username, Realm, AccountId) of
                    {'ok', Opts} -> Opts;
                    _ -> kz_json:new()
                end
        end,
    SrvOptions = kz_json:get_value([<<"server">>, <<"options">>], Options, kz_json:new()),
    case knm_converters:is_reconcilable(ToDID)
        orelse knm_converters:classify(ToDID) =:= <<"emergency">>
        orelse kz_json:is_true(<<"hunt_non_reconcilable">>, SrvOptions, 'false')
        orelse kapps_config:get_is_true(?CONFIG_CAT, <<"default_hunt_non_reconcilable">>, 'false')
    of
        'false' ->
            lager:debug("number ~p is non_reconcilable and the server does not allow it", [ToDID]);
        'true' ->
            onnet_data(CallID, AccountId, FromUser, ToDID, Options, State)
    end.

onnet_data(CallID, AccountId, FromUser, ToDID, Options, State) ->
    DIDOptions = kz_json:get_value(<<"DID_Opts">>, Options, kz_json:new()),
    AccountOptions = kz_json:get_value(<<"account">>, Options, kz_json:new()),
    SrvOptions = kz_json:get_value([<<"server">>, <<"options">>], Options, kz_json:new()),
    RouteReq = ts_callflow:get_request_data(State),
    CustomSIPHeaders = kz_json:get_value(<<"Custom-SIP-Headers">>, RouteReq),
    MediaHandling = ts_util:get_media_handling([kz_json:get_value(<<"media_handling">>, DIDOptions)
                                               ,kz_json:get_value(<<"media_handling">>, SrvOptions)
                                               ,kz_json:get_value(<<"media_handling">>, AccountOptions)
                                               ]),
    SIPHeaders = ts_util:sip_headers([kz_json:get_value(<<"sip_headers">>, DIDOptions)
                                     ,kz_json:get_value(<<"sip_headers">>, SrvOptions)
                                     ,kz_json:get_value(<<"sip_headers">>, AccountOptions)
                                     ,CustomSIPHeaders
                                     ]),

    EmergencyCallerID =
        case ts_util:caller_id([kz_json:get_value(<<"emergency_caller_id">>, DIDOptions)
                               ,kz_json:get_value(<<"emergency_caller_id">>, SrvOptions)
                               ,kz_json:get_value(<<"emergency_caller_id">>, AccountOptions)
                               ])
        of
            {'undefined', 'undefined'} -> [];
            {ECIDName, ECIDNum} ->
                [{<<"Emergency-Caller-ID-Name">>, ECIDName}
                ,{<<"Emergency-Caller-ID-Number">>
                 ,ts_util:maybe_ensure_cid_valid('emergency', ECIDNum, FromUser, AccountId, CustomSIPHeaders)}

                ]
        end,
    OriginalCIdNumber = kz_json:get_value(<<"Caller-ID-Number">>, RouteReq),
    OriginalCIdName = kz_json:get_value(<<"Caller-ID-Name">>, RouteReq),
    CallerID =
        case ts_util:caller_id([kz_json:get_value(<<"caller_id">>, DIDOptions)
                               ,kz_json:get_value(<<"caller_id">>, SrvOptions)
                               ,kz_json:get_value(<<"caller_id">>, AccountOptions)
                               ])
        of
            {'undefined', 'undefined'} ->
                case kapps_config:get_is_true(?CONFIG_CAT, <<"ensure_valid_caller_id">>, 'false') of
                    'true' ->
                        ValidCID = ts_util:maybe_ensure_cid_valid('external', OriginalCIdNumber, FromUser, AccountId, CustomSIPHeaders),
                        [{<<"Outbound-Caller-ID-Number">>, ValidCID}
                        ,{<<"Outbound-Caller-ID-Name">>, OriginalCIdName}
                         | EmergencyCallerID
                        ];
                    'false' ->
                        [{<<"Outbound-Caller-ID-Number">>, OriginalCIdNumber}
                        ,{<<"Outbound-Caller-ID-Name">>, OriginalCIdName}
                         | EmergencyCallerID
                        ]
                end;
            {CIDName, CIDNum} ->
                [{<<"Outbound-Caller-ID-Name">>, CIDName}
                ,{<<"Outbound-Caller-ID-Number">>
                 ,ts_util:maybe_ensure_cid_valid('external', CIDNum, FromUser, AccountId, CustomSIPHeaders)}
                 | EmergencyCallerID
                ]
        end,
    DIDFlags = ts_util:offnet_flags([kz_json:get_value(<<"DID_Opts">>, DIDOptions)
                                    ,kz_json:get_value(<<"flags">>, SrvOptions)
                                    ,kz_json:get_value(<<"flags">>, AccountOptions)
                                    ]),
    Q = ts_callflow:get_my_queue(State),
    Command = [ KV
                || {_,V}=KV <- CallerID ++ EmergencyCallerID ++
                       [{<<"Call-ID">>, CallID}
                       ,{<<"Resource-Type">>, <<"audio">>}
                       ,{<<"To-DID">>, ToDID}
                       ,{<<"Account-ID">>, AccountId}
                       ,{<<"Application-Name">>, <<"bridge">>}
                       ,{<<"Flags">>, DIDFlags}
                       ,{<<"Media">>, MediaHandling}
                       ,{<<"Timeout">>, kz_json:get_value(<<"timeout">>, DIDOptions)}
                       ,{<<"Ignore-Early-Media">>, kz_json:get_value(<<"ignore_early_media">>, DIDOptions)}
                       ,{<<"Ringback">>, kz_json:get_value(<<"ringback">>, DIDOptions)}
                       ,{<<"Custom-SIP-Headers">>, SIPHeaders}
                       ,{<<"Hunt-Account-ID">>, kz_json:get_value(<<"hunt_account_id">>, SrvOptions)}
                       ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}])}
                        | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                       ],
                   V =/= 'undefined',
                   V =/= <<>>
              ],
    try
        lager:debug("we know how to route this call, sending park route response"),
        send_park(State, Command)
    catch
        _A:_B ->
            ST = erlang:get_stacktrace(),
            lager:info("exception ~p:~p", [_A, _B]),
            kz_util:log_stacktrace(ST),
            ts_callflow:send_hangup(State)
    end.

send_park(State, Command) ->
    wait_for_win(ts_callflow:send_park(State), Command).

wait_for_win(State, Command) ->
    case ts_callflow:wait_for_win(State) of
        {'lost', _} -> 'normal';
        {'won', State1} ->
            case ts_util:maybe_restrict_call(State1, Command) of
                'true' ->
                    lager:debug("Trunkstore call to ~p restricted", [props:get_value(<<"To-DID">>, Command)]),
                    ts_callflow:send_hangup(State1, <<"403">>);
                _ ->
                    send_offnet(State1, Command)
            end
    end.

send_offnet(State, Command) ->
    CtlQ = ts_callflow:get_control_queue(State),
    _ = kapi_offnet_resource:publish_req([{<<"Control-Queue">>, CtlQ}
                                          |Command
                                         ]),
    wait_for_bridge(CtlQ, State).

wait_for_bridge(CtlQ, State1) ->
    case ts_callflow:wait_for_bridge(State1) of
        {'hangup', _} -> ts_callflow:send_hangup(State1);
        {'error', #ts_callflow_state{aleg_callid='undefined'}} -> 'ok';
        {'error', #ts_callflow_state{aleg_callid=CallId}=State2} ->
            lager:info("responding to aleg ~s with 686", [CallId]),
            kz_call_response:send(CallId, CtlQ, <<"686">>),
            ts_callflow:send_hangup(State2, <<"686">>)
    end.

-spec maybe_referred_call(kz_json:object()) -> kz_json:object().
maybe_referred_call(JObj) ->
    maybe_fix_request(get_referred_by(JObj), JObj).

-spec maybe_redirected_call(kz_json:object()) -> kz_json:object().
maybe_redirected_call(JObj) ->
    maybe_fix_request(get_redirected_by(JObj), JObj).

-spec maybe_fix_request({binary(), binary()} | 'undefined', kz_json:object()) -> kz_json:object().
maybe_fix_request('undefined', JObj) -> JObj;
maybe_fix_request({Username, Realm}, JObj) ->
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    case ts_util:lookup_user_flags(Username, Realm, AccountId) of
        {'ok', _Opts} -> kz_json:set_values(fix_request_values(Username, Realm), JObj);
        _ -> JObj
    end.

-spec fix_request_values(binary(), binary()) -> kz_proplist().
fix_request_values(Username, Realm) ->
    [{[<<"Custom-Channel-Vars">>, <<"Username">>], Username}
    ,{[<<"Custom-Channel-Vars">>, <<"Realm">>], Realm}
    ,{[<<"Custom-Channel-Vars">>, <<"Authorizing-Type">>], <<"sys_info">>}
    ].

-spec get_referred_by(kz_json:object()) -> api_binary().
get_referred_by(JObj) ->
    ReferredBy = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Referred-By">>], JObj),
    extract_sip_username(ReferredBy).

-spec get_redirected_by(kz_json:object()) -> api_binary().
get_redirected_by(JObj) ->
    RedirectedBy = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Redirected-By">>], JObj),
    extract_sip_username(RedirectedBy).

-spec extract_sip_username(api_binary()) -> api_binary().
extract_sip_username('undefined') -> 'undefined';
extract_sip_username(Contact) ->
    ReOptions = [{'capture', 'all_but_first', 'binary'}],
    case catch(re:run(Contact, <<".*sip:(.*)@(.*)">>, ReOptions)) of
        {'match', [User, Realm]} -> {User, Realm};
        _ -> 'undefined'
    end.
