%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handler for route requests, responds if Callflows match.
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_req).

-export([handle_req/2
        ,allow_no_match/1
        ]).

-include("callflow.hrl").

-define(DEFAULT_METAFLOWS(AccountId)
       ,kapps_account_config:get(AccountId, <<"metaflows">>, <<"default_metaflow">>, 'false')
       ).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CF_CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(kapi_route:req(), kz_term:proplist()) -> 'ok'.
handle_req(RouteReq, Props) ->
    CallId = kapi_route:call_id(RouteReq),
    kz_log:put_callid(CallId),
    'true' = kapi_route:req_v(RouteReq),

    Routines = [fun maybe_referred_call/1
               ,fun maybe_device_redirected/1
               ],
    Call = kapps_call:exec(Routines, kapps_call:from_route_req(RouteReq)),
    case is_binary(kapps_call:account_id(Call))
        andalso callflow_should_respond(Call)
    of
        'true' ->
            lager:info("received request ~s asking if callflows can route the call to ~s"
                      ,[kapi_route:fetch_id(RouteReq), kapps_call:request_user(Call)]
                      ),
            AllowNoMatch = allow_no_match(Call),
            case cf_flow:lookup(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch)
                                           orelse AllowNoMatch ->
                    maybe_prepend_preflow(RouteReq, Props, Call, Flow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find callflow ~p", [R])
            end;
        'false' ->
            lager:debug("callflow not handling fetch-id ~s", [kapi_route:fetch_id(RouteReq)])
    end.

-spec maybe_prepend_preflow(kapi_route:req(), kz_term:proplist()
                           ,kapps_call:call(), kzd_callflows:doc()
                           ,boolean()
                           ) -> 'ok'.
maybe_prepend_preflow(RouteReq, Props, Call, Callflow, NoMatch) ->
    AccountId = kapps_call:account_id(Call),
    case kzd_accounts:fetch(AccountId) of
        {'error', _E} ->
            lager:warning("could not open account doc ~s : ~p", [AccountId, _E]),
            maybe_reply_to_req(RouteReq, Props, Call, Callflow, NoMatch);
        {'ok', AccountDoc} ->
            case kzd_accounts:preflow_id(AccountDoc) of
                'undefined' ->
                    lager:debug("ignore preflow, not set"),
                    maybe_reply_to_req(RouteReq, Props, Call, Callflow, NoMatch);
                PreflowId ->
                    NewCallflow = kzd_callflows:prepend_preflow(Callflow, PreflowId),
                    maybe_reply_to_req(RouteReq, Props, Call, NewCallflow, NoMatch)
            end
    end.

-spec maybe_reply_to_req(kapi_route:req(), kz_term:proplist()
                        ,kapps_call:call(), kz_json:object(), boolean()) -> 'ok'.
maybe_reply_to_req(RouteReq, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request for ~s", [kz_doc:id(Flow)
                                                             ,kapps_call:account_id(Call)
                                                             ,kapps_call:request_user(Call)
                                                             ]),
    case cf_util:token_check(Call, Flow) of
        'false' -> 'ok';
        'true' ->
            ControllerQ = kapi:encode_pid(props:get_value('queue', Props)),
            NewCall = update_call(Flow, NoMatch, ControllerQ, Call),
            send_route_response(Flow, RouteReq, NewCall)
    end.

%%------------------------------------------------------------------------------
%% @doc Should this call be able to use outbound resources, the exact opposite
%% exists in the handoff module.  When updating this one make sure to sync
%% the change with that module.
%% @end
%%------------------------------------------------------------------------------
-spec allow_no_match(kapps_call:call()) -> boolean().
allow_no_match(Call) ->
    is_valid_endpoint(kapps_call:custom_channel_var(<<"Referred-By">>, Call), Call)
        orelse is_valid_endpoint(kapps_call:custom_channel_var(<<"Redirected-By">>, Call), Call)
        orelse allow_no_match_type(Call)
        orelse is_authz_context(Call).

-spec is_authz_context(kapps_call:call()) -> boolean().
is_authz_context(Call) ->
    is_authz_context(Call, kapps_config:is_true(?APP_NAME, <<"allow_authz_context_overrides">>, 'false')).

-spec is_authz_context(kapps_call:call(), boolean()) -> boolean().
is_authz_context(_Call, 'false') ->
    lager:debug("authz context overrides disabled"),
    'false';
is_authz_context(Call, 'true') ->
    AuthzContexts = kapps_config:get_ne_binaries(?APP_NAME, <<"authz_contexts">>, []),
    CallContext = kapps_call:context(Call),
    lager:debug("checking authz contexts: ~p against call's ~p", [AuthzContexts, CallContext]),
    is_binary(CallContext)
        andalso lists:member(CallContext, AuthzContexts).

-spec allow_no_match_type(kapps_call:call()) -> boolean().
allow_no_match_type(Call) ->
    case kapps_call:authorizing_type(Call) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _Type ->
            lager:debug("allowing no-match for authz type ~s", [_Type]),
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc Determine if Callflows should respond to a route request.
%% @end
%%------------------------------------------------------------------------------
-spec callflow_should_respond(kapps_call:call()) -> boolean().
callflow_should_respond(Call) ->
    case kapps_call:authorizing_type(Call) of
        <<"account">> -> 'true';
        <<"user">> -> 'true';
        <<"device">> -> 'true';
        <<"mobile">> -> 'true';
        <<"callforward">> -> 'true';
        <<"clicktocall">> -> 'true';
        <<"click2call">> -> 'true';
        <<"conference">> -> 'true';
        <<"resource">> -> 'true';
        <<"sys_info">> ->
            timer:sleep(500),
            Number = kapps_call:request_user(Call),
            (not knm_converters:is_reconcilable(Number));
        'undefined' -> 'true';
        _Else ->
            lager:debug("not responding to calls from auth-type ~s", [_Else]),
            'false'
    end.

-spec ccvs(kz_json:object(), kapi_route:req(), kapps_call:call()) -> kz_term:api_object().
ccvs(Flow, _RouteReq, _Call) ->
    kz_json:from_list([{<<"CallFlow-ID">>, kz_doc:id(Flow)}]).

-spec cavs(kz_json:object(), kapi_route:req(), kapps_call:call()) -> kz_term:api_object().
cavs(_Flow, _RouteReq, _Call) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc Send a route response for a route request that can be fulfilled by this
%% process.
%% @end
%%------------------------------------------------------------------------------
-spec send_route_response(kz_json:object(), kapi_route:req(), kapps_call:call()) -> 'ok'.
send_route_response(Flow, RouteReq, Call) ->
    lager:info("callflows knows how to route the call! sending park response"),
    AccountId = kapps_call:account_id(Call),
    CallId = kapps_call:call_id(Call),
    ControllerQ = kapps_call:controller_queue(Call),
    FetchId =  kz_api:msg_id(RouteReq),
    Resp = props:filter_undefined(
             [{?KEY_MSG_ID, FetchId}
             ,{?KEY_API_ACCOUNT_ID, AccountId}
             ,{?KEY_API_CALL_ID, CallId}
             ,{<<"Routes">>, []}
             ,{<<"Method">>, <<"park">>}
             ,{<<"Transfer-Media">>, get_transfer_media(Flow, RouteReq)}
             ,{<<"Ringback-Media">>, get_ringback_media(Flow, RouteReq)}
             ,{<<"Pre-Park">>, pre_park_action(Call)}
             ,{<<"From-Realm">>, kzd_accounts:fetch_realm(AccountId)}
             ,{<<"Custom-Channel-Vars">>, ccvs(Flow, RouteReq, Call)}
             ,{<<"Custom-Application-Vars">>, cavs(Flow, RouteReq, Call)}
             ,{<<"Context">>, kapps_call:context(Call)}
              | kz_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
             ]),
    ServerId = kz_api:server_id(RouteReq),
    kapi_route:publish_resp(ServerId, Resp),
    wait_for_route_win(Call, FetchId).

wait_for_route_win(Call, FetchId) ->
    receive
        {'kapi', {_, {'call_event', 'CHANNEL_DESTROY'}, JObj}} ->
            case kz_call_event:fetch_id(JObj) of
                FetchId -> lager:info("received channel destroy while waiting for route_win, exiting");
                _Other -> wait_for_route_win(Call, FetchId)
            end;
        {'kapi', {_, {'dialplan', 'route_win'}, RouteWin}} ->
            lager:info("callflow has received a route win, taking control of the call"),
            cf_route_win:execute_callflow(RouteWin, kapps_call:from_route_win(RouteWin, Call))
    after ?ROUTE_WIN_TIMEOUT ->
            lager:warning("callflow didn't received a route win, exiting")
    end.

-spec get_transfer_media(kz_json:object(), kapi_route:req()) -> kz_term:api_binary().
get_transfer_media(Flow, RouteReq) ->
    case kz_json:get_value([<<"ringback">>, <<"transfer">>], Flow) of
        'undefined' ->
            kz_json:get_ne_binary_value(<<"Transfer-Media">>, RouteReq);
        MediaId -> MediaId
    end.

-spec get_ringback_media(kz_json:object(), kapi_route:req()) -> kz_term:api_binary().
get_ringback_media(Flow, RouteReq) ->
    case kz_json:get_value([<<"ringback">>, <<"early">>], Flow) of
        'undefined' ->
            kz_json:get_ne_binary_value(<<"Ringback-Media">>, RouteReq);
        MediaId -> MediaId
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pre_park_action(kapps_call:call()) -> kz_term:ne_binary().
pre_park_action(Call) ->
    case kapps_config:get_is_true(?CF_CONFIG_CAT, <<"ring_ready_offnet">>, 'true')
        andalso kapps_call:inception(Call) =/= 'undefined'
    of
        'false' -> <<"none">>;
        'true' -> <<"ring_ready">>
    end.

%%------------------------------------------------------------------------------
%% @doc process
%% @end
%%------------------------------------------------------------------------------
-spec update_call(kz_json:object(), boolean(), kz_term:ne_binary(), kapps_call:call()) ->
          kapps_call:call().
update_call(Flow, NoMatch, ControllerQ, Call) ->
    Props = [{'cf_flow_id', kz_doc:id(Flow)}
            ,{'cf_flow_name', kz_json:get_ne_binary_value(<<"name">>, Flow, kapps_call:request_user(Call))}
            ,{'cf_flow', kz_json:get_value(<<"flow">>, Flow)}
            ,{'cf_capture_group', kz_json:get_ne_value(<<"capture_group">>, Flow)}
            ,{'cf_capture_groups', kz_json:get_value(<<"capture_groups">>, Flow, kz_json:new())}
            ,{'cf_no_match', NoMatch}
            ,{'cf_metaflow', kz_json:get_value(<<"metaflows">>, Flow, ?DEFAULT_METAFLOWS(kapps_call:account_id(Call)))}
            ],

    Updaters = [{fun kapps_call:kvs_store_proplist/2, Props}
               ,{fun kapps_call:set_controller_queue/2, ControllerQ}
               ,{fun kapps_call:set_application_name/2, ?APP_NAME}
               ,{fun kapps_call:set_application_version/2, ?APP_VERSION}
               ,{fun kapps_call:insert_custom_channel_var/3, <<"CallFlow-ID">>, kz_doc:id(Flow)}
               ],
    kapps_call:exec(Updaters, Call).

%%------------------------------------------------------------------------------
%% @doc process
%% @end
%%------------------------------------------------------------------------------
-spec maybe_referred_call(kapps_call:call()) -> kapps_call:call().
maybe_referred_call(Call) ->
    maybe_fix_restrictions(get_referred_by(Call), Call).

-spec maybe_device_redirected(kapps_call:call()) -> kapps_call:call().
maybe_device_redirected(Call) ->
    maybe_fix_restrictions(get_redirected_by(Call), Call).

-spec maybe_fix_restrictions(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
maybe_fix_restrictions('undefined', Call) -> Call;
maybe_fix_restrictions(Device, Call) ->
    case cf_util:endpoint_id_by_sip_username(kapps_call:account_db(Call), Device) of
        {'ok', EndpointId} -> kapps_call:kvs_store(?RESTRICTED_ENDPOINT_KEY, EndpointId, Call);
        {'error', 'not_found'} ->
            Keys = [<<"Authorizing-ID">>],
            kapps_call:remove_custom_channel_vars(Keys, Call)
    end.

-spec get_referred_by(kapps_call:call()) -> kz_term:api_binary().
get_referred_by(Call) ->
    ReferredBy = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
    extract_sip_username(ReferredBy).

-spec get_redirected_by(kapps_call:call()) -> kz_term:api_binary().
get_redirected_by(Call) ->
    RedirectedBy = kapps_call:custom_channel_var(<<"Redirected-By">>, Call),
    extract_sip_username(RedirectedBy).

-spec is_valid_endpoint(kz_term:api_binary(), kapps_call:call()) -> boolean().
is_valid_endpoint('undefined', _) -> 'false';
is_valid_endpoint(Contact, Call) ->
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Contact, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} ->
            case cf_util:endpoint_id_by_sip_username(kapps_call:account_db(Call), Match) of
                {'ok', _EndpointId} ->
                    lager:debug("matched ~p to endpoint ~p", [Match, _EndpointId]),
                    'true';
                {'error', 'not_found'} -> 'false'
            end;
        _ -> 'false'
    end.

-spec extract_sip_username(kz_term:api_binary()) -> kz_term:api_binary().
extract_sip_username('undefined') -> 'undefined';
extract_sip_username(Contact) ->
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Contact, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} -> Match;
        _ -> 'undefined'
    end.
