%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% handler for route requests, responds if callflows match
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_route_req).

-export([handle_req/2]).

-include("callflow.hrl").

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(DEFAULT_METAFLOWS(AccountId)
        ,kapps_account_config:get(AccountId, <<"metaflows">>, <<"default_metaflow">>, 'false')
       ).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CF_CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    _ = kz_util:put_callid(JObj),
    'true' = kapi_route:req_v(JObj),
    Routines = [fun maybe_referred_call/1
                ,fun maybe_device_redirected/1
               ],
    Call = kapps_call:exec(Routines, kapps_call:from_route_req(JObj)),
    case is_binary(kapps_call:account_id(Call))
        andalso callflow_should_respond(Call)
        andalso callflow_resource_allowed(Call)
    of
        'true' ->
            lager:info("received request ~s asking if callflows can route this call", [kapi_route:fetch_id(JObj)]),
            AllowNoMatch = allow_no_match(Call),
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch)
                                           orelse AllowNoMatch ->
                    maybe_prepend_preflow(JObj, Props, Call, Flow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find callflow ~p", [R])
            end;
        'false' ->
            lager:debug("callflow not handling fetch-id ~s", [kapi_route:fetch_id(JObj)])
    end.

-spec maybe_prepend_preflow(kz_json:object(), kz_proplist()
                            ,kapps_call:call(), kz_json:object()
                            ,boolean()
                           ) -> 'ok'.
maybe_prepend_preflow(JObj, Props, Call, Flow, NoMatch) ->
    AccountId = kapps_call:account_id(Call),
    case kz_account:fetch(AccountId) of
        {'error', _E} ->
            lager:warning("could not open account doc ~s : ~p", [AccountId, _E]),
            maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
        {'ok', Doc} ->
            case kz_json:get_ne_value([<<"preflow">>, <<"always">>], Doc) of
                'undefined' ->
                    lager:debug("ignore preflow, not set"),
                    maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
                PreflowId ->
                    NewFlow = prepend_preflow(AccountId, PreflowId, Flow),
                    maybe_reply_to_req(JObj, Props, Call, NewFlow, NoMatch)
            end
    end.

-spec prepend_preflow(ne_binary(), ne_binary(), kz_json:object()) ->
                             kz_json:object().
prepend_preflow(AccountId, PreflowId, Flow) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, PreflowId) of
        {'error', _E} ->
            lager:warning("could not open ~s in ~s : ~p", [PreflowId, AccountDb, _E]),
            Flow;
        {'ok', Doc} ->
            Children = kz_json:from_list([{<<"_">>, kz_json:get_value(<<"flow">>, Flow)}]),
            Preflow = kz_json:set_value(<<"children">>
                                        ,Children
                                        ,kz_json:get_value(<<"flow">>, Doc)
                                       ),
            kz_json:set_value(<<"flow">>, Preflow, Flow)
    end.

-spec maybe_reply_to_req(kz_json:object(), kz_proplist()
                         ,kapps_call:call(), kz_json:object(), boolean()) -> 'ok'.
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request", [kz_doc:id(Flow)
                                                       ,kapps_call:account_id(Call)
                                                      ]),
    case maybe_consume_token(Call, Flow) of
        'false' -> 'ok';
        'true' ->
            ControllerQ = props:get_value('queue', Props),
            NewCall = update_call(Flow, NoMatch, ControllerQ, Call),
            send_route_response(Flow, JObj, NewCall)
    end.

-spec maybe_consume_token(kapps_call:call(), kz_json:object()) -> boolean().
maybe_consume_token(Call, Flow) ->
    case kapps_config:get_is_true(?CF_CONFIG_CAT, <<"calls_consume_tokens">>, 'true') of
        'false' ->
            %% If configured to not consume tokens then don't block the call
            'true';
        'true' ->
            {Name, Cost} = bucket_info(Call, Flow),
            case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
                'true' -> 'true';
                'false' ->
                    lager:debug("bucket ~s doesn't have enough tokens(~b needed) for this call", [Name, Cost]),
                    'false'
            end
    end.

-spec bucket_info(kapps_call:call(), kz_json:object()) ->
                         {ne_binary(), pos_integer()}.
bucket_info(Call, Flow) ->
    case kz_json:get_value(<<"pvt_bucket_name">>, Flow) of
        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
        Name -> {Name, bucket_cost(Flow)}
    end.

-spec bucket_name_from_call(kapps_call:call(), kz_json:object()) -> ne_binary().
bucket_name_from_call(Call, Flow) ->
    <<(kapps_call:account_id(Call))/binary, ":", (kz_doc:id(Flow))/binary>>.

-spec bucket_cost(kz_json:object()) -> pos_integer().
bucket_cost(Flow) ->
    Min = kapps_config:get_integer(?CF_CONFIG_CAT, <<"min_bucket_cost">>, 5),
    case kz_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
        'undefined' -> Min;
        N when N < Min -> Min;
        N -> N
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Should this call be able to use outbound resources, the exact opposite
%% exists in the handoff module.  When updating this one make sure to sync
%% the change with that module
%% @end
%%-----------------------------------------------------------------------------
-spec allow_no_match(kapps_call:call()) -> boolean().
allow_no_match(Call) ->
    is_valid_endpoint(kapps_call:custom_channel_var(<<"Referred-By">>, Call), Call)
        orelse is_valid_endpoint(kapps_call:custom_channel_var(<<"Redirected-By">>, Call), Call)
        orelse allow_no_match_type(Call).

-spec allow_no_match_type(kapps_call:call()) -> boolean().
allow_no_match_type(Call) ->
    case kapps_call:authorizing_type(Call) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _ -> 'true'
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if callflows should respond to a route request
%% @end
%%-----------------------------------------------------------------------------
-spec callflow_should_respond(kapps_call:call()) -> boolean().
callflow_should_respond(Call) ->
    case kapps_call:authorizing_type(Call) of
        <<"account">> -> 'true';
        <<"user">> -> 'true';
        <<"device">> -> 'true';
        <<"callforward">> -> 'true';
        <<"clicktocall">> -> 'true';
        <<"resource">> -> 'true';
        <<"sys_info">> ->
            timer:sleep(500),
            Number = kapps_call:request_user(Call),
            (not knm_converters:is_reconcilable(Number));
        'undefined' -> 'true';
        _Else -> 'false'
    end.

-spec callflow_resource_allowed(kapps_call:call()) -> boolean().
callflow_resource_allowed(Call) ->
    is_resource_allowed(kapps_call:resource_type(Call)).

-spec is_resource_allowed(api_binary()) -> boolean().
is_resource_allowed('undefined') -> 'true';
is_resource_allowed(ResourceType) ->
    lists:member(ResourceType, ?RESOURCE_TYPES_HANDLED).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response(kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
send_route_response(Flow, JObj, Call) ->
    lager:info("callflows knows how to route the call! sending park response"),
    AccountId = kapps_call:account_id(Call),
    Resp = props:filter_undefined(
             [{?KEY_MSG_ID, kz_api:msg_id(JObj)}
              ,{?KEY_MSG_REPLY_ID, kapps_call:call_id_direct(Call)}
              ,{<<"Routes">>, []}
              ,{<<"Method">>, <<"park">>}
              ,{<<"Transfer-Media">>, get_transfer_media(Flow, JObj)}
              ,{<<"Ringback-Media">>, get_ringback_media(Flow, JObj)}
              ,{<<"Pre-Park">>, pre_park_action(Call)}
              ,{<<"From-Realm">>, kz_util:get_account_realm(AccountId)}
              ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    case kz_amqp_worker:call(Resp
                             ,Publisher
                             ,fun kapi_route:win_v/1
                             ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("callflow has received a route win, taking control of the call"),
            cf_route_win:execute_callflow(RouteWin, kapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec get_transfer_media(kz_json:object(), kz_json:object()) -> api_binary().
get_transfer_media(Flow, JObj) ->
    case kz_json:get_value([<<"ringback">>, <<"transfer">>], Flow) of
        'undefined' ->
            kz_json:get_value(<<"Transfer-Media">>, JObj);
        MediaId -> MediaId
    end.

-spec get_ringback_media(kz_json:object(), kz_json:object()) -> api_binary().
get_ringback_media(Flow, JObj) ->
    case kz_json:get_value([<<"ringback">>, <<"early">>], Flow) of
        'undefined' ->
            kz_json:get_value(<<"Ringback-Media">>, JObj);
        MediaId -> MediaId
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec pre_park_action(kapps_call:call()) -> ne_binary().
pre_park_action(Call) ->
    case kapps_config:get_is_true(<<"callflow">>, <<"ring_ready_offnet">>, 'true')
        andalso kapps_call:inception(Call) =/= 'undefined'
        andalso kapps_call:authorizing_type(Call) =:= 'undefined'
    of
        'false' -> <<"none">>;
        'true' -> <<"ring_ready">>
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec update_call(kz_json:object(), boolean(), ne_binary(), kapps_call:call()) ->
                         kapps_call:call().
update_call(Flow, NoMatch, ControllerQ, Call) ->
    Props = [{'cf_flow_id', kz_doc:id(Flow)}
             ,{'cf_flow', kz_json:get_value(<<"flow">>, Flow)}
             ,{'cf_capture_group', kz_json:get_ne_value(<<"capture_group">>, Flow)}
             ,{'cf_no_match', NoMatch}
             ,{'cf_metaflow', kz_json:get_value(<<"metaflows">>, Flow, ?DEFAULT_METAFLOWS(kapps_call:account_id(Call)))}
            ],

    Updaters = [{fun kapps_call:kvs_store_proplist/2, Props}
                ,{fun kapps_call:set_controller_queue/2, ControllerQ}
                ,{fun kapps_call:set_application_name/2, ?APP_NAME}
                ,{fun kapps_call:set_application_version/2, ?APP_VERSION}
               ],
    kapps_call:exec(Updaters, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec maybe_referred_call(kapps_call:call()) -> kapps_call:call().
maybe_referred_call(Call) ->
    maybe_fix_restrictions(get_referred_by(Call), Call).

-spec maybe_device_redirected(kapps_call:call()) -> kapps_call:call().
maybe_device_redirected(Call) ->
    maybe_fix_restrictions(get_redirected_by(Call), Call).

-spec maybe_fix_restrictions(api_binary(), kapps_call:call()) -> kapps_call:call().
maybe_fix_restrictions('undefined', Call) -> Call;
maybe_fix_restrictions(Device, Call) ->
    case cf_util:endpoint_id_by_sip_username(kapps_call:account_db(Call), Device) of
        {'ok', EndpointId} -> kapps_call:kvs_store(?RESTRICTED_ENDPOINT_KEY, EndpointId, Call);
        {'error', 'not_found'} ->
            Keys = [<<"Authorizing-ID">>],
            kapps_call:remove_custom_channel_vars(Keys, Call)
    end.

-spec get_referred_by(kapps_call:call()) -> api_binary().
get_referred_by(Call) ->
    ReferredBy = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
    extract_sip_username(ReferredBy).

-spec get_redirected_by(kapps_call:call()) -> api_binary().
get_redirected_by(Call) ->
    RedirectedBy = kapps_call:custom_channel_var(<<"Redirected-By">>, Call),
    extract_sip_username(RedirectedBy).

-spec is_valid_endpoint(api_binary(), kapps_call:call()) -> boolean().
is_valid_endpoint('undefined', _) -> 'false';
is_valid_endpoint(Contact, Call) ->
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Contact, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} ->
            case cf_util:endpoint_id_by_sip_username(kapps_call:account_db(Call), Match) of
                {'ok', _EndpointId} -> 'true';
                {'error', 'not_found'} -> 'false'
            end;
        _ -> 'false'
    end.

-spec extract_sip_username(api_binary()) -> api_binary().
extract_sip_username('undefined') -> 'undefined';
extract_sip_username(Contact) ->
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Contact, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} -> Match;
        _ -> 'undefined'
    end.
