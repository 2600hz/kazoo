%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(doodle_route_req).

-export([handle_req/2]).

-include("doodle.hrl").

-define(RESOURCE_TYPES_HANDLED,[<<"sms">>]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    Call = whapps_call:from_route_req(JObj),
    case is_binary(whapps_call:account_id(Call))
        andalso resource_allowed(Call)
    of
        'false' -> 'ok';
        'true' ->
            lager:info("received a request asking if doodle can route this message"),
            AllowNoMatch = allow_no_match(Call),
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    NewFlow = maybe_prepend_preflow(Call, Flow),
                    maybe_reply_to_req(JObj, Props, Call, NewFlow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find callflow ~p", [R])
            end
    end.

-spec maybe_prepend_preflow(whapps_call:call(), wh_json:object()) -> wh_json:object().
maybe_prepend_preflow(Call, CallFlow) ->
    AccountDb = whapps_call:account_db(Call),
    case kz_account:fetch(AccountDb) of
        {'error', _E} ->
            lager:warning("could not open account doc ~s : ~p", [AccountDb, _E]),
            CallFlow;
        {'ok', Doc} ->
            case wh_json:get_ne_value([<<"preflow">>, <<"always">>], Doc) of
                'undefined' -> CallFlow;
                PreflowId   -> prepend_preflow(AccountDb, PreflowId, CallFlow)
            end
    end.

-spec prepend_preflow(ne_binary(), ne_binary(), wh_json:object()) -> wh_json:object().
prepend_preflow(AccountDb, PreflowId, CallFlow) ->
    case couch_mgr:open_cache_doc(AccountDb, PreflowId) of
        {'error', _E} ->
            lager:warning("could not open ~s in ~s : ~p", [PreflowId, AccountDb, _E]),
            CallFlow;
        {'ok', Doc} ->
            Children = wh_json:from_list([{<<"_">>, wh_json:get_value(<<"flow">>, CallFlow)}]),
            Preflow = wh_json:set_value(<<"children">>, Children, wh_json:get_value(<<"flow">>, Doc)),
            wh_json:set_value(<<"flow">>, Preflow, CallFlow)
    end.



-spec resource_allowed(whapps_call:call()) -> boolean().
resource_allowed(Call) ->
    is_resource_allowed(whapps_call:resource_type(Call)).

-spec is_resource_allowed(api_binary()) -> boolean().
is_resource_allowed('undefined') -> 'true';
is_resource_allowed(ResourceType) ->
    lists:member(ResourceType, ?RESOURCE_TYPES_HANDLED).

-spec allow_no_match(whapps_call:call()) -> boolean().
allow_no_match(Call) ->
    whapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= 'undefined'
        orelse allow_no_match_type(Call).

-spec allow_no_match_type(whapps_call:call()) -> boolean().
allow_no_match_type(Call) ->
    case whapps_call:authorizing_type(Call) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _ -> 'true'
    end.

-spec maybe_reply_to_req(wh_json:object(), wh_proplist(), whapps_call:call(), wh_json:object(), boolean()) ->
                                'ok'.
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request"
               ,[wh_doc:id(Flow), whapps_call:account_id(Call)]),
    {Name, Cost} = bucket_info(Call, Flow),

    case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
        'false' ->
            lager:debug("bucket ~s doesn't have enough tokens(~b needed) for this call", [Name, Cost]);
        'true' ->
            ControllerQ = props:get_value('queue', Props),
            UpdatedCall = cache_call(Flow, NoMatch, ControllerQ, Call, JObj),
            send_route_response(Flow, JObj, ControllerQ, UpdatedCall)
    end.

-spec bucket_info(whapps_call:call(), wh_json:object()) -> {ne_binary(), pos_integer()}.
bucket_info(Call, Flow) ->
    case wh_json:get_value(<<"pvt_bucket_name">>, Flow) of
        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
        Name -> {Name, bucket_cost(Flow)}
    end.

-spec bucket_name_from_call(whapps_call:call(), wh_json:object()) -> ne_binary().
bucket_name_from_call(Call, Flow) ->
    <<(whapps_call:account_id(Call))/binary, ":", (wh_doc:id(Flow))/binary>>.

-spec bucket_cost(wh_json:object()) -> pos_integer().
bucket_cost(Flow) ->
    Min = whapps_config:get_integer(?CONFIG_CAT, <<"min_bucket_cost">>, 1),
    case wh_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
        'undefined' -> Min;
        N when N < Min -> Min;
        N -> N
    end.

-spec send_route_response(wh_json:object(), wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
send_route_response(_Flow, JObj, Q, _Call) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"sms">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("doodle knows how to route the message! sent sms response").

-spec cache_call(wh_json:object(), boolean(), ne_binary(), whapps_call:call(), wh_json:object()) -> whapps_call:call().
cache_call(Flow, NoMatch, ControllerQ, Call, JObj) ->
    Updaters = [{fun whapps_call:kvs_store_proplist/2
                 ,[{'cf_flow_id', wh_doc:id(Flow)}
                   ,{'cf_flow', wh_json:get_value(<<"flow">>, Flow)}
                   ,{'cf_capture_group', wh_json:get_ne_value(<<"capture_group">>, Flow)}
                   ,{'cf_no_match', NoMatch}
                   ,{'cf_metaflow', wh_json:get_value(<<"metaflows">>, Flow)}
                   ,{'flow_status', <<"queued">>}
                  ]
                }
                ,{fun whapps_call:set_controller_queue/2, ControllerQ}
                ,{fun whapps_call:set_application_name/2, ?APP_NAME}
                ,{fun whapps_call:set_application_version/2, ?APP_VERSION}
                ,fun(C) -> cache_resource_types(Flow, C, JObj) end
               ],
    UpdatedCall = whapps_call:exec(Updaters, Call),
    whapps_call:cache(UpdatedCall),
    UpdatedCall.

-spec cache_resource_types(wh_json:object(), whapps_call:call(), wh_json:object()) -> whapps_call:call().
cache_resource_types(Flow, Call, JObj) ->
    lists:foldl(fun(K, C1) ->
                        whapps_call:kvs_store(K, wh_json:get_value(K, JObj), C1)
                end
                ,Call
                ,cache_resource_types(whapps_call:resource_type(Call), Flow, Call, JObj)
               ).

-spec cache_resource_types(ne_binary(), wh_json:object(), whapps_call:call(), wh_json:object()) -> ne_binaries().
cache_resource_types(<<"sms">>, _Flow, _Call, _JObj) ->
    [<<"Message-ID">>, <<"Body">>];
cache_resource_types(_Other, _Flow, _Call, _JObj) -> [].
