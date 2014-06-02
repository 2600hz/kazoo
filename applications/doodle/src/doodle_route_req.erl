%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
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
        'true' ->
            lager:info("received a request asking if doodle can route this message"),
            AllowNoMatch = allow_no_match(Call),
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find callflow ~p", [R])
            end;
        
        'false' ->
            'ok'
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

   
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)
                                                       ,whapps_call:account_id(Call)
                                                      ]),
    {Name, Cost} = bucket_info(Call, Flow),

    case kz_buckets:consume_tokens(Name, Cost) of
        'false' ->
            lager:debug("bucket ~s doesn't have enough tokens(~b needed) for this call", [Name, Cost]);
        'true' ->
            ControllerQ = props:get_value('queue', Props),
            cache_call(Flow, NoMatch, ControllerQ, Call, JObj),
            send_route_response(Flow, JObj, ControllerQ, Call)
    end.

-spec bucket_info(whapps_call:call(), wh_json:object()) -> {ne_binary(), pos_integer()}.
bucket_info(Call, Flow) ->
    case wh_json:get_value(<<"pvt_bucket_name">>, Flow) of
        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
        Name -> {Name, bucket_cost(Flow)}
    end.

-spec bucket_name_from_call(whapps_call:call(), wh_json:object()) -> ne_binary().
bucket_name_from_call(Call, Flow) ->
    <<(whapps_call:account_id(Call))/binary, ":", (wh_json:get_value(<<"_id">>, Flow))/binary>>.

-spec bucket_cost(wh_json:object()) -> pos_integer().
bucket_cost(Flow) ->
    Min = whapps_config:get_integer(?CONFIG_CAT, <<"min_bucket_cost">>, 5),
    case wh_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
        'undefined' -> Min;
        N when N < Min -> Min;
        N -> N
    end.



-spec send_route_response(wh_json:object(), wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
send_route_response(Flow, JObj, Q, Call) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"sms">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("doodle knows how to route the message! sent sms response").

-spec cache_call(wh_json:object(), boolean(), ne_binary(), whapps_call:call(), wh_json:object()) -> 'ok'.
cache_call(Flow, NoMatch, ControllerQ, Call, JObj) ->
    
    Updaters = [fun(C) ->
                        Props = [{'cf_flow_id', wh_json:get_value(<<"_id">>, Flow)}
                                 ,{'cf_flow', wh_json:get_value(<<"flow">>, Flow)}
                                 ,{'cf_capture_group', wh_json:get_ne_value(<<"capture_group">>, Flow)}
                                 ,{'cf_no_match', NoMatch}
                                 ,{'cf_metaflow', wh_json:get_value(<<"metaflows">>, Flow)}
                                ],
                        whapps_call:kvs_store_proplist(Props, C)
                end
                ,fun(C) -> whapps_call:set_controller_queue(ControllerQ, C) end
                ,fun(C) -> whapps_call:set_application_name(?APP_NAME, C) end
                ,fun(C) -> whapps_call:set_application_version(?APP_VERSION, C) end
                ,fun(C) -> cache_resource_types(Flow, C, JObj) end
               ],
    whapps_call:cache(lists:foldr(fun(F, C) -> F(C) end, Call, Updaters)).

-spec cache_resource_types(wh_json:object(), whapps_call:call(), wh_json:object()) -> whapps_call:call().
cache_resource_types(Flow, Call, JObj) ->
     lists:foldl(fun([K | K1]=KL , C1) ->
                         whapps_call:kvs_store(lists:nthtail(1, KL), wh_json:get_value(KL, JObj), C1);
                    (K, C1) ->
                         whapps_call:kvs_store(K, wh_json:get_value(K, JObj), C1)
                 end, Call, cache_resource_types(whapps_call:resource_type(Call), Flow, Call, JObj)).


-spec cache_resource_types(ne_binary(), wh_json:object(), whapps_call:call(), wh_json:object()) -> wh_proplist().
cache_resource_types(<<"sms">>, Flow, Call, JObj) ->
    [<<"Message-ID">>, <<"Body">>];
cache_resource_types(_Other, _Flow, _Call, _JObj) -> [].
    

-spec save_sms(wh_json:object(), wh_proplist(), wh_json:object(), boolean(), whapps_call:call()) -> 'ok'.
save_sms(JObj, Props, Flow, Offnet, Call) ->
    AccountId = whapps_call:account_id(Call),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    OwnerId = wh_json:get_value(<<"Owner-ID">>, CCVs),
    AuthType = wh_json:get_value(<<"Authorizing-Type">>, CCVs),
    AuthId = wh_json:get_value(<<"Authorizing-ID">>, CCVs),
    Body = wh_json:get_value(<<"Body">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    From = wh_json:get_value(<<"From">>, JObj),
    Request = wh_json:get_value(<<"Request">>, JObj),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),

    Module =  wh_json:get_value([<<"flow">>, <<"module">>], Flow),
    Data =  wh_json:get_value([<<"flow">>, <<"data">>], Flow),

    Endpoints = local_delivery(Module, Data, Call),
    lager:debug("ENDPOINTS ~p",[Endpoints]),
    
    
    Doc = props:filter_undefined([
           {<<"pvt_type">>, <<"sms">> }
          ,{<<"account_id">>, AccountId }
          ,{<<"owner_id">>, OwnerId }
          ,{AuthType, AuthId }
          ,{<<"to">>, To }
          ,{<<"from">>, From }
          ,{<<"request">>, Request }
          ,{<<"body">>, Body }
%          ,{<<"inception">>, whapps_call:inception(Call)}
          ,{<<"message_id">>, MessageId}
          ,{<<"pvt_created">>, wh_util:current_tstamp()}
          ,{<<"status">>, <<"queued">>}
          ,{<<"flow_id">>, wh_json:get_value(<<"_id">>, Flow)}
          ,{<<"flow">>, wh_json:get_value(<<"flow">>, Flow)}
          ,{<<"flowdoc">>, Flow}
          ,{<<"module">>, wh_json:get_value([<<"flow">>,<<"module">>], Flow)}
          ,{<<"local">>, Endpoints }
          ,{<<"Call">>, whapps_call:to_json(Call)}
                                 
           ]),
    
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    
    lager:info("saving ~p into ~s",[Doc, AccountDb]),
%%    {'ok', JObjSaved} = kazoo_modb:save_doc(AccountId, Doc),
    {'ok', JObjSaved} = couch_mgr:save_doc(AccountDb, wh_json:from_list( Doc) ),
    
    
    save_sms_worker(AccountDb, wh_json:get_value(<<"_id">>, JObjSaved), Doc, Flow).

-spec save_sms_worker(ne_binary(), ne_binary(), wh_proplist(), wh_json:object()) -> 'ok'.
save_sms_worker(AccountDb, DocId, Doc, Flow) ->
    Doc1 = [ 
            {<<"account_db">>, AccountDb}
           ,{<<"sms_id">>, DocId}
           ,{<<"retries">>, 20}
%           ,{<<"flow">>, wh_json:get_value(<<"flow">>, Flow)}
           ,{<<"pvt_job_status">>, <<"test">>}
           | Doc],
    couch_mgr:save_doc(?DOODLE_DB, wh_json:from_list(Doc1)).
  
local_delivery(<<"device">>, Data, Call) ->
    EndpointId = wh_json:get_value(<<"id">>, Data),
    case cf_endpoint:build(EndpointId, [], Call) of
        {'ok', Endpoint} ->
            [Endpoint];
        _ -> []
    end;            
local_delivery(<<"user">>, Data, Call) ->
    UserId = wh_json:get_value(<<"id">>, Data),
    lager:debug("cf ~p",[cf_attributes:owned_by(UserId, <<"device">>, Call)]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, [], Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], cf_attributes:owned_by(UserId, <<"device">>, Call));   
local_delivery(_, _, _) -> [].

