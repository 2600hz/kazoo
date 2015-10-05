%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors

%%%-------------------------------------------------------------------
-module('cb_presence').

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,post/1, post/2
        ]).

-include("../crossbar.hrl").

-define(PRESENTITY, <<"presentity">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Bindings = [{<<"*.allowed_methods.presence">>, 'allowed_methods'}
                ,{<<"*.resource_exists.presence">>, 'resource_exists'}
                ,{<<"*.validate.presence">>, 'validate'}
                ,{<<"*.execute.post.presence">>, 'post'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(_Extension) ->
    [?HTTP_POST].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_Extension) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
validate(Context) ->
    validate_thing(Context, cb_context:req_verb(Context)).

validate(Context, _Extension) ->
    cb_context:set_resp_status(Context, 'success').

-spec validate_thing(cb_context:context(), http_method()) ->
                            cb_context:context().
validate_thing(Context, ?HTTP_GET) ->
    validate_search(Context);
validate_thing(Context, ?HTTP_POST) ->
    validate_thing_reset(Context, cb_context:req_nouns(Context)).

validate_search(Context) ->
    validate_search(Context, cb_context:req_param(Context, <<"type">>)).

validate_search(Context, ?PRESENTITY) ->
    validate_presentity_search(Context);
validate_search(Context, _Type) ->
    case search_req(Context) of
        {'ok', JObj} ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_data/2, JObj}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]
                              );
        {'error', Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Reason), 500, Context)
    end.

-spec search_req(cb_context:context()) ->
                        {'ok', wh_json:object()} |
                        {'error', _}.
search_req(Context) ->
    Req = [{<<"Realm">>, cb_context:account_realm(Context)}
           ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
           ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call_collect(Req
                                     ,fun wapi_presence:publish_search_req/1
                                     ,{'omnipresence', 'true', 'true'}
                                    )
    of
        {'error', _R}=Err -> Err;
        {'ok', JObjs} ->
            process_search_responses(JObjs);
        {'timeout', JObjs} ->
            process_search_responses(JObjs, 'true')
    end.

-spec process_search_responses(wh_json:objects()) ->
                                      {'ok', wh_json:object()}.
-spec process_search_responses(wh_json:objects(), api_boolean()) ->
                                      {'ok', wh_json:object()}.
process_search_responses(JObjs) ->
    process_search_responses(JObjs, 'undefined').

process_search_responses(JObjs, Timeout) ->
    Subscriptions = extract_subscriptions_from_results(JObjs),
    {'ok'
     ,wh_json:from_list(
        props:filter_undefined(
          [{<<"subscriptions">>, Subscriptions}
           ,{<<"timeout">>, Timeout}
          ]
         )
       )
    }.

-spec extract_subscriptions_from_results(wh_json:objects()) ->
                                                wh_json:object().
extract_subscriptions_from_results(JObjs) ->
    lists:foldl(fun extract_subscriptions_from_result/2, wh_json:new(), JObjs).

-spec extract_subscriptions_from_result(wh_json:object(), wh_json:object()) ->
                                               wh_json:object().
extract_subscriptions_from_result(JObj, Acc) ->
    Subscriptions = wh_json:get_value(<<"Subscriptions">>, JObj, []),
    lists:foldl(fun extract_subscription/2, Acc, Subscriptions).

-spec extract_subscription(wh_json:object(), wh_json:object()) ->
                                  wh_json:object().
extract_subscription(Subscription, Acc) ->
    Key = [wh_json:get_value(<<"username">>, Subscription)
           ,wh_json:get_value(<<"event">>, Subscription)
           ,wh_json:get_value(<<"call_id">>, Subscription)
          ],
    case wh_json:get_value(Key, Acc) of
        'undefined' ->
            add_subscription(Subscription, Acc, Key);
        _Sub -> Acc
    end.

-spec add_subscription(wh_json:object(), wh_json:object(), wh_json:key()) ->
                              wh_json:object().
add_subscription(Subscription, Acc, Key) ->
    wh_json:set_value(Key
                      ,wh_json:delete_keys(
                         [<<"username">>
                          ,<<"user">>
                          ,<<"event">>
                          ,<<"realm">>
                          ,<<"protocol">>
                          ,<<"contact">>
                          ,<<"call_id">>
                         ]
                         ,Subscription
                        )
                      ,Acc
                     ).

-spec validate_presentity_search(cb_context:context()) ->
                                        cb_context:context().
validate_presentity_search(Context) ->
    case presentity_search_req(Context) of
        {'ok', JObj} ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_data/2, JObj}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]
                              );
        {'error', Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Reason), 500, Context)
    end.

-spec presentity_search_req(cb_context:context()) ->
                                   {'ok', wh_json:object()} |
                                   {'error', _}.
presentity_search_req(Context) ->
    Req = [{<<"Realm">>, cb_context:account_realm(Context)}
           ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
           ,{<<"Scope">>, <<"presentity">>}
           ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Count = wh_nodes:whapp_count(<<"kamailio">>, 'true'),

    lager:debug("attempting presentity search from ~p servers", [Count]),

    case wh_amqp_worker:call_collect(Req
                                     ,fun wapi_omnipresence:publish_search_req/1
                                     ,{fun collect_presentities/2, {0, Count}}
                                    )
    of
        {'error', _E}=Err -> Err;
        {'ok', JObjs} ->
            process_presentity_responses(JObjs);
        {'timeout', JObjs} ->
            process_presentity_responses(JObjs, 'true')
    end.

-spec collect_presentities(wh_json:objects(), {integer(), integer()}) ->
                                  'true' |
                                  {'false', {integer(), integer()}}.
collect_presentities([Response | _], {Count, Max}) ->
    case Count + resp_value(Response) of
        Max -> 'true';
        V -> {'false', {V, Max}}
    end.

-spec resp_value(wh_json:object()) -> 0..1.
resp_value(Response) ->
    case wh_api:event_name(Response) of
        <<"search_resp">> -> 1;
        _EventName -> 0
    end.

-spec process_presentity_responses(wh_json:objects()) ->
                                          {'ok', wh_json:object()}.
-spec process_presentity_responses(wh_json:objects(), api_boolean()) ->
                                          {'ok', wh_json:object()}.
process_presentity_responses(JObjs) ->
    process_presentity_responses(JObjs, 'undefined').

process_presentity_responses(JObjs, Timeout) ->
    Presentities = extract_presentities_from_responses(JObjs),
    {'ok'
     ,wh_json:from_list(
        props:filter_undefined(
          [{<<"presentities">>, Presentities}
           ,{<<"timeout">>, Timeout}
          ]
         )
       )
    }.

extract_presentities_from_responses(JObjs) ->
    lists:foldl(fun extract_presentities_from_response/2, wh_json:new(), JObjs).

extract_presentities_from_response(JObj, Acc) ->
    extract_presentities_from_response(JObj, Acc, wh_api:event_name(JObj)).
extract_presentities_from_response(JObj, Acc, <<"search_partial_resp">>) ->
    process_partial_response(JObj, Acc);
extract_presentities_from_response(_JObj, Acc, _EventName) ->
    Acc.

process_partial_response(JObj, Acc) ->
    SubscriptionKeys = wh_json:get_keys(<<"Subscriptions">>, JObj),
    Node = wh_api:node(JObj),

    lists:foldl(fun(Key, Acc1) ->
                        process_partial_response(Key, Acc1, JObj, Node)
                end
                ,Acc
                ,SubscriptionKeys
               ).

process_partial_response(Key, Acc, JObj, Node) ->
    wh_json:set_value([Key, Node]
                      ,wh_json:get_value([<<"Subscriptions">>, Key], JObj)
                      ,Acc
                     ).

-spec validate_thing_reset(cb_context:context(), req_nouns()) ->
                                  cb_context:context().
validate_thing_reset(Context, [{<<"presence">>, []}
                               ,{<<"devices">>, [DeviceId]}
                               ,{<<"accounts">>, [_AccountId]}
                              ]) ->
    maybe_load_thing(Context, DeviceId);
validate_thing_reset(Context, [{<<"presence">>, []}
                               ,{<<"users">>, [UserId]}
                               ,{<<"accounts">>, [_AccountId]}
                              ]) ->
    case is_reset_request(Context) of
        'true' -> validate_user_reset(Context, UserId);
        'false' -> reset_validation_error(Context)
    end;
validate_thing_reset(Context, _ReqNouns) ->
    crossbar_util:response_faulty_request(Context).

-spec validate_user_reset(cb_context:context(), ne_binary()) -> cb_context:context().
validate_user_reset(Context, UserId) ->
    Context1 = crossbar_doc:load(UserId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_load_user_devices(Context1);
        _Status ->
            Context
    end.

-spec maybe_load_user_devices(cb_context:context()) -> cb_context:context().
maybe_load_user_devices(Context) ->
    User = cb_context:doc(Context),
    case kzd_user:presence_id(User) of
        'undefined' -> load_user_devices(Context);
        _PresenceId -> Context
    end.

-spec load_user_devices(cb_context:context()) -> cb_context:context().
load_user_devices(Context) ->
    User = cb_context:doc(Context),
    Devices = kzd_user:devices(User),
    cb_context:set_doc(Context, Devices).

-spec maybe_load_thing(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_load_thing(Context, ThingId) ->
    case is_reset_request(Context) of
        'true' -> crossbar_doc:load(ThingId, Context);
        'false' ->
            lager:debug("user failed to include reset=true"),
            reset_validation_error(Context)
    end.

-spec is_reset_request(cb_context:context()) -> boolean().
is_reset_request(Context) ->
    wh_util:is_true(cb_context:req_value(Context, <<"reset">>)).

-spec reset_validation_error(cb_context:context()) -> cb_context:context().
reset_validation_error(Context) ->
    cb_context:add_validation_error(<<"reset">>
                                    ,<<"required">>
                                    ,wh_json:from_list(
                                       [{<<"message">>, <<"Field must be set to true">>}
                                        ,{<<"target">>, <<"required">>}
                                       ]
                                      )
                                    ,Context
                                   ).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    Things = cb_context:doc(Context),
    send_reset(Context, Things).

-spec post(cb_context:context(), ne_binary()) -> cb_context:context().
post(Context, Extension) ->
    publish_presence_reset(cb_context:account_realm(Context), Extension),
    crossbar_util:response_202(<<"reset command sent for extension ", Extension/binary>>, Context).

-spec send_reset(cb_context:context(), wh_json:object() | wh_json:objects()) ->
                        cb_context:context().
send_reset(Context, []) ->
    lager:debug("nothing to reset"),
    crossbar_util:response(<<"nothing to reset">>, Context);
send_reset(Context, [_|_]=Things) ->
    publish_reset(cb_context:account_realm(Context), Things),
    crossbar_util:response_202(<<"reset commands sent">>, Context);
send_reset(Context, Thing) ->
    send_reset(Context, [Thing]).

-spec publish_reset(ne_binary(), wh_json:objects()) -> 'ok'.
publish_reset(Realm, Things) ->
    _ = [publish_presence_reset(Realm, find_presence_id(Thing))
         || Thing <- Things
        ],
    'ok'.

-spec publish_presence_reset(ne_binary(), api_binary()) -> 'ok'.
publish_presence_reset(_Realm, 'undefined') -> 'ok';
publish_presence_reset(Realm, PresenceId) ->
    lager:debug("resetting ~s @ ~s", [PresenceId, Realm]),
    API = [{<<"Realm">>, Realm}
           ,{<<"Username">>, PresenceId}
           ,{<<"Msg-ID">>, wh_util:get_callid()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wh_amqp_worker:cast(API, fun wapi_presence:publish_reset/1).

-spec find_presence_id(wh_json:object()) -> ne_binary().
find_presence_id(JObj) ->
    case kz_device:is_device(JObj) of
        'true' -> kz_device:presence_id(JObj);
        'false' -> kzd_user:presence_id(JObj)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
