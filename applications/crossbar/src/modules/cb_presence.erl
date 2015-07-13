%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module('cb_presence').

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,post/1, post/2
         ,collect_presentities/2
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
    [?HTTP_GET, ?HTTP_POST].

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
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_thing(Context, cb_context:req_verb(Context)).

validate_thing(Context, ?HTTP_GET) ->
    validate_thing_presence(Context, cb_context:req_nouns(Context));
validate_thing(Context, ?HTTP_POST) ->
    validate_thing_reset(Context, cb_context:req_nouns(Context)).

validate_thing_presence(Context, [{<<"presence">>, []}
                                  ,{<<"devices">>, [DeviceId]}
                                  ,{<<"accounts">>, [_AccountId]}
                                 ]) ->
    validate_device_presence(Context, DeviceId);
validate_thing_presence(Context, [{<<"presence">>, []}
                                  ,{<<"users">>, [UserId]}
                                  ,{<<"accounts">>, [_AccountId]}
                                 ]) ->
    validate_user_presence(Context, UserId);
validate_thing_presence(Context, _ReqNouns) ->
    crossbar_util:response_faulty_request(Context).

validate_device_presence(Context, DeviceId) ->
    Context1 = cb_context:load(DeviceId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            PresenceState = fetch_device_presence_state(Context1),
            crossbar_util:response(PresenceState, Context1);
        _Status ->
            Context1
    end.

fetch_device_presence_state(Context) ->
    DeviceJObj = cb_context:doc(Context),
    PresenceId = kz_device:presence_id(DeviceJObj),
    Realm = cb_context:account_realm(Context),

    lager:debug("fetching presence for ~s @ ~s", [PresenceId, Realm]),

    Req = [{<<"Realm">>, Realm}
           ,{<<"Username">>, PresenceId}
           ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call_collect(Req
                                     ,fun wapi_presence:publish_search_req/1
                                     ,{'omnipresence', fun wapi_presence:search_resp_v/1, 'true', 'true'}
                                    )
    of
        {'ok', JObjs} ->
            J = lists:foldl(fun extract_subscriptions/2, wh_json:new(), JObjs),
            JObj = wh_json:from_list([{<<"subscriptions">>, J}]),
            crossbar_util:response(JObj, Context);
        {'timeout', JObjs} ->
            J = lists:foldl(fun extract_subscriptions/2, wh_json:new(), JObjs),
            JObj = wh_json:from_list([{<<"subscriptions">>, J}
                                      ,{<<"timeout">>, 'true'}
                                     ]),
            crossbar_util:response(JObj, Context);
        {'error', Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Reason), 500, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
publish_search(<<"presentity">>, Realm, Context) ->
    publish_presentity_search_req(Realm, Context).

publish_presentity_search_req('undefined', Context) ->
    crossbar_util:response('error', <<"realm could not be found">>, 500, Context);
publish_presentity_search_req(Realm, Context) ->
    Req = [{<<"Realm">>, Realm}
           ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
           ,{<<"Scope">>, <<"presentity">>}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Count = wh_nodes:whapp_count(<<"kamailio">>, 'true'),
    lager:debug("attempting to collect ~p responses from kamailio", [Count]),
    case wh_amqp_worker:call_collect(Req
                                     ,fun wapi_omnipresence:publish_search_req/1
                                     ,{fun collect_presentities/2 , {0, Count}}
                                    )
    of
        {'ok', JObjs} ->
            J = lists:foldl(fun extract_presentities/2, wh_json:new(), JObjs),
            JObj = wh_json:set_value(<<"Presentities">>, J, wh_json:new()),
            Context#cb_context{resp_status='success', resp_data=JObj};
        {'timeout', JObjs} ->
            J = lists:foldl(fun extract_presentities/2, wh_json:new(), JObjs),
            JObj = wh_json:set_values([{<<"Presentities">>, J}
                                       ,{<<"timeout">>, 'true'}
                                      ], wh_json:new()),
            Context#cb_context{resp_status='success', resp_data=JObj};
        {'error', Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Reason), 500, Context)
    end.

collect_presentities([Response|_], {Count, Max}) ->
    Evt = wh_json:get_value(<<"Event-Name">>, Response),
    case Count + check_event(Evt) of
         Max -> 'true';
        V -> {'false', {V, Max}}
    end.

check_event(<<"search_resp">>) -> 1;
check_event(_) -> 0.

extract_presentities(JObj, Acc) ->
    Event = wh_json:get_value(<<"Event-Name">>, JObj),
    maybe_process_event(Event, JObj, Acc).

maybe_process_event(<<"search_partial_resp">>, JObj, Acc) ->
    Keys = wh_json:get_keys(<<"Subscriptions">>, JObj),
    Node = wh_json:get_value(<<"Node">>, JObj),
    process_event(Keys, Node, JObj, Acc);
maybe_process_event(_Event, _JObj, Acc) -> Acc.

process_event([], __Node, _JObj, Acc) -> Acc;
process_event([Key | Keys], Node, JObj, Acc) ->
    V = wh_json:get_value([<<"Subscriptions">>, Key], JObj),
    process_event(Keys, Node, JObj, wh_json:set_value([Key, Node], V , Acc)).

extract_subscriptions(JObj, Acc) ->
    Subs = wh_json:get_value(<<"Subscriptions">>, JObj, []),
    lists:foldl(fun extract_subscription/2, Acc, Subs).

extract_subscription(JObj, Acc) ->
    User = wh_json:get_value(<<"username">>, JObj),
    Event = wh_json:get_value(<<"event">>, JObj),
    CallId = wh_json:get_value(<<"call_id">>, JObj),
    case wh_json:get_value([User, Event, CallId], Acc) of
        'undefined' ->
            wh_json:set_value([User, Event, CallId],
                              wh_json:delete_keys([<<"username">>
                                                   ,<<"user">>
                                                   ,<<"event">>
                                                   ,<<"realm">>
                                                   ,<<"protocol">>
                                                   ,<<"contact">>
                                                   ,<<"call_id">>
                                                  ], JObj), Acc);
        _ -> Acc
    end.
