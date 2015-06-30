%%%-------------------------------------------------------------------
%% @copyright (C) 2013, VoIP INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module('cb_presence').

-export([init/0
         ,allowed_methods/0, allowed_methods/2
         ,resource_exists/0, resource_exists/2
         ,validate/1, validate/3
         ,post/3
         ,collect_presentities/2
        ]).

-include("../crossbar.hrl").

-define(RESET, <<"reset">>).
-define(PRESENTITY, <<"presentity">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.presence">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.presence">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.presence">>, ?MODULE, 'validate'),
    crossbar_bindings:bind(<<"*.execute.post.presence">>, ?MODULE, 'post').

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
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_, ?RESET) ->
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
-spec resource_exists(path_token(), ne_binary()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_, ?RESET) -> 'true'.

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
-spec validate(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    publish_search(cb_context:req_param(Context, <<"type">>), cb_context:account_realm(Context), Context).

validate(#cb_context{req_verb = ?HTTP_POST}=Context, _, ?RESET) ->
    Context#cb_context{resp_status='success'}.

-spec post(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
post(#cb_context{}=Context, User, ?RESET) ->
    [Username |_] = binary:split(User, <<"@">>),
    Req = [{<<"Username">>, Username}
           ,{<<"Realm">>, cb_context:account_realm(Context)}
           |wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wh_federation:cast(Req, fun wapi_presence:publish_reset/1),
    Context#cb_context{resp_status='success'}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
publish_search(<<"presentity">>, Realm, Context) ->
    publish_presentity_search_req(Realm, Context);
publish_search(_, Realm, Context) ->
    publish_search_req(Realm, Context).

publish_search_req('undefined', Context) ->
    crossbar_util:response('error', <<"realm could not be found">>, 500, Context);
publish_search_req(Realm, Context) ->
    Req = [{<<"Realm">>, Realm}
           ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
           |wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_federation:collect(Req
                               ,fun wapi_presence:publish_search_req/1
                               ,{'omnipresence', 'true', 'true'}
                              )
    of
        {'ok', JObjs} ->
            J = lists:foldl(fun extract_subscriptions/2, wh_json:new(), JObjs),
            JObj = wh_json:set_value(<<"Subscriptions">>, J, wh_json:new()),
            Context#cb_context{resp_status='success', resp_data=JObj};
        {'timeout', JObjs} ->
            J = lists:foldl(fun extract_subscriptions/2, wh_json:new(), JObjs),
            JObj = wh_json:set_values([{<<"Subscriptions">>, J}
                                       ,{<<"timeout">>, 'true'}
                                      ], wh_json:new()),
            Context#cb_context{resp_status='success', resp_data=JObj};
        {'error', Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Reason), 500, Context)
    end.

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
    case wh_federation:collect(Req
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
    