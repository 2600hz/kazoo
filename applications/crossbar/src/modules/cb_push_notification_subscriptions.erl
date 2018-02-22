%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% API that registers mobile devices for push notifications
%%%
%%% @end
%%% @contributors:
%%%   Ben Partridge
%%%-------------------------------------------------------------------
-module(cb_push_notification_subscriptions).

-export([init/0
        ,allowed_methods/2
        ,resource_exists/2
        ,validate/3
        ,put/3
        ,post/3
        ,delete/3
        ]).

-include("crossbar.hrl").

-define(BY_USER, <<"push_notification_subscriptions/by_user">>).
-define(BY_DEVICE, <<"push_notification_subscriptions/by_device">>).
-define(BY_APP_BY_DEVICE, <<"push_notification_subscriptions/by_app_by_device">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.push_notification_subscriptions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.push_notification_subscriptions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.push_notification_subscriptions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.push_notification_subscriptions">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.push_notification_subscriptions">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.push_notification_subscriptions">>, ?MODULE, 'delete').


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_App, _MobileDeviceId) ->
    [?HTTP_PUT, ?HTTP_DELETE, ?HTTP_POST, ?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /push_notification_subscriptions => []
%%    /push_notification_subscriptions/foo => [<<"foo">>]
%%    /push_notification_subscriptions/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /push_notification_subscriptions mights load a list of push_notificatio_subscriptionn objects
%% /push_notification_subscriptions/123 might load the push_notificatio_subscriptionn object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, App, MobileDeviceId) ->
    validate_push_notification_subscription(Context, App, MobileDeviceId, cb_context:req_verb(Context)).

-spec validate_push_notification_subscription(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_push_notification_subscription(Context, App, MobileDeviceId, ?HTTP_PUT) ->
    lager:debug("validating push notification subscription for PUT"),
    create(Context, App, MobileDeviceId);
validate_push_notification_subscription(Context, App, MobileDeviceId, ?HTTP_POST) ->
    lager:debug("validating push notification subscription for POST"),
    update(App, MobileDeviceId, Context);
validate_push_notification_subscription(Context, App, MobileDeviceId, ?HTTP_DELETE) ->
    lager:debug("validating push notification subscription for DELETE"),
    read_docs_for_app_and_mobile_device_id(Context, App, MobileDeviceId);
validate_push_notification_subscription(Context, App, MobileDeviceId, ?HTTP_GET) ->
    lager:debug("validating push notification subscription for GET"),
    read_docs_for_app_and_mobile_device_id(Context, App, MobileDeviceId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, _App, _MobileDeviceId) ->
    %% Set the app on the doc
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _, _) ->
    Id = kz_json:get_value(<<"id">>, cb_context:doc(Context)),
    lager:debug("Deleting document ~p", [Id]),
    case kz_datamgr:del_doc(cb_context:account_db(Context), Id) of
        {'ok', _} -> Context;
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context(), path_token(), path_token()) -> cb_context:context().
create(Context, App, MobileDeviceId) ->
    lager:debug("Reading push notification subscription: ~p - ~p", [App, MobileDeviceId]),
    case subscription_does_exist(App, MobileDeviceId, Context) of
        {'error', BadContext} -> BadContext;
        {'not_exists', _} ->
            OnSuccess = fun(C) -> on_successful_validation('undefined', App, MobileDeviceId, C) end,
            cb_context:validate_request_data(<<"push_notification_subscriptions">>, Context, OnSuccess);
        {'exists', _Id} ->
            lager:error("Database already contains push notification subscription for [~p, ~p]", [MobileDeviceId, App]),
            crossbar_util:response_400(<<"subscription already exists for this device on this app">>, <<"bad request">>, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
update(App, MobileDeviceId, Context) ->
    lager:debug("Reading push notification subscription: ~p - ~p", [App, MobileDeviceId]),
    case subscription_does_exist(App, MobileDeviceId, Context) of
        {'exists', Doc} ->
            Id = kz_json:get_binary_value(<<"id">>, Doc),
            OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
            cb_context:validate_request_data(<<"push_notification_subscriptions">>, Context, OnSuccess);
        {_Status, BadContext} -> BadContext
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read all matching mobile device registration keys for the given mobile device
%% @end
%%--------------------------------------------------------------------
-spec read_docs_for_app_and_mobile_device_id(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_docs_for_app_and_mobile_device_id(Context, App, MobileDeviceId) ->
    lager:debug("Reading push notification subscription: ~p - ~p", [App, MobileDeviceId]),
    case subscription_does_exist(App, MobileDeviceId, Context) of
        {'exists', Data} ->
            lager:debug("found document for push notification: ~p", [Data]),
            NewContext = cb_context:set_doc(Context, Data),
            crossbar_util:response(Data, NewContext);
        {_Status, BadContext} ->
            lager:debug("Could not read document for push notifications"),
            BadContext
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called when request has validated successfully, prepares document for saving in db
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_ne_binary(), ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', App, MobileDeviceId, Context) ->
    %% Set values that came from path
    lager:debug("New push notification subscription validated successfully"),
    NewDoc = lists:foldl(fun(F, Doc) -> F(Doc) end, cb_context:doc(Context),
                         [fun(X) -> kz_json:set_value(<<"pvt_user_id">>, cb_context:auth_user_id(Context), X) end
                         ,fun(X) -> kz_json:set_value(<<"pvt_app_name">>, App, X) end
                         ,fun(X) -> kz_json:set_value(<<"pvt_device_id">>, MobileDeviceId, X) end
                         ,fun(X) -> kz_doc:set_type(X, <<"push_notification_subscription">>) end
                         ]),
    lager:debug("Set push notification subscription doc filled with private values: ~p", [NewDoc]),
    cb_context:set_doc(Context, NewDoc).
-spec on_successful_validation(api_ne_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"push_notification_subscription">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_json_value(<<"value">>, JObj)|Acc].

-spec subscription_does_exist(ne_binary(), ne_binary(), cb_context:context()) -> {'not_exists' | 'error', cb_context:context()} | {'exists', api_object()}.
subscription_does_exist(App, MobileDeviceId, Context) ->
    ViewContext = crossbar_doc:load_view(?BY_APP_BY_DEVICE, [{'key', [cb_context:auth_user_id(Context), MobileDeviceId, App]}], Context, fun normalize_view_results/2),
    case {cb_context:has_errors(ViewContext), cb_context:doc(ViewContext)} of
        {'true', _} -> {'error', ViewContext};
        {'false', []} -> {'not_exists', crossbar_util:response_bad_identifier(MobileDeviceId, Context)};
        {'false', [Item|[]]=Doc} ->
            lager:debug("Loaded doc from context: ~p", [Doc]),
            {'exists', Item};
        {'false', [_|_]} ->
            lager:error("Database inconsistency: multiple docs for one mobile device push notification subscription"),
            {'error', crossbar_util:response('error', <<"datastore inconsistency: duplicate docs">>, ViewContext)}
    end.
