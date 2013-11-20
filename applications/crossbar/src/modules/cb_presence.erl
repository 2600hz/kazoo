%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP INC
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
        ]).

-include("../crossbar.hrl").

-define(RESET, <<"reset">>).

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
validate(#cb_context{req_verb = ?HTTP_GET, account_id=AccountId}=Context) ->
    Doc = cb_context:doc(crossbar_doc:load(AccountId, Context)),
    Realm = wh_json:get_value(<<"realm">>, Doc),
    publish_search_req(Realm, Context).

validate(#cb_context{req_verb = ?HTTP_POST}=Context, _, ?RESET) ->
    Context#cb_context{resp_status='success'}.

-spec post(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
post(#cb_context{}=Context, User, ?RESET) ->
    [Username, Realm|_] = binary:split(User, <<"@">>),
    Req = [{<<"Username">>, Username}
           ,{<<"Realm">>, Realm}
           |wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_presence:publish_reset/1),
    Context#cb_context{resp_status='success'}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

publish_search_req('undefined', Context) ->
    crossbar_util:response('error', <<"realm could not be found">>, 500, Context);
publish_search_req(Realm, Context) ->
    Req = [{<<"Realm">>, Realm}
           |wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_presence:publish_search_req/1
                                       ,fun wapi_presence:search_resp_v/1
                                      )
    of
        {'ok', Subs} ->
            JObj = wh_json:set_value(<<"subscriptions">>, wh_json:get_value(<<"Subscriptions">>, Subs), wh_json:new()),
            Context#cb_context{resp_status='success', resp_data=JObj};
        {'error', Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Reason), 500, Context)
    end.
