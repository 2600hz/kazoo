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
    [?HTTP_POST].
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
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_thing(Context, cb_context:req_verb(Context)).

validate(Context, _Extension) ->
    cb_context:set_resp_status(Context, 'success').

validate_thing(Context, ?HTTP_POST) ->
    validate_thing_reset(Context, cb_context:req_nouns(Context)).

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
    lager:debug("reseting things: ~p", [Things]),
    publish_reset(cb_context:account_realm(Context), Things),
    crossbar_util:response_202(<<"reset commands sent">>, Context);
send_reset(Context, Thing) ->
    send_reset(Context, [Thing]).

-spec publish_reset(ne_binary(), wh_json:objects()) -> 'ok'.
publish_reset(Realm, Things) ->
    _ = [publish_presence_reset(Realm, kz_device:presence_id(Thing))
         || Thing <- Things
        ],
    'ok'.

-spec publish_presence_reset(ne_binary(), ne_binary()) -> 'ok'.
publish_presence_reset(Realm, PresenceId) ->
    API = [{<<"Realm">>, Realm}
           ,{<<"Username">>, PresenceId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wh_amqp_worker:cast(API, fun wapi_presence:publish_reset/1).

%%%===================================================================
%%% Internal functions
%%%===================================================================
