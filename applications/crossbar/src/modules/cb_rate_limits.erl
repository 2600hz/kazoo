%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Rate limits
%%% /accounts/{account_id}/rate_limits - manip account's access lists
%%% /accounts/{account_id}/devices/{device_id}/rate_limits - manip device's access lists
%%%
%%% @end
%%% @contributors:
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cb_rate_limits).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,post/1
         ,delete/1
        ]).

-include("../crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.rate_limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.rate_limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.rate_limits">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.rate_limits">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.rate_limits">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /rate_limits => []
%%    /rate_limits/foo => [<<"foo">>]
%%    /rate_limits/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /rate_limits mights load a list of metaflow objects
%% /rate_limits/123 might load the metaflow object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_rate_limits(Context, cb_context:req_verb(Context)).

-spec validate_rate_limits(cb_context:context(), http_method()) -> cb_context:context().
validate_rate_limits(Context, ?HTTP_GET) ->
    validate_get_rate_limits(Context, thing_doc(Context));
validate_rate_limits(Context, ?HTTP_POST) ->
    ValidateDevice = fun (C) -> validate_section(<<"device">>, C, fun validate_set_rate_limits/1) end,
    case cb_context:req_nouns(Context) of
        [{<<"rate_limits">>, []}, {<<"accounts">>, [_]} | _] ->
            validate_section(<<"account">>, Context, ValidateDevice);
        [{<<"rate_limits">>, []}, {<<"devices">>, [_]} | _] ->
            cb_context:validate_request_data(<<"rate_limits">>, Context, fun validate_set_rate_limits/1);
        _Else ->
            crossbar_util:response_faulty_request(Context)
    end;
validate_rate_limits(Context, ?HTTP_DELETE) ->
    validate_delete_acls(Context, thing_doc(Context)).

-spec validate_section(ne_binary(), cb_context:context(), cb_context:after_fun()) -> cb_context:context().
validate_section(Section, Context, OnSuccess) ->
    Data = cb_context:req_data(Context),
    NewData = wh_json:get_value(Section, Data),
    Continue = fun (C) -> OnSuccess(cb_context:set_req_data(C, Data)) end,
    cb_context:validate_request_data(<<"rate_limits">>, cb_context:set_req_data(Context, NewData), Continue).

-spec thing_doc(cb_context:context()) -> api_object().
-spec thing_doc(cb_context:context(), ne_binary()) -> api_object().
thing_doc(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"rate_limits">>, []}, {<<"accounts">>, [AccountId]} | _] ->
            lager:debug("loading rate limits from account: '~s'", [AccountId]),
            thing_doc(Context, AccountId);
        [{<<"rate_limits">>, []}, {<<"devices">>, [DeviceId]} | _] ->
            lager:debug("loading rate limits from device: '~s'", [DeviceId]),
            thing_doc(Context, DeviceId);
        _Nouns -> 'undefined'
    end.

thing_doc(Context, ThingId) ->
    Context1 = crossbar_doc:load(ThingId, Context),
    case cb_context:resp_status(Context1) of
        'success' -> cb_context:doc(Context1);
        _Status ->
            lager:debug("failed to load thing ~s", [ThingId]),
            'undefined'
    end.

-spec validate_get_rate_limits(cb_context:context(), api_object()) -> cb_context:context().
validate_get_rate_limits(Context, 'undefined') ->
    crossbar_util:response_faulty_request(Context);
validate_get_rate_limits(Context, Doc) ->
    RateLimits = wh_json:get_value(<<"rate_limits">>, Doc, wh_json:new()),
    crossbar_util:response(RateLimits, Context).

-spec validate_delete_acls(cb_context:context(), api_object()) -> cb_context:context().
validate_delete_acls(Context, 'undefined') ->
    crossbar_util:response_faulty_request(Context);
validate_delete_acls(Context, Doc) ->
    crossbar_util:response(wh_json:new()
                           ,cb_context:set_doc(Context
                                               ,wh_json:delete_key(<<"rate_limits">>, Doc)
                                              )).

-spec validate_set_rate_limits(cb_context:context()) ->
                                    cb_context:context().
-spec validate_set_rate_limits(cb_context:context(), wh_json:object(), api_object()) ->
                                    cb_context:context().
validate_set_rate_limits(Context) ->
    lager:debug("rate limits data is valid, setting on thing"),
    validate_set_rate_limits(Context, cb_context:doc(Context), thing_doc(Context)).

validate_set_rate_limits(Context, _, 'undefined') ->
    lager:debug("no doc found"),
    crossbar_util:response_faulty_request(Context);
validate_set_rate_limits(Context, RateLimits, Doc) ->
    Doc1 = wh_json:set_value(<<"rate_limits">>, RateLimits, Doc),

    crossbar_util:response(RateLimits
                           ,cb_context:set_doc(Context, Doc1)
                          ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    lager:debug("saving ~p", [cb_context:doc(Context)]),
    after_post(crossbar_doc:save(Context)).

-spec after_post(cb_context:context()) -> cb_context:context().
-spec after_post(cb_context:context(), crossbar_status()) -> cb_context:context().
after_post(Context) ->
    after_post(Context, cb_context:resp_status(Context)).

after_post(Context, 'success') ->
    lager:debug("saved, returning the rate limits"),
    crossbar_util:response(wh_json:get_value(<<"rate_limits">>, cb_context:doc(Context))
                           ,Context
                          );
after_post(Context, _RespStatus) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    after_delete(crossbar_doc:save(Context)).

-spec after_delete(cb_context:context()) -> cb_context:context().
-spec after_delete(cb_context:context(), crossbar_status()) -> cb_context:context().
after_delete(Context) ->
    after_delete(Context, cb_context:resp_status(Context)).

after_delete(Context, 'success') ->
    crossbar_util:response(wh_json:new(), Context);
after_delete(Context, _RespStatus) ->
    Context.