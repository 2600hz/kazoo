%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_cccps).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2, validate/3
        ,put/1, put/2
        ,post/2
        ,delete/2
        ]).

-include_lib("crossbar/src/crossbar.hrl").

-define(CB_LIST, <<"cccps/crossbar_listing">>).
-define(AUTODIAL, <<"autodial">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cccps">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cccps">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.cccps">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.cccps">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.cccps">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.cccps">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_CCCPId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% ```
%%    /cccps => []
%%    /cccps/foo => [<<"foo">>]
%%    /cccps/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /cccps mights load a list of cccp objects
%% /cccps/123 might load the cccp object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_cccps(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_cccp(Context, Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, _Id, ?AUTODIAL) ->
    validate_cccp(Context, ?AUTODIAL, cb_context:req_verb(Context)).

-spec validate_cccps(cb_context:context(), http_method()) -> cb_context:context().
validate_cccps(Context, ?HTTP_GET) ->
    summary(Context);
validate_cccps(Context, ?HTTP_PUT) ->
    case kz_json:get_value(<<"cid">>, cb_context:req_data(Context)) of
        'undefined' ->
            check_pin(Context);
        _ ->
            check_cid(Context)
    end.

-spec validate_cccp(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_cccp(Context, ?AUTODIAL, ?HTTP_PUT) ->
    AccountId = cb_context:account_id(Context),
    case (cb_context:auth_account_id(Context) == AccountId) of
        'true' ->
            ReqData = cb_context:req_data(Context),
            Values = [{<<"account_id">>, cb_context:account_id(Context)}
                     ,{<<"user_id">>, cb_context:auth_user_id(Context)}
                     ],
            JObj = kz_json:set_values(Values, ReqData),
            send_new_camping(JObj, cb_context:account_id(Context)),
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            Resp = kz_json:from_list([{<<"message">>, <<"For direct use by account holder only">>}]),
            cb_context:add_validation_error(<<"account">>, <<"forbidden">>, Resp, Context)
    end;
validate_cccp(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_cccp(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_cccp(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec send_new_camping(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_new_camping(JObj, AccountId) ->
    Req = [{<<"Camping-Request">>, JObj}
          ,{<<"Account-ID">>, AccountId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_camping:publish_req/1).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context2 = crossbar_doc:save(Context),
    _ = kz_datamgr:ensure_saved(<<"cccps">>, cb_context:doc(Context2)),
    Context2.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?AUTODIAL) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    Context2 = crossbar_doc:save(Context),
    _ = kz_datamgr:ensure_saved(<<"cccps">>, cb_context:doc(Context2)),
    Context2.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    Context2 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context2) of
        'success' ->
            _ = kz_datamgr:del_doc(?KZ_CCCPS_DB, kz_doc:id(cb_context:doc(Context2))),
            Context2;
        _ ->
            Context2
    end.

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"cccp">>)).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_view:load(Context, ?CB_LIST, [{'mapper', crossbar_view:get_value_fun()}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"cccp">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"cccp">>)).

%%------------------------------------------------------------------------------
%% @doc Checks whether cid and pin are unique
%% @end
%%------------------------------------------------------------------------------

-spec check_pin(cb_context:context()) -> cb_context:context().
check_pin(Context) ->
    case unique_pin(Context) of
        'true' -> create(Context);
        _ -> error_pin_exists(Context)
    end.

-spec check_cid(cb_context:context()) -> cb_context:context().
check_cid(Context) ->
    ReqData = cb_context:req_data(Context),
    CID = kz_json:get_value(<<"cid">>, ReqData),
    case knm_converters:is_reconcilable(CID) of
        'false' ->
            error_number_is_not_reconcilable(Context, CID);
        'true' ->
            ReqData2 = kz_json:set_value(<<"cid">>, knm_converters:normalize(CID), ReqData),
            Context2 = cb_context:set_req_data(Context, ReqData2),
            case unique_cid(Context2) of
                'true' -> create(Context2);
                _ -> error_cid_exists(Context2, CID)
            end

    end.

-spec error_pin_exists(cb_context:context()) -> cb_context:context().
error_pin_exists(Context) ->
    cb_context:add_validation_error(<<"cccp">>
                                   ,<<"unique">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Pin already exists">>}]
                                     )
                                   ,Context
                                   ).

-spec error_number_is_not_reconcilable(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
error_number_is_not_reconcilable(Context, CID) ->
    cb_context:add_validation_error(<<"cccp">>
                                   ,<<"unique">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Number is non reconcilable">>}
                                      ,{<<"cause">>, CID}
                                      ])
                                   ,Context
                                   ).

error_cid_exists(Context, CID) ->
    cb_context:add_validation_error(<<"cccp">>
                                   ,<<"unique">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"CID already exists">>}
                                      ,{<<"cause">>, CID}
                                      ])
                                   ,Context
                                   ).

-spec unique_cid(cb_context:context()) -> boolean().
unique_cid(Context) ->
    CID = kz_json:get_value(<<"cid">>, cb_context:req_data(Context)),
    case authorize_listing(CID, <<"cccps/cid_listing">>) of
        {'ok',[]} -> 'true';
        _ -> 'false'
    end.

-spec unique_pin(cb_context:context()) -> boolean().
unique_pin(Context) ->
    Pin = kz_json:get_value(<<"pin">>, cb_context:req_data(Context)),
    case authorize_listing(Pin, <<"cccps/pin_listing">>) of
        {'ok',[]} -> 'true';
        _ -> 'false'
    end.

-spec authorize_listing(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object() | kz_json:objects()} |
          'empty' |
          'error'.
authorize_listing(Value, View) ->
    ViewOptions = [{'key', Value}],
    case kz_datamgr:get_results(?KZ_CCCPS_DB, View, ViewOptions) of
        {'ok', [JObj]} ->
            {'ok', kz_json:get_value(<<"value">>, JObj)};
        Reply ->
            lager:info("authorize listing failed for ~p with reply: ~p", [Value, Reply]),
            Reply
    end.
