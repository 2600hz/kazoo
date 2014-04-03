%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Metaflows execute on top of a call
%%% /accounts/{account_id}/metaflows - manip account metaflows
%%% /accounts/{account_id}/users/{user_id}/metaflows - manip user's metaflows
%%% /accounts/{account_id}/devices/{device_id}/metaflows - manip user's metaflows
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_metaflows).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,post/1
         ,delete/1
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"metaflows/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.metaflows">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.metaflows">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.metaflows">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.metaflows">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.metaflows">>, ?MODULE, 'delete').

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
%% So /metaflows => []
%%    /metaflows/foo => [<<"foo">>]
%%    /metaflows/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /metaflows mights load a list of metaflow objects
%% /metaflows/123 might load the metaflow object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_metaflows(Context, cb_context:req_verb(Context)).

-spec validate_metaflows(cb_context:context(), http_method()) -> cb_context:context().
validate_metaflows(Context, ?HTTP_GET) ->
    validate_get_metaflows(Context, cb_context:doc(Context));
validate_metaflows(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"metaflows">>, Context, fun validate_set_metaflows/1);
validate_metaflows(Context, ?HTTP_DELETE) ->
    validate_delete_metaflows(Context, cb_context:doc(Context)).

validate_get_metaflows(Context, 'undefined') ->
    {'ok', AccountDoc} =
        couch_mgr:open_cache_doc(
          cb_context:account_db(Context)
          ,cb_context:account_id(Context)
         ),
    validate_get_metaflows(Context, AccountDoc);
validate_get_metaflows(Context, Doc) ->
    Metaflows = wh_json:get_value(<<"metaflows">>, Doc, wh_json:new()),
    OwnerId = wh_json:get_first_defined([<<"_id">>, <<"pvt_account_id">>], Doc),
    crossbar_util:response(wh_json:set_value(<<"owner_id">>, OwnerId, Metaflows), Context).

validate_delete_metaflows(Context, 'undefined') ->
    {'ok', AccountDoc} =
        couch_mgr:open_cache_doc(
          cb_context:account_db(Context)
          ,cb_context:account_id(Context)
         ),
    validate_delete_metaflows(Context, AccountDoc);
validate_delete_metaflows(Context, Doc) ->
    crossbar_util:response(wh_json:new()
                           ,cb_context:set_doc(Context
                                               ,wh_json:delete_key(<<"metaflows">>, Doc)
                                              )).

-spec validate_set_metaflows(cb_context:context()) ->
                                    cb_context:context().
-spec validate_set_metaflows(cb_context:context(), wh_json:object(), api_object()) ->
                                    cb_context:context().
validate_set_metaflows(Context) ->
    validate_set_metaflows(Context, cb_context:req_data(Context), cb_context:doc(Context)).

validate_set_metaflows(Context, Metaflows, 'undefined') ->
    lager:debug("no doc found, using account doc"),
    {'ok', AccountDoc} =
        couch_mgr:open_cache_doc(
          cb_context:account_db(Context)
          ,cb_context:account_id(Context)
         ),
    validate_set_metaflows(Context, Metaflows, AccountDoc);
validate_set_metaflows(Context, Metaflows, Doc) ->
    OwnerId = wh_json:get_first_defined([<<"_id">>, <<"pvt_account_id">>], Doc),
    Doc1 = wh_json:set_value(<<"metaflows">>
                             ,wh_json:set_value(<<"owner_id">>, OwnerId, Metaflows)
                             ,Doc
                            ),
    crossbar_util:response(Metaflows
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
    after_post(crossbar_doc:save(Context)).

after_post(Context) ->
    after_post(Context, cb_context:resp_status(Context)).

after_post(Context, 'success') ->
    crossbar_util:response(wh_json:get_value(<<"metaflows">>, cb_context:doc(Context))
                           ,Context);
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

after_delete(Context) ->
    after_delete(Context, cb_context:resp_status(Context)).

after_delete(Context, 'success') ->
    crossbar_util:response(wh_json:new(), Context);
after_delete(Context, _RespStatus) ->
    Context.
