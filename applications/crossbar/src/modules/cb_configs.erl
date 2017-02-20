%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_configs).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/2
        ,post/2
        ,put/2
        ,delete/2
        ,patch/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_json/include/kazoo_json.hrl").
-define(DEFAULT_NODE, <<"default">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.configs">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.configs">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.configs">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.post.configs">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"*.execute.put.configs">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.patch.configs">>, ?MODULE, patch),
    _ = crossbar_bindings:bind(<<"*.execute.delete.configs">>, ?MODULE, delete).

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_ConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_PATCH, ?HTTP_DELETE].

-spec resource_exists() -> false.
-spec resource_exists(path_tokens()) -> true.
resource_exists() -> false.
resource_exists(_) -> true.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ConfigId) ->
    validate(Context, cb_context:req_verb(Context), ConfigId).

-spec validate(cb_context:context(), http_method(), path_token()) -> cb_context:context().

validate(Context, ?HTTP_GET, ConfigId) ->
    JObj = kapps_config_util:read(cb_context:account_id(Context), ConfigId),
    crossbar_doc:handle_datamgr_success(set_id(ConfigId, JObj), Context);

validate(Context, ?HTTP_DELETE, ConfigId) ->
    Doc = case kapps_config_util:load_config_from_account(cb_context:account_id(Context), ConfigId) of
              {ok, Document} -> Document;
              _ -> set_id(ConfigId, kz_json:new())
          end,
    cb_context:setters(Context,[
                                {fun cb_context:set_doc/2, Doc}
                               ,{fun cb_context:set_resp_status/2, success}
                               ]);

validate(Context, _, ConfigId) ->
    Config = kapps_config_util:read(cb_context:account_id(Context), ConfigId),
    FullConfig = kz_json:merge_recursive(Config, strip_id(cb_context:req_data(Context))),
    Ctx1 = cb_context:validate_request_data(kapps_config_util:account_schema_name(ConfigId), cb_context:set_req_data(Context, FullConfig)),
    case cb_context:resp_status(Ctx1) of
        success -> Context;
        _ -> Ctx1
    end.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ConfigId) -> post(Context, ConfigId).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, ConfigId) ->
    case kapps_config_util:update(cb_context:account_id(Context), ConfigId, strip_id(cb_context:req_data(Context))) of
        {ok, Document} -> crossbar_doc:handle_datamgr_success(Document, Context);
        {error, Error} -> crossbar_doc:handle_datamgr_errors(Error, ConfigId, Context)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ConfigId) ->
    case kapps_config_util:write(cb_context:account_id(Context), ConfigId, strip_id(cb_context:req_data(Context))) of
        {ok, Document} -> crossbar_doc:handle_datamgr_success(Document, Context);
        {error, Error} -> crossbar_doc:handle_datamgr_errors(Error, ConfigId, Context)
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _ConfigId) ->
    case strip_id(cb_context:doc(Context)) of
        ?JSON_WRAPPER([]) -> Context;
        _ ->
            crossbar_doc:delete(Context, permanent)
    end.

-spec doc_id(ne_binary()) -> ne_binary().
doc_id(ConfigId) -> kapps_account_config:config_doc_id(ConfigId).

-spec set_id(ne_binary(), kz_json:object()) -> kz_json:object().
set_id(ConfigId, JObj) -> kz_json:set_value(<<"id">>, doc_id(ConfigId), JObj).

-spec strip_id(kz_json:object()) -> kz_json:object().
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj, prune).
