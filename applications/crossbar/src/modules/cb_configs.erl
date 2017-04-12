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
        ,put/2
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

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
validate(Context, ?HTTP_GET, ConfigId) -> set_config_to_context(ConfigId, Context);

validate(Context, ?HTTP_DELETE, ConfigId) ->
    Doc = case kapps_config_util:load_config_from_account(cb_context:account_id(Context), ConfigId) of
              {ok, Document} -> Document;
              _ -> set_id(ConfigId, kz_json:new())
          end,
    cb_context:setters(Context,[
                                {fun cb_context:set_doc/2, Doc}
                               ,{fun cb_context:set_resp_status/2, success}
                               ]);

validate(Context, ?HTTP_PUT, ConfigId) ->
    validate(Context, ?HTTP_POST, ConfigId);

validate(Context, ?HTTP_PATCH, ConfigId) ->
    Parent = kapps_config_util:get_config(cb_context:account_id(Context), ConfigId),
    validate_with_parent(Context, ConfigId, Parent);

validate(Context, ?HTTP_POST, ConfigId) ->
    Parent = kapps_config_util:get_reseller_config(cb_context:account_id(Context), ConfigId),
    validate_with_parent(Context, ConfigId, Parent).

-spec validate_with_parent(cb_context:context(), ne_binary(), kz_json:object()) -> cb_context:context().
validate_with_parent(Context, ConfigId, Parent) ->
    RequestData = strip_id(cb_context:req_data(Context)),
    FullConfig = kz_json:merge_recursive(Parent, RequestData),
    Schema = kapps_config_util:account_schema_name(ConfigId),
    cb_context:validate_request_data(Schema, cb_context:set_req_data(Context, FullConfig),
                                     fun(Ctx) ->
                                             cb_context:set_req_data(Ctx, kz_json:diff(RequestData, Parent))
                                     end
                                    ).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ConfigId) ->
    maybe_save_or_delete(Context, ConfigId).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, ConfigId) ->
    maybe_save_or_delete(Context, ConfigId).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ConfigId) ->
    maybe_save_or_delete(Context, ConfigId).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _ConfigId) ->
    case strip_id(cb_context:doc(Context)) of
        ?EMPTY_JSON_OBJECT -> Context;
        _ -> crossbar_doc:delete(Context, permanent)
    end.

-spec set_config_to_context(ne_binary(), cb_context:context()) -> cb_context:context().
set_config_to_context(ConfigId, Context) ->
    Config = kapps_config_util:get_config(cb_context:account_id(Context), ConfigId),
    crossbar_doc:handle_datamgr_success(set_id(ConfigId, Config), Context).

-spec doc_id(ne_binary()) -> ne_binary().
doc_id(ConfigId) -> kapps_account_config:config_doc_id(ConfigId).

-spec set_id(ne_binary(), kz_json:object()) -> kz_json:object().
set_id(ConfigId, JObj) -> kz_json:set_value(<<"id">>, doc_id(ConfigId), JObj).

-spec strip_id(kz_json:object()) -> kz_json:object().
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj, prune).

-spec maybe_save_or_delete(cb_context:context(), path_token()) -> cb_context:context().
maybe_save_or_delete(Context, ConfigId) ->
    Stored = kz_doc:private_fields(kapps_account_config:get(cb_context:account_id(Context), ConfigId)),
    case {cb_context:req_data(Context), kz_doc:revision(Stored)} of
        {?EMPTY_JSON_OBJECT, undefined} -> Context;
        {?EMPTY_JSON_OBJECT, _} ->
            crossbar_doc:delete(cb_context:set_doc(Context, Stored));
        {Diff, _} ->
            crossbar_doc:save(Context, kz_json:merge_recursive(Stored, Diff), [])
    end,
    set_config_to_context(ConfigId, Context).
