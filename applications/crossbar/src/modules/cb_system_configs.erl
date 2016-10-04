%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_system_configs).

-export([init/0
        ,authorize/1, authorize/2, authorize/3
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/2
        ,post/2, post/3
        ,delete/2, delete/3
        ]).

-include("crossbar.hrl").

-define(DEFAULT, <<"default">>).


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
    _ = kz_datamgr:db_create(?KZ_CONFIG_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_CONFIG_DB, 'crossbar', <<"views/system_configs.json">>),

    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.system_configs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.system_configs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.system_configs">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.system_configs">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.system_configs">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.system_configs">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) -> cb_context:is_superduper_admin(Context).
authorize(Context, _Id) -> cb_context:is_superduper_admin(Context).
authorize(Context, _Id, _Node) -> cb_context:is_superduper_admin(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_SystemConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PUT].
allowed_methods(_SystemConfigId, _Node) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /system_configs => []
%%    /system_configs/foo => [<<"foo">>]
%%    /system_configs/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_Id) -> 'true'.
resource_exists(_Id, _Node) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /system_configs mights load a list of system_config objects
%% /system_configs/123 might load the system_config object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
validate(Context) ->
    validate_system_configs(update_db(Context), cb_context:req_verb(Context)).

validate(Context, Id) ->
    Node = cb_context:req_value(Context, <<"node">>, ?DEFAULT),
    validate_system_config(update_db(Context), Id, cb_context:req_verb(Context), Node).

validate(Context, Id, Node) ->
    validate_system_config(update_db(Context), Id, cb_context:req_verb(Context), Node).

-spec validate_system_configs(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_system_config(cb_context:context(), path_token(), http_method(), ne_binary()) -> cb_context:context().
validate_system_configs(Context, ?HTTP_GET) ->
    summary(Context).

validate_system_config(Context, Id, ?HTTP_GET, Node) ->
    read(Id, Context, Node);
validate_system_config(Context, Id, ?HTTP_PUT, _Node) ->
    create(Id, Context);
validate_system_config(Context, Id, ?HTTP_POST, Node) ->
    update(Id, Context, Node);
validate_system_config(Context, Id, ?HTTP_DELETE, _Node) ->
    read_for_delete(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, _Id) ->
    save(Context, ?DEFAULT).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    save(Context, ?DEFAULT).
post(Context, _Id, Node) ->
    save(Context, Node).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    Context1 =
        case kz_util:is_true(cb_context:req_param(Context, <<"hard">>, 'false')) of
            false -> crossbar_doc:delete(Context);
            true -> crossbar_doc:delete(Context, permanent)
        end,
    case cb_context:resp_status(Context1) of
        'success' -> cb_context:set_resp_data(Context1, undefined);
        _Status -> Context1
    end.

delete(Context, Id, Node) ->
    Doc = cb_context:doc(Context),
    case {Doc
         ,kz_json:get_ne_value(Node, Doc)
         }
    of
        {undefined, _} -> crossbar_util:response_bad_identifier(Id, Context);
        {_, undefined} -> crossbar_util:response_bad_identifier(Node, Context);
        _ ->
            Context1 = cb_context:set_doc(Context, kz_json:delete_key(Node, Doc)),
            save(Context1, ?DEFAULT)
    end.

-spec save(cb_context:context(), ne_binary()) -> cb_context:context().
save(Context, Node) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context1, node_value(Node, Context1));
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context(), ne_binary()) -> cb_context:context().
create(Id, Context) ->
    Doc = cb_context:req_data(Context),
    case binary:split(Id, <<$@>>) of
        [Id] ->
            SysDoc = kz_json:from_list([{<<"_id">>, Id}
                                       ,{?DEFAULT, Doc}
                                       ]),
            lager:debug("trying to create ~s/~s: ~p", [?KZ_CONFIG_DB, Id, SysDoc]),
            Context1 = cb_context:set_doc(Context, SysDoc),
            cb_context:set_resp_status(Context1, 'success');
        _ ->
            crossbar_util:response_bad_identifier(Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec filter_read(cb_context:context(), ne_binary()) -> kz_json:object().
filter_read(Context, ?DEFAULT) ->
    kz_doc:public_fields(cb_context:doc(Context));
filter_read(Context, Node) ->
    node_value(Node, Context).

-spec read(ne_binary(), cb_context:context(), ne_binary()) ->
                  cb_context:context().
read(Id, Context, Node) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"config">>)),
    case cb_context:resp_status(Context1) of
        success ->
            Data = filter_read(Context1, Node),
            cb_context:set_resp_data(Context1, Data);
        _Status -> Context1
    end.

-spec read_for_delete(ne_binary(), cb_context:context()) ->
                             cb_context:context().
read_for_delete(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"config">>)),
    case cb_context:resp_status(Context) of
        'success' -> Context1;
        _Status ->
            lager:debug("failed to find ~s(~s) for delete", [Id, _Status]),
            Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing system_config document with the data provided
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context(), ne_binary()) -> cb_context:context().
update(Id, Context, Node) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"config">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            NodeDoc = node_value(Node, Context1),
            ReqData = cb_context:req_data(Context1),
            NewNodeDoc = kz_json:normalize_jobj(kz_json:merge_recursive(NodeDoc, ReqData)),
            NewDoc = kz_json:set_value(Node, NewNodeDoc, cb_context:doc(Context1)),
            cb_context:set_doc(Context1, NewDoc);
        _Status ->
            Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    View = <<"system_configs/crossbar_listing">>,
    crossbar_doc:load_view(View, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), ne_binaries()) -> ne_binaries().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"key">>, JObj) | Acc].

-spec update_db(cb_context:context()) -> cb_context:context().
update_db(Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_account_db/2, ?KZ_CONFIG_DB}
                       ,{fun cb_context:set_account_id/2, cb_context:auth_account_id(Context)}
                       ]).

-spec node_value(ne_binary(), cb_context:context()) -> kz_json:object().
node_value(Node, Context) ->
    kz_json:get_ne_value(Node, cb_context:doc(Context), kz_json:new()).
