%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
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
         ,get/2
         ,put/2
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(PVT_TYPE, <<"config">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"configs/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.configs">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.configs">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.configs">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.configs">>, ?MODULE, get),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.configs">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.configs">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.configs">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_token()) -> http_methods() | [].
allowed_methods(_) ->
    ['GET', 'PUT', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /configs => []
%%    /configs/foo => [<<"foo">>]
%%    /configs/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'false'.
-spec resource_exists/1 :: (path_tokens()) -> 'true'.
resource_exists() -> false.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /configs mights load a list of config objects
%% /configs/123 might load the config object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context, Config) ->
    read(Config, Context);
validate(#cb_context{req_verb = <<"put">>}=Context, Config) ->
    create(Config, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Config) ->
    update(Config, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Config) ->
    read(Config, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
get(#cb_context{}=Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
put(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
create(Config, #cb_context{req_data=Data, db_name=Db}=Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    case couch_mgr:lookup_doc_rev(Db, Id) of
        {ok, _} -> crossbar_util:response_conflicting_docs(Context);
        {error, _} ->
            case wh_json_validator:is_valid(wh_json:set_value(<<"_id">>, Id, Data), <<"configs">>) of
                {fail, Errors} ->
                    crossbar_util:response_invalid_data(Errors, Context);
                {pass, JObj} ->
                    {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                                     {F(J, C), C}
                                             end, {JObj, Context}, ?PVT_FUNS),
                    Context#cb_context{doc=JObj1, resp_status=success}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Config, Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Config, #cb_context{req_data=Data}=Context) ->
    Id = <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    case wh_json_validator:is_valid(Data, <<"configs">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            crossbar_doc:load_merge(Id, JObj1, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).
