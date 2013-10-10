%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600hz
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_apps_store).

-export([init/0
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
         ,content_types_provided/3 ,content_types_provided/4
         ,get/1, get/2, get/3, get/4
         ,post/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"apps_store/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.apps_store">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.apps_store">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.apps_store">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.apps_store">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.apps_store">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.apps_store">>, ?MODULE, 'post').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods() | [].
-spec allowed_methods(path_token(), path_token()) -> http_methods() | [].
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods() | [].
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(_, _) ->
    [?HTTP_GET].
allowed_methods(_, _, _) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /apps_store => []
%%    /apps_store/foo => [<<"foo">>]
%%    /apps_store/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.
resource_exists(_, _, _) -> 'true'.


-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context, Id, <<"icon">>) ->
    case read(Id, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            Icon = wh_json:get_value(<<"icon">>, JObj),
            case wh_json:get_value([<<"_attachments">>, Icon], JObj) of
                'undefined' -> Context;
                Attachment ->
                    CT = wh_json:get_value(<<"content_type">>, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end;
        _ -> Context
    end;
content_types_provided(Context, _, _) -> Context.

content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context, Id, <<"screenshot">>, Num) ->
    case read(Id, Context) of
        #cb_context{resp_status='success'}=Con ->
            case maybe_get_screenshot(Num, Con) of
                'error' -> Context;
                {'ok', _, Attachment} ->
                    CT = wh_json:get_value(<<"content_type">>, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end;
        _ -> Context
    end;
content_types_provided(Context, _, _, _) -> Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /apps_store mights load a list of skel objects
%% /apps_store/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, <<"icon">>) ->
    get_icon(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, <<"screenshot">>, Num) ->
    get_sreenshot(Id, Context, Num).

-spec get_icon(ne_binary(), cb_context:context()) -> cb_context:context().
get_icon(Id, Context) ->
    case read(Id, Context) of
        #cb_context{resp_status='success', doc=JObj}=Con ->
            Icon = wh_json:get_value(<<"icon">>, JObj),
            case wh_json:get_value([<<"_attachments">>, Icon], JObj) of
                'undefined' -> crossbar_util:response_bad_identifier(Id, Context);
                Attachment ->
                    lists:foldl(fun({K, V}, C) ->
                                    cb_context:add_resp_header(K, V, C)
                                end
                                ,crossbar_doc:load_attachment(Id, Icon, Con)
                                ,[{<<"Content-Disposition">>, <<"attachment; filename=", Icon/binary>>}
                                  ,{<<"Content-Type">>, wh_json:get_value(<<"content_type">>, Attachment)}
                                  ,{<<"Content-Length">>, wh_json:get_value(<<"length">>, Attachment)}
                                 ])
            end;
        Context1 -> Context1
    end.

-spec get_sreenshot(ne_binary(), cb_context:context(), ne_binary()) -> cb_context:context().
get_sreenshot(Id, Context, Num) ->
    case read(Id, Context) of
        #cb_context{resp_status='success'}=Con ->
            case maybe_get_screenshot(Num, Con) of
                'error' ->
                    crossbar_util:response_bad_identifier(Num, Context);
                {'ok', Screenshot, Attachment} ->
                    lists:foldl(
                        fun({K, V}, C) ->
                            cb_context:add_resp_header(K, V, C)
                        end
                        ,crossbar_doc:load_attachment(Id, Screenshot, Con)
                        ,[{<<"Content-Disposition">>, <<"attachment; filename=", Screenshot/binary>>}
                          ,{<<"Content-Type">>, wh_json:get_value(<<"content_type">>, Attachment)}
                          ,{<<"Content-Length">>, wh_json:get_value(<<"length">>, Attachment)}
                         ])
            end;
        Context1 -> Context1
    end.

maybe_get_screenshot(Num, #cb_context{doc=JObj}=Context) ->
    Screenshots = wh_json:get_value(<<"screenshots">>, JObj),
    try lists:nth(wh_util:to_integer(Num)+1, Screenshots) of
        S -> maybe_get_attachment(Context, S)
    catch
        _:_ -> 'error'
    end.

maybe_get_attachment(#cb_context{doc=JObj}, Name) ->
    case wh_json:get_value([<<"_attachments">>, Name], JObj) of
        'undefined' -> 'error';
        Attachment -> {'ok', Name, Attachment}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
get(#cb_context{}=Context) ->
    Context.
get(#cb_context{}=Context, _) ->
    Context.
get(#cb_context{}=Context, _, _) ->
    Context.
get(#cb_context{}=Context, _, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"apps_store">>, Context, OnSuccess).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"app">>, JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

