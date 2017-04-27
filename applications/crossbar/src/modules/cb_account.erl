%%%-------------------------------------------------------------------
%%% @doc
%%% Alias for /accounts/{AUTH_ACCOUNT_ID}
%%% @end
%%% @contributors:
%%%   Max Lay
%%%-------------------------------------------------------------------
-module(cb_account).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate_resource/1, validate_resource/2
        ,validate/1, validate/2
        ,put/2
        ,post/1, post/2
        ,patch/1
        ,delete/2
        ]).

-include("crossbar.hrl").

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
    Bindings = [{<<"*.allowed_methods.account">>, 'allowed_methods'}
               ,{<<"*.resource_exists.account">>, 'resource_exists'}
               ,{<<"*.validate_resource.account">>, 'validate_resource'}
               ,{<<"*.validate.account">>, 'validate'}
               ,{<<"*.execute.put.account">>, 'put'}
               ,{<<"*.execute.post.account">>, 'post'}
               ,{<<"*.execute.patch.account">>, 'patch'}
               ,{<<"*.execute.delete.account">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    cb_accounts:allowed_methods('undefined').
allowed_methods(PathToken) ->
    cb_accounts:allowed_methods('undefined', PathToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> boolean().
-spec resource_exists(path_token()) -> boolean().
resource_exists() ->
    cb_accounts:resource_exists('undefined').
resource_exists(PathToken) ->
    cb_accounts:resource_exists('undefined', PathToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with account data
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec validate_resource(cb_context:context()) -> cb_context:context().
-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context) ->
    cb_accounts:validate_resource(transform_nouns(Context), cb_context:auth_account_id(Context)).
validate_resource(Context, PathToken) ->
    cb_accounts:validate_resource(transform_nouns(Context), cb_context:auth_account_id(Context), PathToken).

transform_nouns(Context) ->
    lager:debug("original request nouns ~p", [cb_context:req_nouns(Context)]),
    NewNouns = [maybe_transform_group(Context, X) || X <- cb_context:req_nouns(Context)],
    lager:debug("new request nouns ~p", [NewNouns]),
    cb_context:set_req_nouns(Context, NewNouns).

maybe_transform_group(Context, {<<"account">>, PathTokens}) ->
    {<<"accounts">>, [cb_context:auth_account_id(Context) | PathTokens]};
maybe_transform_group(_Context, Group) ->
    Group.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    cb_accounts:validate(Context, cb_context:auth_account_id(Context)).
validate(Context, PathToken) ->
    cb_accounts:validate(Context, cb_context:auth_account_id(Context), PathToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, PathToken) ->
    cb_accounts:put(Context, cb_context:auth_account_id(Context), PathToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context) ->
    cb_accounts:post(Context, cb_context:auth_account_id(Context)).
post(Context, PathToken) ->
    cb_accounts:post(Context, cb_context:auth_account_id(Context), PathToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context()) -> cb_context:context().
patch(Context) ->
    cb_accounts:patch(Context, cb_context:auth_account_id(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, PathToken) ->
    cb_accounts:delete(Context, cb_context:auth_account_id(Context), PathToken).
