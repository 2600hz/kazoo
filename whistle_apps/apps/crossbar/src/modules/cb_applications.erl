%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_applications).

-export([init/0
         ,allowed_methods/0 ,allowed_methods/1
         ,resource_exists/0 ,resource_exists/1
         ,validate/1 ,validate/2
        ]).

-include("src/crossbar.hrl").

-define(APPLICATIONS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".applications">>).


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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.applications">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.applications">>, ?MODULE, resource_exists),
    crossbar_bindings:bind(<<"v1_resource.validate.applications">>, ?MODULE, validate).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(<<"default">>) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /applications => []
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(<<"default">>) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /applications mights load a list of group objects
%% /applications/123 might load the group object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).
validate(#cb_context{req_verb = ?HTTP_GET}=Context, <<"default">>) ->
    default_applications(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(#cb_context{}) -> #cb_context{}.
summary(Context) ->
    Context#cb_context{resp_status=success}.

-spec default_applications(#cb_context{}) -> #cb_context{}.
default_applications(Context) ->
    Apps = whapps_config:get(?APPLICATIONS_CONFIG_CAT, <<"default_applications">>, []),
    Context#cb_context{resp_data=Apps, resp_status=success}.