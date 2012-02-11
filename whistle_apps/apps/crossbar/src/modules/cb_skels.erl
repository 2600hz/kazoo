%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%% Contributors: Karl Anderson
%%%               James Aimonetti
%%% 
%%%-------------------------------------------------------------------
-module(cb_skels).

-export([init/0
         ,authenticate/2
         ,authorize/2
         ,allowed_methods/1
         ,resource_exists/1
         ,content_types_provided/1
         ,content_types_accepted/1
         ,languages_provided/1
         ,charsets_provided/1
         ,encodings_provided/1
         ,validate/2
         ,billing/1
         ,get/2
         ,put/2
         ,post/2
         ,delete/2
         ,etag/1
         ,expires/1
         ,finish_request/1
        ]).

-include("../../include/crossbar.hrl").

-define(PVT_TYPE, <<"skel">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"skels/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.languages_provided.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.charsets_provided.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.encodings_provided.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.billing.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.get.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.put.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.post.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.delete.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.etag.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.expires.skels">>),
    _ = crossbar_bindings:bind(<<"v1_resource.finish_request.skels">>),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate/2 :: (path_tokens(), #cb_context{}) -> {boolean(), #cb_context{}}.
authenticate(_, #cb_context{}=Context) ->
    {false, Context}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize/2 :: (path_tokens(), #cb_context{}) -> {boolean(), #cb_context{}}.
authorize(_, #cb_context{}=Context) ->
    {false, Context}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_tokens()) -> http_methods() | [].
allowed_methods([]) ->
    ['GET', 'PUT'];
allowed_methods([_]) ->
    ['GET', 'POST', 'DELETE'];
allowed_methods(_) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (path_tokens()) -> boolean().
resource_exists([]) ->
    true;
resource_exists([_]) ->
    true;
resource_exists(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/1 :: (path_tokens()) -> [crossbar_content_handler(),...] | [].
content_types_provided(_) ->
    [].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted/1 :: (path_tokens()) -> crossbar_content_handler().
content_types_accepted(_) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative languages, return a list of languages and optional
%% quality value:
%% [<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]
%% @end
%%--------------------------------------------------------------------
-spec languages_provided/1 :: (path_tokens()) -> [ne_binary(),...] | [].
languages_provided(_) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative charsets, return a list of charsets and optional
%% quality value:
%% [<<"iso-8859-5">>, <<"unicode-1-1;q=0.8">>]
%% @end
%%--------------------------------------------------------------------
-spec charsets_provided/1 :: (path_tokens()) -> [ne_binary(),...] | [].
charsets_provided(_) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative encodings, return a list of encodings and optional
%% quality value:
%% [<<"gzip;q=1.0">>, <<"identity">>;q=0.5">>, <<"*;q=0">>]
%% @end
%%--------------------------------------------------------------------
-spec encodings_provided/1 :: (path_tokens()) -> [ne_binary(),...] | [].
encodings_provided(_) ->
    [].

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
-spec validate/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create(Context);
validate([Id], #cb_context{req_verb = <<"get">>}=Context) ->
    read(Id, Context);
validate([Id], #cb_context{req_verb = <<"post">>}=Context) ->
    update(Id, Context);
validate([Id], #cb_context{req_verb = <<"delete">>}=Context) ->
    read(Id, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you handle billing-related calls, this callback will allow you to
%% execute those.
%% @end
%%--------------------------------------------------------------------
-spec billing/1 :: (#cb_context{}) -> #cb_context{}.
billing(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
get(_, #cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
put(_, #cb_context{}=Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
post(_, #cb_context{}=Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
delete(_, #cb_context{}=Context) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you want to manipulate the etag header, change it here in the cb_context{}
%% @end
%%--------------------------------------------------------------------
-spec etag/1 :: (#cb_context{}) -> #cb_context{}.
etag(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the expires header
%% @end
%%--------------------------------------------------------------------
-spec expires/1 :: (#cb_context{}) -> #cb_context{}.
expires(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% The response has gone out, do some cleanup of your own here.
%% @end
%%--------------------------------------------------------------------
-spec finish_request/1 :: (#cb_context{}) -> #cb_context{}.
finish_request(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"skels">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"skels">>) of
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
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

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
