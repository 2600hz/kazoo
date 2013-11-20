%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_services).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,get/1, get/2
         ,post/1
        ]).

-include("../crossbar.hrl").

-define(PVT_TYPE, <<"service">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"services/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.services">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.services">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.services">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.get.services">>, ?MODULE, get),
    _ = crossbar_bindings:bind(<<"*.execute.put.services">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.services">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.services">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods() | [].

allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

allowed_methods(<<"plan">>) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /services => []
%%    /services/foo => [<<"foo">>]
%%    /services/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> boolean().
resource_exists() -> true.

resource_exists(<<"plan">>) -> true;
resource_exists(_) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /services mights load a list of service objects
%% /services/123 might load the service object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.

validate(#cb_context{account_id=AccountId, req_verb = ?HTTP_GET}=Context) ->
    crossbar_util:response(wh_services:public_json(AccountId), Context);
validate(#cb_context{req_data=JObj, account_id=AccountId, req_verb = ?HTTP_POST}=Context) ->
    BillingId = wh_json:get_value(<<"billing_id">>, JObj),
    try wh_services:set_billing_id(BillingId, AccountId) of
        undefined -> Context#cb_context{doc=undefined, resp_status=success};
        Services -> Context#cb_context{doc=Services, resp_status=success}
    catch
        throw:{Error, Reason} ->
            R = wh_json:set_value([<<"billing_id">>, <<"invalid">>], Reason, wh_json:new()),
            crossbar_util:response(error, wh_util:to_binary(Error), 400, R, Context)
    end.

validate(#cb_context{account_id=AccountId}=Context, <<"plan">>) ->
    crossbar_util:response(wh_services:service_plan_json(AccountId), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(#cb_context{}) -> #cb_context{}.
-spec get(#cb_context{}, path_token()) -> #cb_context{}.

get(#cb_context{}=Context) ->
    Context.

get(Context, <<"plan">>) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(#cb_context{}) -> #cb_context{}.
post(#cb_context{doc=undefined}=Context) ->
    Context;
post(#cb_context{doc=Services}=Context) ->
    try wh_services:save(Services) of
        NewServices ->
            crossbar_util:response(wh_services:public_json(NewServices), Context)
    catch
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.
