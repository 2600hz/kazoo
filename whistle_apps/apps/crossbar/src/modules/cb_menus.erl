%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Menus module
%%%
%%% Handle client requests for menu documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_menus).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(CB_LIST, <<"menus/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.menus">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.menus">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.menus">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.menus">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.menus">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.menus">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_menu_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_menu(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_menu(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_menu(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_menu(DocId, Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _DocId) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _DocId) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_menu_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_menu_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new menu document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_menu/1 :: (#cb_context{}) -> #cb_context{}.
create_menu(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"menus">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Pvts = [fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"menu">>, J) end
                    ,fun(J) ->wh_json:set_value(<<"pvt_vsn">>, <<"2">>, J) end
                   ],
            Context#cb_context{
              doc=lists:foldr(fun(F, J) -> F(J) end, JObj, Pvts)
              ,resp_status=success
             }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a menu document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_menu/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_menu(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_menu/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_menu(DocId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"menus">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
