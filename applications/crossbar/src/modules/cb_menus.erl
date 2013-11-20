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

-include("../crossbar.hrl").

-define(CB_LIST, <<"menus/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.menus">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.menus">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.menus">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.menus">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.menus">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.execute.delete.menus">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
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
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_menu_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create_menu(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, DocId) ->
    load_menu(DocId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, DocId) ->
    update_menu(DocId, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, DocId) ->
    load_menu(DocId, Context).

-spec put(#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _DocId) ->
    crossbar_doc:save(Context).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
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
-spec load_menu_summary(#cb_context{}) -> #cb_context{}.
load_menu_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new menu document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_menu(#cb_context{}) -> #cb_context{}.
create_menu(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(undefined, C) end,
    cb_context:validate_request_data(<<"menus">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a menu document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_menu(ne_binary(), #cb_context{}) -> #cb_context{}.
load_menu(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_menu(ne_binary(), #cb_context{}) -> #cb_context{}.
update_menu(DocId, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"menus">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | ne_binary(), #cb_context{}) -> #cb_context{}.
on_successful_validation(undefined, #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_values([{<<"pvt_type">>, <<"menu">>}
                                               ,{<<"pvt_vsn">>, <<"2">>}
                                              ], JObj)};
on_successful_validation(DocId, #cb_context{}=Context) ->
    crossbar_doc:load_merge(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
