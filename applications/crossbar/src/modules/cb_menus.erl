%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Menus module
%%% Handle client requests for menu documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_menus).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"menus/crossbar_listing">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.menus">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.menus">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.menus">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.menus">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.menus">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.menus">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.menus">>, ?MODULE, 'delete'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_MenuId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_menus(Context, cb_context:req_verb(Context)).

validate_menus(Context, ?HTTP_GET) ->
    load_menu_summary(Context);
validate_menus(Context, ?HTTP_PUT) ->
    create_menu(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, DocId) ->
    validate_menu(Context, DocId, cb_context:req_verb(Context)).

validate_menu(Context, DocId, ?HTTP_GET) ->
    load_menu(DocId, Context);
validate_menu(Context, DocId, ?HTTP_POST) ->
    update_menu(DocId, Context);
validate_menu(Context, DocId, ?HTTP_PATCH) ->
    validate_patch(DocId, Context);
validate_menu(Context, DocId, ?HTTP_DELETE) ->
    load_menu(DocId, Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _DocId) ->
    crossbar_doc:save(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _DocId) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _DocId) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of accounts, each summarized. Or a specific
%% account summary.
%% @end
%%------------------------------------------------------------------------------
-spec load_menu_summary(cb_context:context()) -> cb_context:context().
load_menu_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Create a new menu document with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create_menu(cb_context:context()) -> cb_context:context().
create_menu(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(kzd_menus:schema_name(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load a menu document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_menu(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_menu(DocId, Context) ->
    crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION(kzd_menus:type())).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update_menu(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update_menu(DocId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(kzd_menus:schema_name(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(DocId, Context) ->
    crossbar_doc:patch_and_validate(DocId, Context, fun update_menu/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) ->
                                      cb_context:context().
on_successful_validation('undefined', Context) ->
    MenuReq = cb_context:doc(Context),
    MenuDoc = kz_doc:set_vsn(kz_doc:set_type(MenuReq, kzd_menus:type()), <<"2">>),
    cb_context:set_doc(Context, MenuDoc);
on_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context, ?TYPE_CHECK_OPTION(kzd_menus:type())).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
