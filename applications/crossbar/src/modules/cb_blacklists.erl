%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_blacklists).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"blacklists/crossbar_listing">>).
-define(CB_LIST_BY_NUMBER, <<"blacklists/listing_by_number">>).
-define(CB_LIST_BY_PATTERN, <<"blacklists/listing_by_pattern">>).

-define(PATH_NUMBERS, <<"numbers">>).
-define(PATH_PATTERNS, <<"patterns">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.blacklists">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.blacklists">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.blacklists">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.blacklists">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.blacklists">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.blacklists">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.blacklists">>, ?MODULE, 'delete'),
    ok.

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
allowed_methods(?PATH_NUMBERS) ->
    [?HTTP_GET];
allowed_methods(?PATH_PATTERNS) ->
    [?HTTP_GET];
allowed_methods(_BlacklistId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?PATH_NUMBERS, _PhoneNumber) ->
    [?HTTP_GET];
allowed_methods(?PATH_PATTERNS, _Pattern) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists(?PATH_NUMBERS, _) -> 'true';
resource_exists(?PATH_PATTERNS, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_set(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?PATH_NUMBERS) ->
    read_by_view('undefined', ?CB_LIST_BY_NUMBER, Context);
validate(Context, ?PATH_PATTERNS) ->
    read_by_view('undefined', ?CB_LIST_BY_PATTERN, Context);
validate(Context, DocId) ->
    validate_set(Context, cb_context:req_verb(Context), DocId).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?PATH_NUMBERS, Number) ->
    read_by_view(Number, ?CB_LIST_BY_NUMBER, Context);
validate(Context, ?PATH_PATTERNS, Pattern) ->
    read_by_view(Pattern, ?CB_LIST_BY_PATTERN, Context).

-spec validate_set(cb_context:context(), path_token()) -> cb_context:context().
validate_set(Context, ?HTTP_GET) ->
    summary(Context);
validate_set(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_set(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_set(Context, ?HTTP_GET, DocId) ->
    read(DocId, Context);
validate_set(Context, ?HTTP_POST, DocId) ->
    update(DocId, Context);
validate_set(Context, ?HTTP_PATCH, DocId) ->
    validate_patch(DocId, Context);
validate_set(Context, ?HTTP_DELETE, DocId) ->
    read(DocId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(format_numbers(Context)).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(format_numbers(Context)).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(format_numbers(Context)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> validate_owner_id('undefined', C) end,
    cb_context:validate_request_data(kzd_blacklists:type(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_blacklists:type())).

%%------------------------------------------------------------------------------
%% @doc Load blacklists by number
%% @end
%%------------------------------------------------------------------------------
-spec read_by_view(kz_term:api_ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read_by_view(Key, ViewName, Context) ->
    Options = get_view_options(ViewName, Key),
    load_chunk_view(Context, ViewName, Options).

-spec get_view_options(kz_term:ne_binary(), kz_term:api_ne_binary()) -> crossbar_view:options().
get_view_options(?CB_LIST_BY_NUMBER, 'undefined') ->
    props:filter_undefined([]);
get_view_options(?CB_LIST_BY_NUMBER, Number) ->
    props:filter_undefined(
      [{'startkey', [Number]}
      ,{'endkey', [Number, kz_json:new()]}
      ]);
get_view_options(?CB_LIST_BY_PATTERN, 'undefined') ->
    props:filter_undefined([]);
get_view_options(?CB_LIST_BY_PATTERN, Pattern) ->
    props:filter_undefined(
      [{'startkey', [Pattern]}
      ,{'endkey', [Pattern, kz_json:new()]}
      ]).

-spec load_chunk_view(cb_context:context(), kz_term:ne_binary(), kz_term:proplist()) -> cb_context:context().
load_chunk_view(Context, ViewName, Options) ->
    crossbar_doc:load_view(ViewName, Options, Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> validate_owner_id(Id, C) end,
    cb_context:validate_request_data(kzd_blacklists:type(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document partially with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_crossbar_list_view_results/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_owner_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_owner_id(DocId, Context) ->
    AccountId = cb_context:account_id(Context),
    OwnerId = cb_context:req_value(Context, <<"owner_id">>),
    case kzd_blacklists:is_valid_owner_id(OwnerId, AccountId) of
        'false' ->
            Resp = [{<<"message">>, <<"Provided owner_id value is not valid">>}],
            cb_context:add_validation_error(<<"owner_id">>, <<"invalid">>, kz_json:from_list(Resp), Context);
        'true' -> maybe_delete_owner(DocId, Context, AccountId, OwnerId)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete_owner(kz_term:api_binary(), cb_context:context(), kz_term:api_binary(), kz_term:api_binary()) -> cb_context:context().
maybe_delete_owner(DocId, Context, AccountId, AccountId) ->
    Doc = cb_context:doc(Context),
    ContextNew = cb_context:set_doc(Context, kzd_blacklists:set_owner_id(Doc, 'undefined')),
    on_successful_validation(DocId, ContextNew);
maybe_delete_owner(DocId, Context, _AccountId, _OwnerId) ->
    on_successful_validation(DocId, Context).
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = cb_context:doc(Context),
    cb_context:set_doc(Context, kz_doc:set_type(Doc, <<"blacklist">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"blacklist">>)).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [Key, _Owner] = kz_json:get_list_value(<<"key">>, JObj),
    case kz_json:get_value(<<"value">>, JObj) of
        'undefined' -> Acc;
        Value -> [kz_json:set_value(<<"key">>, Key, Value)| Acc]
    end.

-spec normalize_crossbar_list_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_crossbar_list_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_numbers(cb_context:context()) -> cb_context:context().
format_numbers(Context) ->
    Doc = cb_context:doc(Context),
    Numbers =
        kz_json:map(fun format_number_map/2
                   ,kz_json:get_value(<<"numbers">>, Doc, kz_json:new())
                   ),
    cb_context:set_doc(Context
                      ,kz_json:set_value(<<"numbers">>, Numbers, Doc)
                      ).

-spec format_number_map(kz_term:ne_binary(), kz_json:object()) ->
                               {kz_term:ne_binary(), kz_json:object()}.
format_number_map(Number, Data) ->
    {knm_converters:normalize(Number), Data}.
