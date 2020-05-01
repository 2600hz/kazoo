%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_schemas).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authorize/1, authorize/2
        ,authenticate/1, authenticate/2
        ,validate/1, validate/2, validate/3
        ]).

-include("crossbar.hrl").

-define(VALIDATION_PATH_TOKEN, <<"validation">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.schemas">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.schemas">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.schemas">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.authenticate.schemas">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.validate.schemas">>, ?MODULE, 'validate'),
    ok.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    auth_nouns(Context, cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, _Schema) ->
    auth_nouns(Context, cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    auth_nouns(Context, cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _Schema) ->
    auth_nouns(Context, cb_context:req_nouns(Context)).

-spec auth_nouns(cb_context:context(), req_nouns()) -> boolean().
auth_nouns(Context, [{<<"schemas">>,_}]) ->
    lager:debug("authenticating request to fetch schema(s)"),
    cb_context:req_verb(Context) =:= ?HTTP_GET.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_SchemaName) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_SchemaName, ?VALIDATION_PATH_TOKEN) ->
    [?HTTP_PUT].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() ->  'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_SchemaName) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_SchemaName, ?VALIDATION_PATH_TOKEN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    lager:debug("load summary of schemas from ~s", [?KZ_SCHEMA_DB]),
    summary(cb_context:set_db_name(Context, ?KZ_SCHEMA_DB)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    read(Id, cb_context:set_db_name(Context, ?KZ_SCHEMA_DB)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id, ?VALIDATION_PATH_TOKEN) ->
    cb_context:validate_request_data(Id, Context, fun on_success/1).

-spec on_success(cb_context:context()) -> cb_context:context().
on_success(Context) ->
    cb_context:set_resp_data(Context, cb_context:doc(Context)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION_ANY).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = crossbar_doc:load_docs(Context, normalize_fun(Context)),
    cb_context:set_resp_data(Context1
                            ,lists:sort(cb_context:resp_data(Context1))
                            ).

-type normalizer_fun() :: fun((kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries()).
-spec normalize_fun(cb_context:context()) -> normalizer_fun().
normalize_fun(Context) ->
    case kz_term:to_boolean(cb_context:req_value(Context, <<"internals">>, 'false')) of
        'true' -> fun normalize_all_results/2;
        'false' -> fun normalize_doc_results/2
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_all_results(kz_json:object(), kz_json:objects()) -> kz_term:ne_binaries().
normalize_all_results(JObj, Acc) ->
    case kz_doc:id(JObj) of
        <<"_design/", _/binary>> -> Acc;
        ID -> [ID | Acc]
    end.

-spec normalize_doc_results(kz_json:object(), kz_json:objects()) -> kz_term:ne_binaries().
normalize_doc_results(JObj, Acc) ->
    case kz_doc:id(JObj) of
        <<"_design/", _/binary>> -> Acc;
        <<"kapi.", _/binary>> -> Acc;
        ID -> [ID | Acc]
    end.
