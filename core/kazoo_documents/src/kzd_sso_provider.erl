%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Accessors for `sso_provider' document.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_sso_provider).

-export([new/0]).
-export([friendly_name/1, friendly_name/2, set_friendly_name/2]).
-export([name/1, name/2, set_name/2]).
-export([params/1, params/2, set_params/2]).
-export([params_client_id/1, params_client_id/2, set_params_client_id/2]).
-export([params_include_granted_scopes/1, params_include_granted_scopes/2, set_params_include_granted_scopes/2]).
-export([params_response_type/1, params_response_type/2, set_params_response_type/2]).
-export([params_scopes/1, params_scopes/2, set_params_scopes/2]).
-export([url/1, url/2, set_url/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"sso_provider">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec friendly_name(doc()) -> kz_term:api_binary().
friendly_name(Doc) ->
    friendly_name(Doc, 'undefined').

-spec friendly_name(doc(), Default) -> binary() | Default.
friendly_name(Doc, Default) ->
    kz_json:get_binary_value([<<"friendly_name">>], Doc, Default).

-spec set_friendly_name(doc(), binary()) -> doc().
set_friendly_name(Doc, FriendlyName) ->
    kz_json:set_value([<<"friendly_name">>], FriendlyName, Doc).

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec params(doc()) -> kz_term:api_object().
params(Doc) ->
    params(Doc, 'undefined').

-spec params(doc(), Default) -> kz_json:object() | Default.
params(Doc, Default) ->
    kz_json:get_json_value([<<"params">>], Doc, Default).

-spec set_params(doc(), kz_json:object()) -> doc().
set_params(Doc, Params) ->
    kz_json:set_value([<<"params">>], Params, Doc).

-spec params_client_id(doc()) -> kz_term:api_binary().
params_client_id(Doc) ->
    params_client_id(Doc, 'undefined').

-spec params_client_id(doc(), Default) -> binary() | Default.
params_client_id(Doc, Default) ->
    kz_json:get_binary_value([<<"params">>, <<"client_id">>], Doc, Default).

-spec set_params_client_id(doc(), binary()) -> doc().
set_params_client_id(Doc, ParamsClientId) ->
    kz_json:set_value([<<"params">>, <<"client_id">>], ParamsClientId, Doc).

-spec params_include_granted_scopes(doc()) -> kz_term:api_boolean().
params_include_granted_scopes(Doc) ->
    params_include_granted_scopes(Doc, 'undefined').

-spec params_include_granted_scopes(doc(), Default) -> boolean() | Default.
params_include_granted_scopes(Doc, Default) ->
    kz_json:get_boolean_value([<<"params">>, <<"include_granted_scopes">>], Doc, Default).

-spec set_params_include_granted_scopes(doc(), boolean()) -> doc().
set_params_include_granted_scopes(Doc, ParamsIncludeGrantedScopes) ->
    kz_json:set_value([<<"params">>, <<"include_granted_scopes">>], ParamsIncludeGrantedScopes, Doc).

-spec params_response_type(doc()) -> kz_term:api_binary().
params_response_type(Doc) ->
    params_response_type(Doc, 'undefined').

-spec params_response_type(doc(), Default) -> binary() | Default.
params_response_type(Doc, Default) ->
    kz_json:get_binary_value([<<"params">>, <<"response_type">>], Doc, Default).

-spec set_params_response_type(doc(), binary()) -> doc().
set_params_response_type(Doc, ParamsResponseType) ->
    kz_json:set_value([<<"params">>, <<"response_type">>], ParamsResponseType, Doc).

-spec params_scopes(doc()) -> kz_term:api_ne_binaries().
params_scopes(Doc) ->
    params_scopes(Doc, 'undefined').

-spec params_scopes(doc(), Default) -> kz_term:ne_binaries() | Default.
params_scopes(Doc, Default) ->
    kz_json:get_list_value([<<"params">>, <<"scopes">>], Doc, Default).

-spec set_params_scopes(doc(), kz_term:ne_binaries()) -> doc().
set_params_scopes(Doc, ParamsScopes) ->
    kz_json:set_value([<<"params">>, <<"scopes">>], ParamsScopes, Doc).

-spec url(doc()) -> kz_term:api_binary().
url(Doc) ->
    url(Doc, 'undefined').

-spec url(doc(), Default) -> binary() | Default.
url(Doc, Default) ->
    kz_json:get_binary_value([<<"url">>], Doc, Default).

-spec set_url(doc(), binary()) -> doc().
set_url(Doc, Url) ->
    kz_json:set_value([<<"url">>], Url, Doc).
