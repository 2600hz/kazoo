%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2021, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_shared_auth).

-export([new/0]).
-export([shared_auth/1, shared_auth/2, set_shared_auth/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"shared_auth">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec shared_auth(doc()) -> kz_term:api_ne_binary().
shared_auth(Doc) ->
    shared_auth(Doc, 'undefined').

-spec shared_auth(doc(), Default) -> kz_term:ne_binary() | Default.
shared_auth(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"shared_auth">>], Doc, Default).

-spec set_shared_auth(doc(), kz_term:ne_binary()) -> doc().
set_shared_auth(Doc, SharedAuthentication) ->
    kz_json:set_value([<<"shared_auth">>], SharedAuthentication, Doc).
