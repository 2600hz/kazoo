%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_ips).

-export([new/0]).
-export([ips/1, ips/2, set_ips/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"ips">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec ips(doc()) -> kz_term:api_ne_binaries().
ips(Doc) ->
    ips(Doc, 'undefined').

-spec ips(doc(), Default) -> kz_term:ne_binaries() | Default.
ips(Doc, Default) ->
    kz_json:get_list_value([<<"ips">>], Doc, Default).

-spec set_ips(doc(), kz_term:ne_binaries()) -> doc().
set_ips(Doc, Ips) ->
    kz_json:set_value([<<"ips">>], Ips, Doc).
