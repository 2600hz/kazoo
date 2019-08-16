%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Ratedeck document accessors
%%%   Every account can be assigned a ratedeck
%%%
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_ratedeck).

-export([id/0
        ,name/1
        ,database_name/1
        ,format_ratedeck_db/1
        ,format_ratedeck_id/1
        ]).

-include("kz_documents.hrl").
-include_lib("kazoo_documents/include/kzd_ratedeck.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec id() -> kz_term:ne_binary().
id() -> <<"ratedeck">>.

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc).

-spec database_name(doc()) -> kz_term:api_ne_binary().
database_name(Doc) ->
    case name(Doc) of
        'undefined' -> ?KZ_RATES_DB;
        Name -> format_ratedeck_db(Name)
    end.

-spec format_ratedeck_db(kz_term:ne_binary()) -> kz_term:ne_binary().
format_ratedeck_db(?KZ_RATES_DB) -> ?KZ_RATES_DB;
format_ratedeck_db(?MATCH_RATEDECK_DB_ENCODED(_)=Db) -> Db;
format_ratedeck_db(?MATCH_RATEDECK_DB_encoded(_)=Db) -> Db;
format_ratedeck_db(?MATCH_RATEDECK_DB_UNENCODED(RatedeckId)) -> ?ENCODE_RATEDECK_DB(RatedeckId);
format_ratedeck_db(<<_/binary>> = RatedeckId) ->
    ?ENCODE_RATEDECK_DB(kz_http_util:urlencode(RatedeckId)).

-spec format_ratedeck_id(kz_term:ne_binary()) -> kz_term:ne_binary().
format_ratedeck_id(?KZ_RATES_DB) -> ?KZ_RATES_DB;
format_ratedeck_id(?MATCH_RATEDECK_DB_ENCODED(Id)) -> kz_http_util:urldecode(Id);
format_ratedeck_id(?MATCH_RATEDECK_DB_encoded(Id)) -> kz_http_util:urldecode(Id);
format_ratedeck_id(?MATCH_RATEDECK_DB_UNENCODED(Id)) -> kz_http_util:urldecode(Id);
format_ratedeck_id(<<_/binary>> = Id) -> Id.
