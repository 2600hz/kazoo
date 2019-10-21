%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_resource_selectors).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([resource/1, resource/2, set_resource/2]).
-export([selector/1, selector/2, set_selector/2]).
-export([start_time/1, start_time/2, set_start_time/2]).
-export([stop_time/1, stop_time/2, set_stop_time/2]).
-export([value/1, value/2, set_value/2]).

-export([format_resource_selectors_id/1, format_resource_selectors_id/2
        ,format_resource_selectors_db/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"resource_selectors">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec resource(doc()) -> kz_term:api_binary().
resource(Doc) ->
    resource(Doc, 'undefined').

-spec resource(doc(), Default) -> binary() | Default.
resource(Doc, Default) ->
    kz_json:get_binary_value([<<"resource">>], Doc, Default).

-spec set_resource(doc(), binary()) -> doc().
set_resource(Doc, Resource) ->
    kz_json:set_value([<<"resource">>], Resource, Doc).

-spec selector(doc()) -> kz_term:api_binary().
selector(Doc) ->
    selector(Doc, 'undefined').

-spec selector(doc(), Default) -> binary() | Default.
selector(Doc, Default) ->
    kz_json:get_binary_value([<<"selector">>], Doc, Default).

-spec set_selector(doc(), binary()) -> doc().
set_selector(Doc, Selector) ->
    kz_json:set_value([<<"selector">>], Selector, Doc).

-spec start_time(doc()) -> kz_term:api_integer().
start_time(Doc) ->
    start_time(Doc, 'undefined').

-spec start_time(doc(), Default) -> integer() | Default.
start_time(Doc, Default) ->
    kz_json:get_integer_value([<<"start_time">>], Doc, Default).

-spec set_start_time(doc(), integer()) -> doc().
set_start_time(Doc, StartTime) ->
    kz_json:set_value([<<"start_time">>], StartTime, Doc).

-spec stop_time(doc()) -> kz_term:api_integer().
stop_time(Doc) ->
    stop_time(Doc, 'undefined').

-spec stop_time(doc(), Default) -> integer() | Default.
stop_time(Doc, Default) ->
    kz_json:get_integer_value([<<"stop_time">>], Doc, Default).

-spec set_stop_time(doc(), integer()) -> doc().
set_stop_time(Doc, StopTime) ->
    kz_json:set_value([<<"stop_time">>], StopTime, Doc).

-spec value(doc()) -> kz_term:api_binary().
value(Doc) ->
    value(Doc, 'undefined').

-spec value(doc(), Default) -> binary() | Default.
value(Doc, Default) ->
    kz_json:get_binary_value([<<"value">>], Doc, Default).

-spec set_value(doc(), binary()) -> doc().
set_value(Doc, Value) ->
    kz_json:set_value([<<"value">>], Value, Doc).

%% @equiv format_resource_selectors_id(Account, raw)

-spec format_resource_selectors_id(kz_term:api_binary()) -> kz_term:api_binary().
format_resource_selectors_id(Account) ->
    format_resource_selectors_id(Account, 'raw').

%%------------------------------------------------------------------------------
%% @doc Given a representation of an account `resource_selectors'.
%% Returns it in a `encoded', `unencoded' or `raw' format.
%% @end
%%------------------------------------------------------------------------------
-type resource_format() :: 'unencoded' | 'encoded' | 'raw'.

-spec format_resource_selectors_id(kz_term:api_binary(), resource_format()) -> kz_term:api_binary();
                                  (kz_term:api_binary(), kz_time:gregorian_seconds()) -> kz_term:api_binary(). %% MODb!
format_resource_selectors_id('undefined', _Encoding) -> 'undefined';

format_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_RAW(_)=AccountId, 'raw') ->
    AccountId;
format_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_ENCODED(_)=AccountDb, 'encoded') ->
    AccountDb;
format_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_UNENCODED(_)=AccountDbUn, 'unencoded') ->
    AccountDbUn;
format_resource_selectors_id(?MATCH_ACCOUNT_RAW(A, B, Rest), 'raw') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
format_resource_selectors_id(?MATCH_ACCOUNT_RAW(A, B, Rest), 'encoded') ->
    ?MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest);
format_resource_selectors_id(?MATCH_ACCOUNT_RAW(A, B, Rest), 'unencoded') ->
    ?MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest);

format_resource_selectors_id(AccountId, 'raw') ->
    raw_resource_selectors_id(AccountId);
format_resource_selectors_id(AccountId, 'unencoded') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A,B,Rest) = raw_resource_selectors_id(AccountId),
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_resource_selectors_id(AccountId, 'encoded') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A,B,Rest) = raw_resource_selectors_id(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

%%------------------------------------------------------------------------------
%% Returns `account_id() | any()'.
%% Passes input along if not `account_id() | account_db() | account_db_unencoded().'
%%------------------------------------------------------------------------------
-spec raw_resource_selectors_id(kz_term:ne_binary()) -> kz_term:ne_binary().
raw_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_RAW(AccountId)) ->
    AccountId;
raw_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_resource_selectors_id(Other) ->
    case lists:member(Other, ?KZ_SYSTEM_DBS) of
        'true' -> Other;
        'false' ->
            lager:warning("raw account resource_selectors id doesn't process '~p'", [Other]),
            Other
    end.

%% @equiv format_resource_selectors_id(Account, encoded)

-spec format_resource_selectors_db(kz_term:api_binary()) -> kz_term:api_binary().
format_resource_selectors_db(AccountId) ->
    format_resource_selectors_id(AccountId, 'encoded').
