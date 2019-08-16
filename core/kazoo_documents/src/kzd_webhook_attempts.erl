%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_webhook_attempts).

-export([new/0]).
-export([client_error/1, client_error/2, set_client_error/2]).
-export([hook_id/1, hook_id/2, set_hook_id/2]).
-export([reason/1, reason/2, set_reason/2]).
-export([response_body/1, response_body/2, set_response_body/2]).
-export([response_code/1, response_code/2, set_response_code/2]).
-export([result/1, result/2, set_result/2]).
-export([retries_left/1, retries_left/2, set_retries_left/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"webhook_attempts">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec client_error(doc()) -> kz_term:api_binary().
client_error(Doc) ->
    client_error(Doc, 'undefined').

-spec client_error(doc(), Default) -> binary() | Default.
client_error(Doc, Default) ->
    kz_json:get_binary_value([<<"client_error">>], Doc, Default).

-spec set_client_error(doc(), binary()) -> doc().
set_client_error(Doc, ClientError) ->
    kz_json:set_value([<<"client_error">>], ClientError, Doc).

-spec hook_id(doc()) -> kz_term:api_binary().
hook_id(Doc) ->
    hook_id(Doc, 'undefined').

-spec hook_id(doc(), Default) -> binary() | Default.
hook_id(Doc, Default) ->
    kz_json:get_binary_value([<<"hook_id">>], Doc, Default).

-spec set_hook_id(doc(), binary()) -> doc().
set_hook_id(Doc, HookId) ->
    kz_json:set_value([<<"hook_id">>], HookId, Doc).

-spec reason(doc()) -> kz_term:api_binary().
reason(Doc) ->
    reason(Doc, 'undefined').

-spec reason(doc(), Default) -> binary() | Default.
reason(Doc, Default) ->
    kz_json:get_binary_value([<<"reason">>], Doc, Default).

-spec set_reason(doc(), binary()) -> doc().
set_reason(Doc, Reason) ->
    kz_json:set_value([<<"reason">>], Reason, Doc).

-spec response_body(doc()) -> kz_term:api_binary().
response_body(Doc) ->
    response_body(Doc, 'undefined').

-spec response_body(doc(), Default) -> binary() | Default.
response_body(Doc, Default) ->
    kz_json:get_binary_value([<<"response_body">>], Doc, Default).

-spec set_response_body(doc(), binary()) -> doc().
set_response_body(Doc, ResponseBody) ->
    kz_json:set_value([<<"response_body">>], ResponseBody, Doc).

-spec response_code(doc()) -> kz_term:api_binary().
response_code(Doc) ->
    response_code(Doc, 'undefined').

-spec response_code(doc(), Default) -> binary() | Default.
response_code(Doc, Default) ->
    kz_json:get_binary_value([<<"response_code">>], Doc, Default).

-spec set_response_code(doc(), binary()) -> doc().
set_response_code(Doc, ResponseCode) ->
    kz_json:set_value([<<"response_code">>], ResponseCode, Doc).

-spec result(doc()) -> kz_term:api_binary().
result(Doc) ->
    result(Doc, 'undefined').

-spec result(doc(), Default) -> binary() | Default.
result(Doc, Default) ->
    kz_json:get_binary_value([<<"result">>], Doc, Default).

-spec set_result(doc(), binary()) -> doc().
set_result(Doc, Result) ->
    kz_json:set_value([<<"result">>], Result, Doc).

-spec retries_left(doc()) -> kz_term:api_integer().
retries_left(Doc) ->
    retries_left(Doc, 'undefined').

-spec retries_left(doc(), Default) -> integer() | Default.
retries_left(Doc, Default) ->
    kz_json:get_integer_value([<<"retries_left">>], Doc, Default).

-spec set_retries_left(doc(), integer()) -> doc().
set_retries_left(Doc, RetriesLeft) ->
    kz_json:set_value([<<"retries_left">>], RetriesLeft, Doc).
