%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_response).

%% Accessors
-export([account_id/1
        ,number_state/1
        ,error_code/1
        ,message/1
        ,status/1
        ,data/1
        ]).

-include("kazoo_proper.hrl").

-spec account_id(pqc_cb_api:response() | kz_json:object()) -> kz_term:ne_binary().
account_id(JSON) when is_binary(JSON) ->
    account_id(kz_json:decode(JSON));
account_id(APIResp) ->
    kz_json:get_ne_binary_value([<<"data">>, <<"id">>], APIResp).

-spec number_state(pqc_cb_api:response() | kz_json:object()) -> atom().
number_state(JSON) when is_binary(JSON) ->
    number_state(kz_json:decode(JSON));
number_state(APIResp) ->
    kz_json:get_atom_value([<<"data">>, <<"_read_only">>, <<"state">>], APIResp).

-spec error_code(pqc_cb_api:response() | kz_json:object()) -> integer().
error_code(JSON) when is_binary(JSON) ->
    error_code(kz_json:decode(JSON));
error_code({'error', RespBody}) when is_binary(RespBody) ->
    error_code(kz_json:decode(RespBody));
error_code(APIResp) ->
    kz_json:get_integer_value(<<"error">>, APIResp).

-spec status(pqc_cb_api:response() | kz_json:object()) -> kz_term:ne_binary().
status(JSON) when is_binary(JSON) ->
    status(kz_json:decode(JSON));
status(APIResp) ->
    kz_json:get_ne_binary_value(<<"status">>, APIResp).

-spec data(pqc_cb_api:response() | kz_json:object()) ->
          kz_json:object() | kz_json:objects() | kz_term:ne_binaries().
data(JSON) when is_binary(JSON) ->
    data(kz_json:decode(JSON));
data({'error', RespBody}) when is_binary(RespBody) ->
    data(kz_json:decode(RespBody));
data(APIResp) ->
    kz_json:get_value(<<"data">>, APIResp).

-spec message(pqc_cb_api:response() | kz_json:object()) -> kz_term:ne_binary().
message(JSON) when is_binary(JSON) ->
    message(kz_json:decode(JSON));
message(APIResp) ->
    kz_json:get_ne_binary_value(<<"message">>, APIResp).
