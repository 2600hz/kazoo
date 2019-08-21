%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_doc_handler).

-export([handle_req/2]).

-include("doodle.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    Id = kapi_conf:get_id(JObj),
    Db = kapi_conf:get_database(JObj),
    Type = kapi_conf:get_type(JObj),
    Action = kz_api:event_name(JObj),
    handle_doc(Action, Type, Db, Id).

-spec handle_doc(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
handle_doc(?DOC_CREATED, <<"sms">>, Db, Id) ->
    doodle_api:handle_api_sms(Db, Id);
handle_doc(_, <<"device">>, ?MATCH_ACCOUNT_RAW(AccountId), Id) ->
    doodle_maintenance:check_sms_by_device_id(AccountId, Id);
handle_doc(_, <<"user">>, ?MATCH_ACCOUNT_RAW(AccountId), Id) ->
    doodle_maintenance:check_sms_by_owner_id(AccountId, Id);
handle_doc(_, _, _, _) -> 'ok'.
