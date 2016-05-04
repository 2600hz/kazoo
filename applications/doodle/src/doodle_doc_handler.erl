%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(doodle_doc_handler).

-export([handle_req/2]).

-include("doodle.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    Id = kz_json:get_value(<<"ID">>, JObj),
    Db = kz_json:get_value(<<"Database">>, JObj),
    Type = kz_json:get_value(<<"Type">>, JObj),
    Action = kz_json:get_value(<<"Event-Name">>, JObj),
    handle_doc(Action, Type, Db, Id).

-spec handle_doc(maybe(binary()), maybe(binary()), maybe(binary()), maybe(binary())) -> 'ok'.
handle_doc(?DOC_CREATED, <<"sms">>, Db, Id) ->
    doodle_api:handle_api_sms(Db, Id);
handle_doc(_, <<"device">>, ?MATCH_ACCOUNT_RAW(AccountId), Id) ->
    doodle_maintenance:check_sms_by_device_id(AccountId, Id);
handle_doc(_, <<"user">>, ?MATCH_ACCOUNT_RAW(AccountId), Id) ->
    doodle_maintenance:check_sms_by_owner_id(AccountId, Id);
handle_doc(_, _, _, _) -> 'ok'.
