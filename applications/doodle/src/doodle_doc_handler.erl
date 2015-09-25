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
-include_lib("whistle/include/wapi_conf.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_conf:doc_update_v(JObj),
    Id = wh_json:get_value(<<"ID">>, JObj),
    Db = wh_json:get_value(<<"Database">>, JObj),
    Type = wh_json:get_value(<<"Type">>, JObj),
    Action = wh_json:get_value(<<"Event-Name">>, JObj),
    handle_doc(Action, Type, Db, Id).

-spec handle_doc(api_binary(), api_binary(), api_binary(), api_binary()) -> 'ok'.
handle_doc(?DOC_CREATED, <<"sms">>, Db, Id) ->
    doodle_api:handle_api_sms(Db, Id);
handle_doc(_, <<"device">>, <<_:32/binary>>=AccountId, Id) ->
    doodle_maintenance:check_sms_by_device_id(AccountId, Id);
handle_doc(_, <<"user">>, <<_:32/binary>>=AccountId, Id) ->
    doodle_maintenance:check_sms_by_owner_id(AccountId, Id);
handle_doc(_, _, _, _) -> 'ok'.
