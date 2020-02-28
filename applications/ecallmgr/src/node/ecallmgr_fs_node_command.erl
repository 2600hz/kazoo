%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Execute node commands
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_node_command).

-export([handle_req/2]).

-include("ecallmgr.hrl").

-define(NODE_CMD_CONFIG, <<"node_commands">>).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Node = props:get_value('node', Props),
    Options = props:get_value('node_options', Props),
    'true' = kapi_switch:fs_command_v(JObj),
    Cmd = kz_json:get_ne_binary_value(<<"Command">>, JObj),
    Args = kz_json:get_value(<<"Args">>, JObj),
    exec_cmd(Cmd, Args, JObj, Node, Options).

-spec exec_cmd(kz_term:ne_binary(), kz_term:api_object(), kz_json:object(), atom(), kz_term:proplist()) -> 'ok'.
exec_cmd(<<"send_http">>, 'undefined', JObj, _Node, _Options) ->
    lager:debug("received http_send command with empty arguments"),
    reply_error(<<"no arguments">>, JObj);
exec_cmd(<<"send_http">>, Args, JObj, Node, Options) ->
    Version = props:get_value('client_version', Options),
    lager:debug("received http_send command for node ~s with version ~s", [Node, Version]),
    Url = kz_json:get_ne_binary_value(<<"Url">>, Args),
    File = kz_json:get_value(<<"File-Name">>, Args),
    Method = <<"kz_http_", (kz_json:get_value(<<"Http-Method">>, Args, <<"put">>))/binary>>,
    send_http(Node, File, Url, Method, JObj);

exec_cmd(Cmd, _Args, JObj, _Node, _Options) ->
    reply_error(<<Cmd/binary, " not_implemented">>, JObj).

-spec reply_error(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
reply_error(Error, JObj) ->
    Values = [{<<"Result">>, <<"error">>}
             ,{<<"Error">>, Error}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_fs_reply(Queue, P) end).

-spec reply_error(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
reply_error(Error, EventData, JObj) ->
    Values = [{<<"Result">>, <<"error">>}
             ,{<<"Error">>, Error}
             ,{<<"Event-Data">>, EventData}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_fs_reply(Queue, P) end).

-spec reply_success(kz_json:object(), kz_term:proplist()) -> 'ok'.
reply_success(JObj, Response) ->
    Values = [{<<"Result">>, <<"success">>}
             ,{<<"Response">>, kz_json:from_list(Response)}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kapi_switch:publish_fs_reply(Queue, API).

-spec send_http(atom(), binary(), binary(),  kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_http(Node, File, Url, Method, JObj) ->
    lager:debug("processing http_send command : ~s / ~s", [File, Url]),
    Args = <<Url/binary, " ", File/binary>>,
    M = kz_term:to_atom(Method, 'true'),
    A = kz_term:to_list(Args),
    Channel = kz_amqp_channel:consumer_channel(),
    case freeswitch:bgapi4(Node, M, A, fun send_http_cb/4, [JObj, File, Node, Channel]) of
        {'error', _} -> reply_error(<<"failure">>, JObj);
        {'ok', JobId} -> lager:debug("send_http command started ~p", [JobId])
    end.

-spec send_http_cb(atom(),  kz_term:ne_binary(),  kz_term:proplist(), list()) -> 'ok'.
send_http_cb('ok', _Reply, FSProps, [_JobId, JObj, _File, _Node, Channel]) ->
    lager:debug("processed http_send command (~s) ~s for file ~s with success : ~s", [_Node, _JobId, _File, _Reply]),
    _ = kz_amqp_channel:consumer_channel(Channel),
    reply_success(JObj, FSProps);
send_http_cb('error', Reply, FSProps, [JobId, JObj, _File, _Node, Channel]) ->
    lager:debug("error processing http_send command ~s : ~p : ", [JobId, Reply]),
    _ = kz_amqp_channel:consumer_channel(Channel),
    Props = ecallmgr_util:unserialize_fs_props(FSProps),
    reply_error(Reply, kz_json:from_list(Props), JObj).

