%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Execute node commands
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_node_command).

-export([handle_req/2]).

-include("ecallmgr.hrl").

-define(NODE_CMD_CONFIG, <<"node_commands">>).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Node = props:get_value('node', Props),
    'true' = kapi_switch:fs_command_v(JObj),
    Cmd = kz_json:get_ne_binary_value(<<"Command">>, JObj),
    Args = kz_json:get_value(<<"Args">>, JObj),
    exec_cmd(Cmd, Args, JObj, Node).

-spec exec_cmd(ne_binary(), api_object(), kz_json:object(), atom()) -> 'ok'.
exec_cmd(<<"send_http">>, 'undefined', JObj, _Node) ->
    lager:debug("received http_send command with empty arguments"),
    reply_error(<<"no arguments">>, JObj);
exec_cmd(<<"send_http">>, Args, JObj, Node) ->
    lager:debug("received http_send command"),
    Url = kz_json:get_ne_binary_value(<<"Url">>, Args),
    File = kz_json:get_value(<<"File-Name">>, Args),
    Method = <<"http_", (kz_json:get_value(<<"Http-Method">>, Args, <<"put">>))/binary>>,
    Default = ecallmgr_config:is_true([?NODE_CMD_CONFIG, <<"send_http">>, <<"delete_on_success">>], 'true'),
    DeleteOnSuccess = kz_json:is_true(<<"Delete-On-Success">>, JObj, Default),
    send_http(Node, File, Url, Method, JObj, DeleteOnSuccess);

exec_cmd(Cmd, _Args, JObj, _Node) ->
    reply_error(<<Cmd/binary, " not_implemented">>, JObj).

-spec reply_error(ne_binary(), kz_json:object()) -> 'ok'.
reply_error(Error, JObj) ->
    Values = [{<<"Result">>, <<"error">>}
             ,{<<"Error">>, Error}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_reply(Queue, P) end).

-spec reply_success(kz_json:object()) -> 'ok'.
-spec reply_success(kz_json:object(), kz_proplist()) -> 'ok'.
reply_success(JObj) ->
    reply_success(JObj, []).
reply_success(JObj, Response) ->
    Values = [{<<"Result">>, <<"success">>}
             ,{<<"Response">>, kz_json:from_list(Response)}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_reply(Queue, P) end).


-spec send_http(atom(), binary(), binary(), ne_binary(), kz_json:object(), boolean()) -> 'ok'.
send_http(Node, File, Url, Method, JObj, DeleteOnSuccess) ->
    lager:debug("processing http_send command : ~s / ~s", [File, Url]),
    Args = <<Url/binary, " ", File/binary>>,
    Fun = fun send_http_cb/3,
    M = kz_util:to_atom(Method, 'true'),
    A = kz_util:to_list(Args),
    case freeswitch:bgapi(Node, M, A, Fun, [JObj, DeleteOnSuccess, File, Node]) of
        {'error', _} -> reply_error(<<"failure">>, JObj);
        {'ok', JobId} -> lager:debug("send_http command started ~p", [JobId])
    end.

-spec send_http_cb(atom(), ne_binary(), list()) -> 'ok'.
send_http_cb('ok', <<"+OK", _/binary>>, [JobId, JObj, DeleteOnSuccess, File, Node]) ->
    lager:debug("processed http_send command with success : ~s", [JobId]),
    _ = maybe_delete_file(Node, File, DeleteOnSuccess),
    reply_success(JObj);
send_http_cb(_, Reply, [JobId, JObj]) ->
    lager:debug("error processing http_send : ~p : ~s", [Reply, JobId]),
    reply_error(Reply, JObj).

-spec maybe_delete_file(atom(), binary(), boolean()) -> any().
maybe_delete_file(_Node, _File, 'false') -> 'ok';
maybe_delete_file(Node, File, 'true') ->
    freeswitch:api(Node, 'system', <<"rm ", File/binary>>).
