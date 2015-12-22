%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
%%% @doc
%%% Execute node commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_node_command).

-export([handle_req/2]).

-include("ecallmgr.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Node = props:get_value('node', Props),
    'true' = wapi_switch:fs_command_v(JObj),
    Cmd = wh_json:get_ne_binary_value(<<"Command">>, JObj),
    Args = wh_json:get_value(<<"Args">>, JObj),
    exec_cmd(Cmd, Args, JObj, Node).

-spec exec_cmd(ne_binary(), api_object(), wh_json:object(), atom()) -> 'ok'.
exec_cmd(<<"send_http">>, 'undefined', JObj, _Node) ->
    lager:debug("received http_send command with empty arguments"),
    reply_error(<<"no arguments">>, JObj);
exec_cmd(<<"send_http">>, Args, JObj, Node) ->
    lager:debug("received http_send command"),
    Url = wh_json:get_value(<<"Url">>, Args),
    File = wh_json:get_value(<<"File-Name">>, Args),
    Filename = ecallmgr_util:recording_filename(File),
    Method = <<"http_", (wh_json:get_value(<<"Http-Method">>, Args, <<"put">>))/binary>>,
    send_http(Node, Filename, Url, Method, JObj);

exec_cmd(Cmd, _Args, JObj, _Node) ->
    reply_error(<<Cmd/binary, " not_implemented">>, JObj).

-spec reply_error(ne_binary(), wh_json:object()) -> 'ok'.
reply_error(Error, JObj) ->
    Values = [{<<"Result">>, <<"error">>}
              ,{<<"Error">>, Error}
              ,{<<"Msg-ID">>, wh_api:msg_id(JObj)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = wh_json:set_values(Values, wh_api:remove_defaults(JObj)),
    Queue = wh_api:server_id(JObj),
    wh_amqp_worker:cast(API, fun(P) -> wapi_switch:publish_reply(Queue, P) end).

-spec reply_success(wh_json:object()) -> 'ok'.
-spec reply_success(wh_json:object(), wh_proplist()) -> 'ok'.
reply_success(JObj) ->
    reply_success(JObj, []).
reply_success(JObj, Response) ->
    Values = [{<<"Result">>, <<"success">>}
              ,{<<"Response">>, wh_json:from_list(Response)}
              ,{<<"Msg-ID">>, wh_api:msg_id(JObj)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = wh_json:set_values(Values, wh_api:remove_defaults(JObj)),
    Queue = wh_api:server_id(JObj),
    wh_amqp_worker:cast(API, fun(P) -> wapi_switch:publish_reply(Queue, P) end).


-spec send_http(atom(), ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
send_http(Node, File, Url, Method, JObj) ->
    lager:debug("processing http_send command : ~s / ~s", [File, Url]),
    Args = <<Url/binary, " ", File/binary>>,
    Fun = fun send_http_cb/3,
    M = wh_util:to_atom(Method, 'true'),
    A = wh_util:to_list(Args),
    case freeswitch:bgapi(Node, M, A, Fun, [JObj]) of
        {'error', _} -> reply_error(<<"failure">>, JObj);
        {'ok', JobId} -> lager:debug("send_http command started ~p", [JobId])
    end.

-spec send_http_cb(atom(), ne_binary(), list()) -> 'ok'.
send_http_cb('ok', <<"+OK", _/binary>>, [JobId, JObj]) ->
    lager:debug("processed http_send command with success : ~s", [JobId]),
    reply_success(JObj);
send_http_cb(_, Reply, [JobId, JObj]) ->
    lager:debug("error processing http_send : ~p : ~s", [Reply, JobId]),
    reply_error(Reply, JObj).
