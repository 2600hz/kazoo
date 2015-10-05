%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%% Helpers for manipulating the #bh_context{} record
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_context).
-export([new/0, new/2
        ,from_subscription/1, from_subscription/2
        ,is_context/1
        ,account_id/1, set_account_id/2
        ,binding/1, set_binding/2
        ,auth_token/1, set_auth_token/2
        ,auth_account_id/1, set_auth_account_id/2
        ,websocket_session_id/1, set_websocket_session_id/2
        ,websocket_pid/1, set_websocket_pid/2
        ]).

-include("blackhole.hrl").

-record(bh_context, {
           auth_token = <<>> :: api_binary()
          ,auth_account_id :: api_binary()
          ,account_id :: api_binary()
          ,binding :: api_binary()
          ,websocket_session_id :: api_binary()
          ,websocket_pid :: api_pid()
          ,req_id = <<(wh_util:rand_hex_binary(16))/binary, "-bh">> :: ne_binary()
         }).

-type context() :: #bh_context{}.
-export_type([context/0]).

-spec new() -> context().
new()->
    put_reqid(#bh_context{}).

-spec new(pid(), ne_binary()) -> context().
new(SessionPid, SessionId) ->
    put_reqid(#bh_context{websocket_session_id = SessionId
                          ,websocket_pid = SessionPid
                         }).

-spec put_reqid(context()) -> context().
put_reqid(#bh_context{req_id = ReqId} = Context) ->
    wh_util:put_callid(ReqId),
    Context.

-spec from_subscription(wh_json:object()) -> context().
-spec from_subscription(context(), wh_json:object()) -> context().
from_subscription(Data) ->
    from_subscription(new(), Data).

from_subscription(Context, Data) ->
    Context#bh_context{account_id = wh_json:get_value(<<"account_id">>,Data)
                       ,auth_token = wh_json:get_value(<<"auth_token">>,Data)
                       ,binding = wh_json:get_value(<<"binding">>,Data)
                      }.

-spec is_context(_) -> boolean().
is_context(#bh_context{}) -> 'true';
is_context(_) -> 'false'.

-spec account_id(context()) -> api_binary().
-spec auth_token(context()) -> api_binary().
-spec auth_account_id(context()) -> api_binary().
-spec binding(context()) -> api_binary().
-spec websocket_session_id(context()) -> api_binary().
-spec websocket_pid(context()) -> api_binary().
account_id(#bh_context{account_id=AcctId}) -> AcctId.
auth_token(#bh_context{auth_token=AuthToken}) -> AuthToken.
auth_account_id(#bh_context{auth_account_id=AuthBy}) -> AuthBy.
binding(#bh_context{binding=Binding}) -> Binding.
websocket_session_id(#bh_context{websocket_session_id=SessionId}) -> SessionId.
websocket_pid(#bh_context{websocket_pid=SocketPid}) -> SocketPid.

-spec set_account_id(context(), ne_binary()) -> context().
-spec set_auth_token(context(), ne_binary()) -> context().
-spec set_auth_account_id(context(), ne_binary()) -> context().
-spec set_binding(context(), ne_binary()) -> context().
-spec set_websocket_session_id(context(), ne_binary()) -> context().
-spec set_websocket_pid(context(), pid()) -> context().
set_account_id(#bh_context{}=Context, AcctId) -> Context#bh_context{account_id=AcctId}.
set_auth_token(#bh_context{}=Context, AuthToken) -> Context#bh_context{auth_token=AuthToken}.
set_auth_account_id(#bh_context{}=Context, AuthBy) -> Context#bh_context{auth_account_id=AuthBy}.
set_binding(#bh_context{}=Context, Binding) -> Context#bh_context{binding=Binding}.
set_websocket_session_id(#bh_context{}=Context, SessionId) ->
    Context#bh_context{websocket_session_id=SessionId}.
set_websocket_pid(#bh_context{}=Context, SocketPid) ->
    Context#bh_context{websocket_pid=SocketPid}.
