%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%% utility functions
%%% @end
%%% Created : 12 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(dg_util).

-export([channel_status/2, send_command/3
         ,hold_call/1, hold_call/2, pickup_call/3
         ,hangup/1
        ]).

-include("datinggame.hrl").

-spec channel_status/2 :: (#dg_customer{} | #dg_agent{} | ne_binary(), ne_binary()) -> 'ok'.
channel_status(#dg_customer{call_id=CallID}, Q) ->
    channel_status(CallID, Q);
channel_status(#dg_agent{call_id=CallID}, Q) ->
    channel_status(CallID, Q);
channel_status(CallID, Q) when is_binary(CallID) ->
    Command = [{<<"Call-ID">>, CallID}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_call:publish_channel_status_req(CallID, Command).

-spec pickup_call/3 :: (#dg_customer{}, #dg_agent{}, ne_binary()) -> 'ok'.
pickup_call(#dg_customer{call_id=CCallID}, #dg_agent{call_id=ACallID}, CtrlQ) ->
    Command = [{<<"Application-Name">>, <<"call_pickup">>}
               ,{<<"Target-Call-ID">>, CCallID}
              ],
    send_command(Command, ACallID, CtrlQ).

-spec hold_call/1 :: (#dg_customer{} | #dg_agent{}) -> 'ok'.
-spec hold_call/2 :: (ne_binary(), ne_binary()) -> 'ok'.
hold_call(#dg_customer{call_id=CallID, control_queue=CtrlQ}) ->
    hold_call(CallID, CtrlQ);
hold_call(#dg_agent{call_id=CallID, control_queue=CtrlQ}) ->
    hold_call(CallID, CtrlQ).

hold_call(CallID, Q) ->
    Command = [{<<"Application-Name">>, <<"hold">>}
               ,{<<"Insert-At">>, <<"flush">>}
              ],
    send_command(Command, CallID, Q).

-spec hangup/1 :: (#dg_customer{} | #dg_agent{} | ne_binary()) -> 'ok'.
hangup(#dg_customer{call_id=CallID}) ->
    hangup(CallID);
hangup(#dg_agent{call_id=CallID}) ->
    hangup(CallID);
hangup(CallID) when is_binary(CallID) ->
    send_command([{<<"Application-Name">>, <<"hangup">>}], CallID, <<>>).

-spec send_command/3 :: (proplist(), ne_binary(), binary()) -> 'ok'.
send_command(Command, CallID, CtrlQ) ->
    Prop = Command ++ [{<<"Call-ID">>, CallID}
                       | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                      ],
    wapi_dialplan:publish_command(CtrlQ, Prop).
