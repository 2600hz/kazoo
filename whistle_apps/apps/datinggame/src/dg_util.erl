%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%% utility functions
%%% @end
%%% Created : 12 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(dg_util).

-export([channel_status/2]).

-include("datinggame.hrl").

-spec channel_status/2 :: (ne_binary(), #dg_customer{} | #dg_agent{} | ne_binary()) -> 'ok'.
channel_status(Queue, #dg_customer{call_id=CallID}) ->
    channel_status(Queue, CallID);
channel_status(Queue, #dg_agent{call_id=CallID}) ->
    channel_status(Queue, CallID);
channel_status(Queue, CallID) when is_binary(CallID) ->
    Command = [{<<"Call-ID">>, CallID}
               | wh_api:default_headers(Queue, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_call:publish_channel_status_req(CallID, Command).
