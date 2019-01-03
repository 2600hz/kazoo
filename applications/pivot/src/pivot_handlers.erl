%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pivot_handlers).

-export([handle_pivot_req/2]).

-include("pivot.hrl").

-spec handle_pivot_req(kz_json:object(), kz_term:proplist()) -> any().
handle_pivot_req(JObj, _Props) ->
    'true' = kapi_pivot:req_v(JObj),
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    pivot_calls_sup:new(Call, JObj).
