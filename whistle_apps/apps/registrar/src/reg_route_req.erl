%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Look up IP for authorization/replaying of route_req
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_route_req).

-export([handle_route_req/2]).

handle_route_req(JObj, _Props) ->
    case wh_json:get_value(<<"From-Network-Addr">>, JObj) of
        undefined -> ok; %% ignore with no network address available
        IP -> maybe_replay_route_req(JObj, IP)
    end.

maybe_replay_route_req(JObj, IP) ->
    ok.
