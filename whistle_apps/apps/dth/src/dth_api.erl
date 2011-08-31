%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Builder and validator, much like wh_api.erl, of the AMQP APIs
%%% exposed by this WhApp
%%% @end
%%% Created :  7 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_api).

-export([blacklist_req/1, blacklist_resp/1]).

-export([blacklist_req_v/1, blacklist_resp_v/1]).

-include("dth.hrl").

%%--------------------------------------------------------------------
%% @doc Blacklist Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec blacklist_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist() | json_object().
blacklist_req({struct, Prop}) ->
    blacklist_req(Prop);
blacklist_req(Prop) ->
    case blacklist_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?DTH_BLACKLIST_REQ_HEADERS, ?OPTIONAL_DTH_BLACKLIST_REQ_HEADERS);
	false -> {error, "Proplist failed validation for blacklist_req"}
    end.

-spec blacklist_req_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
blacklist_req_v({struct, Prop}) ->
    blacklist_req_v(Prop);
blacklist_req_v(Prop) ->
    wh_api:validate(Prop, ?DTH_BLACKLIST_REQ_HEADERS, ?DTH_BLACKLIST_REQ_VALUES, ?DTH_BLACKLIST_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Blacklist Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec blacklist_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist() | json_object().
blacklist_resp({struct, Prop}) ->
    blacklist_resp(Prop);
blacklist_resp(Prop) ->
    case blacklist_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?DTH_BLACKLIST_RESP_HEADERS, ?OPTIONAL_DTH_BLACKLIST_RESP_HEADERS);
	false -> {error, "Proplist failed validation for blacklist_resp"}
    end.

-spec blacklist_resp_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
blacklist_resp_v({struct, Prop}) ->
    blacklist_resp_v(Prop);
blacklist_resp_v(Prop) ->
    wh_api:validate(Prop, ?DTH_BLACKLIST_RESP_HEADERS, ?DTH_BLACKLIST_RESP_VALUES, ?DTH_BLACKLIST_RESP_TYPES).
