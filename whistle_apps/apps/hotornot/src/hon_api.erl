%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Builder and validator, much like wh_api.erl, of the AMQP APIs
%%% exposed by this WhApp
%%% @end
%%% Created :  7 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hon_api).

-export([rating_req/1, rating_resp/1]).

-export([rating_req_v/1, rating_resp_v/1]).

-include("hotornot.hrl").

%%--------------------------------------------------------------------
%% @doc Rating request
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rating_req/1 :: (Prop) -> {ok, iolist()} | {error, string()} when
      Prop :: proplist() | json_object().
rating_req({struct, Prop}) ->
    rating_req(Prop);
rating_req(Prop) ->
    case rating_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?RATING_REQ_HEADERS, ?OPTIONAL_RATING_REQ_HEADERS);
	false -> {error, "Proplist failed validation for rating_req"}
    end.

-spec rating_req_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
rating_req_v({struct, Prop}) ->
    rating_req_v(Prop);
rating_req_v(Prop) ->
    wh_api:validate(Prop, ?RATING_REQ_HEADERS, ?RATING_REQ_VALUES, ?RATING_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Rating response
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec rating_resp/1 :: (Prop) -> {ok, iolist()} | {error, string()} when
      Prop :: proplist() | json_object().
rating_resp({struct, Prop}) ->
    rating_resp(Prop);
rating_resp(Prop) ->
    case rating_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?RATING_RESP_HEADERS, ?OPTIONAL_RATING_RESP_HEADERS);
	false -> {error, "Proplist failed validation for rating_resp"}
    end.

-spec rating_resp_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
rating_resp_v({struct, Prop}) ->
    rating_resp_v(Prop);
rating_resp_v(Prop) ->
    wh_api:validate(Prop, ?RATING_RESP_HEADERS, ?RATING_RESP_VALUES, ?RATING_RESP_TYPES).
