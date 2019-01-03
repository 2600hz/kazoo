%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Builder and validator, much like kz_api.erl, of the AMQP APIs
%%% exposed by this WhApp
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(dth_api).

-export([blacklist_req/1, blacklist_resp/1]).

-export([blacklist_req_v/1, blacklist_resp_v/1]).

-include("dth.hrl").

%%------------------------------------------------------------------------------
%% @doc Blacklist Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec blacklist_req(kz_term:api_terms()) -> {'ok', iolist()} |
                                            {'error', string()}.
blacklist_req(Prop) when is_list(Prop) ->
    case blacklist_req_v(Prop) of
        true -> kz_api:build_message(Prop, ?DTH_BLACKLIST_REQ_HEADERS, ?OPTIONAL_DTH_BLACKLIST_REQ_HEADERS);
        false -> {error, "Proplist failed validation for blacklist_req"}
    end;
blacklist_req(JObj) ->
    blacklist_req(kz_json:to_proplist(JObj)).

-spec blacklist_req_v(kz_term:api_terms()) -> boolean().
blacklist_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DTH_BLACKLIST_REQ_HEADERS, ?DTH_BLACKLIST_REQ_VALUES, ?DTH_BLACKLIST_REQ_TYPES);
blacklist_req_v(JObj) ->
    blacklist_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Blacklist Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec blacklist_resp(kz_term:api_terms()) -> {'ok', iolist()} |
                                             {'error', string()}.
blacklist_resp(Prop) when is_list(Prop) ->
    case blacklist_resp_v(Prop) of
        true -> kz_api:build_message(Prop, ?DTH_BLACKLIST_RESP_HEADERS, ?OPTIONAL_DTH_BLACKLIST_RESP_HEADERS);
        false -> {error, "Proplist failed validation for blacklist_resp"}
    end;
blacklist_resp(JObj) ->
    blacklist_resp(kz_json:to_proplist(JObj)).


-spec blacklist_resp_v(kz_term:api_terms()) -> boolean().
blacklist_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DTH_BLACKLIST_RESP_HEADERS, ?DTH_BLACKLIST_RESP_VALUES, ?DTH_BLACKLIST_RESP_TYPES);
blacklist_resp_v(JObj) ->
    blacklist_resp_v(kz_json:to_proplist(JObj)).
