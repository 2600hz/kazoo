%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handles authentication requests, responses, queue bindings
%%% @end
%%% Created : 14 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_authn).

-export([req/1, resp/1, req_v/1, resp_v/1, bind_q/2, unbind_q/1]).

-include("../wh_api.hrl").

-define(AUTHN_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
				, <<"Auth-User">>, <<"Auth-Realm">>]).
-define(OPTIONAL_AUTHN_REQ_HEADERS, [<<"Method">>]).
-define(AUTHN_REQ_VALUES, [{<<"Event-Category">>, <<"directory">>}
			   ,{<<"Event-Name">>, <<"authn_req">>}
			  ]).
-define(AUTHN_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			  ,{<<"To">>, fun is_binary/1}
			  ,{<<"From">>, fun is_binary/1}
			  ,{<<"Orig-IP">>, fun is_binary/1}
			  ,{<<"Auth-User">>, fun is_binary/1}
			  ,{<<"Auth-Realm">>, fun is_binary/1}
			 ]).

-spec req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
        case req_v(Prop) of
	    true -> wh_api:build_message(Prop, ?AUTHN_REQ_HEADERS, ?OPTIONAL_AUTHN_REQ_HEADERS);
	    false -> {error, "Proplist failed validation for authn_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (proplist() | json_object()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTHN_REQ_HEADERS, ?AUTHN_REQ_VALUES, ?AUTHN_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

resp(_) ->
    ok.

resp_v(_) ->
    ok.

bind_q(_,_) ->
    ok.

unbind_q(_) ->
    ok.
