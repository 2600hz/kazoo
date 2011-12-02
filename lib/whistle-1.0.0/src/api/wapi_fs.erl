%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% FS passthrough API
%%% @end
%%% Created : 17 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_fs).

-export([req/1, req_v/1, publish_req/2, publish_req/3]).

-include("../wh_api.hrl").

-define(FS_REQ_HEADERS, [<<"Application-Name">>, <<"Args">>]).
-define(OPTIONAL_FS_REQ_HEADERS, [<<"Insert-At">>]).
-define(FS_REQ_VALUES, [{<<"Event-Category">>, <<"fs">>}
			,{<<"Event-Name">>, <<"command">>}
		       ]).
-define(FS_REQ_TYPES, [{<<"Application-Name">>, fun(<<"uuid_", _/binary>>) -> true; (App) -> lists:member(App, ?FS_COMMAND_WHITELIST) end}]).

%%--------------------------------------------------------------------
%% @doc FS Request
%%     Pass-through of FS dialplan commands
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
	true -> wh_api:build_message(Prop, ?FS_REQ_HEADERS, ?OPTIONAL_FS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for fs_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FS_REQ_HEADERS, ?FS_REQ_VALUES, ?FS_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

-spec publish_req/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_req(Queue, JObj) ->
    publish_req(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Queue, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?FS_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:callctl_publish(Queue, Payload, ContentType).
