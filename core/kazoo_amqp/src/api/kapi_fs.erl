%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc FreeSWITCH `pass-through' API.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_fs).

-export([req/1, req_v/1]).
-export([declare_exchanges/0]).
-export([publish_req/2, publish_req/3]).

-include_lib("kz_amqp_util.hrl").

%% The AMQP pass-through of FS commands - whitelist commands allowed (excluding any prefixed by uuid_ which are auto-allowed)
-define(FS_COMMAND_WHITELIST, [<<"set">>, <<"hangup">>, <<"bridge">>]).

-define(FS_REQ_HEADERS, [<<"Application-Name">>, <<"Args">>]).
-define(OPTIONAL_FS_REQ_HEADERS, [<<"Insert-At">>]).
-define(FS_REQ_VALUES, [{<<"Event-Category">>, <<"fs">>}
                       ,{<<"Event-Name">>, <<"command">>}
                       ]).
-define(FS_REQ_TYPES, [{<<"Application-Name">>, fun(<<"uuid_", _/binary>>) -> 'true';
                                                   (App) -> lists:member(App, ?FS_COMMAND_WHITELIST)
                                                end}]).

%%------------------------------------------------------------------------------
%% @doc FreeSWITCH Request, Pass-through of FreeSWITCH dialplan commands.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        true -> kz_api:build_message(Prop, ?FS_REQ_HEADERS, ?OPTIONAL_FS_REQ_HEADERS);
        false -> {error, "Proplist failed validation for fs_req"}
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FS_REQ_HEADERS, ?FS_REQ_VALUES, ?FS_REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callctl_exchange().

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_req(Queue, JObj) ->
    publish_req(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Queue, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?FS_REQ_VALUES, fun req/1),
    kz_amqp_util:callctl_publish(Queue, Payload, ContentType).
