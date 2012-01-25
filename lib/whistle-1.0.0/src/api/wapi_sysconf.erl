%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Expose system configuration data.
%%% System configuration data is stored as key/values in a namespace
%%% (a doc) in system_config DB. 
%%% @end
%%%-------------------------------------------------------------------
-module(wapi_sysconf).

-export([read_req/1, read_req_v/1
        ,read_resp/1, read_resp_v/1
	 ,write_req/1, write_req_v/1
         ,write_resp/1, write_resp_v/1
	 ,bind_q/2, unbind_q/2
         ,publish_read_resp/1
         ,publish_write_resp/1]).

-include("../wh_api.hrl").

%% Configuration Document Update
%% request to read 
-define(SYSCONF_READ_REQ_HEADERS, [<<"Namespace">>, <<"Key">>]).
-define(OPTIONAL_SYSCONF_READ_REQ_HEADERS, []).

%% answer to a read request
-define(SYSCONF_READ_RESP_HEADERS, [<<"Namespace">>, <<"Key">>, <<"Value">>]).
-define(OPTIONAL_SYSCONF_READ_RESP_HEADERS, []).

%% request a write
-define(SYSCONF_WRITE_REQ_HEADERS, [<<"Namespace">>, <<"Key">>, <<"Value">>]).
-define(OPTIONAL_SYSCONF_WRITE_REQ_HEADERS, []).

%% answer to a write request
-define(SYSCONF_WRITE_RESP_HEADERS, [<<"Namespace">>, <<"Key">>, <<"Value">>, <<"Status">>]).
-define(OPTIONAL_SYSCONF_WRITE_RESP_HEADERS, []).


-define(SYSCONF_VALUES, [{<<"Event-Category">>, <<"sysconf">>}
                               ,{<<"Event-Name">>, [<<"read_req">>, <<"read_resp">>, <<"write_req">>, <<"write_resp">>]}]).

-define(SYSCONF_TYPES, [{<<"Namespace">>, fun is_binary/1}
                                ,{<<"Key">>, fun is_binary/1}]).


%%--------------------------------------------------------------------
%% @doc
%% READ
%% @end
%%--------------------------------------------------------------------
-spec read_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
read_req(Prop) when is_list(Prop) ->
    case read_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?SYSCONF_READ_REQ_HEADERS, ?OPTIONAL_SYSCONF_READ_REQ_HEADERS);
	false -> {error, "Proplist failed validation for sysconf read"}
    end;
read_req(JObj) ->
    read_req(wh_json:to_proplist(JObj)).

-spec read_req_v/1 :: (api_terms()) -> boolean().
read_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYSCONF_READ_REQ_HEADERS, ?SYSCONF_VALUES, ?SYSCONF_TYPES);
read_req_v(JObj) ->
    read_req_v(wh_json:to_proplist(JObj)).

-spec read_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
read_resp(Prop) when is_list(Prop) ->
    case read_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?SYSCONF_READ_RESP_HEADERS, ?OPTIONAL_SYSCONF_READ_RESP_HEADERS);
	false -> {error, "Proplist failed validation for sysconf read"}
    end;
read_resp(JObj) ->
    read_resp(wh_json:to_proplist(JObj)).

-spec read_resp_v/1 :: (api_terms()) -> boolean().
read_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYSCONF_READ_RESP_HEADERS, ?SYSCONF_VALUES, ?SYSCONF_TYPES);
read_resp_v(JObj) ->
    read_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% WRITE
%% @end
%%--------------------------------------------------------------------
-spec write_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
write_req(Prop) when is_list(Prop) ->
    case write_req_v(Prop) of
	true -> wh_api:build_message(Prop, ?SYSCONF_WRITE_REQ_HEADERS, ?OPTIONAL_SYSCONF_WRITE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for sysconf write"}
    end;
write_req(JObj) ->
    write_req(wh_json:to_proplist(JObj)).

-spec write_req_v/1 :: (api_terms()) -> boolean().
write_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYSCONF_WRITE_REQ_HEADERS, ?SYSCONF_VALUES, ?SYSCONF_TYPES);
write_req_v(JObj) ->
    write_req_v(wh_json:to_proplist(JObj)).

-spec write_resp/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
write_resp(Prop) when is_list(Prop) ->
    case write_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?SYSCONF_WRITE_RESP_HEADERS, ?OPTIONAL_SYSCONF_WRITE_RESP_HEADERS);
	false -> {error, "Proplist failed validation for sysconf write"}
    end;
write_resp(JObj) ->
    write_resp(wh_json:to_proplist(JObj)).

-spec write_resp_v/1 :: (api_terms()) -> boolean().
write_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYSCONF_WRITE_RESP_HEADERS, ?SYSCONF_VALUES, ?SYSCONF_TYPES);
write_resp_v(JObj) ->
    write_resp_v(wh_json:to_proplist(JObj)).


-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Q, _Prop) -> 
    amqp_util:sysconf_exchange(),
    amqp_util:bind_q_to_sysconf(Q, <<>>),
    ok.

-spec unbind_q/2 :: (binary(), proplist()) -> 'ok'.
unbind_q(Q, _Prop) ->
    amqp_util:unbind_q_from_sysconf(Q).

-spec publish_read_resp/1 :: (api_terms()) -> 'ok'.
publish_read_resp(Data) ->
    {ok, Payload} = wh_api:prepare_api_payload(Data, ?SYSCONF_VALUES, fun ?MODULE:read_resp/1),
    amqp_util:sysconf_publish(?KEY_SYSCONF_READ_RESP, Payload).

-spec publish_write_resp/1 :: (api_terms()) -> 'ok'.
publish_write_resp(Data) ->
    {ok, Payload} = wh_api:prepare_api_payload(Data, ?SYSCONF_VALUES, fun ?MODULE:write_resp/1),
    amqp_util:sysconf_publish(?KEY_SYSCONF_WRITE_RESP, Payload).
