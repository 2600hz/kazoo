%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Federation Utilities
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(wh_federation).

-export([send/2
         ,request/3, request/4
         ,request_custom/4, request_custom/5
         ,collect/2, collect/3, collect/4
         ,cast/2
        ]).

-export([federated/1, federated/2]).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-spec federated(wh_proplist() | wh_json:object()) -> wh_proplist() | wh_json:object().
-spec federated(wh_proplist() | wh_json:object(), atom()) -> wh_proplist() | wh_json:object().
federated(Props) ->
    federated(Props, 'true').

federated(Props, Value) when is_list(Props) ->
    props:set_value(?FEDERATION_MESSAGE, Value, Props);
federated(JObj, Value) ->
    wh_json:set_value(?FEDERATION_MESSAGE, Value, JObj).

-spec send(api_terms(), wh_amqp_worker:publish_fun()) ->
                            'ok' | {'error', any()}.
send(Api, PubFun) when is_function(PubFun, 1) ->
    wh_amqp_worker:cast(federated(Api), PubFun).

-spec request(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun()) ->
                               wh_amqp_worker:request_return().
-spec request(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun(), wh_timeout()) ->
                               wh_amqp_worker:request_return().
request(Api, PubFun, ValidateFun)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1) ->
    request(Api, PubFun, ValidateFun, wh_amqp_worker:default_timeout()).
request(Api, PubFun, ValidateFun, Timeout)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1),
       ((is_integer(Timeout) andalso Timeout >= 0)
        orelse Timeout =:= 'infinity') ->
    wh_amqp_worker:call(federated(Api), PubFun, ValidateFun, Timeout).

-spec request_custom(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun(), gen_listener:binding()) ->
                                      wh_amqp_worker:request_return().
-spec request_custom(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun(), wh_timeout(), gen_listener:binding()) ->
                                      wh_amqp_worker:request_return().
request_custom(Api, PubFun, ValidateFun, Bind)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1) ->
    request_custom(Api, PubFun, ValidateFun, wh_amqp_worker:default_timeout(), Bind).
request_custom(Api, PubFun, ValidateFun, Timeout, Bind)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1),
       ((is_integer(Timeout) andalso Timeout >= 0)
        orelse Timeout =:= 'infinity') ->
    wh_amqp_worker:call_custom(federated(Api), PubFun, ValidateFun, Timeout, Bind).

-spec collect(api_terms(), wh_amqp_worker:publish_fun()) ->
                               {'ok', wh_json:objects()} |
                               {'timeout', wh_json:objects()} |
                               {'error', any()}.
collect(Api, PubFun) ->
    collect(Api, PubFun, wh_amqp_worker:default_timeout()).

-spec collect(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:timeout_or_until()) ->
                               {'ok', wh_json:objects()} |
                               {'timeout', wh_json:objects()} |
                               {'error', any()}.
collect(Api, PubFun, TimeoutOrUntil) ->
    wh_amqp_worker:call_collect(federated(Api), PubFun, TimeoutOrUntil).

-spec collect(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:collect_until(), wh_timeout()) ->
                               {'ok', wh_json:objects()} |
                               {'timeout', wh_json:objects()} |
                               {'error', any()}.
collect(Api, PubFun, Until, Timeout) ->
    wh_amqp_worker:call_collect(federated(Api), PubFun, Until, Timeout).

-spec cast(api_terms(), wh_amqp_worker:publish_fun()) -> 'ok' | {'error', _}.
cast(Req, PubFun) ->
    wh_amqp_worker:cast(federated(Req), PubFun).
