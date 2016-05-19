%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kz_global_proxy).

-behaviour(gen_server).

-export([start_link/1
         ,stop/1, stop/2
         ,send/2
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("kazoo_globals.hrl").

start_link(Global) ->
    gen_server:start_link(?MODULE, [Global], []).

stop(Pid) ->
    stop(Pid, 'normal').

stop(Pid, Reason) ->
    gen_server:cast(Pid, {'$proxy_stop', Reason}).

send(Pid, Message) ->
    gen_server:cast(Pid, {'$proxy_send', Message}).

init([Global]) ->
    {'ok', Global}.

handle_call(Request, _From, Global) ->
    {'reply', amqp_call(Global, Request), Global}.

handle_cast({'$proxy_stop', Reason}, Global) ->
    gen_server:call('kz_globals', {'delete_remote', self()}),
    {'stop', Reason, Global};
handle_cast({'$proxy_send', Message}, Global) ->
    amqp_send(Global, Message),
    {'noreply', Global};
handle_cast(_Msg, Global) ->
    {'noreply', Global}.

handle_info(_Msg, Global) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', Global}.

terminate(_Reason, _Global) ->
    lager:debug("global proxy ~p down: ~p", [_Global, _Reason]).

code_change(_OldVsn, Global, _Extra) ->
    {'ok', Global}.

-spec amqp_send(kz_global:global(), term()) -> 'ok'.
amqp_send(Global, Message) ->
    Payload = [{<<"Name">>, kz_global:name(Global)}
              ,{<<"Message">>, kapi_globals:encode(Message)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun kapi_globals:publish_send/1,
    kz_amqp_worker:cast(Payload, Publisher).

-spec amqp_call(kz_global:global(), term()) -> term().
amqp_call(Global, Msg) ->
    Payload = [{<<"Name">>, kz_global:name(Global)}
              ,{<<"Message">>, kapi_globals:encode(Msg)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun kapi_globals:publish_call/1,
    case kz_amqp_worker:call(Payload, Publisher, fun kapi_globals:reply_msg_v/1) of
        {'ok', JObj} -> kapi_globals:reply(JObj);
        Error -> Error
    end.
