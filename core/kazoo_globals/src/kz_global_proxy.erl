%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
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

-type state() :: any().

-spec start_link(any()) -> startlink_ret().
start_link(Global) ->
    gen_server:start_link(?MODULE, [Global], []).

-spec stop(pid()) -> 'ok'.
-spec stop(pid(), any()) -> 'ok'.
stop(Pid) ->
    stop(Pid, 'normal').

stop(Pid, Reason) ->
    gen_server:cast(Pid, {'$proxy_stop', Reason}).

-spec send(pid(), any()) -> any().
send(Pid, Message) ->
    Pid ! Message.

-spec init(list()) -> {'ok', state()}.
init([Global]) ->
    {'ok', Global}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(Request, _From, Global) ->
    {'reply', amqp_call(Global, Request), Global}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'$proxy_stop', Reason}, Global) ->
    gen_server:call('kz_globals', {'delete_remote', self()}),
    {'stop', Reason, Global};
handle_cast(Message, Global) ->
    lager:debug("relaying cast: ~p", [Message]),
    amqp_send(Global, {'$gen_cast', Message}),
    {'noreply', Global}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(Message, Global) ->
    lager:debug("relaying msg: ~p", [Message]),
    amqp_send(Global, Message),
    {'noreply', Global}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _Global) ->
    lager:debug("global proxy ~p down: ~p", [_Global, _Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
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
