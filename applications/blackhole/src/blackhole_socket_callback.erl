%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_socket_callback).
-include("blackhole.hrl").

-export([open/1, recv/2, close/1]).

-type cb_return() :: {'ok', bh_context:context()}.

-spec open(any()) -> cb_return().
open(Ipaddr) ->
    lager:debug("ws open pid:~p ip:~p", [self(), kz_util:to_binary(inet_parse:ntoa(Ipaddr))]),
    blackhole_limit:ip(Ipaddr),
    {'ok', bh_context:new(self())}.

-spec recv(kz_json:object(), bc_context:context()) -> bc_context:context().
recv(JObj, Context) ->
    {Command, Msg} = parse_incoming_message(JObj),
    Bindings = [<<"message">>
               ,<<"authenticate">>
               ,<<"command.", Command/binary>>
    ],
    lists:foldl(
      fun
          (Binding, Ctx=#bh_context{}) ->
                       blackhole_bindings:fold(Binding, [Ctx, Command, Msg]);
          (Binding, Error) ->
                       erlang:error({Binding, Error})
               end,
      Context, Bindings).

-spec close(bh_context:context()) -> 'ok'.
close(Context = #bh_context{websocket_pid=Pid}) ->
    blackhole_limit:release(Context),
    Filter = fun(Binding, Module, Function, Payload) -> filter_bindings(Pid, Binding, Module, Function, Payload) end,
    blackhole_bindings:filter(Filter),
    'ok'.

%% true mean to keep binding. if not ours then keep, otherwise remove
filter_bindings(Pid, <<"v1.blackhole", _/binary>> = _Binding, _Module, _Function, Context) ->
    bh_context:get_pid(Context) =/= Pid;
filter_bindings(_Pid, _Binding, _Module, _Function, _Context) -> 'true'.

parse_incoming_message(JObj) ->
    lager:debug("incoming ws message:~p", [JObj]),
    Command = kz_json:get_value(<<"command">>, JObj),
    JMsg = kz_json:get_value(<<"data">>, JObj),
    {Command, JMsg}.
