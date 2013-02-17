%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_channels).

-behaviour(gen_server).

-include("amqp_util.hrl").

-export([start_link/0]).
-export([add/1]).
-export([remove/1]).
-export([command/2]).
-export([find/0
         ,find/1
        ]).
-export([get_channel/0]).
-export([lost_connection/1]).
-export([reconnected/1]).
-export([monitor_channel/1]).
-export([monitor_consumer/1]).
-export([demonitor_channel/1]).
-export([demonitor_consumer/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(TAB, ?MODULE).


-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Pid) ->
    gen_server:call(?MODULE, {add, #wh_amqp_channel{consumer=Pid}}).

remove(#wh_amqp_channel{consumer=Pid}=Channel) ->
    gen_server:cast(?MODULE, {remove, Pid}),
    Channel.

command(#wh_amqp_channel{reconnecting=true}, _) -> ok;
command(#wh_amqp_channel{consumer=Pid}=Channel, Command) ->
    gen_server:cast(?MODULE, {command, Pid, Command}),
    Channel.

find() ->
    Pid = wh_amqp_channel:consumer_pid(),
    find(Pid).

find(Pid) ->
    case ets:lookup(?TAB, Pid) of
        [] -> {error, no_channel};
        [#wh_amqp_channel{}=Channel] ->
            Channel
    end.

get_channel() ->
    case find() of
        #wh_amqp_channel{channel=undefined}=Channel ->
            wh_amqp_channel:new(Channel);
        #wh_amqp_channel{}=Channel ->
            Channel;
        {error, no_channel} ->
            wh_amqp_channel:new()
    end.

find_reference(Ref) ->
    MatchSpec = [{#wh_amqp_channel{channel_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{channel, '$_'}}]},
                 {#wh_amqp_channel{consumer_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{consumer, '$_'}}]}
                ],
    ets:select(?TAB, MatchSpec).

lost_connection(#wh_amqp_connection{connection=Pid}) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {lost_connection, Pid});
lost_connection(_) -> ok.

reconnected(#wh_amqp_connection{}=Connection) ->
    spawn(fun() -> reconnected(ets:match_object(?TAB, #wh_amqp_channel{channel=undefined, _='_'}, 1), Connection) end).

reconnected('$end_of_table', _) -> ok;
reconnected({[Channel], Continuation}, Connection) ->
    catch wh_amqp_channel:open(Channel, Connection),
    reconnected(ets:match(Continuation), Connection).

monitor_channel(#wh_amqp_channel{channel=Pid}=Channel) when is_pid(Pid) ->
    gen_server:call(?MODULE, {monitor_channel, Channel});
monitor_channel(Channel) -> Channel.

monitor_consumer(#wh_amqp_channel{consumer=Pid}=Channel) when is_pid(Pid) ->
    gen_server:call(?MODULE, {monitor_consumer, Channel});
monitor_consumer(Channel) -> Channel.

demonitor_channel(#wh_amqp_channel{channel_ref=Ref}=Channel) when is_reference(Ref) ->
    gen_server:call(?MODULE, {demonitor_channel, Channel});
demonitor_channel(Channel) -> Channel.

demonitor_consumer(#wh_amqp_channel{consumer_ref=Ref}=Channel) when is_reference(Ref) ->
    gen_server:call(?MODULE, {demonitor_consumer, Channel});
demonitor_consumer(Channel) -> Channel.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    _ = ets:new(?TAB, [named_table, {keypos, #wh_amqp_channel.consumer}]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, #wh_amqp_channel{consumer=Pid}=Channel}, _, State) ->
    case ets:insert_new(?TAB, Channel) of
        true -> {reply, Channel, State};
        false ->
            io:format("~p already has a channel, use that one...~n", [Pid]),
            {reply, find(Pid), State}
    end;
handle_call({monitor_channel, Channel}, _, State) ->
    {reply, maybe_monitor_channel(Channel), State};
handle_call({monitor_consumer, Channel}, _, State) ->
    {reply, maybe_monitor_consumer(Channel), State};
handle_call({demonitor_channel, Channel}, _, State) ->
    {reply, maybe_demonitor_channel(Channel), State};
handle_call({demonitor_consumer, Channel}, _, State) ->
    {reply, maybe_demonitor_consumer(Channel), State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    io:format("NOT IMPLEMENTED(call): ~p~n", [_Msg]),
    {reply, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({lost_connection, Pid}, State) ->
    _ = demonitor_all_connection_channels(Pid),
    {noreply, State};
handle_cast({command, Pid, #'queue.delete'{queue=Queue}}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> ok;
            [[Commands]] ->
                C = lists:filter(fun(#'queue.declare'{queue=Q}) when Q =:= Queue -> 
                                         false;
                                    (_) -> true
                                 end, Commands),
                ets:update_element(?TAB, Pid, {#wh_amqp_channel.commands, C})
        end,
    {noreply, State};
handle_cast({command, Pid, #'queue.unbind'{queue=Queue, exchange=Exchange
                                          ,routing_key=RoutingKey}}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> ok;
            [[Commands]] ->
                C = lists:filter(fun(#'queue.bind'{queue=Q, exchange=E, routing_key=R})
                                       when Q =:= Queue, E =:= Exchange, R =:= RoutingKey -> 
                                         false;
                                    (_) -> true
                                 end, Commands),
                ets:update_element(?TAB, Pid, {#wh_amqp_channel.commands, C})
        end,
    {noreply, State};
handle_cast({command, Pid, #'basic.qos'{}=Command}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> ok;
            [[Commands]] ->
                C = [Command
                     |lists:filter(fun(#'basic.qos'{}) ->
                                           false;
                                      (_) -> true
                                   end, Commands)
                    ],
                ets:update_element(?TAB, Pid, {#wh_amqp_channel.commands, C})
        end,
    {noreply, State};
handle_cast({command, Pid, #'basic.cancel'{consumer_tag=CTag}}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> ok;
            [[Commands]] ->
                C = lists:filter(fun(#'basic.consume'{consumer_tag=T}) when T =:= CTag -> 
                                         false;
                                    (_) -> true
                                 end, Commands),
                ets:update_element(?TAB, Pid, {#wh_amqp_channel.commands, C})
        end,
    {noreply, State};
handle_cast({command, Pid, Command}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> ok;
            [[Commands]] ->
                ets:update_element(?TAB, Pid, {#wh_amqp_channel.commands, [Command|Commands]})
        end,
    {noreply, State};
handle_cast({update, Channel}, State) ->
    ets:insert(?TAB, Channel),
    {noreply, State};
handle_cast({update, Pid, Updates}, State) ->
    ets:update_element(?TAB, Pid, Updates),
    {noreply, State};
handle_cast({remove, Pid}, State) ->
    ets:delete(?TAB, Pid),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    io:format("~p: down msg ~p ~p ~p~n", [self(), Ref, _Pid, Reason]),
    erlang:demonitor(Ref, [flush]),
    handle_down_msg(find_reference(Ref), Reason),
    {noreply, State, hibernate};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("connections terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_down_msg([], _) -> ok;
handle_down_msg([Match|Matches], Reason) ->
    _ = handle_down_msg(Match, Reason),
    handle_down_msg(Matches, Reason);
handle_down_msg({channel, #wh_amqp_channel{connection=Pid}=Channel}, _) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false -> demonitor_all_connection_channels(Pid);
        true ->
            spawn(fun() -> channel_connection_down(Channel) end)
    end;
handle_down_msg({channel, #wh_amqp_channel{}=Channel}, _) ->
    spawn(fun() -> channel_connection_down(Channel) end);
handle_down_msg({consumer, #wh_amqp_channel{started=Started}=Channel}, _) ->
    io:format("~p: down message for a consumer~n", [self()]),
    Duration = wh_util:elapsed_s(Started),
    _ = case Duration < 5 of
            false -> ok;
            true ->
                lager:warning("short lived channel (~ps): ~p", [Duration, Channel])
        end,
    spawn(fun() -> wh_amqp_channel:remove(Channel) end).

demonitor_all_connection_channels(Pid) ->
    Channels = ets:match_object(?TAB, #wh_amqp_channel{connection=Pid, _='_'}),
    io:format("lost connection to ~p, demonitoring ~p channels~n", [Pid, length(Channels)]),
    _ = maybe_demonitor_channels(Channels),
    io:format("DEMONITORED ALL THE THINGS~n", []).

channel_connection_down(#wh_amqp_channel{started=Started}=Channel) ->
    io:format("~p: down message for a channel while connected, restarting~n", [self()]),
    Duration = wh_util:elapsed_s(Started),
    _ = case Duration < 5 of
            false -> ok;
            true ->
                lager:warning("short lived channel (~ps): ~p", [Duration, Channel])
        end,
    wh_amqp_channel:new(wh_amqp_channel:close(Channel)).

maybe_monitor_consumer(#wh_amqp_channel{consumer=Consumer}=Channel) when is_pid(Consumer) ->
%%    io:format("~p: monitor consumer ~p~n", [self(), Consumer]),
    Ref = erlang:monitor(process, Consumer),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.consumer_ref, Ref}]),
    Channel#wh_amqp_channel{consumer_ref=Ref};
maybe_monitor_consumer(Channel) ->
    Channel.

maybe_monitor_channel(#wh_amqp_channel{consumer=Consumer, channel=Pid
                                       ,connection=Connection}=Channel) when is_pid(Pid) ->
%%    io:format("~p: monitor channel ~p~n", [self(), Pid]),
    Ref = erlang:monitor(process, Pid),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.channel_ref, Ref}
                                        ,{#wh_amqp_channel.channel, Pid}
                                        ,{#wh_amqp_channel.connection, Connection}
                                       ]),
    Channel#wh_amqp_channel{channel_ref=Ref};
maybe_monitor_channel(Channel) ->
    Channel.

maybe_demonitor_channels([]) ->
    ok;
maybe_demonitor_channels([Channel|Channels]) ->
    _ = maybe_demonitor_channel(Channel),
    maybe_demonitor_channels(Channels).

maybe_demonitor_channel(#wh_amqp_channel{consumer=Consumer, channel_ref=Ref}=Channel) when is_reference(Ref) ->
%%    io:format("~p: demonitor channel ~p~n", [self(), Ref]),
    erlang:demonitor(Ref, [flush]),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.channel_ref, undefined}
                                        ,{#wh_amqp_channel.channel, undefined}
                                       ]),
    Channel#wh_amqp_channel{channel_ref=undefined, channel=undefined};
maybe_demonitor_channel(#wh_amqp_channel{}=Channel) ->
    Channel.

maybe_demonitor_consumer(#wh_amqp_channel{consumer=Consumer, consumer_ref=Ref}=Channel) when is_reference(Ref) ->
%%    io:format("~p: demonitor consumer ~p~n", [self(), Consumer]),
    erlang:demonitor(Ref, [flush]),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.consumer_ref, undefined}]),
    Channel#wh_amqp_channel{consumer_ref=undefined};
maybe_demonitor_consumer(#wh_amqp_channel{}=Channel) ->
    Channel.
