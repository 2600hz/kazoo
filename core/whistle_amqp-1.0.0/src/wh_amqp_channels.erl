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

-export([start_link/0]).
-export([remove/1]).
-export([command/2]).
-export([is_exchange_declared/2]).
-export([find/0
         ,find/1
        ]).
-export([get_channel/0]).
-export([lost_connection/1]).
-export([reconnect/0
         ,reconnect/1
        ]).
-export([force_reconnect/0
         ,force_reconnect/1
        ]).
-export([demonitor_channel/1]).
-export([demonitor_consumer/1]).
-export([all/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(TAB, ?MODULE).

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
start_link() -> gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec remove(wh_amqp_channel()) -> 'ok'.
remove(#wh_amqp_channel{consumer=Pid}) -> gen_server:cast(?MODULE, {'remove', Pid}).

-spec command(wh_amqp_channel(), wh_amqp_command()) -> wh_amqp_channel().
command(#wh_amqp_channel{reconnecting='true'}, _) -> 'ok';
command(#wh_amqp_channel{uri=URI}, #'exchange.declare'{}=Command) ->
    wh_amqp_connection:exchange_declared(URI, Command);
command(#wh_amqp_channel{consumer=Pid}=Channel, Command) ->
    gen_server:cast(?MODULE, {'command', Pid, Command}),
    Channel.

-spec is_exchange_declared(wh_amqp_channel(), #'exchange.declare'{}) -> boolean().
is_exchange_declared(#wh_amqp_channel{uri=URI}, #'exchange.declare'{}=Command) ->
    wh_amqp_connection:is_exchange_declared(URI, Command).

-spec find() -> wh_amqp_channel() | {'error', 'no_channel'}.
find() -> find(wh_amqp_channel:consumer_pid()).

-spec find(pid()) -> wh_amqp_channel() | {'error', 'no_channel'}.
find(Pid) ->
    case ets:lookup(?TAB, Pid) of
        [] -> {'error', 'no_channel'};
        [#wh_amqp_channel{}=Channel] -> Channel
    end.

-spec get_channel() -> wh_amqp_channel().
get_channel() ->
    case find() of
        {'error', 'no_channel'} -> new();
        #wh_amqp_channel{channel=Pid}=Channel
          when is_pid(Pid) -> Channel;
        #wh_amqp_channel{}=Channel ->
            gen_server:call(?MODULE, {'open', Channel})
    end.

-spec lost_connection(atom() | ne_binary()) -> 'ok'.
lost_connection(URI) when not is_binary(URI) ->
    lost_connection(wh_util:to_binary(URI));
lost_connection(URI) ->
    gen_server:call(?MODULE, {'lost_connection', URI}).

-spec reconnect() -> pid() | {'error', _}.
reconnect() ->
    case wh_amqp_connections:current() of
        {'ok', URI} -> reconnect(URI);
        {'error', _}=E -> E
    end.

-spec reconnect(ne_binary()) -> 'ok'.
reconnect(URI) ->
    gen_server:cast(?MODULE, {'reconnect', URI}).

-spec force_reconnect() -> pid() | {'error', _}.
force_reconnect() ->
    case wh_amqp_connections:current() of
        {'ok', URI} -> force_reconnect(URI);
        {'error', _}=E -> E
    end.

-spec force_reconnect(ne_binary()) -> 'ok'.
force_reconnect(URI) ->
    gen_server:cast(?MODULE, {'force_reconnect', URI}).

-spec demonitor_channel(wh_amqp_channel()) -> wh_amqp_channel().
demonitor_channel(#wh_amqp_channel{channel_ref=Ref}=Channel) when is_reference(Ref) ->
    gen_server:call(?MODULE, {'demonitor_channel', Channel});
demonitor_channel(Channel) -> Channel.

-spec demonitor_consumer(wh_amqp_channel()) -> wh_amqp_channel().
demonitor_consumer(#wh_amqp_channel{consumer_ref=Ref}=Channel) when is_reference(Ref) ->
    gen_server:call(?MODULE, {'demonitor_consumer', Channel});
demonitor_consumer(Channel) -> Channel.

all() ->
    ets:tab2list(?TAB).

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
%%                     {'stop', Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    put(callid, ?LOG_SYSTEM_ID),
    _ = ets:new(?TAB, ['named_table', {'keypos', #wh_amqp_channel.consumer}]),
    {'ok', 'ok'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, Reply, State} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({'new', #wh_amqp_channel{consumer=Pid}=Channel}, _, State) ->
    case ets:insert_new(?TAB, Channel) of
        'true' -> {'reply', open(Channel), State};
        'false' -> {'reply', find(Pid), State}
    end;
handle_call({'open', #wh_amqp_channel{}=Channel}, _, State) ->
    {'reply', open(Channel), State};
handle_call({'demonitor_channel', Channel}, _, State) ->
    {'reply', maybe_demonitor_channel(Channel), State};
handle_call({'demonitor_consumer', Channel}, _, State) ->
    {'reply', maybe_demonitor_consumer(Channel), State};
handle_call({'lost_connection', URI}, _, State) ->
    _ = demonitor_all_connection_channels(URI),
    {'reply', 'ok', State};
handle_call('stop', _, State) ->
    {'stop', 'normal', 'ok', State};
handle_call(_Msg, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'reconnect', URI}, State) ->
    spawn(fun() ->
                  Disconnected = #wh_amqp_channel{channel='undefined', _='_'},
                  case ets:match_object(?TAB, Disconnected, 1) of
                      '$end_of_table' -> 'ok';
                      Matches ->
                          lager:notice("reconnecting all disconnected channels to '~s'", [URI]),
                          reconnect(Matches, URI)
                  end
          end),
    {'noreply', State};
handle_cast({'force_reconnect', URI}, State) ->
    spawn(fun() ->
                  MatchSpec = [{#wh_amqp_channel{uri = '$2', _ = '_'}
                                ,[{'=/=', '$2', {'const', URI}}]
                                ,['$_']
                               }],
                  case ets:select(?TAB, MatchSpec, 1) of
                      '$end_of_table' -> 'ok';
                      Matches ->
                          lager:notice("forcing all channels to connect to '~s'", [URI]),
                          force_reconnect(Matches, URI)
                  end
          end),
    {'noreply', State};
handle_cast({'command', Pid, #'basic.qos'{}=Command}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> 'ok';
            [[Commands]] ->
                C = [Command
                     |lists:filter(fun(#'basic.qos'{}) -> 'false';
                                      (_) -> 'true'
                                   end, Commands)
                    ],
                ets:update_element(?TAB, Pid, {#wh_amqp_channel.commands, C})
        end,
    {'noreply', State};
handle_cast({'command', Pid, #'queue.delete'{queue=Queue}}, State) ->
    Filter = fun(#'queue.declare'{queue=Q}) when Q =:= Queue -> 'false';
                (_) -> 'true'
             end,
    _ = filter_commands(Pid, Filter),
    {'noreply', State};
handle_cast({'command', Pid, #'queue.unbind'{queue=Queue
                                             ,exchange=Exchange
                                             ,routing_key=RoutingKey
                                            }}, State) ->
    Filter = fun(#'queue.bind'{queue=Q, exchange=E, routing_key=R})
                   when Q =:= Queue, E =:= Exchange, R =:= RoutingKey ->
                     'false';
                (_) -> 'true'
             end,
    _ = filter_commands(Pid, Filter),
    {'noreply', State};
handle_cast({'command', Pid, #'basic.cancel'{consumer_tag=CTag}}, State) ->
    lager:debug("recv cancel for consumer ~s(~p)", [CTag, Pid]),
    Filter = fun(#'basic.consume'{consumer_tag=T}) -> T =/= CTag;
                (_) -> 'true'
             end,
    _ = filter_commands(Pid, Filter),
    {'noreply', State};
handle_cast({'command', Pid, Command}, State) ->
    _ = case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
            [] -> 'ok';
            [[Commands]] ->
                C = [C || C <- Commands, C =/= Command],
                Update = {#wh_amqp_channel.commands, [Command|C]},
                ets:update_element(?TAB, Pid, Update)
        end,
    {'noreply', State};
handle_cast({'remove', Pid}, State) ->
    ets:delete(?TAB, Pid),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, 'process', _Pid, Reason}, State) ->
    erlang:demonitor(Ref, ['flush']),
    handle_down_msg(find_reference(Ref), Reason),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec new() -> wh_amqp_channel().
new() ->
    case wh_amqp_connections:current() of
        {'ok', URI} -> new(URI);
        {'error', _} ->
            gen_server:call(?MODULE, {'new', #wh_amqp_channel{}})
    end.

-spec new(ne_binary()) -> wh_amqp_channel().
new(URI) ->
    gen_server:call(?MODULE, {'new', #wh_amqp_channel{uri=URI}}).

-spec reconnect({wh_amqp_channels(), _} | '$end_of_table', ne_binary()) -> 'ok'.
reconnect('$end_of_table', _) -> 'ok';
reconnect({[Channel], Continuation}, URI) ->
    _ = open(Channel#wh_amqp_channel{reconnecting='true'}),
    reconnect(ets:match(Continuation), URI).

-spec force_reconnect({wh_amqp_channels(), _} | '$end_of_table', ne_binary()) -> 'ok'.
force_reconnect('$end_of_table', _) -> 'ok';
force_reconnect({[Channel], Continuation}, URI) ->
    catch wh_amqp_channel:close(Channel),
    _ = open(Channel#wh_amqp_channel{reconnecting='true'}),
    force_reconnect(ets:select(Continuation), URI).

-spec filter_commands(pid(), function()) -> 'ok'.
filter_commands(Pid, Filter) ->
    case ets:match(?TAB, #wh_amqp_channel{consumer=Pid, commands='$1', _='_'}) of
        [] -> 'ok';
        [[Commands]] ->
            Update = {#wh_amqp_channel.commands, lists:filter(Filter, Commands)},
            ets:update_element(?TAB, Pid, Update)
    end.

-type down_match() :: {'channel' | 'consumer', wh_amqp_channel()}.
-type down_matches() :: [down_match(),...] | [].

-spec handle_down_msg(down_matches(), _) -> 'ok'.
handle_down_msg([], _) -> 'ok';
handle_down_msg([_|_]=Matches, Reason) ->
    _ = [handle_down_match(Match, Reason) || Match <- Matches],
    'ok'.

-spec handle_down_match(down_match(), _) -> any().
handle_down_match({'channel', #wh_amqp_channel{uri=URI}}
                  ,{'shutdown',{'connection_closing', Reason}}) ->
    lager:critical("channel died when connection to '~s' was lost: ~p", [URI, Reason]),
    demonitor_all_connection_channels(URI);
handle_down_match({'channel', #wh_amqp_channel{uri=URI}}
                  ,'shutdown') ->
    lager:critical("channel died when connection to '~s' was lost: shutdown", [URI]),
    demonitor_all_connection_channels(URI);
handle_down_match({'channel', #wh_amqp_channel{uri=URI}=Channel}, Reason) ->
    lager:notice("channel went down while still connected to '~s': ~p", [URI, Reason]),
    spawn(fun() -> attempt_rebuild_channel(Channel) end);
handle_down_match({'consumer', Channel}, Reason) ->
    _ = log_short_lived(Channel),
    lager:notice("consumer went down without closing channel: ~p", [Reason]),
    spawn(fun() -> wh_amqp_channel:remove(Channel) end).

-spec log_short_lived(wh_amqp_channel()) -> 'ok'.
log_short_lived(#wh_amqp_channel{started=Started}=Channel) ->
    Duration = wh_util:elapsed_s(Started),
    case Duration < 5 of
        'false' -> 'ok';
        'true' -> lager:info("short lived channel (~ps): ~p", [Duration, Channel])
    end.

-spec find_reference(reference()) -> down_matches().
find_reference(Ref) ->
    MatchSpec = [{#wh_amqp_channel{channel_ref = '$1', _ = '_'}
                  ,[{'=:=', '$1', {'const', Ref}}]
                  ,[{{'channel', '$_'}}]
                 }
                 ,{#wh_amqp_channel{consumer_ref = '$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', Ref}}]
                   ,[{{'consumer', '$_'}}]
                  }],
    ets:select(?TAB, MatchSpec).

-spec demonitor_all_connection_channels(ne_binary() | wh_amqp_channels() | '$end_of_table') -> 'ok'.
demonitor_all_connection_channels(URI) when is_binary(URI) ->
    MatchSpec = [{#wh_amqp_channel{channel = '$1', uri = '$2', _ = '_'}
                  ,[{'=:=', '$2', {'const', URI}}]
                  ,['$_']
                 }],
    lager:notice("removing channels belonging to lost connection '~s'", [URI]),
    demonitor_all_connection_channels(ets:select(?TAB, MatchSpec, 1));
demonitor_all_connection_channels('$end_of_table') -> 'ok';
demonitor_all_connection_channels({[Channel], Continuation}) ->
    _ = maybe_demonitor_channel(Channel),
    demonitor_all_connection_channels(ets:match(Continuation)).

-spec attempt_rebuild_channel(wh_amqp_channel()) -> wh_amqp_channel().
attempt_rebuild_channel(Channel) ->
    _ = log_short_lived(Channel),
    C1 = wh_amqp_channel:close(Channel#wh_amqp_channel{reconnecting='true'}),
    open(C1).

-spec maybe_monitor_consumer(wh_amqp_channel()) -> wh_amqp_channel().
maybe_monitor_consumer(#wh_amqp_channel{consumer=Consumer}=Channel) when is_pid(Consumer) ->
    Ref = erlang:monitor('process', Consumer),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.consumer_ref, Ref}]),
    Channel#wh_amqp_channel{consumer_ref=Ref};
maybe_monitor_consumer(Channel) -> Channel.

-spec maybe_monitor_channel(wh_amqp_channel()) -> wh_amqp_channel().
maybe_monitor_channel(#wh_amqp_channel{consumer=Consumer
                                       ,channel=Pid
                                      }=Channel) when is_pid(Pid) ->
    Ref = erlang:monitor('process', Pid),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.channel_ref, Ref}
                                        ,{#wh_amqp_channel.channel, Pid}
                                       ]),
    Channel#wh_amqp_channel{channel_ref=Ref};
maybe_monitor_channel(Channel) -> Channel.

-spec maybe_demonitor_channel(wh_amqp_channel()) -> wh_amqp_channel().
maybe_demonitor_channel(#wh_amqp_channel{consumer=Consumer
                                         ,channel_ref=Ref
                                        }=Channel) when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.channel_ref, 'undefined'}
                                        ,{#wh_amqp_channel.channel, 'undefined'}
                                       ]),
    Channel#wh_amqp_channel{channel_ref='undefined', channel='undefined'};
maybe_demonitor_channel(#wh_amqp_channel{}=Channel) -> Channel.

-spec maybe_demonitor_consumer(wh_amqp_channel()) -> wh_amqp_channel().
maybe_demonitor_consumer(#wh_amqp_channel{consumer=Consumer
                                          ,consumer_ref=Ref
                                         }=Channel) when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.consumer_ref, 'undefined'}]),
    Channel#wh_amqp_channel{consumer_ref='undefined'};
maybe_demonitor_consumer(#wh_amqp_channel{}=Channel) -> Channel.

-spec open(wh_amqp_channel()) -> wh_amqp_channel().
open(#wh_amqp_channel{uri=URI}=Channel) ->
    case wh_amqp_connection:get_channel(URI) of
        {'ok', Pid}  ->
            Routines = [fun update_channel_pid/1                        
                        ,fun maybe_monitor_channel/1
                        ,fun maybe_monitor_consumer/1                        
                        ,fun register_return_handler/1
                        ,fun notify_consumer/1
                        ,fun maybe_reestablish/1
                       ],
            NewChannel = lists:foldl(fun open_fold_fun/2
                                     ,Channel#wh_amqp_channel{channel=Pid}
                                     ,Routines),
            lager:debug("opened AMQP channel ~p for ~p on ~s"
                        ,[Pid, NewChannel#wh_amqp_channel.consumer, URI]),
            NewChannel;
        _Else -> Channel
    end.

-spec open_fold_fun(function(), wh_amqp_channel()) -> wh_amqp_channel().
open_fold_fun(Fun, Channel) ->
    try Fun(Channel) of
        #wh_amqp_channel{}=C -> C
    catch
        _:_ -> Channel
    end.

-spec update_channel_pid(wh_amqp_channel()) -> wh_amqp_channel().
update_channel_pid(#wh_amqp_channel{consumer=Consumer, channel=Pid}=Channel) ->
    ets:update_element(?TAB, Consumer, [{#wh_amqp_channel.channel, Pid}]),
    Channel.

-spec register_return_handler(wh_amqp_channel()) -> wh_amqp_channel().
register_return_handler(#wh_amqp_channel{consumer=Consumer
                                         ,channel=Pid}=Channel) ->
    catch amqp_channel:register_return_handler(Pid, Consumer),
    Channel.

-spec notify_consumer(wh_amqp_channel()) -> wh_amqp_channel().
notify_consumer(#wh_amqp_channel{consumer=Consumer
                                 ,reconnecting=Reconnecting}=Channel) ->
    gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', Reconnecting}}),
    Channel.

-spec maybe_reestablish(wh_amqp_channel()) -> wh_amqp_channel().
maybe_reestablish(#wh_amqp_channel{commands=Commands}=Channel) ->
    spawn(fun() -> 
                  reestablish(Channel#wh_amqp_channel{commands=lists:reverse(Commands)
                                                      ,reconnecting='true'
                                                     })
          end),
    Channel#wh_amqp_channel{reconnecting='false'}.

-spec reestablish(wh_amqp_channel()) -> 'ok'.
reestablish(#wh_amqp_channel{commands=[]}) -> 'ok';
reestablish(#wh_amqp_channel{commands=[Command|Commands]}=Channel) ->
    catch wh_amqp_channel:command(Channel#wh_amqp_channel{commands=[]}
                                  ,Command),
    reestablish(Channel#wh_amqp_channel{commands=Commands}).
