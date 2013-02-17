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
-export([update/1
         ,update/2
        ]).
-export([remove/1]).
-export([find/0
         ,find/1
        ]).
-export([get_channel/0]).
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

update(#wh_amqp_channel{}=Channel) ->
    ets:insert(?WH_AMQP_ETS, Channel),
    Channel.

update(#wh_amqp_channel{consumer=undefined}, _) ->
    false;
update(#wh_amqp_channel{consumer=Pid}, Updates) ->
    ets:update_element(?WH_AMQP_ETS, Pid, Updates).

remove(#wh_amqp_channel{consumer=Pid}=Channel) ->
    ets:delete(?WH_AMQP_ETS, Pid),
    Channel.

find() ->
    Pid = wh_amqp_channel:consumer_pid(),
    find(Pid).

find(Pid) ->
    case ets:lookup(?WH_AMQP_ETS, Pid) of
        [#wh_amqp_channel{}=Channel] ->
            Channel;
        _Else -> {error, not_found}
    end.

get_channel() ->
    case find() of
        #wh_amqp_channel{channel=undefined}=Channel ->
            wh_amqp_channel:new(Channel);
        #wh_amqp_channel{}=Channel ->
            Channel;
        {error, not_found} ->
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
    ets:select(?WH_AMQP_ETS, MatchSpec).

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
handle_call({monitor_channel, #wh_amqp_channel{channel=Pid}=C}, _, State) ->
%%    io:format("~p: monitor channel ~p~n", [self(), Pid]),
    {reply, C#wh_amqp_channel{channel_ref=erlang:monitor(process, Pid)}, State};
handle_call({monitor_consumer, #wh_amqp_channel{consumer=Pid}=C}, _, State) ->
%%    io:format("~p: monitor consumer ~p~n", [self(), Pid]),
    {reply, C#wh_amqp_channel{consumer_ref=erlang:monitor(process, Pid)}, State};
handle_call({demonitor_channel, #wh_amqp_channel{channel_ref=Ref}=C}, _, State) ->
%%    io:format("~p: demonitor channel ~p~n", [self(), Ref]),
    erlang:demonitor(Ref, [flush]),
    {reply, C#wh_amqp_channel{channel_ref=undefined}, State};
handle_call({demonitor_consumer, #wh_amqp_channel{consumer_ref=Ref}=C}, _, State) ->
%%    io:format("~p: demonitor consumer ~p~n", [self(), Ref]),
    erlang:demonitor(Ref, [flush]),
    {reply, C#wh_amqp_channel{consumer_ref=undefined}, State};
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
    _ = spawn(fun() ->
                      handle_down_msg(find_reference(Ref), Reason)
              end),
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
handle_down_msg({channel, #wh_amqp_channel{started=Started}=Channel}, _) ->
    io:format("~p: down message for a channel while connected~n", [self()]),
    Duration = wh_util:elapsed_s(Started),
    _ = case Duration < 5 of
            false -> ok;
            true ->
                lager:warning("short lived channel (~ps): ~p", [Duration, Channel])
        end,
    wh_amqp_channel:new(wh_amqp_channel:close(Channel));
handle_down_msg({consumer, #wh_amqp_channel{started=Started}=Channel}, _) ->
    io:format("~p: down message for a consumer~n", [self()]),
    Duration = wh_util:elapsed_s(Started),
    _ = case Duration < 5 of
            false -> ok;
            true ->
                lager:warning("short lived channel (~ps): ~p", [Duration, Channel])
        end,
    wh_amqp_channel:remove(Channel).
