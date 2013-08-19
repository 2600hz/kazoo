%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% @doc
%%% Master process to gather information send by all kazoo nodes.
%%% It listens to targeted/statistics AMPQ messages, and is accessed by
%%% whistle_snmp for SNMP queries.
%%%
%%% Created :  3 Jul 2013 by Stephen Gibberd <stephen.gibberd@2600hz.com>
%%%-------------------------------------------------------------------
-module(stats_listener).
-behaviour(gen_listener).

%% API
-export([start_link/0, start/0, init/1,handle_call/3,handle_cast/2
	 ,handle_info/2,terminate/2,code_change/3]).

-record(state,{}).

-define(SERVER,?MODULE).
-define(RESPONDERS, [{stats_handler,[{<<"*">>, <<"*">>}]}]).
-define(BINDINGS, [{self,[]}]).
-define(QUEUE_NAME, <<"statistics">>).
%%%-define(QUEUE_OPTIONS, [{exclusive, true}]).
%%%-define(CONSUME_OPTIONS, [{exclusive, true}]).


start() ->
    application:start(stats_master).

start_link() ->
    lager:debug("starting new stats proc"),
    [wh_util:ensure_started(A) || A <- [sasl, whistle_amqp]],
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                              ,{queue_name, ?QUEUE_NAME}
			      %%,{queue_options, ?QUEUE_OPTIONS}
                              %%,{consume_options, ?CONSUME_OPTIONS}
                             ]
                            ,[]).

init([]) ->
    {ok,#state{}}.

handle_call(_Request,_From,State) ->
    {reply,{error,'not_implemented'},State}.

handle_cast(_Request,State) ->
    {noreply,State}.

handle_info(_Request,State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).
