%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receives reports from the freeSWITCH socket and forwards them to
%%% the queue
%%% @end
%%% Created : 27 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(rscrpt_reporter).

-include("../include/amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-import(rscrpt_logger, [log/2, format_log/3]).

%% API
-export([start_link/0, send_report/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {channel, ticket, tag, exchange}).

-define(SERVER, ?MODULE).
-define(EXCHANGE, <<"resource">>).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_report(Rpt) ->
    gen_server:call(?MODULE, {report, Rpt}).

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
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    process_flag(trap_exit, true),

    Exchange = #'exchange.declare'{
      ticket = Ticket,
      exchange = ?EXCHANGE,
      type = <<"fanout">>,
      passive = false,
      durable = false,
      auto_delete=false,
      internal = false,
      nowait = false,
      arguments = []
     },
    amqp_channel:call(Channel, Exchange),

    %% If the registration was sucessful, then consumer will be notified
    receive
        #'basic.consume_ok'{consumer_tag = Tag} -> ok
    end,

    {ok, #state{channel = Channel, ticket = Ticket, tag = Tag, exchange=Exchange}}.

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
handle_call({report, Rpt}, _From, State) ->
    Box = rscrpt_fsbox:get_box_update(),
    Msg = lists:concat([Box, Rpt]),
    format_log(info, "reporter: Send Msg: ~p~n", [Msg]),

    send_msg(Msg, State),

    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
    ok.

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
send_msg(Msg, #state{ticket=Ticket,exchange=Exchange,channel=Channel}) ->
    %% Create a basic publish command
    BasicPublish = #'basic.publish'{
      ticket = Ticket,
      exchange = Exchange,
      mandatory = false,
      immediate = false
     },

    %% Add the message to the publish, converting to binary
    AmqpMsg = #'amqp_msg'{
      payload = term_to_binary(Msg)
     },

    %% execute the publish command
    format_log(info, "~p amqp_broadcast_dispatcher publish to ~p: ~p~n", [self(), BasicPublish, AmqpMsg]),
    amqp_channel:call(Channel, BasicPublish, AmqpMsg).
