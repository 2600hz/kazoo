%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(doodle_inbound_listener).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("doodle.hrl").

-record(state, {}).

-define(DEFAULT_EXCHANGE, <<"sms">>).
-define(DEFAULT_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_EXCHANGE_OPTIONS, [{'passive', 'true'}]).
-define(DEFAULT_BROKER, <<"amqp://user:pass@server.com:5672/babble">>).
-define(QUEUE_NAME, <<"smsc_inbound_queue_", (wh_util:rand_hex_binary(6))/binary>>).

-define(DOODLE_INBOUND_QUEUE, whapps_config:get_ne_binary(?CONFIG_CAT, <<"inbound_queue_name">>, ?QUEUE_NAME)).
-define(DOODLE_INBOUND_BROKER, whapps_config:get_ne_binary(?CONFIG_CAT, <<"inbound_broker">>, ?DEFAULT_BROKER)).
-define(DOODLE_INBOUND_EXCHANGE, whapps_config:get_ne_binary(?CONFIG_CAT, <<"inbound_exchange">>, ?DEFAULT_EXCHANGE)).
-define(DOODLE_INBOUND_EXCHANGE_TYPE, whapps_config:get_ne_binary(?CONFIG_CAT, <<"inbound_exchange_type">>, ?DEFAULT_EXCHANGE_TYPE)).
-define(DOODLE_INBOUND_EXCHANGE_OPTIONS,  whapps_config:get(?CONFIG_CAT, <<"inbound_exchange_options">>, ?DEFAULT_EXCHANGE_OPTIONS)).


-define(BINDINGS(Ex), [{'sms', [{'exchange', Ex}
                                ,{'restrict_to', ['inbound']}
                               ]}
                       ,{'self', []}
                      ]).
-define(RESPONDERS, [{'doodle_inbound_handler',[{<<"message">>, <<"inbound">>}]}]).

-define(QUEUE_OPTIONS, [{'exclusive', 'false'}
                        ,{'durable', 'true'}
                        ,{'auto_delete', 'false'}
                        ,{'arguments', [{<<"x-message-ttl">>, 'infinity'}
                                        ,{<<"x-max-length">>, 'infinity'}
                                       ]}
                       ]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}
                          ,{'no_ack', 'false'}
                         ]).

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
    Broker = ?DOODLE_INBOUND_BROKER,
    Exchange = ?DOODLE_INBOUND_EXCHANGE,
    Type = ?DOODLE_INBOUND_EXCHANGE_TYPE,
    QUEUE = ?DOODLE_INBOUND_QUEUE,
    Options = [{'passive', 'true'}],
    Exchanges = [{Exchange, Type, Options}],
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS(Exchange)}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', QUEUE}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                              ,{'declare_exchanges', Exchanges}
                              ,{'broker', Broker}
                             ]
                            ,[]
                           ).

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
    {'ok', #state{}}.

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
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

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
handle_info({'send_outbound', Payload}, State) ->
    wapi_sms:publish_outbound(Payload),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

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
