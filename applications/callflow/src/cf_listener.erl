%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Listener for route requests that can be fulfilled by Callflows.
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_listener).
-behaviour(gen_listener).

-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/3
        ,terminate/2
        ,code_change/3
        ]).

-include("callflow.hrl").

-type state() :: map().

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{'cf_route_req', [{<<"dialplan">>, <<"route_req">>}]}
                    ]).
-define(BINDINGS, [{'route', [{'types', ?RESOURCE_TYPES_HANDLED}
                             ,{'restrict_to', ['account']}
                             ]
                   }
                  ,{'self', []}
                  ,{'call', []}
                  ,{'dialplan', []}
                  ]).
-define(QUEUE_NAME(I), <<"callflow_route_", I/binary>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_types:ne_binary()) -> kz_types:startlink_ret().
start_link(Instance) ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME(Instance)}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ,{'auto_ack', ?USE_AUTO_ACK}
                                     ], #{queue => ?QUEUE_NAME(Instance)}).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(map()) -> {'ok', state()}.
init(State) ->
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'forward', Msg}, #{queue := Queue}=State) ->
    handle_msg(Msg, [{'queue', Queue}], State),
    {'noreply', State};

handle_info(_Info, State) ->
    lager:info("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling AMQP event objects
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, Props, State) ->
    Msg = kz_api:kapi_delivery_message(JObj, Props),
    handle_msg(Msg, Props, State).


-spec handle_msg(term(), kz_term:proplist(), state()) -> gen_listener:handle_event_return().
handle_msg({{<<"callevt">>, _, _}, _, JObj}=Msg, _Props, _State) ->
    CallId = kz_call_event:call_id(JObj),
    gproc:send({'p', 'l', {'cf_exe', CallId}}, {'kapi', Msg}),
    'ignore';

handle_msg({_, {'dialplan', 'route_req'}, JObj}, Props, _State) ->
    cf_exe_sup:new(JObj, [{channel, kz_amqp_channel:consumer_channel()} | Props], fun cf_route_req:handle_req/2),
    'ignore';

handle_msg({_, {'callflow', 'resume'}, JObj}, Props, _State) ->
    cf_exe_sup:new(JObj, [{channel, kz_amqp_channel:consumer_channel()} | Props], fun cf_route_resume:handle_req/2),
    'ignore';

handle_msg(_Msg, _Props, _State) ->
    lager:debug("UNHANDLED MSG => ~p", [_Msg]),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("callflow listener ~p termination", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
