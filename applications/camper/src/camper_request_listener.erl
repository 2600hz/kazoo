%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(camper_request_listener).
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

-include("camper.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

%% gen_listener callbacks
-export([handle_camper_req/3]).

-define(RESPONDERS, [{{?MODULE, 'handle_camper_req'}
                     ,[{<<"delegate">>, <<"job">>}]
                     }
                    ]).
-define(BINDINGS, [{'delegate', [{'app_name', ?APP_NAME}
                                ,{'route_key', <<"*">>}
                                ]}
                  ]).
-define(QUEUE_NAME, <<"camper_requests">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

%%%===================================================================
%%% gen_listener callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_camper_req(kz_json:object(), kz_proplist(), gen_listener:basic_deliver()) -> any().
handle_camper_req(JObj, _Props, #'basic.deliver'{'routing_key' = Key}) ->
    case binary:split(Key, <<".">>, ['global']) of
        [_, ?APP_NAME, <<"offnet">>] ->
            Msg = kz_json:get_value(<<"Delegate-Message">>, JObj),
            camper_offnet_handler:add_request(Msg);
        [_, ?APP_NAME, <<"onnet">>] ->
            Msg = kz_json:get_value(<<"Delegate-Message">>, JObj),
            camper_onnet_handler:add_request(Msg)
    end.

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
-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:info("started request listener"),
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'gen_listener',{'created_queue',_QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:info("unhandled cast: ~p", [_Msg]),
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:info("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("request listener ~p termination", [_Reason]),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
