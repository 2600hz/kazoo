%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_call_control).

-behaviour(gen_listener).

-export([start_link/2]).

-export([control_q/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/4
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(RESPONDERS, []).

-define(BINDINGS, [{'dialplan', []}
                  ,{'self', []}
                  ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(SERVER, ?MODULE).


-type state() :: map().

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node, Options) ->
    gen_listener:start_link(?MODULE
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, Options]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {'ok', State} |
%%                     {'ok', State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([atom() | kz_proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs amqp event listener for ~s", [Node]),
    {'ok', #{node => Node, options => Options}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('control_q', _From, #{queue := Q}=State) ->
    {'reply', {'ok', Q, kz_amqp_channel:consumer_channel()}, State};
handle_call('control_q', _From, State) ->
    {'reply', {'error', 'no_queue'}, State};
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
-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'gen_listener',{'is_consuming', 'false'}}, State) ->
    {'noreply', State#{active => 'false'}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue', Q}}, State) ->
    {'noreply', State#{queue => Q}};
handle_cast(_Cast, State) ->
    lager:debug("unhandled cast: ~p", [_Cast]),
    {'noreply', State, 'hibernate'}.

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
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), basic_deliver(), amqp_basic(), state()) -> 'ignore'.
handle_event(JObj, _Deliver, Basic, _State) ->
    Props = [{'basic', Basic}],
    case kz_api:event_name(JObj) of
        <<"route_resp">> -> handle_route_resp(JObj, Props);
        ?ROUTE_WINNER_EVENT -> handle_route_winner(JObj, Props);
        Event -> handle_call_control(Event, JObj, Props)
    end.

handle_call_control(Event, JObj, _Props) ->
    kz_util:put_callid(JObj),
    case kz_api:deliver_to(JObj) of
        'undefined' -> lager:debug_unsafe("received event without deliver_to : ~s", [kz_json:encode(JObj, ['pretty'])]);
        Pid -> lager:debug("delivering ~s to ~s", [Event, Pid]),
               kz_term:to_pid(Pid) ! {'call_control', JObj}
    end,
    'ignore'.

handle_route_resp(JObj, Props) ->
    Pid = kz_term:to_pid(kz_api:reply_to(JObj)),
    Pid ! {'route_resp', JObj, Props},
    'ignore'.

handle_route_winner(JObj, Props) ->
    Pid = kz_term:to_pid(kz_api:reply_to(JObj)),
    Pid ! {'route_winner', JObj, Props},
    'ignore'.

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
    lager:debug("fs amqp authn termination: ~p", [ _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec control_q(pid()) -> {'ok', ne_binary(), pid()} | {'error', 'no_queue'}.
control_q(Pid) ->
    gen_listener:call(Pid, 'control_q').
