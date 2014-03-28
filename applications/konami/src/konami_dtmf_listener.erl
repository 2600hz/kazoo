%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_dtmf_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,add_call_binding/1
         ,rm_call_binding/1
         ,handle_destroy/2
         ,handle_dtmf/2
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("konami.hrl").

-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, []).
-define(RESPONDERS, [{{?MODULE, 'handle_dtmf'}
                      ,[{<<"call_event">>, <<"DTMF">>}]
                     }
                     ,{{?MODULE, 'handle_destroy'}
                       ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DYN_BINDINGS(CallId), {'call', [{'restrict_to', ['DTMF', 'CHANNEL_DESTROY']}
                                        ,{'callid', CallId}
                                       ]}).

-define(KONAMI_REG(CallId), {'p', 'l', {'dtmf', CallId}}).

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
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                             ]
                            ,[]
                           ).

-spec add_call_binding(api_binary() | whapps_call:call()) -> 'ok'.
add_call_binding('undefined') -> 'ok';
add_call_binding(CallId) when is_binary(CallId) ->
    lager:debug("add binding for call ~s", [CallId]),
    gproc:reg(?KONAMI_REG(CallId)),
    gen_listener:add_binding(?MODULE, ?DYN_BINDINGS(CallId));
add_call_binding(Call) ->
    add_call_binding(whapps_call:call_id_direct(Call)).

-spec rm_call_binding(api_binary() | whapps_call:call()) -> 'ok'.
rm_call_binding('undefined') -> 'ok';
rm_call_binding(CallId) when is_binary(CallId) ->
    lager:debug("rm binding for call ~s", [CallId]),
    gen_listener:rm_binding(?MODULE, ?DYN_BINDINGS(CallId));
rm_call_binding(Call) ->
    rm_call_binding(whapps_call:call_id_direct(Call)).

-spec handle_dtmf(wh_json:object(), wh_proplist()) -> any().
handle_dtmf(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("recv DTMF ~s", [DTMF]),
    [konami_code_fsm:dtmf(FSM, CallId, DTMF) || FSM <- gproc:lookup_pids(?KONAMI_REG(CallId))].

-spec handle_destroy(wh_json:object(), wh_proplist()) -> 'ok'.
handle_destroy(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    rm_call_binding(wh_json:get_value(<<"Call-ID">>, JObj)).

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
