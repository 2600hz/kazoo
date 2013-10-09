%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_local_extension).

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

-include("skel.hrl").

-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'route', []}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [
                     %% Received because of our route binding
                     {{'skel_handlers', 'handle_route_req'}, [{<<"dialplan">>, <<"route_req">>}]}

                     %% Received because of our self binding (route_wins are sent to the route_resp's Server-ID
                     %% which is usually populated with the listener's queue name
                     ,{{'skel_handlers', 'handle_route_win'}, [{<<"dialplan">>, <<"route_win">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
    gen_listener:start_link(?MODULE, [
                                      {'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], []).

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
build_local_extension(NumberProps, JObj, Props) ->
    Number = props:get_value('number', NumberProps),
    AccountId = props:get_value('account_id', NumberProps),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj, get('callid')),
    CIDNum = wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                        ,<<"Emergency-Caller-ID-Number">>
                                       ], JObj),    
    CIDNum = wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                        ,<<"Emergency-Caller-ID-Name">>
                                       ], JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Inception">>, <<"off-net">>}
                    ,{<<"Retain-CID">>, <<"true">>}
                    ,{<<"Global-Resource">>, <<"false">>}
                    ,{<<"Account-ID">>, AccountId}                    
                    ,{<<"Caller-ID-Number">>, CIDNum}
                    ,{<<"Caller-ID-Name">>, CIDName}
                    ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
                    ,{<<"Callee-ID-Name">>, get_account_name(Number, AccountId)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),    
    [{<<"Application-Name">>, <<"execute_extension">>}
     ,{<<"Reset">>, <<"true">>}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Extension">>, Number}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
     | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
    ].

-spec get_account_name(ne_binary(), ne_binary()) -> ne_binary().
get_account_name(Number, AccountId) when is_binary(Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:get_ne_value(<<"name">>, JObj, Number);
        _ -> Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the channel to end or the
%% execute extension to complete.  However, if we receive a rate
%% response then set the CCVs accordingly.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_execute_extension() -> execute_ext_resp().
wait_for_execute_extension() ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} -> hangup_result(JObj);
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"execute_extension">>} -> {'ok', 'execute_extension'};
                _  -> wait_for_execute_extension()
            end;
        _ -> wait_for_execute_extension()
    end.
