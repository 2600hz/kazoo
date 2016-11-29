%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(crossbar_search).
-behaviour(gen_listener).

-export([is_local/1]).

-export([table_id/0, table_options/0]).
-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("crossbar.hrl").

-define(POLLING_INTERVAL, 5000).

-type state() :: #{}.

-define(BINDINGS, [{'self', []}
                  ,{'search', [{'restrict_to', ['search']}, 'federate']}
                  ]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).


%%%===================================================================
%%% API
%%%===================================================================
-spec is_local(ne_binary()) -> boolean().
is_local(QID) ->
    Node = kz_util:to_binary(node()),
    case kz_cache:fetch_local(?CACHE_NAME, {'search', QID}) of
        {'ok', Node} -> 'true';
        {'ok', _OtherNode} -> 'false';
        {'error', 'not_found'} ->'false'
    end.

-spec table_id() -> atom().
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() ->
    [bag, public, named_table, {write_concurrency, true} , {read_concurrency, true}].


%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]).

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
-spec init([]) -> {'ok', state(), kz_timeout()}.
init([]) ->
    {'ok', #{node => kz_util:to_binary(node())
            }, ?POLLING_INTERVAL}.

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
    {'reply', {'error', 'not_implemented'}, State, ?POLLING_INTERVAL}.

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
handle_cast({'gen_listener',{'created_queue', Queue}}, State) ->
    {'noreply', State#{queue => Queue}, ?POLLING_INTERVAL};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
    {'noreply', State, ?POLLING_INTERVAL}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #{node := Node}) ->
    case {kz_api:node(JObj) =:= Node, kz_api:event_name(JObj)} of
        {_, <<"register">>} -> kz_util:spawn(fun handle_register/1, [JObj]);
        {'false', <<"request">>} -> kz_util:spawn(fun handle_search/1, [JObj]);
        _ -> lager:debug("search event ~s not for this node", [kz_api:event_name(JObj)])
    end,
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
terminate(_Reason, #{}) ->
    lager:debug("terminating crossbar search : ~p", [_Reason]).

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
handle_register(JObj) ->
    'true' = kapi_search:register_v(JObj),
    QID = kapi_search:query_id(JObj),
    Node = kz_api:node(JObj),
    Props = [{'callback', fun handle_expire/3}
            ,{'expires', ?SECONDS_IN_HOUR}
            ],
    kz_cache:store_local(?CACHE_NAME, {'search', QID}, Node, Props).

-spec handle_expire({'search', ne_binary()}, ne_binary(), atom()) -> 'true'.
handle_expire({'search', QID}, Node, 'expire') ->
    lager:debug("query id ~s from node ~s expired", [QID, Node]),
    ets:delete(table_id(), QID).

handle_search(JObj) ->
    'true' = kapi_search:req_v(JObj),
    handle_search(JObj, is_local(kapi_search:query_id(JObj))).
    
handle_search(JObj, 'false') ->
    lager:debug("query id ~s not handled locally", [kapi_search:query_id(JObj)]);
handle_search(JObj, 'true') ->
    'true' = kapi_search:req_v(JObj),
    QID = kapi_search:query_id(JObj),
    Offset = kapi_search:offset(JObj),
    Quantity = kapi_search:quantity(JObj),
    QOptions = kapi_search:options(JObj),
    AccountId = kz_api:account_id(JObj),
    Module = kapi_search:module(JObj),
    Method = kapi_search:method(JObj),
    Options = [{'quantity', Quantity}
              ,{'query_options', QOptions}
              ,{'offset', Offset}
              ,{'account_id', AccountId}
              ,{'query_id', QID}
              ,{'ets', table_id()}
              ],
    Results = erlang:apply(Module, Method, [Options]),    
    Payload = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              ,{<<"Query-ID">>, QID}
              ,{<<"Results">>, Results}
               | kz_api:default_headers(kz_api:server_id(JObj), ?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun(P) -> kapi_search:publish_resp(kz_api:server_id(JObj), P) end,
    kz_amqp_worker:cast(Payload, Publisher).
