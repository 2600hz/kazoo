%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hangups_query_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_query/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("hangups.hrl").
-include_lib("folsom/include/folsom.hrl").

-define(RESPONDERS, [{{?MODULE, 'handle_query'}
                      ,[{<<"hangups">>, <<"query_req">>}]
                     }
                    ]).
-define(BINDINGS, [{'hangups', []}]).
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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

handle_query(JObj, _Props) ->
    'true' = wapi_hangups:query_req_v(JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    HangupCause = wh_json:get_value(<<"Hangup-Cause">>, JObj),
    N = hangups_listener:meter_name(AccountId, HangupCause),
    handle_query(JObj, N, wh_json:is_true(<<"Raw-Data">>, JObj)).

handle_query(JObj, N, 'true') ->
    raw_resp(JObj, folsom_metrics_meter:get_value(N));
handle_query(JObj, N, 'false') ->
    meter_resp(JObj, folsom_metrics_meter:get_values(N)).

raw_resp(JObj, #meter{one = OneMin
                      ,five = FiveMin
                      ,fifteen = FifteenMin
                      ,day = OneDay
                      ,count = Count
                      ,start_time = StartTime
                     }) ->
    Resp = [{<<"one">>, ewma_to_json(OneMin)}
            ,{<<"five">>, ewma_to_json(FiveMin)}
            ,{<<"fifteen">>, ewma_to_json(FifteenMin)}
            ,{<<"day">>, ewma_to_json(OneDay)}
            ,{<<"count">>, Count}
            ,{<<"start_time">>, StartTime}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

ewma_to_json(#ewma{alpha=Alpha
                   ,interval=Interval
                   ,initialized=Init
                   ,rate=Rate
                   ,total=Total
                  }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"alpha">>, Alpha}
         ,{<<"interval">>, Interval}
         ,{<<"initialized">>, Init}
         ,{<<"rate">>, Rate}
         ,{<<"total">>, Total}
        ])).

meter_resp(JObj, Values) ->
    Vs = [{wh_util:to_binary(K), V}
          || {K, V} <- Values,
             K =/= 'acceleration'
         ],
    Resp = get_accel(props:get_value('acceleration', Values))
        ++ wh_api:default_header(?APP_NAME, ?APP_VERSION)
        ++ Vs,
    publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

publish_resp(Queue, Resp) ->
    whapps_util:amqp_pool_send(Resp, fun(API) ->
                                             wapi_hangups:publish_query_resp(Queue, API)
                                     end).

get_accel(AccelVs) ->
    [{wh_util:to_binary(K), V}
     || {K, V} <- AccelVs
    ].

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
    lager:debug("started hangups query listener"),
    {'ok', 'ok'}.

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
    lager:debug("hangups listener ~p termination", [_Reason]),
    'ok'.

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
