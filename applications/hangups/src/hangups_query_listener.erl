%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
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
        ,meter_resp/1
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

-record(state, {}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

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

-spec handle_query(kz_json:object(), kz_proplist()) -> any().
-spec handle_query(kz_json:object(), ne_binary(), boolean()) -> any().
handle_query(JObj, _Props) ->
    'true' = kapi_hangups:query_req_v(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    HangupCause = kz_json:get_value(<<"Hangup-Cause">>, JObj),
    N = hangups_util:meter_name(HangupCause, AccountId),

    handle_query(JObj, N, kz_json:is_true(<<"Raw-Data">>, JObj)).

handle_query(_JObj, _N, 'true') ->
    lager:error("who is using this ?");
handle_query(JObj, N, 'false') ->
    lager:debug("finding meter stats for '~p'", [N]),
    publish_resp(JObj, meter_resp(N)).

-spec meter_resp(ne_binary()) -> kz_proplist().
-spec meter_resp(ne_binary(), kz_proplist()) -> kz_proplist().
meter_resp(<<"*">>) ->
    [{<<"meters">>, [kz_json:from_list(meter_resp(Name))
                     || {Name, _Info} <- folsom_metrics:get_metrics_info()
                    ]
     }
    ];
meter_resp(N) ->
    meter_resp(N, folsom_metrics_meter:get_values(N)).

meter_resp(_, []) -> [];
meter_resp(N, [_|_]=Values) ->
    Vs = [{kz_term:to_binary(K), V}
          || {K, V} <- Values,
             K =/= 'acceleration'
         ],
    props:filter_undefined(
      [{<<"hangup_cause">>, hangups_util:meter_hangup_cause(N)}
      ,{<<"account_id">>, hangups_util:meter_account_id(N)}
       | get_accel(props:get_value('acceleration', Values))
      ]
      ++ Vs
     ).

-spec publish_resp(kz_json:object(), kz_proplist()) -> 'ok'.
publish_resp(JObj, Resp) ->
    Queue = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),

    PublishFun = fun(API) ->
                         publish_to(Queue, API)
                 end,
    kz_amqp_worker:cast([{<<"Msg-ID">>, MsgId} | Resp]
                       ,PublishFun
                       ).

-spec publish_to(ne_binary(), kz_proplist()) -> 'ok'.
publish_to(Queue, API) ->
    kapi_hangups:publish_query_resp(Queue
                                   ,kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++ API
                                   ).

-spec get_accel(kz_proplist()) -> kz_proplist().
get_accel(AccelVs) ->
    [{kz_term:to_binary(K), V}
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
-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:debug("started hangups query listener"),
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
    lager:debug("unhandled cast: ~p", [_Msg]),
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
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
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
    lager:debug("hangups listener ~p termination", [_Reason]).

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
