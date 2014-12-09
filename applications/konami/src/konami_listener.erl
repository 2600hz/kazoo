%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_metaflow/2
         ,handle_channel_create/2
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
-define(BINDINGS, [{'dialplan', ['metaflow']}]).
-define(RESPONDERS, [{{?MODULE, 'handle_metaflow'}
                      ,[{<<"call">>, <<"command">>}]
                     }
                     ,{{?MODULE, 'handle_channel_create'}
                       ,[{<<"call_event">>, <<"CHANNEL_CREATE">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<"konami_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

-spec handle_metaflow(wh_json:object(), wh_proplist()) -> no_return().
handle_metaflow(JObj, Props) ->
    'true' = wapi_dialplan:metaflow_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    whapps_call:put_callid(Call),

    try konami_code_fsm:start_fsm(
          whapps_call:kvs_store('consumer_pid', props:get_value('server', Props), Call)
          ,JObj
         )
    of
        _ -> 'ok'
    catch
        'exit':'normal' -> 'ok';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to run FSM: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    after
        konami_tracker:untrack()
    end.

-spec handle_channel_create(wh_json:object(), wh_proplist()) -> 'ok'.
handle_channel_create(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),

    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    maybe_start_metaflows(wh_json:get_value(<<"Account-ID">>, CCVs)
                          ,wh_json:get_value(<<"Authorizing-Type">>, CCVs)
                          ,wh_json:get_value(<<"Authorizing-ID">>, CCVs)
                          ,wh_json:get_value(<<"Owner-ID">>, CCVs)
                          ,wh_json:get_value(<<"Call-ID">>, JObj)
                         ).

-spec maybe_start_metaflows(api_binary(), api_binary(), api_binary(), api_binary(), api_binary()) -> 'ok'.
maybe_start_metaflows('undefined', _AuthorizingType, _AuthorizingId, _OwnerId, _CallId) ->
    lager:debug("no account id for ~s(~s) owned by ~s", [_AuthorizingId, _AuthorizingType, _OwnerId]);
maybe_start_metaflows(AccountId, <<"device">>, DeviceId, OwnerId, CallId) ->
    maybe_start_device_metaflows(AccountId, DeviceId, CallId),
    maybe_start_user_metaflows(AccountId, OwnerId, CallId);
maybe_start_metaflows(AccountId, 'undefined', DeviceId, OwnerId, CallId) ->
    maybe_start_device_metaflows(AccountId, DeviceId, CallId),
    maybe_start_user_metaflows(AccountId, OwnerId, CallId);
maybe_start_metaflows(_AccountId, _AuthorizingType, _AuthorizingId, _OwnerId, _CallId) ->
    lager:debug("unhandled channel for account ~s: ~s(~s) owned by ~s"
                ,[_AccountId, _AuthorizingId, _AuthorizingType, _OwnerId]).

-spec maybe_start_device_metaflows(ne_binary(), api_binary(), api_binary()) -> 'ok'.
maybe_start_device_metaflows(_AccountId, 'undefined', _CallId) -> 'ok';
maybe_start_device_metaflows(AccountId, DeviceId, CallId) ->
    {'ok', Endpoint} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                                ,DeviceId
                                               ),
    maybe_start_metaflows(AccountId, CallId, wh_json:get_value(<<"metaflows">>, Endpoint)).

-spec maybe_start_user_metaflows(ne_binary(), api_binary(), api_binary()) -> 'ok'.
maybe_start_user_metaflows(_AccountId, 'undefined', _CallId) -> 'ok';
maybe_start_user_metaflows(AccountId, UserId, CallId) ->
    {'ok', User} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                            ,UserId
                                           ),
    maybe_start_metaflows(AccountId, CallId, wh_json:get_value(<<"metaflows">>, User)).

-spec maybe_start_metaflows(ne_binary(), api_binary(), api_object()) -> 'ok'.
maybe_start_metaflows(_AccountId, _CallId, 'undefined') -> 'ok';
maybe_start_metaflows(_AccountId, _CallId, Metaflows) ->
    lager:debug("starting ~p", [Metaflows]).

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
