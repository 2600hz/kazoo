%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is listener for
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_listener).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
    , handle_call/3
    , handle_cast/2
    , handle_info/2
    , handle_event/2
    , terminate/2
    , code_change/3
    , handle_authz_req/3]).

-include("circlemaker.hrl").
-include_lib("whistle/src/wh_json.hrl").
-include_lib("rabbitmq_client/include/amqp_client.hrl").

-record(state, {}).

-define(RESPONDERS, [{{'cm_listener', 'handle_authz_req'}
                        ,[{<<"authz">>, <<"authz_req">>}]}]).
-define(BINDINGS, [{'authz', []}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"circlemaker_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(SERVER, ?MODULE).

-define(SYNC_REQ_HEADERS_ON_AUTHZ, [<<"Account-ID">>, <<"Agent-ID">>]).
-define(SYNC_REQ_VALUES_ON_AUTHZ, [{<<"Event-Category">>, <<"agent">>}
    ,{<<"Event-Name">>, <<"sync_req">>}
]).
-define(SYNC_REQ_TYPES_ON_AUTHZ, []).

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
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'bindings', ?BINDINGS}
                                                          ,{'responders', ?RESPONDERS}
                                                          ,{'queue_name', ?QUEUE_NAME}
                                                          ,{'queue_options', ?QUEUE_OPTIONS}
                                                          ,{'consume_options', ?CONSUME_OPTIONS}
                                                         ], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle requests from another process.
%% Processes can request for next actions:
%% 1. authz
%% 2. accounting start/stop
%% 3. accounting CDR
%% @end
%%--------------------------------------------------------------------
-spec handle_authz_req(wh_json:object(), wh_proplist(), gen_listener:basic_deliver()) -> any().
handle_authz_req(JObj, _Props, #'basic.deliver'{'routing_key' = Key}) ->
    io:format("handle_authz_req() handled.~n"),
    case binary:split(Key, <<".">>, ['global']) of
        [_, ?APP_NAME, <<"auth">>] ->
            cm_radius:add_request_authz(JObj, wh_api:validate(wh_json:to_proplist(JObj),
                ?SYNC_REQ_HEADERS_ON_AUTHZ, ?SYNC_REQ_VALUES_ON_AUTHZ, ?SYNC_REQ_TYPES_ON_AUTHZ));
        [_, ?APP_NAME, <<"start">>] ->
            cm_radius:add_request_start(JObj, wh_api:validate(wh_json:to_proplist(JObj),
                ?SYNC_REQ_HEADERS_ON_AUTHZ, ?SYNC_REQ_VALUES_ON_AUTHZ, ?SYNC_REQ_TYPES_ON_AUTHZ));
        [_, ?APP_NAME, <<"stop">>] ->
            cm_radius:add_request_stop(JObj, wh_api:validate(wh_json:to_proplist(JObj),
                ?SYNC_REQ_HEADERS_ON_AUTHZ, ?SYNC_REQ_VALUES_ON_AUTHZ, ?SYNC_REQ_TYPES_ON_AUTHZ));
        _ ->
            'ok'
    end.


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
