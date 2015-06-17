%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is listener for AMQP events
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_listener).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
         ,handle_authn_req/2
         ,handle_authz_req/2]).

-include("circlemaker.hrl").
-include_lib("rabbitmq_client/include/amqp_client.hrl").

-record(state, {}).

-define(RESPONDERS, [{{?MODULE, 'handle_authn_req'}, [wapi_aaa:req_event_type()]},
                     {{?MODULE, 'handle_authz_req'}, [{<<"authz">>, <<"authz_req">>}]}]).
-define(BINDINGS, [{'aaa', []}
                   ,{'authz', []}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"circlemaker_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(SERVER, ?MODULE).

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle AuthN requests from another process.
%% @end
%%--------------------------------------------------------------------
-spec handle_authn_req(wh_json:object(), wh_proplist()) -> any().
handle_authn_req(JObj, _Props) ->
    lager:debug("cm_listener handled authn request ~p", [JObj]),
    'true' = wapi_aaa:req_v(JObj),
    cm_pool_mgr:do_request(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle AuthZ requests from another process.
%% @end
%%--------------------------------------------------------------------
-spec handle_authz_req(wh_json:object(), wh_proplist()) -> any().
handle_authz_req(JObj, _Props) ->
    'true' = wapi_authz:authz_req_v(JObj),
    lager:debug("cm_listener handled authz request ~p", [JObj]),
    case wh_json:get_value([<<"Custom-Auth-Vars">>, <<"AAA-Authz-Disabled">>], JObj) of
        <<"true">> ->
            lager:debug("Authz disabled, no processing: ~p", [JObj]),
            'ok';
        _ ->
            maybe_processing_authz(JObj)
    end.

maybe_processing_authz(JObj) ->
    % add neccessary set of fields
    case wapi_authz:maybe_determine_account_id(wapi_authz:from_jobj(JObj)) of
        {'ok', AccountId} ->
            lager:debug("Account ID found. Value is ~p", [AccountId]),
            {'ok', AaaDoc} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"aaa">>),
            JObj1 = case {wh_json:get_value(<<"User-Name">>, JObj), wh_json:get_value(<<"User-Password">>, JObj)} of
                        {'undefined', 'undefined'} ->
                            lager:debug("No User-Name and User-Password AVPs in request. "
                                        "Insert it from account fields authz_username and authz_password"),
                            Username = wh_json:get_value(<<"authz_username">>, AaaDoc),
                            Password = wh_json:get_value(<<"authz_password">>, AaaDoc),
                            wh_json:set_values([{<<"Auth-User">>, Username}
                                ,{<<"Auth-Password">>, Password}
                                ,{<<"User-Name">>, Username}
                                ,{<<"User-Password">>, Password}
                                ,{<<"Account-ID">>, AccountId}
                            ], JObj);
                        {Username, Password} ->
                            lager:debug("The User-Name and User-Password AVPs were found in request"),
                            wh_json:set_values([{<<"Auth-User">>, Username}
                                ,{<<"Auth-Password">>, Password}
                                ,{<<"User-Name">>, Username}
                                ,{<<"User-Password">>, Password}
                                ,{<<"Account-ID">>, AccountId}
                            ], JObj)
                    end,
            cm_pool_mgr:do_request(JObj1);
        {'error', Error} ->
            lager:debug("Account ID not found. Error is ~p", [Error]),
            Queue = wh_json:get_value(<<"Response-Queue">>, JObj),
            JObj1 = wh_json:set_value(<<"Is-Authorized">>, <<"false">>, JObj),
            wapi_authz:publish_authz_resp(Queue, JObj1)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([]) -> {ok, State} |
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
