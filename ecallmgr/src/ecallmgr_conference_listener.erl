%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0
         ,handle_command/2
         ,handle_search_req/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(RESPONDERS, [{{?MODULE, 'handle_command'}
                      ,[{<<"conference">>, <<"command">>}]
                     }
                     ,{{?MODULE, 'handle_search_req'}
                       ,[{<<"conference">>, <<"search_req">>}]
                      }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', ['command', 'discovery']}]}]).
-define(QUEUE_NAME, <<"ecallmgr_conference_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{'responders', ?RESPONDERS}
                             ,{'bindings', ?BINDINGS}
                             ,{'queue_name', ?QUEUE_NAME}
                             ,{'queue_options', ?QUEUE_OPTIONS}
                             ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

-spec handle_command(wh_json:object(), wh_proplist()) -> any().
handle_command(JObj, _Props) ->
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    Focus = get_conference_focus(ConferenceId),
    ecallmgr_conference_command:exec(Focus, ConferenceId, JObj).

-spec handle_search_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    lager:debug("received search request for conference id ~s", [ConferenceId]),
    case ecallmgr_fs_conference:fetch_full(ConferenceId) of
        {'error', 'not_found'} ->
            lager:debug("sending error search response, conference not found"),
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                     ,{<<"Error-Message">>, <<"Conference ", ConferenceId/binary, " not found">>}
                     ,{<<"Request">>, JObj}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Error);
        {'ok', Conference} ->
            lager:debug("sending affirmative search response for conference ~s", [ConferenceId]),
            Resp = wh_json:set_values([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                      ], Conference),
            wapi_conference:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    put('callid', ?LOG_SYSTEM_ID),
    lager:debug("starting new ecallmgr conference listener process"),
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
    {'reply', 'ok', State}.

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
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all amqp messages
%%
%% @spec handle_event(JObj, Props) -> {reply, Props} |
%%                                    ignore
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
    lager:debug("ecallmgr conference listener terminating: ~p", [_Reason]).

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
%% TODO: Flush cache on conference end
-spec get_conference_focus(ne_binary()) -> atom().
get_conference_focus(ConferenceId) ->
    case ecallmgr_fs_conference:node(ConferenceId) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', Focus} -> Focus
    end.
