%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0
        ,handle_printer_start/2
        ,handle_printer_stop/2
        ,start_all_printers/0
        ]).

%% gen_listener callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).


-include("fax.hrl").

-define(BINDINGS, [{'xmpp', [{'restrict_to', ['start']}]}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_printer_start'}
                      ,[{<<"xmpp_event">>, <<"start">>}]
                     }
                     ,{{?MODULE, 'handle_printer_stop'}
                       ,[{<<"xmpp_event">>, <<"stop">>}]
                      }
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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

-spec handle_printer_start(wh_json:object(), wh_proplist()) -> sup_startchild_ret().
handle_printer_start(JObj, _Props) ->
    'true' = wapi_xmpp:event_v(JObj),
    PrinterId = wh_json:get_value(<<"Application-Data">>, JObj),
    fax_xmpp_sup:start_printer(PrinterId).

-spec handle_printer_stop(wh_json:object(), wh_proplist()) -> 'ok'.
handle_printer_stop(JObj, _Props) ->
    'true' = wapi_xmpp:event_v(JObj),
    PrinterId = wh_json:get_value(<<"Application-Data">>, JObj),
    fax_xmpp_sup:stop_printer(PrinterId).

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
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    _ = wh_util:spawn(?MODULE, 'start_all_printers', []),
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("fax listener terminating: ~p", [_Reason]).

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

start_all_printers() ->
    {'ok', Results} = couch_mgr:get_results(?WH_FAXES_DB, <<"faxbox/cloud">>),
    [ send_start_printer(Id, Jid)
       || {Id, Jid, <<"claimed">>}
              <- [{wh_doc:id(Result)
                   ,wh_json:get_value([<<"value">>,<<"xmpp_jid">>], Result)
                   ,wh_json:get_value([<<"value">>,<<"state">>], Result)
                  }
                  || Result <- Results
                 ]
    ].

-spec send_start_printer(ne_binary(), ne_binary()) -> _.
send_start_printer(PrinterId, JID) ->
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"start">>}
                 ,{<<"Application-Name">>, <<"fax">>}
                 ,{<<"Application-Event">>, <<"init">>}
                 ,{<<"Application-Data">>, PrinterId}
                 ,{<<"JID">>, JID}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wh_amqp_worker:cast(Payload, fun wapi_xmpp:publish_event/1).
