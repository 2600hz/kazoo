%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_record_call_listener).

-behaviour(gen_listener).

-export([start_link/2
         ,handle_call_event/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("../callflow.hrl").

-record(state, {url                        :: ne_binary()
                ,format                    :: ne_binary()
                ,media_name                :: ne_binary()
                ,call                      :: whapps_call:call()
                ,record_on_answer          :: ne_binary()
                ,time_limit                :: pos_integer()
                ,store_attempted = 'false' :: boolean()
                ,record_started = 'false'  :: boolean()
                ,channel_status_ref        :: reference() | 'undefined'
                ,time_limit_ref            :: reference() | 'undefined'
               }).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallId), [{'call', [{'callid', CallId}
                                     ,{'restrict_to', [<<"RECORD_STOP">>
                                                       ,<<"CHANNEL_DESTROY">>
                                                       ,<<"CHANNEL_EXECUTE_COMPLETE">>
                                                       ,<<"CHANNEL_BRIDGE">>
                                                       ,<<"RECORD_START">>
                                                      ]}
                                    ]}
                           ,{'self', []}
                          ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                      ,[{<<"*">>, <<"*">>}]
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
-spec start_link(whapps_call:call(), wh_json:object()) -> startlink_ret().
start_link(Call, Data) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS(whapps_call:call_id(Call))}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], [Call, Data]).

handle_call_event(JObj, Props) ->
    wh_util:put_callid(JObj),
    lager:debug("cf_record_call_listener:handle_call_event() JObj ~p", [JObj]),
    lager:debug("cf_record_call_listener:handle_call_event() Props ~p", [Props]),
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
          lager:debug("channel bridge maybe start recording"),
          gen_listener:cast(props:get_value('server', Props), 'maybe_start_recording');
      {<<"call_event">>, <<"RECORD_START">>} ->
        lager:debug("record_start event recv'd"),
        gen_listener:cast(props:get_value('server', Props), 'record_start');
        {<<"call_event">>, <<"RECORD_STOP">>} ->
            lager:debug("record_stop event recv'd"),
            gen_listener:cast(props:get_value('server', Props), 'store_recording');
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(props:get_value('server', Props), 'stop_call');
        {<<"call_detail">>, <<"cdr">>} ->
            gen_listener:cast(props:get_value('server', Props), 'stop_call');
        {<<"call_event">>, <<"channel_status_resp">>} ->
            gen_listener:cast(props:get_value('server', Props), {'channel_status', wh_json:get_value(<<"Status">>, JObj)});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case {wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Application-Response">>, JObj)} of
                {<<"store">>, <<"failure">>} ->
                    gen_listener:cast(props:get_value('server', Props), 'store_failed');
                {<<"store">>, <<"success">>} ->
                    gen_listener:cast(props:get_value('server', Props), 'store_succeeded');
                {_App, _Res} -> lager:debug("ignore exec complete: ~s: ~s", [_App, _Res])
            end;
        {<<"error">>,<<"dialplan">>} ->
            case wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj) of
                <<"store">> ->
                    gen_listener:cast(props:get_value('server', Props), 'store_failed');
                _App ->
                    lager:debug("ignore dialplan error: ~s", [_App])
            end;
        {_, _Evt} -> lager:debug("ignore event ~p", [_Evt])
    end.

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
init([Call, Data]) ->
    whapps_call:put_callid(Call),
    lager:info("starting event listener for cf_record_call"),

    Format = cf_record_call:get_format(wh_json:get_value(<<"format">>, Data)),

    {'ok', #state{url=cf_record_call:get_url(Data)
                  ,format=Format
                  ,media_name=cf_record_call:get_media_name(whapps_call:call_id(Call), Format)
                  ,call=Call
                  ,time_limit = cf_record_call:get_timelimit(Data)
                  ,record_on_answer = cf_record_call:get_record_on_answer(Data)
                 }}.

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
handle_cast('record_start', #state{time_limit=TimeLimit}=State) ->
  {'noreply', State#state{record_started='true'
                          ,channel_status_ref=start_check_call_timer()
                          ,time_limit_ref=start_time_limit_timer(TimeLimit)
  }};

handle_cast('maybe_start_recording', #state{record_started='true'}=State) ->
  lager:debug("we've already starting a recording for this call"),
  {'noreply', State};
handle_cast('maybe_start_recording', #state{record_started='false'
                                            ,call=Call
                                            ,media_name=MediaName
                                            ,time_limit=TimeLimit
                                           }=State) ->
  cf_record_call:start_recording(Call, MediaName, TimeLimit),
  {'noreply', State};
handle_cast('maybe_start_recording', State) ->
  {'noreply', State};

handle_cast('stop_call', #state{store_attempted='true'}=State) ->
    lager:debug("we've already sent a store attempt, waiting to hear back"),
    {'noreply', State};
handle_cast('stop_call', #state{media_name=MediaName
                                ,format=Format
                                ,url=Url
                                ,call=Call
                               }=State) ->
    lager:debug("recv stop_call event"),
    cf_record_call:save_recording(Call, MediaName, Format, cf_record_call:should_store_recording(Url)),
    lager:debug("sent store command"),
    {'noreply', State};
handle_cast('store_recording', #state{media_name=MediaName
                                      ,format=Format
                                      ,url=Url
                                      ,call=Call
                                     }=State) ->
    lager:debug("recv store_recording event"),
    cf_record_call:save_recording(Call, MediaName, Format, cf_record_call:should_store_recording(Url)),
    {'noreply', State#state{store_attempted='true',record_started='false'}};

handle_cast({'channel_status',<<"active">>}, #state{channel_status_ref='undefined'}=State) ->
    {'noreply', State#state{channel_status_ref=start_check_call_timer()}};
handle_cast({'channel_status', <<"terminated">>}, #state{channel_status_ref='undefined'
                                                         ,store_attempted='false'
                                                        }=State) ->
    lager:debug("channel terminated, we're done here"),
    {'stop', 'normal', State};
handle_cast({'channel_status', <<"terminated">>}, #state{channel_status_ref='undefined'
                                                         ,store_attempted='true'
                                                        }=State) ->
    lager:debug("channel terminated, but we've sent a store attempt, so hold on"),
    {'noreply', State};
handle_cast({'channel_status', _S}, #state{channel_status_ref='undefined'}=State) ->
    Ref = start_check_call_timer(),
    lager:debug("unknown channel status respoonse: ~s, starting timer back up: ~p", [_S, Ref]),
    {'noreply', State#state{channel_status_ref=Ref}};
handle_cast({'channel_status', _S}, State) ->
    {'noreply', State};

handle_cast('store_succeeded', State) ->
    lager:debug("store succeeded"),
    {'stop', 'normal', State};
handle_cast('store_failed', State) ->
    lager:debug("store failed"),
    {'stop', 'normal', State};

handle_cast({'gen_listener',{'created_queue',Queue}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=whapps_call:set_controller_queue(Queue, Call)}};
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
handle_info('stop_recording', #state{media_name=MediaName
                                     ,call=Call
                                    }=State) ->
    lager:debug("recv stop_recording event"),
    whapps_call_command:record_call(MediaName, <<"stop">>, Call),
    {'noreply', State};
handle_info({'check_call', Ref}, #state{call=Call
                                        ,channel_status_ref=Ref
                                       }=State) ->
    whapps_call_command:channel_status(Call),
    {'noreply', State#state{channel_status_ref='undefined'}};
handle_info(_Info, #state{channel_status_ref=_Ref}=State) ->
    lager:debug("unhandled message(~p): ~p", [_Ref, _Info]),
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
-spec start_check_call_timer() -> reference().
start_check_call_timer() ->
    CheckRef = erlang:make_ref(),
    {'ok', _} = timer:send_after(5000, self(), {'check_call', CheckRef}),
    CheckRef.

-spec start_time_limit_timer(wh_json:object()) -> reference().
start_time_limit_timer(TimeLimit) ->
    TLRef = erlang:make_ref(),
    {'ok', _} = timer:send_after((TimeLimit+10) * 1000, self(), {'stop_recording', TLRef}),
    TLRef.
