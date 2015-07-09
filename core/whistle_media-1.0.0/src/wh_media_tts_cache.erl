%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_tts_cache).

-behaviour(gen_server).

%% API
-export([start_link/2
         ,single/1
         ,continuous/1
         ,stop/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("whistle_media.hrl").

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(TIMEOUT_LIFETIME
        ,whapps_config:get_integer(?CONFIG_CAT, <<"tts_cache">>, ?MILLISECONDS_IN_HOUR)
       ).
-define(TIMEOUT_MESSAGE, {'$wh_media_tts_cache', 'tts_timeout'}).

-record(state, {
          text :: ne_binary()
          ,contents = <<>> :: binary()
          ,status :: 'streaming' | 'ready'
          ,ibrowse_req_id :: ibrowse_req_id()
          ,reqs :: [{pid(), reference()},...] | []
          ,meta :: wh_json:object()
          ,timer_ref :: reference()
          ,id :: ne_binary() %% used in publishing doc_deleted
         }).

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
-spec start_link(ne_binary(), wh_json:object()) -> startlink_ret().
start_link(Text, JObj) ->
    gen_server:start_link(?MODULE, [Text, JObj], []).

-spec single(pid()) -> {wh_json:object(), ne_binary()}.
single(Srv) -> gen_server:call(Srv, 'single').

continuous(Srv) -> gen_server:call(Srv, 'continuous').

-spec stop(pid()) -> 'ok'.
stop(Srv) ->
    gen_server:cast(Srv, 'stop').

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
init([Text, JObj]) ->
    wh_util:put_callid(wh_util:binary_md5(Text)),

    Voice = list_to_binary([wh_json:get_value(<<"Voice">>, JObj, <<"female">>), "/"
                            ,get_language(wh_json:get_value(<<"Language">>, JObj, <<"en-us">>))
                           ]),

    Format = wh_json:get_value(<<"Format">>, JObj, <<"wav">>),
    Engine = wh_json:get_value(<<"Engine">>, JObj),

    {'ok', ReqID} = whapps_speech:create(Engine, Text, Voice, Format, [{'stream_to', self()}]),

    MediaName = wh_util:binary_md5(Text),
    lager:debug("text '~s' has id '~s'", [Text, MediaName]),

    Meta = wh_json:from_list([{<<"content_type">>, wh_mime_types:from_extension(Format)}
                              ,{<<"media_name">>, MediaName}
                             ]),

    {'ok', #state{ibrowse_req_id = ReqID
                  ,status = 'streaming'
                  ,meta = Meta
                  ,contents = <<>>
                  ,reqs = []
                  ,timer_ref = start_timer()
                  ,id = MediaName
                 }}.

-spec get_language(ne_binary()) -> ne_binary().
get_language(<<"en">>) -> <<"en-us">>;
get_language(<<L:2/binary>>) -> <<L/binary, "-", L/binary>>;
get_language(Language) -> Language.

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
handle_call('single', _From, #state{meta=Meta
                                    ,contents=Contents
                                    ,status=ready
                                    ,timer_ref=TRef
                                   }=State) ->
    %% doesn't currently check whether we're still streaming in from the DB
    lager:debug("returning media contents"),
    _ = stop_timer(TRef),
    {'reply', {Meta, Contents}, State#state{timer_ref=start_timer()}};
handle_call('single', From, #state{reqs=Reqs
                                   ,status='streaming'
                                  }=State) ->
    lager:debug("file not ready for ~p, queueing", [From]),
    {'noreply', State#state{reqs=[From | Reqs]}};
handle_call('continuous', _From, #state{}=State) ->
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
handle_cast('stop', State) ->
    lager:debug("asked to stop, going down"),
    {'stop', 'normal', State};
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
handle_info({'timeout', TRef, ?TIMEOUT_MESSAGE}, #state{timer_ref=TRef}=State) ->
    lager:debug("timeout expired, going down"),
    {'stop', 'normal', State};

handle_info({'ibrowse_async_headers', ReqID, "200", Hdrs}, #state{ibrowse_req_id=ReqID
                                                                  ,timer_ref=TRef
                                                                 }=State) ->
    lager:debug("successfully retrieved audio file for tts"),
    _ = stop_timer(TRef),
    {'noreply', State#state{meta=wh_json:normalize(wh_json:from_list(kv_to_bin(Hdrs)))
                            ,timer_ref=start_timer()
                           }};

handle_info({'ibrowse_async_headers', ReqID, "202", Hdrs}, #state{ibrowse_req_id=ReqID}=State) ->
    lager:debug("202 recv for tts request, awaiting further instructions"),
    {'noreply', State#state{meta=wh_json:normalize(wh_json:from_list(kv_to_bin(Hdrs)))}};

handle_info({'ibrowse_async_response', ReqID, Bin}, #state{ibrowse_req_id=ReqID
                                                           ,meta=Meta
                                                           ,contents=Contents
                                                           ,timer_ref=TRef
                                                          }=State) ->
    _ = stop_timer(TRef),
    case wh_json:get_value(<<"content_type">>, Meta) of
        <<"audio/", _/binary>> ->
            lager:debug("recv ~b bytes", [byte_size(Bin)]),
            {'noreply', State#state{contents = <<Contents/binary, Bin/binary>>
                                        ,timer_ref=start_timer()
                                   }};
        <<"application/json">> ->
            lager:debug("JSON response: ~s", [Bin]),
            {'noreply', State, 'hibernate'}
    end;

handle_info({'ibrowse_async_headers', ReqID, Bin}, #state{ibrowse_req_id=ReqID
                                                          ,contents=Contents
                                                         }=State) ->
    lager:debug("recv ~b bytes", [byte_size(Bin)]),
    {'noreply', State#state{contents = <<Contents/binary, Bin/binary>>}};

handle_info({'ibrowse_async_response_end', ReqID}, #state{ibrowse_req_id=ReqID
                                                          ,contents = <<>>
                                                          ,timer_ref=TRef
                                                         }=State) ->
    _ = stop_timer(TRef),
    lager:debug("no tts contents were received, going down"),
    {'stop', 'normal', State};
handle_info({'ibrowse_async_response_end', ReqID}, #state{ibrowse_req_id=ReqID
                                                          ,contents=Contents
                                                          ,meta=Meta
                                                          ,reqs=Reqs
                                                          ,timer_ref=TRef
                                                         }=State) ->
    _ = stop_timer(TRef),
    Res = {Meta, Contents},
    _ = [gen_server:reply(From, Res) || From <- Reqs],

    lager:debug("finished receiving file contents"),
    {'noreply', State#state{status=ready
                            ,timer_ref=start_timer()
                           }
     ,'hibernate'
    };
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

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
terminate(_Reason, #state{id=Id}) ->
    publish_doc_update(Id),
    lager:debug("media tts ~s going down: ~p", [Id, _Reason]).

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
-spec kv_to_bin(wh_proplist()) -> wh_proplist().
kv_to_bin(L) ->
    [{wh_util:to_binary(K), wh_util:to_binary(V)} || {K,V} <- L].

-spec start_timer() -> reference().
start_timer() ->
    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_MESSAGE).

-spec stop_timer(reference() | any()) -> 'ok'.
stop_timer(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    'ok';
stop_timer(_) -> 'ok'.

-spec publish_doc_update(ne_binary()) -> 'ok'.
publish_doc_update(Id) ->
    API =
        [{<<"ID">>, Id}
         ,{<<"Type">>, Type = <<"media">>}
         ,{<<"Database">>, Db = <<"tts">>}
         ,{<<"Rev">>, <<"0">>}
         | wh_api:default_headers(<<"configuration">>, <<"doc_deleted">>, ?APP_NAME, ?APP_VERSION)
        ],
    wh_amqp_worker:cast(API
                        ,fun(P) -> wapi_conf:publish_doc_update('deleted', Db, Type, Id, P) end
                       ).
