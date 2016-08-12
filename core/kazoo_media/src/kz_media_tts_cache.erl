%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_tts_cache).

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

-include("kazoo_media.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

-define(SERVER, ?MODULE).

-define(MOD_CONFIG_CAT, <<"speech">>).

-define(TIMEOUT_LIFETIME
       ,kapps_config:get_integer(?CONFIG_CAT, <<"tts_cache">>, ?MILLISECONDS_IN_HOUR)
       ).
-define(TIMEOUT_MESSAGE, {'$kz_media_tts_cache', 'tts_timeout'}).

-record(state, {
          text :: ne_binary()
               ,contents = <<>> :: binary()
               ,status :: 'streaming' | 'ready'
               ,kz_http_req_id :: kz_http:req_id()
               ,reqs :: [{pid(), reference()}]
               ,meta :: kz_json:object()
               ,timer_ref :: reference()
               ,id :: ne_binary() %% used in publishing doc_deleted
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(ne_binary(), kz_json:object()) -> startlink_ret().
start_link(Text, JObj) ->
    gen_server:start_link(?SERVER, [Text, JObj], []).

-spec single(pid()) -> {kz_json:object(), ne_binary()}.
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
    kz_util:put_callid(kz_util:binary_md5(Text)),

    Voice = list_to_binary([kz_json:get_value(<<"Voice">>, JObj, <<"female">>), "/"
                           ,get_language(kz_json:get_value(<<"Language">>, JObj, <<"en-us">>))
                           ]),

    Format = kz_json:get_value(<<"Format">>, JObj, <<"wav">>),
    Engine = kz_json:get_value(<<"Engine">>, JObj),

    {'ok', ReqID} = kapps_speech:create(Engine, Text, Voice, Format, [{'receiver', self()}]),

    MediaName = kz_util:binary_md5(Text),
    lager:debug("text '~s' has id '~s'", [Text, MediaName]),

    Meta = kz_json:from_list([{<<"content_type">>, kz_mime:from_extension(Format)}
                             ,{<<"media_name">>, MediaName}
                             ]),

    {'ok', #state{kz_http_req_id = ReqID
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'timeout', TRef, ?TIMEOUT_MESSAGE}, #state{timer_ref=TRef}=State) ->
    lager:debug("timeout expired, going down"),
    {'stop', 'normal', State};

handle_info({'http', {ReqID, 'stream_start', Hdrs}}, #state{kz_http_req_id=ReqID
                                                           ,timer_ref=TRef
                                                           }=State) ->
    lager:debug("start retrieving audio file for tts"),
    _ = stop_timer(TRef),
    {'noreply', State#state{meta=kz_json:normalize(kz_json:from_list(kv_to_bin(Hdrs)))
                           ,timer_ref=start_timer()
                           }};

handle_info({'http', {ReqID, 'stream', Bin}}, #state{kz_http_req_id=ReqID
                                                    ,meta=Meta
                                                    ,contents=Contents
                                                    ,timer_ref=TRef
                                                    }=State) ->
    _ = stop_timer(TRef),
    case kz_json:get_value(<<"content_type">>, Meta) of
        <<"audio/", _/binary>> ->
            lager:debug("recv ~b bytes", [byte_size(Bin)]),
            {'noreply', State#state{contents = <<Contents/binary, Bin/binary>>
                                   ,timer_ref=start_timer()
                                   }};
        <<"application/json">> ->
            lager:debug("JSON response: ~s", [Bin]),
            {'noreply', State, 'hibernate'}
    end;

handle_info({'http', {ReqID, 'stream_end', _FinalHeaders}}, #state{kz_http_req_id=ReqID
                                                                  ,contents = <<>>
                                                                  ,timer_ref=TRef
                                                                  }=State) ->
    _ = stop_timer(TRef),
    lager:debug("no tts contents were received, going down"),
    {'stop', 'normal', State};
handle_info({'http', {ReqID, 'stream_end', _FinalHeaders}}, #state{kz_http_req_id=ReqID
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

handle_info({'http', {ReqID, {{_, _StatusCode, _}, Hdrs, Contents}}}, #state{kz_http_req_id=ReqID
                                                                            ,reqs=Reqs
                                                                            ,timer_ref=TRef
                                                                            }=State) ->
    _ = stop_timer(TRef),
    Res = {kz_json:normalize(kz_json:from_list(kv_to_bin(Hdrs))), Contents},

    lager:debug("finished receiving file contents with status code ~p", [_StatusCode]),
    _ = [gen_server:reply(From, Res) || From <- Reqs],

    lager:debug("finished receiving file contents"),
    {'noreply', State#state{status=ready
                           ,timer_ref=start_timer()
                           }
    ,'hibernate'
    };

handle_info({'http', {ReqID, {'error', Error}}}, #state{kz_http_req_id=ReqID
                                                       ,contents=Contents
                                                       ,timer_ref=TRef
                                                       }=State) ->
    _ = stop_timer(TRef),
    lager:info("recv error ~p : collected: ~p", [Error, Contents]),
    {'stop', 'normal', State};

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
-spec terminate(any(), state()) -> 'ok'.
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
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec kv_to_bin(kz_proplist()) -> kz_proplist().
kv_to_bin(L) ->
    [{kz_util:to_binary(K), kz_util:to_binary(V)} || {K,V} <- L].

-spec start_timer() -> reference().
start_timer() ->
    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_MESSAGE).

-spec stop_timer(reference() | _) -> 'ok'.
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
         | kz_api:default_headers(<<"configuration">>, ?DOC_DELETED, ?APP_NAME, ?APP_VERSION)
        ],
    kz_amqp_worker:cast(API
                       ,fun(P) -> kapi_conf:publish_doc_update('deleted', Db, Type, Id, P) end
                       ).
