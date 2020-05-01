%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-define(SERVER, ?MODULE).

-define(TIMEOUT_LIFETIME, kazoo_tts:cache_time_ms()).

-define(TIMEOUT_MESSAGE, {'$kz_media_tts_cache', 'tts_timeout'}).

-record(state, {text :: kz_term:api_ne_binary()
               ,contents = <<>> :: binary()
               ,status :: 'streaming' | 'ready'
               ,kz_http_req_id :: kz_http:req_id()
               ,reqs :: [{pid(), reference()}]
               ,meta :: kz_json:object()
               ,engine = kz_term:ne_binary()
               ,engine_data = 'undefined' :: any()
               ,timer_ref :: reference()
               ,id :: kz_term:ne_binary() %% used in publishing doc_deleted
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary(), kz_json:object()) -> kz_types:startlink_ret().
start_link(Id, JObj) ->
    gen_server:start_link(?SERVER, [Id, JObj], []).

-spec single(pid()) -> {kz_json:object(), kz_term:ne_binary()}.
single(Srv) -> gen_server:call(Srv, 'single').

-spec continuous(pid()) -> {kz_json:object(), kz_term:ne_binary()}.
continuous(Srv) -> gen_server:call(Srv, 'continuous').

-spec stop(pid()) -> 'ok'.
stop(Srv) ->
    gen_server:cast(Srv, 'stop').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([Id, JObj]) ->
    kz_log:put_callid(Id),

    Voice = list_to_binary([kz_json:get_value(<<"Voice">>, JObj, kazoo_tts:default_voice()), "/"
                           ,get_language(kz_json:get_value(<<"Language">>, JObj, kazoo_tts:default_language()))
                           ]),

    Text = kz_json:get_value(<<"Text">>, JObj),
    Format = kz_json:get_value(<<"Format">>, JObj, <<"wav">>),
    Engine = kz_json:get_value(<<"Engine">>, JObj),

    {'async', ReqID, EngineData} = kazoo_tts:create(Engine, Text, Voice, Format, [{'receiver', self()}]),

    lager:debug("text '~s' has id '~s'", [Text, Id]),

    Meta = kz_json:from_list([{<<"content_type">>, kz_mime:from_extension(Format)}
                             ,{<<"media_name">>, Id}
                             ]),

    {'ok', #state{kz_http_req_id = ReqID
                 ,status = 'streaming'
                 ,meta = Meta
                 ,engine = Engine
                 ,engine_data = EngineData
                 ,contents = <<>>
                 ,reqs = []
                 ,timer_ref = start_timer()
                 ,id = Id
                 }}.

-spec get_language(kz_term:ne_binary()) -> kz_term:ne_binary().
get_language(<<"en">>) -> <<"en-us">>;
get_language(<<L:2/binary>>) -> <<L/binary, "-", L/binary>>;
get_language(Language) -> Language.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('single', _From, #state{meta=Meta
                                   ,contents=Contents
                                   ,status=ready
                                   ,timer_ref=TRef
                                   }=State) ->
    %% doesn't currently check whether we're still streaming in from the DB
    lager:debug("returning media contents ~p", [kz_util:pretty_print_bytes(byte_size(Contents))]),
    _ = stop_timer(TRef),
    {'reply', {Meta, Contents}, State#state{timer_ref=start_timer()}};
handle_call('single', From, #state{reqs=Reqs
                                  ,status='streaming'
                                  }=State) ->
    lager:debug("file not ready for ~p, queuing", [From]),
    {'noreply', State#state{reqs=[From | Reqs]}};
handle_call('continuous', _From, #state{}=State) ->
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("asked to stop, going down"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', TRef, ?TIMEOUT_MESSAGE}, #state{timer_ref=TRef}=State) ->
    lager:debug("timeout expired, going down"),
    {'stop', 'normal', State};

handle_info({'http', {ReqID, 'stream_start', Hdrs}}, #state{kz_http_req_id=ReqID
                                                           ,timer_ref=TRef
                                                           }=State) ->
    lager:debug("start retrieving audio file for tts: ~p", [Hdrs]),
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
        <<"audio/", _/binary>>=_CT ->
            lager:debug("adding ~p bytes of ~s", [byte_size(Bin), _CT]),
            {'noreply', State#state{contents = <<Contents/binary, Bin/binary>>
                                   ,timer_ref=start_timer()
                                   }};
        <<"application/json; charset=UTF-8">> ->
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
                                                                  ,engine_data='undefined'
                                                                  ,reqs=Reqs
                                                                  ,timer_ref=TRef
                                                                  }=State) ->
    _ = stop_timer(TRef),
    Res = {Meta, Contents},
    _ = [gen_server:reply(From, Res) || From <- Reqs],

    lager:debug("finished receiving file contents: ~p", [kz_util:pretty_print_bytes(byte_size(Contents))]),
    {'noreply', State#state{status=ready
                           ,timer_ref=start_timer()
                           }
    ,'hibernate'
    };
handle_info({'http', {ReqID, 'stream_end', _FinalHeaders}}, #state{kz_http_req_id=ReqID
                                                                  ,contents=Contents
                                                                  ,meta=Meta
                                                                  ,engine=Engine
                                                                  ,engine_data=EngineData
                                                                  ,reqs=Reqs
                                                                  ,timer_ref=TRef
                                                                  }=State) ->
    _ = stop_timer(TRef),
    {BinaryContents, NewMeta} = kazoo_tts:decode(Engine, Contents, Meta, EngineData),
    Res = {NewMeta, BinaryContents},
    _ = [gen_server:reply(From, Res) || From <- Reqs],

    lager:debug("finished receiving file contents: ~p", [kz_util:pretty_print_bytes(byte_size(Contents))]),
    {'noreply', State#state{status=ready
                           ,timer_ref=start_timer()
                           ,contents=BinaryContents
                           ,meta=NewMeta
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
    log_error(Error, Contents),
    {'stop', 'normal', State};

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

-spec log_error(any(), binary()) -> 'ok'.
log_error({'failed_connect',[{'to_address',{_Server, _Port}},{'inet',['inet'],'econnrefused'}]}, _) ->
    lager:error("server ~s:~p refusing connections", [_Server, _Port]);
log_error(_Error, _Contents) ->
    lager:info("recv error ~p : collected: ~p", [_Error, _Contents]).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{id=Id, reqs=Reqs}) ->
    publish_doc_update(Id),
    _ = [gen_server:reply(From, {'error', 'shutdown'}) || From <- Reqs],
    lager:debug("media tts ~s going down: ~p", [Id, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec kv_to_bin(kz_term:proplist()) -> kz_term:proplist().
kv_to_bin(L) ->
    [{kz_term:to_binary(K), kz_term:to_binary(V)} || {K,V} <- L].

-spec start_timer() -> reference().
start_timer() ->
    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_MESSAGE).

-spec stop_timer(reference()) -> 'ok'.
stop_timer(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    'ok'.

-spec publish_doc_update(kz_term:ne_binary()) -> 'ok'.
publish_doc_update(Id) ->
    DbId = kz_binary:md5(Id),
    API =
        [{<<"ID">>, DbId}
        ,{<<"Type">>, Type = <<"media">>}
        ,{<<"Database">>, Db = <<"tts">>}
        ,{<<"Rev">>, <<"0">>}
         | kz_api:default_headers(<<"configuration">>, ?DOC_DELETED, ?APP_NAME, ?APP_VERSION)
        ],
    kz_amqp_worker:cast(API
                       ,fun(P) -> kapi_conf:publish_doc_update('deleted', Db, Type, Id, P) end
                       ).
