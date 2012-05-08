%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Handle registrations of Name/CallID combos for media, creating
%%% temp names to store on the local box.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_media_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup_media/2, lookup_media/3,
         register_local_media/2, register_local_media/3, is_local/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(LOCAL_MEDIA_PATH, "/tmp/").
-define(TIMEOUT_MEDIA_TRANSFER, 240000). %% if we can't transfer in four minutes, something's wrong

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_local_media/2 :: (ne_binary(), ne_binary()) -> ne_binary().
-spec register_local_media/3 :: (ne_binary(), ne_binary(), 'path' | 'url') -> ne_binary().
register_local_media(MediaName, CallId) ->
    register_local_media(MediaName, CallId, path).

register_local_media(<<"local_stream://", FSPath/binary>>, _, _) ->
    lager:debug("local media on media server at ~s", [FSPath]),
    FSPath;
register_local_media(MediaName, CallId, Version) when Version =:= path orelse Version =:= url ->
    gen_server:call(?MODULE, {register_local_media, MediaName, CallId, Version}).

-spec lookup_media/2 :: (ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', 'not_local'}.
-spec lookup_media/3 :: (ne_binary() , 'extant' | 'new', ne_binary()) -> {'ok', ne_binary()} | {'error', 'not_local'}.
lookup_media(MediaName, CallId) ->
    request_media(MediaName, new, CallId).
lookup_media(MediaName, Type, CallId) ->
    request_media(MediaName, Type, CallId).

-spec is_local/2 :: (ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', 'not_local'}.
is_local(MediaName, CallId) ->
    gen_server:call(?MODULE, {is_local, MediaName, CallId}).

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
    process_flag(trap_exit, true),
    _ = put(callid, ?LOG_SYSTEM_ID),
    {ok, dict:new()}.

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
handle_call({register_local_media, MediaName, CallId, Version}, {Pid, _Ref}, Dict) ->
    Old = put(callid, CallId),
    Dict1 = dict:filter(fun({Pid1, CallId1, MediaName1, _}, _) when Pid =:= Pid1 andalso
                                                                    CallId =:= CallId1 andalso
                                                                    MediaName =:= MediaName1 ->
                                true;
                           (_, _) -> false
                        end, Dict),
    case dict:size(Dict1) =:= 1 andalso dict:to_list(Dict1) of
        false ->
            link(Pid),
            Path = binary:replace(generate_local_path(MediaName), <<".wav">>, <<".mp3">>),
            {ok, RecvSrv} = ecallmgr_shout_sup:start_recv(Path),
            lager:debug("recv shout server on ~p for media ~s", [RecvSrv, Path]),
            Url = ecallmgr_shout:get_recv_url(RecvSrv),
            lager:debug("recv at ~s", [Url]),
            _ = put(callid, Old),
            {reply, Url, dict:store({Pid, CallId, MediaName, RecvSrv}, {Path, Url}, Dict), hibernate};
        [{_, {Path, Url}}] ->
            case Version of
                path -> _ = put(callid, Old), {reply, Path, Dict};
                url -> _ = put(callid, Old), {reply, Url, Dict}
            end
    end;

handle_call({lookup_local, MediaName, CallId}, {FromPid, _Ref}=From, Dict) ->
    Old = put(callid, CallId),
    lager:debug("lookup local media ~s for ~s", [MediaName, CallId]),
    Dict1 = dict:filter(fun({Pid1, CallId1, MediaName1, _}, _) when FromPid =:= Pid1 andalso CallId =:= CallId1 andalso MediaName =:= MediaName1 ->
                                true;
                           (_, _) -> false
                        end, Dict),
    case dict:size(Dict1) =:= 1 andalso dict:to_list(Dict1) of
        false ->
            _ = put(callid, Old),
            {reply, {error, not_local}, Dict};
        [{{_,_,_,RecvSrv},{Path,_}}] when is_pid(RecvSrv) ->
            spawn(fun() ->
                          process_flag(trap_exit, true),
                          link(RecvSrv),
                          link(FromPid),
                          _ = wait_for_fs_media(Path, RecvSrv, FromPid),
                          gen_server:reply(From, {ok, Path})
                  end),
            _ = put(callid, Old),
            {noreply, Dict}
    end;

handle_call({is_local, MediaName, CallId}, {FromPid, _Ref}=From, Dict) ->
    Old = put(callid, CallId),
    lager:debug("is ~s for ~s", [MediaName, CallId]),
    Dict1 = dict:filter(fun({Pid1, CallId1, MediaName1, _}, _) when FromPid =:= Pid1 andalso CallId =:= CallId1 andalso MediaName =:= MediaName1 ->
                                true;
                           (_, _) -> false
                        end, Dict),
    case dict:size(Dict1) =:= 1 andalso dict:to_list(Dict1) of
        false ->
            _ = put(callid, Old),
            {reply, {error, not_local}, Dict};
        [{{_,_,_,RecvSrv},{Path,_}}] when is_pid(RecvSrv) ->
            spawn(fun() ->
                          process_flag(trap_exit, true),
                          link(RecvSrv),
                          link(FromPid),
                          gen_server:reply(From, wait_for_fs_media(Path, RecvSrv, FromPid))
                  end),
            _ = put(callid, Old),
            {noreply, Dict}
    end.

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
handle_cast(_Msg, Dict) ->
    {noreply, Dict}.

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
handle_info({'DOWN', _Ref, process, Pid, _Reason}, Dict) ->
    lager:debug("Pid ~p down, Reason: ~p, cleaning up...", [Pid, _Reason]),
    {noreply, dict:filter(fun({Pid1, _CallId, _Name}, _Value) -> Pid =/= Pid1 end, Dict), hibernate};

handle_info({'EXIT', Pid, _Reason}, Dict) ->
    {noreply, dict:filter(fun({Pid1, _CallId, _Name, _RecvSrv}, Path) ->
                                  case Pid =/= Pid1 of
                                      true -> true;
                                      false ->
                                          lager:debug("pid ~p exited, reason ~p, cleaning up ~p...", [Pid, _Reason, Path]),
                                          _ = file:delete(Path),
                                          false
                                  end
                          end, Dict), hibernate};

handle_info(_Info, Dict) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, Dict}.

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
    lager:debug("media_registry terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_local_path(MediaName) ->
    M = wh_util:to_binary(MediaName),
    <<?LOCAL_MEDIA_PATH, M/binary>>.

-spec request_media/3 :: (ne_binary(), 'extant' | 'new', ne_binary()) -> {'ok', ne_binary()} | {'error', 'not_local'}.
request_media(MediaName, Type, CallID) ->
    case gen_server:call(?MODULE, {lookup_local, MediaName, CallID}, infinity) of
        {ok, Path} ->
            {ok, Srv} = ecallmgr_shout_sup:start_srv(Path),
            Url = ecallmgr_shout:get_srv_url(Srv),
            {ok, Url};
        {error, _} ->
            lookup_remote(MediaName, Type, CallID)
    end.

lookup_remote(MediaName, extant, CallID) ->
    Request = [{<<"Media-Name">>, MediaName}
               ,{<<"Stream-Type">>, <<"extant">>}
               ,{<<"Call-ID">>, CallID}
               ,{<<"Msg-ID">>, wh_util:to_binary(wh_util:current_tstamp())}
               | wh_api:default_headers(<<>>, <<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    lookup_remote(MediaName, Request);
lookup_remote(MediaName, new, CallID) ->
    Request = [{<<"Media-Name">>, MediaName}
               ,{<<"Stream-Type">>, <<"new">>}
               ,{<<"Call-ID">>, CallID}
               ,{<<"Msg-ID">>, wh_util:to_binary(wh_util:current_tstamp())}
               | wh_api:default_headers(<<>>, <<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    lookup_remote(MediaName, Request).

-spec lookup_remote/2 :: (ne_binary(), proplist()) -> {'ok', ne_binary()} | {'error', 'not_local'}.
lookup_remote(MediaName, Request) ->
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,Request
                                  ,fun wapi_media:publish_req/1
                                  ,fun wapi_media:resp_v/1),
    case ReqResp of 
        {error, _R}=E -> 
            lager:debug("media lookup for '~s' failed: ~p", [MediaName, _R]),
            E;
        {ok, MediaResp} ->
            MediaName = wh_json:get_value(<<"Media-Name">>, MediaResp),
            {ok, wh_json:get_value(<<"Stream-URL">>, MediaResp, <<>>)}
    end.

wait_for_fs_media({Path,_}, RecvSrv, FromPid) ->
    wait_for_fs_media(Path, RecvSrv, FromPid);
wait_for_fs_media(Path, RecvSrv, FromPid) ->
    case erlang:is_process_alive(RecvSrv) of
        true ->
            lager:debug("Waiting for ~p to die for ~s", [RecvSrv, Path]),
            receive
                {'EXIT', RecvSrv, Reason} ->
                    lager:debug("SHOUT srv ~p went down(~p) for ~s", [RecvSrv, Reason, Path]),
                    {ok, Path};
                {'EXIT', FromPid, Reason} ->
                    lager:debug("Caller ~p went down(~p) waiting for ~p(~s)", [FromPid, Reason, RecvSrv, Path]),
                    exit(timeout);
                _Other ->
                    lager:debug("Received unhandled message: ~p", [_Other]),
                    wait_for_fs_media(Path, RecvSrv, FromPid)
            after ?TIMEOUT_MEDIA_TRANSFER ->
                    lager:debug("Waited long enough for ~p, going down with timeout", [RecvSrv]),
                    exit(timeout)
            end;
        false ->
            {ok, Path}
    end.
