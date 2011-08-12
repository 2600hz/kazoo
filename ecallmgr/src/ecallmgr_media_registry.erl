%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Handle registrations of Name/CallID combos for media, creating
%%% temp names to store on the local box.
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_media_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup_media/2, lookup_media/3,
         register_local_media/2, is_local/2]).

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

register_local_media(MediaName, CallId) ->
    gen_server:call(?MODULE, {register_local_media, MediaName, CallId}).

-spec lookup_media/2 :: (MediaName, CallId) -> tuple(ok, binary()) | tuple(error, not_local) when
      MediaName :: binary(),
      CallId :: binary().
lookup_media(MediaName, CallId) ->
    request_media(MediaName, new, CallId).

-spec lookup_media/3 :: (MediaName , Type, CallId) -> tuple(ok, binary()) | tuple(error, not_local) when
      MediaName :: binary(),
      Type :: extant | new,
      CallId :: binary().
lookup_media(MediaName, Type, CallId) ->
    request_media(MediaName, Type, CallId).

-spec(is_local/2 :: (MediaName :: binary(), CallId :: binary()) -> tuple(ok, binary()) | tuple(error, not_local)).
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
handle_call({register_local_media, MediaName, CallId}, {Pid, _Ref}, Dict) ->
    Dict1 = dict:filter(fun({Pid1, CallId1, MediaName1, _}, _) when Pid =:= Pid1 andalso CallId =:= CallId1 andalso MediaName =:= MediaName1 ->
				true;
			   (_, _) -> false
			end, Dict),
    case dict:size(Dict1) =:= 1 andalso dict:to_list(Dict1) of
	false ->
	    link(Pid),
	    Path = binary:replace(generate_local_path(MediaName), <<".wav">>, <<".mp3">>),
	    {ok, RecvSrv} = ecallmgr_shout_sup:start_recv(Path),
	    Url = ecallmgr_shout:get_recv_url(RecvSrv),
	    {reply, Url, dict:store({Pid, CallId, MediaName, RecvSrv}, Path, Dict)};
	[{_, Path}] ->
	    {reply, Path, Dict}
    end;

handle_call({lookup_local, MediaName, CallId}, {FromPid, _Ref}=From, Dict) ->
    ?LOG(CallId, "Lookup local media: ~s", [MediaName]),
    Dict1 = dict:filter(fun({Pid1, CallId1, MediaName1, _}, _) when FromPid =:= Pid1 andalso CallId =:= CallId1 andalso MediaName =:= MediaName1 ->
				true;
			   (_, _) -> false
			end, Dict),
    case dict:size(Dict1) =:= 1 andalso dict:to_list(Dict1) of
        false ->
            {reply, {error, not_local}, Dict};
	[{{_,_,_,RecvSrv},Path}] when is_pid(RecvSrv) ->
	    spawn(fun() ->
			  process_flag(trap_exit, true),
			  link(RecvSrv),
			  link(FromPid),
			  _ = wait_for_fs_media(Path, RecvSrv, FromPid),
			  gen_server:reply(From, {ok, Path})
		  end),
	    {noreply, Dict}
    end;

handle_call({is_local, MediaName, CallId}, {FromPid, _Ref}=From, Dict) ->
    ?LOG(CallId, "Is local: ~s", [MediaName]),
    Dict1 = dict:filter(fun({Pid1, CallId1, MediaName1, _}, _) when FromPid =:= Pid1 andalso CallId =:= CallId1 andalso MediaName =:= MediaName1 ->
				true;
			   (_, _) -> false
			end, Dict),
    case dict:size(Dict1) =:= 1 andalso dict:to_list(Dict1) of
        false ->
            {reply, {error, not_local}, Dict};
	[{{_,_,_,RecvSrv},Path}] when is_pid(RecvSrv) ->
	    spawn(fun() ->
			  process_flag(trap_exit, true),
			  link(RecvSrv),
			  link(FromPid),
			  gen_server:reply(From, wait_for_fs_media(Path, RecvSrv, FromPid))
		  end),
	    {noreply, Dict}
    end;

handle_call(_Request, _From, Dict) ->
    {reply, {error, bad_request}, Dict}.

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
    ?LOG("Pid ~p down, Reason: ~p, cleaning up...", [Pid, _Reason]),
    {noreply, dict:filter(fun({Pid1, _CallId, _Name}, _Value) -> Pid =/= Pid1 end, Dict)};

handle_info({'EXIT', Pid, _Reason}, Dict) ->
    {noreply, dict:filter(fun({Pid1, CallId, _Name, _RecvSrv}, Path) ->
				  case Pid =/= Pid1 of
				      true -> true;
				      false ->
					  ?LOG(CallId, "Pid ~p exited, Reason ~p, cleaning up ~p...", [Pid, _Reason, Path]),
					  _ = file:delete(Path),
					  false
				  end
			  end, Dict)};

handle_info(_Info, Dict) ->
    ?LOG("Unhandled message: ~p", [self(), _Info]),
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
    ok.

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

-spec request_media/3 :: (MediaName, Type, CallID) -> tuple(ok, binary()) | tuple(error, not_local) when
      MediaName :: binary(),
      Type :: extant | new,
      CallID :: binary().
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
    Request = [
                {<<"Media-Name">>, MediaName}
               ,{<<"Stream-Type">>, <<"extant">>}
	       ,{<<"Call-ID">>, CallID}
               | wh_api:default_headers(<<>>, <<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    lookup_remote(MediaName, Request);
lookup_remote(MediaName, new, CallID) ->
    Request = [
                {<<"Media-Name">>, MediaName}
               ,{<<"Stream-Type">>, <<"new">>}
	       ,{<<"Call-ID">>, CallID}
               | wh_api:default_headers(<<>>, <<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    lookup_remote(MediaName, Request).

-spec(lookup_remote/2 :: (MediaName :: binary(), Request :: proplist()) -> tuple('ok', binary()) | tuple('error', 'not_local')).
lookup_remote(MediaName, Request) ->
    try
	{ok, MediaResp} = ecallmgr_amqp_pool:media_req(Request, 1000),
	true = wh_api:media_resp_v(MediaResp),
	MediaName = wh_json:get_value(<<"Media-Name">>, MediaResp),

	{ok, wh_json:get_value(<<"Stream-URL">>, MediaResp, <<>>)}
    catch
	_:B ->
	    {error, B}
    end.

wait_for_fs_media(Path, RecvSrv, FromPid) ->
    case erlang:is_process_alive(RecvSrv) of
	true ->
	    ?LOG("Waiting for ~p to die for ~s", [RecvSrv, Path]),
	    receive
		{'EXIT', RecvSrv, Reason} ->
		    ?LOG("SHOUT srv ~p went down(~p) for ~s", [RecvSrv, Reason, Path]),
		    {ok, Path};
		{'EXIT', FromPid, Reason} ->
		    ?LOG("Caller ~p went down(~p) waiting for ~p(~s)", [FromPid, Reason, RecvSrv, Path]),
		    exit(timeout);
		_Other ->
		    ?LOG("Received unhandled message: ~p", [_Other]),
		    wait_for_fs_media(Path, RecvSrv, FromPid)
	    after ?TIMEOUT_MEDIA_TRANSFER ->
		    ?LOG("Waited long enough for ~p, going down with timeout", [RecvSrv]),
		    exit(timeout)
	    end;
	false ->
	    {ok, Path}
    end.
