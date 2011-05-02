%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Handle registrations of Name/CallID combos for media, creating
%%% temp names to store on the local box.
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_media_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup_media/2, lookup_media/3,
         register_local_media/2, is_local/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [log/2, format_log/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(LOCAL_MEDIA_PATH, "/tmp/").

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

lookup_media(MediaName, CallId) ->
    request_media(MediaName, CallId).

lookup_media(MediaName, Type, CallId) ->
    request_media(MediaName, Type, CallId).

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
    case dict:find({Pid, CallId, MediaName}, Dict) of
	error ->
	    link(Pid),
	    Path = generate_local_path(MediaName),
	    {reply, Path, dict:store({Pid, CallId, MediaName}, Path, Dict)};
	{ok, Path} ->
	    {reply, Path, Dict}
    end;

handle_call({lookup_local, MediaName, CallId}, {Pid, _Ref}, Dict) ->
    case dict:find({Pid, CallId, MediaName}, Dict) of
        error ->
            {reply, {error, not_local}, Dict};        
        {ok, Path} ->
            {reply, {ok, Path}, Dict}
    end;

handle_call({is_local, MediaName, CallId}, {Pid, _Ref}, Dict) ->
    case dict:find({Pid, CallId, MediaName}, Dict) of
        error ->
            {reply, false, Dict};
        {ok, Path} ->
            {reply, Path, Dict}
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
    format_log(info, "MEDIA_REG(~p): Pid ~p down, Reason: ~p, cleaning up...~n", [self(), Pid, _Reason]),
    {noreply, dict:filter(fun({Pid1, _CallId, _Name}, _Value) -> Pid =/= Pid1 end, Dict)};

handle_info({'EXIT', Pid, _Reason}, Dict) ->
    format_log(info, "MEDIA_REG(~p): Pid ~p exited, Reason ~p, cleaning up...~n", [self(), Pid, _Reason]),
    {noreply, dict:filter(fun({Pid1, _CallId, _Name}, _Value) ->
				  format_log(info, "MEDIA_REG.filter P: ~p P1: ~p~n", [Pid, Pid1]),
				  Pid =/= Pid1
			  end, Dict)};

handle_info(_Info, Dict) ->
    format_log(info, "MEDIA_REG(~p): Info Msg: ~p~n", [self(), _Info]),
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
    M = whistle_util:to_binary(MediaName),
    <<?LOCAL_MEDIA_PATH, M/binary>>.

request_media(MediaName, CallId) ->
    request_media(MediaName, <<"new">>, CallId).

request_media(MediaName, Type, CallId) ->
    case gen_server:call(?MODULE, {lookup_local, MediaName, CallId}) of
        {ok, Path} ->
            Path;
        {error, _} ->
            lookup_remote(MediaName, Type)
    end.

lookup_remote(MediaName, StreamType) ->
    Request = [
                {<<"Media-Name">>, MediaName}
               ,{<<"Stream-Type">>, StreamType}
               | whistle_api:default_headers(<<>>, <<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
              ],

    try
	{ok, MediaResp} = ecallmgr_amqp_pool:media_req(Request, 1000),
	true = whistle_api:media_resp_v(MediaResp),
	MediaName = wh_json:get_value(<<"Media-Name">>, MediaResp),

	wh_json:get_value(<<"Stream-URL">>, MediaResp, <<>>)
    catch
	_:B ->
	    {error, B}
    end.
