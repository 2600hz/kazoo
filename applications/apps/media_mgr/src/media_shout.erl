%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(media_shout).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("media.hrl").

-define(SERVER, ?MODULE).
-define(APP_VSN, <<"0.2.0">>).
-define(MEDIA_DB, "media_files").

-record(state, {
	  media_file = #media_file{} :: #media_file{}
	  ,media_id = <<>> :: binary()
	  ,port = undefined :: undefined | port()
	  ,socket = undefined :: undefined | port()
	  ,send_to = [] :: list(binary()) | []
	  ,stream_type = single :: single | continuous
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
start_link(MediaID, To, Type, Port) ->
    gen_server:start_link(?MODULE, [MediaID, To, Type, Port], []).

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
init([MediaID, To, Type, Port]) ->
    logger:format_log(info, "Starting up SHOUT on ~p for Media ~p of type ~p~n", [Port, MediaID, Type]),
    {ok, #state{media_id=MediaID, port=Port, send_to=[To], stream_type=Type}, 0}.

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
    Reply = ok,
    {reply, Reply, State}.

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
    {noreply, State}.

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
handle_info(_, #state{stream_type=continuous}=S) ->
    {stop, continuous_not_implemented, S};

handle_info(timeout, #state{media_id=M}=S) ->
    try
    case couch_mgr:open_doc(?MEDIA_DB, M) of
	{ok, Doc} ->
	    logger:format_log(info, "SHOUT(~p): Opened Doc for ~p~n", [self(), M]),
	    case whistle_util:is_true(whapps_json:get_value(<<"streamable">>, Doc, true))
		andalso whapps_json:get_value(<<"_attachments">>, Doc) of
		false ->
		    {stop, media_streaming_disallowed, S};
		undefined ->
		    {stop, no_attachment_exists, S};
		{struct, [{Attachment, Props} | _]} ->
		    MediaName = mochiweb_util:quote_plus(whapps_json:get_value(<<"display_name">>, Doc)),
		    CT = whapps_json:get_value(<<"content-type">>, Doc, whapps_json:get_value(<<"content_type">>, Props)),
		    {ok, Content} = couch_mgr:fetch_attachment(?MEDIA_DB, M, Attachment),

		    {ok, PortNo} = inet:port(S#state.port),
		    StreamUrl = list_to_binary(["shout://", net_adm:localhost(), ":", integer_to_list(PortNo), "/stream"]),
		    logger:format_log(info, "SHOUT(~p): Send ~p to ~p~n", [self(), StreamUrl, S#state.send_to]),

		    Media = #media_file{stream_url=StreamUrl, contents=Content, content_type=CT, media_name=MediaName},

		    shout:start(S#state.port, Media, S#state.stream_type),

		    lists:foreach(fun(To) -> send_media_resp(M, StreamUrl, To) end, S#state.send_to),

		    logger:format_log(info, "SHOUT(~p): URL: ~p~n", [self(), StreamUrl]),
		    {noreply, S#state{media_file=Media}};
		{error, Err} ->
		    logger:format_log(error, "SHOUT(~p): accept failed ~p~n", [self(), Err]),
		    {stop, normal, S}
	    end;
	{error, E} ->
	    logger:format_log(error, "Failed to find ~p:~p - ~p~n", [?MEDIA_DB, M, E]),
	    {stop, doc_not_found, S}
    end
catch A:B ->
	logger:format_log(info, "Exception Thrown: ~p:~p~n~p~n", [A, B, erlang:get_stacktrace()]),
	{stop, normal, S}
end;


handle_info({add_listener, ListenerQ}, #state{stream_type=single, media_id=M}=S) ->
    spawn(fun() ->
		  {ok, ShoutSrv} = media_shout_sup:start_shout(M, ListenerQ, continuous, media_srv:next_port()),
		  media_srv:add_stream(M, ShoutSrv)
	  end),
    {noreply, S};

handle_info({add_listener, ListenerQ}, #state{media_id=M, media_file=Media}=S) ->
    send_media_resp(M, Media#media_file.stream_url, ListenerQ),
    {noreply, S#state{send_to=[ListenerQ | S#state.send_to]}};

handle_info(_Info, State) ->
    logger:format_log(info, "MEDIA_SHOUT(~p): Recv info ~p~n", [self(), _Info]),
    {noreply, State}.

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
send_media_resp(MediaName, Url, To) ->
    Prop = [{<<"Media-Name">>, MediaName}
	    ,{<<"Stream-URL">>, Url}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_resp">>, ?SERVER, ?APP_VSN)],

    {ok, JSON} = whistle_api:media_resp(Prop),
    logger:format_log(info, "SHOUT(~p): Sending ~p to ~p~n", [self(), JSON, To]),
    amqp_util:targeted_publish(whapps_controller:get_amqp_host(), To, JSON).
