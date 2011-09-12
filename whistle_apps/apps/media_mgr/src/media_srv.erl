%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Receive AMQP requests for media, spawn a handler for the response
%%% TODO: convert to gen_listener
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(media_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, add_stream/2, next_port/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("media_mgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  	   streams = [] :: list(tuple(binary(), pid(), reference())) | [] % MediaID, StreamPid, Ref
	  ,amqp_q = <<>> :: binary()
	  ,ports = queue:new() :: queue()
          ,port_range = ?PORT_RANGE
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_stream(MediaID, ShoutSrv) ->
    gen_server:cast(?SERVER, {add_stream, MediaID, ShoutSrv}).

next_port() ->
    gen_server:call(?SERVER, next_port).

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
    ?LOG_SYS("starting new media server"),
    amqp_util:callmgr_exchange(),
    amqp_util:targeted_exchange(),
    {ok, #state{}, 0}.

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
handle_call(next_port, From, #state{ports=Ports, port_range=PortRange}=S) ->
    case queue:out(Ports) of
	{{value, Port}, Ps1} ->
	    {reply, Port, S#state{ports=updated_reserved_ports(Ps1, PortRange)}, hibernate};
	{empty, _} ->
	    handle_call({next_port, from_empty}, From, S#state{ports=updated_reserved_ports(Ports, PortRange)})
    end;
handle_call({next_port, from_empty}, _, #state{ports=Ports, port_range=PortRange}=S) ->
    case queue:out(Ports) of
	{{value, Port}, Ps1} ->
	    {reply, Port, S#state{ports=updated_reserved_ports(Ps1, PortRange)}, hibernate};
	{empty, _} ->
	    {reply, no_ports, S}
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
handle_cast({add_stream, MediaID, ShoutSrv}, #state{streams=Ss}=S) ->
    {noreply, S#state{streams=[{MediaID, ShoutSrv, erlang:monitor(process, ShoutSrv)} | Ss]}, hibernate}.

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
handle_info(timeout, #state{amqp_q = <<>>, ports=Ps, port_range=PortRange}=S) ->
    try
	{ok, Q} = start_amqp(),
	Ps1 = updated_reserved_ports(Ps, PortRange),
	{noreply, S#state{amqp_q=Q, ports=Ps1}, hibernate}
    catch
	_:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    Ps2 = updated_reserved_ports(Ps, PortRange),
	    {noreply, S#state{amqp_q = <<>>, ports=Ps2}, hibernate}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ}, hibernate}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}, hibernate};

handle_info({_, #amqp_msg{payload = Payload}}, #state{ports=Ports, port_range=PortRange, streams=Streams}=State) ->
    {{value, Port}, Ps1} = queue:out(Ports),
    spawn(fun() ->
		  JObj = mochijson2:decode(Payload),
		  put(callid, wh_json:get_value(<<"Call-ID">>, JObj, <<"0000000000">>)),
		  ?LOG_START("received request for media"),

		  try
		      handle_req(JObj, Port, Streams)
		  catch
		      _Type:Err ->
			  ?LOG_END("caught ~p: ~p", [_Type, Err]),
			  send_error_resp(JObj, <<"other">>, Err)
		  end
	  end),

    Ps2 = case queue:is_empty(Ps1) of
	      true -> updated_reserved_ports(Ps1, PortRange);
	      false -> Ps1
	  end,

    false = queue:is_empty(Ps2),

    {noreply, State#state{ports=Ps2}, hibernate};

handle_info({'DOWN', Ref, process, ShoutSrv, Info}, #state{streams=Ss}=S) ->
    case lists:keyfind(ShoutSrv, 2, Ss) of
	{MediaID, _, Ref1} when Ref =:= Ref1 ->
	    ?LOG_SYS("MediaSrv for ~s(~p) went down(~p)", [MediaID, ShoutSrv, Info]),
	    {noreply, S#state{streams=lists:keydelete(MediaID, 1, Ss)}, hibernate};
	_ ->
	    ?LOG_SYS("Unknown 'DOWN' received for ~p(~p)", [ShoutSrv, Info]),
	    {noreply, S}
    end;

handle_info(#'basic.consume_ok'{}, S) ->
    {noreply, S};

handle_info(_Info, State) ->
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
    ?LOG_SYS("media server ~p termination", [_Reason]),
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
-spec(start_amqp/0 :: () -> tuple(ok, binary()) | tuple(error, amqp_error)).
start_amqp() ->
    try
	Q = amqp_util:new_queue(<<>>),

	amqp_util:bind_q_to_callevt(Q, media_req),
	amqp_util:bind_q_to_targeted(Q, Q),

	amqp_util:basic_consume(Q),

        ?LOG_SYS("connected to AMQP"),
	{ok, Q}
    catch
	_:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

send_error_resp(JObj, ErrCode, <<>>) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    Prop = [{<<"Media-Name">>, MediaName}
	    ,{<<"Error-Code">>, wh_util:to_binary(ErrCode)}
	    | wh_api:default_headers(<<>>, <<"media">>, <<"media_error">>, ?APP_NAME, ?APP_VERSION)],
    {ok, Payload} = wh_api:media_error(Prop),
    ?LOG_END("sending error reply ~s for ~s", [ErrCode, MediaName]),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload);
send_error_resp(JObj, _ErrCode, ErrMsg) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    Prop = [{<<"Media-Name">>, MediaName}
	    ,{<<"Error-Code">>, <<"other">>}
	    ,{<<"Error-Msg">>, wh_util:to_binary(ErrMsg)}
	    | wh_api:default_headers(<<>>, <<"media">>, <<"media_error">>, ?APP_NAME, ?APP_VERSION)],
    {ok, Payload} = wh_api:media_error(Prop),
    ?LOG_END("sending error reply ~s for ~s", [_ErrCode, MediaName]),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

-spec(handle_req/3 :: (JObj :: json_object(), Port :: port(), Streams :: list()) -> no_return()).
handle_req(JObj, Port, Streams) ->
    true = wh_api:media_req_v(JObj),
    case find_attachment(binary:split(wh_json:get_value(<<"Media-Name">>, JObj, <<>>), <<"/">>, [global, trim])) of
        not_found ->
            send_error_resp(JObj, <<"not_found">>, <<>>);
        no_data ->
            send_error_resp(JObj, <<"no_data">>, <<>>);
        {Db, Doc, Attachment, _MetaData, CType} ->
	    case wh_json:get_value(<<"Stream-Type">>, JObj, <<"new">>) of
		<<"new">> ->
		    start_stream(JObj, Db, Doc, Attachment, CType, Port);
		<<"extant">> ->
		    join_stream(JObj, Db, Doc, Attachment, CType, Port, Streams)
	    end
    end.

find_attachment([<<>>, Doc]) ->
    find_attachment([Doc]);
find_attachment([Doc]) ->
    find_attachment([?MEDIA_DB, Doc]);
find_attachment([<<>>, Db, Doc]) ->
    find_attachment([Db, Doc, first]);
find_attachment([Db, Doc]) ->
    find_attachment([Db, Doc, first]);
find_attachment([<<>>, Db, Doc, Attachment]) ->
    find_attachment([Db, Doc, Attachment]);
find_attachment([Db, Doc, first]) ->
    DbName = case couch_mgr:db_exists(Db) of
                 true -> Db;
                 false -> whapps_util:get_db_name(Db, encoded)
             end,
    case couch_mgr:open_doc(DbName, Doc) of
	{ok, JObj} ->
	    case is_streamable(JObj)
		andalso wh_json:get_value(<<"_attachments">>, JObj, false) of
		false ->
		    no_data;
		{struct, [{Attachment, MetaData} | _]} ->
                    {DbName, Doc, Attachment, MetaData, get_content_type(JObj, MetaData)}
            end;
        _->
            not_found
    end;
find_attachment([Db, Doc, Attachment]) ->
    DbName = case couch_mgr:db_exists(Db) of
                 true -> Db;
                 false -> whapps_util:get_db_name(Db, encoded)
             end,
    case couch_mgr:open_doc(DbName, Doc) of
        {ok, JObj} ->
	    case is_streamable(JObj)
                andalso wh_json:get_value([<<"_attachments">>, Attachment], JObj, false) of
		false ->
		    no_data;
		MetaData ->
                    {DbName, Doc, Attachment, MetaData, get_content_type(JObj, MetaData)}
            end;
        _ ->
            not_found
    end.

-spec get_content_type/2 :: (JObj, MetaData) -> undefined | binary() when
      JObj :: json_object(),
      MetaData :: json_object().
get_content_type(JObj, MetaData) ->
    case valid_content_type(JObj) of
        undefined ->
            valid_content_type(MetaData);
        ContentType ->
            ContentType
    end.

-spec valid_content_type/1 :: (JObj) -> undefined | binary() when
      JObj :: json_object().
valid_content_type(JObj) ->
    case wh_json:get_value(<<"content_type">>, JObj) of
        <<"audio/mp3">> -> <<"mp3">>; %% Jon's computer uses this, is this legit?
        <<"audio/mpeg">> -> <<"mp3">>;
        <<"audio/x-wav">> -> <<"wav">>;
        <<"audio/wav">> -> <<"wav">>;
        _ -> undefined
    end.

-spec(is_streamable/1 :: (JObj :: json_object()) -> boolean()).
is_streamable(JObj) ->
    wh_util:is_true(wh_json:get_value(<<"streamable">>, JObj, true)).

-spec start_stream/6 :: (JObj, Db, Doc, Attachment, CType, Port) -> no_return() when
      JObj :: json_object(),
      Db :: binary(),
      Doc :: binary(),
      Attachment :: binary(),
      CType :: undefined | binary(),
      Port :: port().
start_stream(JObj, Db, Doc, Attachment, CType, Port) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    To = wh_json:get_value(<<"Server-ID">>, JObj),
    Media = {MediaName, Db, Doc, Attachment, CType},
    ?LOG_END("request for ~s is starting in new stream server", [MediaName]),
    {ok, _} = media_shout_sup:start_shout(Media, To, single, Port, get(callid)).

-spec join_stream/7 :: (JObj, Db, Doc, Attachment, CType, Port, Streams) -> no_return() when
      JObj :: json_object(),
      Db :: binary(),
      Doc :: binary(),
      Attachment :: binary(),
      CType :: undefined | binary(),
      Port :: port(),
      Streams :: list().
join_stream(JObj, Db, Doc, Attachment, CType, Port, Streams) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    To = wh_json:get_value(<<"Server-ID">>, JObj),
    Media = {MediaName, Db, Doc, Attachment, CType},

    case lists:keyfind(MediaName, 1, Streams) of
	{_, ShoutSrv, _} ->
            ?LOG_END("request for ~s is joining running stream server", [MediaName]),
	    ShoutSrv ! {add_listener, To};
	false ->
            ?LOG_END("request for ~s is starting a new continuous stream server", [MediaName]),
	    {ok, ShoutSrv} = media_shout_sup:start_shout(Media, To, continuous, Port, get(callid)),
	    ?SERVER:add_stream(MediaName, ShoutSrv)
    end.

updated_reserved_ports(Ps, PortRange) ->
    case queue:len(Ps) of
	Len when Len >= ?MAX_RESERVED_PORTS ->
	    Ps;
	SubLen ->
	    fill_ports(Ps, SubLen, PortRange)
    end.

-spec(fill_ports/3 :: (Ps :: queue(), Len :: integer(), Range :: 0 | tuple(pos_integer(), pos_integer())) -> queue()).
fill_ports(Ps, Len, _) when Len >= ?MAX_RESERVED_PORTS ->
    Ps;

%% randomly assign port numbers
fill_ports(Ps, _, 0) ->
    {ok, Port} = gen_tcp:listen(0, ?PORT_OPTIONS),
    Ps1 = queue:in(Port, Ps),
    fill_ports(Ps1, queue:len(Ps1), 0);

%% assign ports in a range
fill_ports(Ps, Len, {End, End}) ->
    fill_ports(Ps, Len, ?PORT_RANGE);
fill_ports(Ps, _, {Curr, End}) ->
    case gen_tcp:listen(Curr, ?PORT_OPTIONS) of
	{ok, Port} ->
	    Ps1 = queue:in(Port, Ps),
	    fill_ports(Ps1, queue:len(Ps1), {Curr+1, End});
	{error, _} ->
	    fill_ports(Ps, queue:len(Ps), {Curr+1, End})
    end.
