%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INCE
%%% @doc
%%% Receive AMQP requests for media, spawn a handler for the response
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
	  ,amqp_q = <<>> :: binary() | tuple(error, term())
          ,is_amqp_up = true :: boolean()
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
	    {reply, Port, S#state{ports=updated_reserved_ports(Ps1, PortRange)}};
	{empty, _} ->
	    handle_call({next_port, from_empty}, From, S#state{ports=updated_reserved_ports(Ports, PortRange)})
    end;
handle_call({next_port, from_empty}, _, #state{ports=Ports, port_range=PortRange}=S) ->
    case queue:out(Ports) of
	{{value, Port}, Ps1} ->
	    {reply, Port, S#state{ports=updated_reserved_ports(Ps1, PortRange)}};
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
    {noreply, S#state{streams=[{MediaID, ShoutSrv, erlang:monitor(process, ShoutSrv)} | Ss]}}.

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
    Q = start_amqp(),
    ?LOG_SYS("AMQP Queue: ~s", [Q]),
    Ps1 = updated_reserved_ports(Ps, PortRange),
    {noreply, S#state{amqp_q=Q, is_amqp_up=is_binary(Q), ports=Ps1}, 2000};

handle_info(timeout, #state{amqp_q={error, _}=QE}=S) ->
    ?LOG_SYS("Failed to start q (~s), trying again", [QE]),
    try
        Q = start_amqp(),
        {noreply, S#state{is_amqp_up=is_binary(Q), amqp_q=Q}}
    catch
        _:_ ->
            {noreply, S#state{is_amqp_up=false}, 1000}
    end;

handle_info(timeout, #state{amqp_q=Q, is_amqp_up=false}=S) ->
    amqp_util:queue_delete(Q),
    NewQ = start_amqp(),
    ?LOG_SYS("Old AMQP Queue: ~s, New AMQP Queue: ~s", [Q, NewQ]),
    {noreply, S#state{amqp_q=NewQ, is_amqp_up=is_binary(NewQ)}};
handle_info(timeout, #state{amqp_q=Q, is_amqp_up=true}=S) when is_binary(Q) ->
    {noreply, S};

handle_info({amqp_host_down, _Host}, S) ->
    ?LOG_SYS("AMQP Host(~s) down", [_Host]),
    {noreply, S#state{amqp_q={error, amqp_down}, is_amqp_up=false}, 0};

handle_info({_, #amqp_msg{payload = Payload}}, #state{ports=Ports, port_range=PortRange, streams=Streams}=State) ->
    {{value, Port}, Ps1} = queue:out(Ports),
    spawn(fun() ->
		  JObj = try mochijson2:decode(Payload) catch _:Err1 -> ?LOG_SYS("Err decoding ~p", [Err1]), {error, Err1} end,
		  put(callid, wh_json:get_value(<<"Call-ID">>, JObj, <<"0000000000">>)),
		  ?LOG_START("Media Request JSON: ~s", [Payload]),
		  try
		      handle_req(JObj, Port, Streams)
		  catch
		      _Type:Err ->
			  ?LOG("Caught ~p: ~p", [_Type, Err]),
			  send_error_resp(JObj, <<"other">>, Err)
		  end,
		  ?LOG_END("Media Request ended")
	  end),

    Ps2 = case queue:is_empty(Ps1) of
	      true -> updated_reserved_ports(Ps1, PortRange);
	      false -> Ps1
	  end,

    false = queue:is_empty(Ps2),

    {noreply, State#state{ports=Ps2}};

handle_info({'DOWN', Ref, process, ShoutSrv, Info}, #state{streams=Ss}=S) ->
    case lists:keyfind(ShoutSrv, 2, Ss) of
	{MediaID, _, Ref1} when Ref =:= Ref1 ->
	    ?LOG_SYS("MediaSrv for ~s(~p) went down(~p)", [MediaID, ShoutSrv, Info]),
	    {noreply, S#state{streams=lists:keydelete(MediaID, 1, Ss)}};
	_ ->
	    ?LOG_SYS("Unknown 'DOWN' received for ~p(~p)", [ShoutSrv, Info]),
	    {noreply, S}
    end;

handle_info(#'basic.consume_ok'{consumer_tag=CTag}, S) ->
    ?LOG_SYS("Basically consuming with tag ~p", [CTag]),
    {noreply, S};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message ~p", [_Info]),
    {noreply, State, 1000}.

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
start_amqp() ->
    try
	Q = amqp_util:new_queue(<<>>),

	amqp_util:bind_q_to_callevt(Q, media_req),
	amqp_util:bind_q_to_targeted(Q, Q),

	amqp_util:basic_consume(Q),
	Q
    catch
	_:_ -> {error, amqp_error}
    end.

send_error_resp(JObj, ErrCode, <<>>) ->
    Prop = [{<<"Media-Name">>, wh_json:get_value(<<"Media-Name">>, JObj)}
	    ,{<<"Error-Code">>, whistle_util:to_binary(ErrCode)}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_error">>, ?APP_NAME, ?APP_VERSION)],
    To = wh_json:get_value(<<"Server-ID">>, JObj),

    {ok, JSON} = whistle_api:media_error(Prop),
    ?LOG("Sending media error to ~s: ~s", [To, JSON]),
    amqp_util:targeted_publish(To, JSON);
send_error_resp(JObj, _ErrCode, ErrMsg) ->
    Prop = [{<<"Media-Name">>, wh_json:get_value(<<"Media-Name">>, JObj)}
	    ,{<<"Error-Code">>, <<"other">>}
	    ,{<<"Error-Msg">>, whistle_util:to_binary(ErrMsg)}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_error">>, ?APP_NAME, ?APP_VERSION)],
    To = wh_json:get_value(<<"Server-ID">>, JObj),

    {ok, JSON} = whistle_api:media_error(Prop),
    ?LOG("Sending media error to ~s: ~s", [To, JSON]),
    amqp_util:targeted_publish(To, JSON).

-spec(handle_req/3 :: (JObj :: json_object(), Port :: port(), Streams :: list()) -> no_return()).
handle_req(JObj, Port, Streams) ->
    true = whistle_api:media_req_v(JObj),
    case find_attachment(binary:split(wh_json:get_value(<<"Media-Name">>, JObj, <<>>), <<"/">>, [global, trim])) of
        not_found ->
            send_error_resp(JObj, <<"not_found">>, <<>>);
        no_data ->
            send_error_resp(JObj, <<"no_data">>, <<>>);
        {Db, Doc, Attachment, _MetaData} ->
	    case wh_json:get_value(<<"Stream-Type">>, JObj, <<"new">>) of
		<<"new">> ->
		    start_stream(JObj, Db, Doc, Attachment, Port);
		<<"extant">> ->
		    join_stream(JObj, Db, Doc, Attachment, Port, Streams)
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
    case couch_mgr:open_doc(Db, Doc) of
	{ok, JObj} ->
	    case is_streamable(JObj)
		andalso wh_json:get_value(<<"_attachments">>, JObj, false) of
		false ->
		    no_data;
		{struct, [{Attachment, MetaData} | _]} ->
                    {whistle_util:to_binary(Db), Doc, Attachment, MetaData}
            end;
        _->
            not_found
    end;
find_attachment([Db, Doc, Attachment]) ->
    case couch_mgr:open_doc(Db, Doc) of
        {ok, JObj} ->
	    case is_streamable(JObj)
                andalso wh_json:get_value([<<"_attachments">>, Attachment], JObj, false) of
		false ->
		    no_data;
		MetaData ->
                    {Db, Doc, Attachment, MetaData}
            end;
        _ ->
            not_found
    end.

-spec(is_streamable/1 :: (JObj :: json_object()) -> boolean()).
is_streamable(JObj) ->
    whistle_util:is_true(wh_json:get_value(<<"streamable">>, JObj, true)).

-spec(start_stream/5 :: (JObj :: json_object(), Db :: binary(), Doc :: binary()
                         ,Attachment :: binary(), Port :: port()) -> no_return()).
start_stream(JObj, Db, Doc, Attachment, Port) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    To = wh_json:get_value(<<"Server-ID">>, JObj),
    Media = {MediaName, Db, Doc, Attachment},

    ?LOG("Starting single stream server for ~s~n", [MediaName]),
    {ok, _} = media_shout_sup:start_shout(Media, To, single, Port, get(callid)).

-spec(join_stream/6 :: (JObj :: json_object(), Db :: binary(), Doc :: binary()
			,Attachment :: binary(), Port :: port(), Streams :: list()) -> no_return()).
join_stream(JObj, Db, Doc, Attachment, Port, Streams) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    To = wh_json:get_value(<<"Server-ID">>, JObj),
    Media = {MediaName, Db, Doc, Attachment},

    case lists:keyfind(MediaName, 1, Streams) of
	{_, ShoutSrv, _} ->
	    ?LOG("Joining continuous stream server(~p) for ~s~n", [ShoutSrv, MediaName]),
	    ShoutSrv ! {add_listener, To};
	false ->
	    ?LOG("Starting continuous stream server for ~s~n", [MediaName]),
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
