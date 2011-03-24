%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
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

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(APP_VSN, <<"0.2.0">>).
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, [binary, {packet,0}, {active,false}, {reuseaddr, true}]).
-define(MAX_RESERVED_PORTS, 10).
-define(MEDIA_DB, <<"media_files">>).

-record(state, {
	  streams = [] :: list(tuple(binary(), pid(), reference())) | [] % MediaID, StreamPid, Ref
	  ,amqp_q = <<>> :: binary()
          ,is_amqp_up = true :: boolean()
	  ,ports = queue:new() :: queue()
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
    H = whapps_controller:get_amqp_host(),

    amqp_util:callmgr_exchange(H),
    amqp_util:targeted_exchange(H),

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
handle_call(next_port, From, #state{ports=Ports}=S) ->
    case queue:out(Ports) of
	{{value, Port}, Ps1} ->
	    {reply, Port, S#state{ports=updated_reserved_ports(Ps1)}};
	{empty, _} ->
	    handle_call({next_port, from_empty}, From, S#state{ports=updated_reserved_ports(Ports)})
    end;
handle_call({next_port, from_empty}, _, #state{ports=Ports}=S) ->
    case queue:out(Ports) of
	{{value, Port}, Ps1} ->
	    {reply, Port, S#state{ports=updated_reserved_ports(Ps1)}};
	{empty, _} ->
	    {reply, no_ports, S}
    end;
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
handle_cast({add_stream, MediaID, ShoutSrv}, #state{streams=Ss}=S) ->
    {noreply, S#state{streams=[{MediaID, ShoutSrv, erlang:monitor(process, ShoutSrv)} | Ss]}};

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
handle_info(timeout, #state{amqp_q = <<>>, ports=Ps}=S) ->
    Q = start_amqp(whapps_controller:get_amqp_host()),
    format_log(info, "MEDIA_SRV(~p): Listening on ~p~n", [self(), Q]),
    Ps1 = updated_reserved_ports(Ps),
    {noreply, S#state{amqp_q=Q, is_amqp_up=is_binary(Q), ports=Ps1}, 2000};
handle_info(timeout, #state{amqp_q={error, _}=QE}=S) ->
    H = whapps_controller:get_amqp_host(),
    format_log(info, "MEDIA_SRV(~p): Failed to start q (~p), trying again on ~p~n", [self(), QE, H]),
    case amqp_util:is_host_available(H) of
	true ->
	    Q = start_amqp(H),
	    {noreply, S#state{is_amqp_up=is_binary(Q), amqp_q=Q}};
	false ->
	    {noreply, S#state{is_amqp_up=false}, 1000}
    end;
handle_info(timeout, #state{amqp_q=Q, is_amqp_up=false}=S) ->
    H = whapps_controller:get_amqp_host(),
    amqp_util:delete_queue(H, Q),
    NewQ = start_amqp(H),
    format_log(info, "MEDIA_SRV(~p): Stopping ~p, listening on ~p @ ~p~n", [self(), Q, NewQ, H]),
    {noreply, S#state{amqp_q=NewQ, is_amqp_up=is_binary(NewQ)}};
handle_info(timeout, #state{amqp_q=Q, is_amqp_up=true}=S) when is_binary(Q) ->
    {noreply, S};

handle_info({amqp_host_down, H}, S) ->
    format_log(info, "MEDIA_SRV(~p): AMQP Host(~p) down~n", [self(), H]),
    {noreply, S#state{amqp_q={error, amqp_down}, is_amqp_up=false}, 0};

handle_info({_, #amqp_msg{payload = Payload}}, State) ->
    format_log(info, "MEDIA_SRV(~p): Recv JSON: ~p~n", [self(), Payload]),
    JObj = try mochijson2:decode(Payload) catch _:Err1 -> format_log(info, "Err decoding ~p~n", [Err1]), {error, Err1} end,

    S1 = try
	     handle_req(JObj, State)
	 catch
	     _:Err ->
		 format_log(info, "Err handling req: ~p~n~p~n", [Err, erlang:get_stacktrace()]),
		 send_error_resp(JObj, whapps_controller:get_amqp_host(), <<"other">>, Err),
		 State
	 end,
    format_log(info, "MEDIA_SRV(~p): Req handled~n", [self()]),
    {noreply, S1};

handle_info({'DOWN', Ref, process, ShoutSrv, Info}, #state{streams=Ss}=S) ->
    case lists:keyfind(ShoutSrv, 2, Ss) of
	{MediaID, _, Ref1} when Ref =:= Ref1 ->
	    logger:format_log(info, "MEDIA_SRV(~p): ShoutSrv for ~p(~p) went down(~p)~n", [self(), MediaID, ShoutSrv, Info]),
	    {noreply, S#state{streams=lists:keydelete(MediaID, 1, Ss)}};
	_ ->
	    logger:format_log(info, "MEDIA_SRV(~p): Bad DOWN recv for ShoutSrv(~p) for ~p~n", [self(), ShoutSrv, Info]),
	    {noreply, S}
    end;

handle_info(#'basic.consume_ok'{consumer_tag=CTag}, S) ->
    format_log(info, "MEDIA_SRV(~p): Consuming, tag: ~p~n", [self(), CTag]),
    {noreply, S};

handle_info(_Info, State) ->
    format_log(info, "MEDIA_SRV(~p): Unhandled info ~p~nstate: ~p~n", [self(), _Info, State]),
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
start_amqp(H) ->
    Q = amqp_util:new_queue(H, <<>>),

    format_log(info, "Bind to callevt: ~p~n", [amqp_util:bind_q_to_callevt(H, Q, media_req)]),
    format_log(info, "Bind to targeted: ~p~n", [amqp_util:bind_q_to_targeted(H, Q, Q)]),

    format_log(info, "Consume: ~p~n", [amqp_util:basic_consume(H, Q)]),
    Q.

send_error_resp(JObj, H, ErrCode, <<>>) ->
    Prop = [{<<"Media-Name">>, whapps_json:get_value(<<"Media-Name">>, JObj)}
	    ,{<<"Error-Code">>, whistle_util:to_binary(ErrCode)}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_error">>, ?SERVER, ?APP_VSN)],
    To = whapps_json:get_value(<<"Server-ID">>, JObj),

    {ok, JSON} = whistle_api:media_error(Prop),
    amqp_util:targeted_publish(H, To, JSON);
send_error_resp(JObj, H, _ErrCode, ErrMsg) ->
    Prop = [{<<"Media-Name">>, whapps_json:get_value(<<"Media-Name">>, JObj)}
	    ,{<<"Error-Code">>, <<"other">>}
	    ,{<<"Error-Msg">>, whistle_util:to_binary(ErrMsg)}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_error">>, ?SERVER, ?APP_VSN)],
    To = whapps_json:get_value(<<"Server-ID">>, JObj),

    {ok, JSON} = whistle_api:media_error(Prop),
    amqp_util:targeted_publish(H, To, JSON).

-spec(handle_req/2 :: (JObj :: json_object(), State :: #state{}) -> #state{}).
handle_req(JObj, #state{ports=Ports}=State) ->
    true = whistle_api:media_req_v(JObj),
    case find_attachment(binary:split(whapps_json:get_value(<<"Media-Name">>, JObj, <<>>), <<"/">>, [global, trim])) of
        {not_found} ->
            send_error_resp(JObj, whapps_controller:get_amqp_host(), <<"not_found">>, <<>>),
            State;
        {no_data} ->
            send_error_resp(JObj, whapps_controller:get_amqp_host(), <<"no_data">>, <<>>),
            State;
        {Db, Doc, Attachment, MetaData} ->
            case is_mp3(Attachment, MetaData) of
                true ->
                    Ports1 = case queue:is_empty(Ports) of
                                 true -> updated_reserved_ports(Ports);
                                 false -> Ports
                             end,
                    false = queue:is_empty(Ports1),
                    case whapps_json:get_value(<<"Stream-Type">>, JObj, <<"new">>) of
                        <<"new">> -> 
                            start_stream(JObj, Db, Doc, Attachment, State#state{ports=Ports1});
                        <<"extant">> -> 
                            join_stream(JObj, Db, Doc, Attachment, State#state{ports=Ports1})
                    end;
                false ->
                    send_couch_stream_resp(Db, Doc, Attachment, 
                                           whapps_json:get_value(<<"Media-Name">>, JObj), 
                                           whapps_json:get_value(<<"Server-ID">>, JObj)),
                    State
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
		andalso whapps_json:get_value(<<"_attachments">>, JObj, false) of
		false ->
		    {no_data};
		{struct, [{Attachment, MetaData} | _]} ->
                    {whistle_util:to_binary(Db), Doc, Attachment, MetaData}
            end;
        _->
            {not_found}
    end;
find_attachment([Db, Doc, Attachment]) ->
    case couch_mgr:open_doc(Db, Doc) of
        {ok, JObj} ->    
	    case is_streamable(JObj)
                andalso whapps_json:get_value([<<"_attachments">>, Attachment], JObj, false) of
		false ->
		    {no_data};
		MetaData ->
                    {Db, Doc, Attachment, MetaData}
            end;
        _ ->
            {not_found}
    end.

-spec(is_streamable/1 :: (JObj :: json_object()) -> boolean()).                              
is_streamable(JObj) ->
    whistle_util:is_true(whapps_json:get_value(<<"streamable">>, JObj, true)).

-spec(is_mp3/2 :: (Attachment :: binary(), MetaData :: json_object()) -> boolean()).
is_mp3(Attachment, MetaData) ->
    case whapps_json:get_value([<<"content_type">>], MetaData, <<"application/octet-stream">>) of
        <<"audio/mpeg">> ->        
            true;
        <<"application/octet-stream">> ->
	    binary:match(Attachment, <<"mp3">>) =/= nomatch;
        _ ->
            false
    end.
    
-spec(start_stream/5 :: (JObj :: json_object(), Db :: binary(), Doc :: binary(), 
                         Attachment :: binary(), S :: #state{}) -> #state{}).
start_stream(JObj, Db, Doc, Attachment, #state{ports=Ports}=S) ->
    MediaName = whapps_json:get_value(<<"Media-Name">>, JObj),
    To = whapps_json:get_value(<<"Server-ID">>, JObj),
    Media = {MediaName, Db, Doc, Attachment},
    {{value, Port}, Ps1} = queue:out(Ports),

    format_log(info, "MEDIA_SRV(~p): Starting stream~n", [self()]),
    {ok, _} = media_shout_sup:start_shout(Media, To, single, Port),
    S#state{ports=Ps1}.

-spec(join_stream/5 :: (JObj :: json_object(), Db :: binary(), Doc :: binary(), 
                         Attachment :: binary(), S :: #state{}) -> #state{}).
join_stream(JObj, Db, Doc, Attachment, #state{streams=Streams, ports=Ports}=S) ->
    MediaName = whapps_json:get_value(<<"Media-Name">>, JObj),
    To = whapps_json:get_value(<<"Server-ID">>, JObj),
    Media = {MediaName, Db, Doc, Attachment},

    case lists:keyfind(MediaName, 1, Streams) of
	{_, ShoutSrv, _} ->
	    format_log(info, "MEDIA_SRV(~p): Joining stream~n", [self()]),
	    ShoutSrv ! {add_listener, To},
	    S;
	false ->
	    {{value, Port}, Ps1} = queue:out(Ports),
	    format_log(info, "MEDIA_SRV(~p): Starting stream instead of joining~n", [self()]),
	    {ok, ShoutSrv} = media_shout_sup:start_shout(Media, To, continuous, Port),
	    ?SERVER:add_stream(MediaName, ShoutSrv),
	    S#state{ports=Ps1}
    end.

updated_reserved_ports(Ps) ->
    case queue:len(Ps) of
	Len when Len >= ?MAX_RESERVED_PORTS ->
	    Ps;
	SubLen ->
	    fill_ports(Ps, SubLen, ?PORT_RANGE)
    end.

-spec(fill_ports/3 :: (Ps :: queue(), Len :: integer(), Range :: 0 | tuple(integer(), integer())) -> queue()).
fill_ports(Ps, ?MAX_RESERVED_PORTS, _) ->
    Ps;

fill_ports(Ps, Len, 0) ->
    {ok, Port} = gen_tcp:listen(0, ?PORT_OPTIONS),
    fill_ports(queue:in(Port, Ps), Len+1, 0);

fill_ports(Ps, Len, {End, End}) ->
    fill_ports(Ps, Len, ?PORT_RANGE);
fill_ports(Ps, Len, {Curr, End}) ->
    case gen_tcp:listen(Curr, ?PORT_OPTIONS) of
	{ok, Port} ->
	    fill_ports(queue:in(Port, Ps), Len+1, {Curr+1, End});
	{error, _} ->
	    fill_ports(Ps, Len, {Curr+1, End})
    end.

-spec(send_couch_stream_resp/5 :: (Db :: binary(), Doc :: binary(), Attachment :: binary(), 
                                  MediaName :: binary(), To :: binary) -> ok | tuple(error, atom())).
send_couch_stream_resp(Db, Doc, Attachment, MediaName, To) ->    
    Url = <<(couch_mgr:get_url())/binary, Db/binary, $/, Doc/binary, $/, Attachment/binary>>,
    Resp = [{<<"Media-Name">>, MediaName}
	    ,{<<"Stream-URL">>, Url}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_resp">>, ?SERVER, ?APP_VSN)],
    logger:format_log(info, "MEDIA_SRV(~p): Sending ~p to ~p~n", [self(), Resp, To]),    
    {ok, Payload} = whistle_api:media_resp(Resp),
    amqp_util:targeted_publish(whapps_controller:get_amqp_host(), To, Payload).
