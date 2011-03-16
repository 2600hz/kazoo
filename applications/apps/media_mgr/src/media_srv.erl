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
-export([start_link/0, add_stream/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("media_mgr.hrl").

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(APP_VSN, "0.2.0").
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, [binary, {packet,0}, {active,true}]).
-define(MAX_RESERVED_PORTS, 10).

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
	true -> {noreply, S#state{is_amqp_up=true, amqp_q=start_amqp(H)}};
	false -> {noreply, S#state{is_amqp_up=false}, 1000}
    end;
handle_info(timeout, #state{amqp_q=Q, is_amqp_up=false}=S) ->
    H = whapps_controller:get_amqp_host(),
    amqp_util:delete_queue(H, Q),
    NewQ = start_amqp(H),
    format_log(info, "MEDIA_SRV(~p): Stopping ~p, listening on ~p @ ~p~n", [self(), Q, NewQ, H]),
    {noreply, S#state{amqp_q=NewQ, is_amqp_up=is_binary(NewQ)}};

handle_info({amqp_host_down, _}, S) ->
    {noreply, S#state{amqp_q={error, amqp_down}, is_amqp_up=false}, 0};

handle_info({_, #amqp_msg{payload = Payload}}, #state{streams=Ss}=S) ->
    spawn(fun() ->
		  JObj = mochijson2:decode(Payload),
		  try
		      handle_req(JObj, Ss)
		  catch
		      _:Err -> send_error_resp(JObj, whapps_controller:get_amqp_host(), <<"other">>, Err)
		  end
	  end),
    {noreply, S};

handle_info({'DOWN', Ref, process, ShoutSrv, Info}, #state{streams=Ss}=S) ->
    case lists:keyfind(ShoutSrv, 2, Ss) of
	{MediaID, _, Ref1} when Ref =:= Ref1 ->
	    logger:format_log(info, "MEDIA_SRV(~p): ShoutSrv for ~p(~p) went down(~p)~n", [self(), MediaID, ShoutSrv, Info]),
	    {noreply, S#state{streams=lists:keydelete(MediaID, 1, Ss)}};
	_ ->
	    logger:format_log(info, "MEDIA_SRV(~p): Bad DOWN recv for ShoutSrv(~p) for ~p~n", [self(), ShoutSrv, Info]),
	    {noreply, S}
    end;

handle_info(_Info, State) ->
    format_log(info, "MEDIA_SRV(~p): Unhandled info ~p~n", [self(), _Info]),
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

    amqp_util:bind_q_to_callevt(H, Q, ?KEY_CALL_MEDIA_REQ),
    amqp_util:bind_q_to_targeted(H, Q, Q),

    amqp_util:basic_consume(H, Q),
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

handle_req(JObj, Streams) ->
    true = whistle_api:media_req_v(JObj),

    case whapps_json:get_value(<<"Stream-Type">>, JObj, <<"new">>) of
	<<"new">> -> start_stream(JObj);
	<<"extant">> -> join_stream(JObj, Streams)
    end.

start_stream(JObj) ->
    MediaID = whapps_json:get_value(<<"Media-Name">>, JObj),
    To = whapps_json:get_value(<<"Server-ID">>, JObj),
    {ok, _} = media_shout_sup:start_shout(MediaID, To, single).

join_stream(JObj, Streams) ->
    MediaID = whapps_json:get_value(<<"Media-Name">>, JObj),
    To = whapps_json:get_value(<<"Server-ID">>, JObj),

    case lists:keyfind(MediaID, 1, Streams) of
	{_, ShoutSrv, _} ->
	    ShoutSrv ! {add_listener, To};
	false ->
	    {ok, ShoutSrv} = media_shout_sup:start_shout(MediaID, To, continuous),
	    ?SERVER:add_stream(MediaID, ShoutSrv)
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
    {ok, PortNo} = inet:port(Port),
    fill_ports(queue:in({PortNo, Port}, Ps), Len+1, 0);

fill_ports(Ps, Len, {End, End}) ->
    fill_ports(Ps, Len, ?PORT_RANGE);
fill_ports(Ps, Len, {Curr, End}) ->
    case gen_tcp:listen(Curr, ?PORT_OPTIONS) of
	{ok, Port} ->
	    fill_ports(queue:in({Curr, Port}, Ps), Len+1, {Curr+1, End});
	{error, _} ->
	    fill_ports(Ps, Len, {Curr+1, End})
    end.


