%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive requests off the broadcast XC and reply if possible, using
%%% couchdb to retrieve data
%%% @end
%%% Created : 20 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(responder_couch).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("apps.hrl").

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).
-import(logger, [log/2, format_log/3]).

-define(SERVER, ?MODULE). 

-record(state, {channel, ticket, my_queue}).

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
    application:start(amqp),
    application:start(dynamic_compile),
    application:start(log_roller),
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    format_log(info, "RESPONDER(~p): Channel open to MQ: ~p Ticket: ~p~n", [self(), Channel, Ticket]),

    amqp_channel:cast(Channel, amqp_util:broadcast_exchange(Ticket)),
    amqp_channel:cast(Channel, amqp_util:targeted_exchange(Ticket)),

    #'queue.declare_ok'{queue = BroadQueue} =
	amqp_channel:call(Channel
			  ,amqp_util:new_broadcast_queue(Ticket, ["responder_couch.", net_adm:localhost()])),

    #'queue.declare_ok'{queue = TarQueue} =
	amqp_channel:call(Channel
			  ,amqp_util:new_targeted_queue(Ticket, ["responder_couch.", net_adm:localhost()])),

    %% Bind the queue to an exchange
    amqp_channel:cast(Channel, amqp_util:bind_q_to_broadcast(Ticket, BroadQueue)),
    amqp_channel:cast(Channel, amqp_util:bind_q_to_targeted(Ticket, TarQueue)),
    format_log(info, "RESPONDER Bound ~p and ~p~n", [BroadQueue, TarQueue]),

    %% Register a consumer to listen to the queue
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, BroadQueue), self()),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, TarQueue), self()),

    format_log(info, "RESPONDER: Consuming on B(~p) and T(~p)~n", [BroadQueue, TarQueue]),

    {ok, #state{channel=Channel, ticket=Ticket, my_queue=TarQueue}}.

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
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    case Props#'P_basic'.content_type of
	<<"application/json">> ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "RESPONDER(~p): Recv Headers: ~p~nPayload: ~p~n"
		       , [self(), Props#'P_basic'.content_type, get_value(<<"Event-Category">>, Prop)]),
	    spawn(fun() -> process_req(get_msg_type(Prop), Prop, State) end);
	_ContentType ->
	    format_log(info, "RESPONDER: ~p recieved unknown msg type: ~p~n", [self(), _ContentType])
    end,
    {noreply, State};
%% catch all so we dont loose state
handle_info(_Unhandled, State) ->
    format_log(info, "RESPONDER(~p): unknown info request: ~p~n", [self(), _Unhandled]),
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

get_msg_type(Prop) ->
    { get_value(<<"Event-Category">>, Prop), get_value(<<"Event-Name">>, Prop) }.

process_req({<<"directory">>, <<"REQUEST_PARAMS">>}, Prop, #state{channel=Channel, ticket=Ticket, my_queue=Q}) ->
    User = get_value(<<"Auth-User">>, Prop),
    Domain = get_value(<<"Auth-Domain">>, Prop),

    %% do lookup
    case User =:= <<"james">> orelse User =:= <<"sipp">> of
	false ->
	    io:format("User ~p@~p not found~n", [User, Domain]);
	true ->
	    %% send response
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    Data = [{<<"App-Name">>, <<"responder_couch">>}
		    ,{<<"App-Version">>, "0.1"}
		    ,{<<"Auth-Pass">>, <<"james1">>}
		    ,{<<"Auth-Method">>, <<"password">>}
		    ,{<<"Access-Group">>, <<"ignore">>}
		    ,{<<"Tenant-ID">>, <<"ignore">>}
		    ,{<<"Server-ID">>, Q}
		    | Prop],
	    {ok, JSON} = whistle_api:auth_resp(Data),
	    io:format("RESPONDER(~p): Directory JSON Resp: ~s~n", [self(), JSON]),
	    send_resp(JSON, RespQ, Channel, Ticket)
    end;
process_req({<<"dialplan">>,<<"REQUEST_PARAMS">>}, Prop, #state{channel=Channel, ticket=Ticket, my_queue=Q}) ->
    %% replace this with a couch lookup
    case get_value(<<"To">>, Prop) of
	<<"james@192.168.0.198">> ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    Route1 = whistle_api:route_resp_route([{<<"Route">>, <<"sip:4158867900@pbx.switchfreedom.com">>}
					       ,{<<"Media">>, <<"process">>}
					       ,{<<"Auth-User">>, <<"dphone">>}
					       ,{<<"Auth-Pass">>, <<"test1234">>}
					       ,{<<"Weight-Cost">>, <<"100">>}
					       ,{<<"Weight-Location">>, <<"0">>}
					       ]),
	    Route2 = whistle_api:route_resp_route([{<<"Route">>, <<"sip:catchall@work-lappy">>}
						   ,{<<"Media">>, <<"bypass">>}
						   ,{<<"Auth-User">>, <<"james">>}
						   ,{<<"Auth-Pass">>, <<"james1">>}
						   ,{<<"Weight-Cost">>, <<"0">>}
						   ,{<<"Weight-Location">>, <<"0">>}
						  ]),

	    Data = [{<<"App-Name">>, <<"responder_couch">>}
		    ,{<<"App-Version">>, "0.1"}
		    ,{<<"Method">>, <<"bridge">>}
		    ,{<<"Server-ID">>, Q}
		    ,{<<"Routes">>, [{struct, Route1}, {struct, Route2}]}
		    | Prop],
	    {ok, JSON} = whistle_api:route_resp(Data),
	    io:format("RESPONDER(~p): Dialplan JSON Resp: ~s~n", [self(), JSON]),
	    send_resp(JSON, RespQ, Channel, Ticket);
	<<"parker@", _Rest/binary>> ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    Data = [{<<"App-Name">>, <<"responder_couch">>}
		    ,{<<"App-Version">>, "0.1"}
		    ,{<<"Method">>, <<"park">>}
		    ,{<<"Server-ID">>, Q}
		    ,{<<"Routes">>, []}
		    | Prop],
	    {ok, JSON} = whistle_api:route_resp(Data),
	    io:format("RESPONDER(~p): Dialplan JSON Resp: ~s~n", [self(), JSON]),
	    send_resp(JSON, RespQ, Channel, Ticket);
	<<"1234@192.168", _Rest/binary>> ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    Data = [{<<"App-Name">>, <<"responder_couch">>}
		    ,{<<"App-Version">>, "0.1"}
		    ,{<<"Method">>, <<"park">>}
		    ,{<<"Server-ID">>, Q}
		    ,{<<"Routes">>, []}
		    | Prop],
	    {ok, JSON} = whistle_api:route_resp(Data),
	    io:format("RESPONDER(~p): Dialplan JSON Resp: ~s~n", [self(), JSON]),
	    send_resp(JSON, RespQ, Channel, Ticket)
    end;
process_req({<<"dialplan">>,<<"Call-Control">>}, Prop, State) ->
    spawn(fun() -> auto_attendant(Prop, State) end);
process_req(_MsgType, _Prop, _State) ->
    io:format("RESPONDER(~p): Unhandled Msg ~p~nJSON: ~p~n", [self(), _MsgType, _Prop]).

send_resp(JSON, RespQ, Channel, Ticket) ->
    {BP, AmqpMsg} = amqp_util:targeted_publish(Ticket, RespQ, JSON, <<"application/json">>),
    amqp_channel:call(Channel, BP, AmqpMsg).

auto_attendant(Prop, #state{channel=Channel, ticket=Ticket, my_queue=Q}) ->
    UUID = get_value(<<"Call-ID">>, Prop),
    Cmds = ?CLUECON_DEMO(UUID),
    Amqp = {Channel, Ticket},
    DefProp = whistle_api:default_headers(Q, <<"Call-Control">>, <<"responder_couch">>, <<"0.1">>, UUID),
    consume_events(Channel, Ticket, get_value(<<"Event-Queue">>, Prop)),
    exec_cmd(lists:umerge([DefProp, Cmds]), Amqp, get_value(<<"Server-ID">>, Prop)),
    control_loop(get_value(<<"Server-ID">>, Prop), Amqp).

control_loop(CtlQ, Amqp) ->
    wait_for_evt(CtlQ, Amqp).

exec_cmd(Cmd, {Channel, Ticket}, CtlQ) ->
    Payload = mochijson2:encode({struct, Cmd}),
    format_log(info, "RESPONDER(~p): Sending ~p JSON ~s~n", [self(), CtlQ, Payload]),
    {BP, AmqpMsg} = amqp_util:callctl_publish(Ticket, CtlQ, Payload, <<"application/json">>),
    amqp_channel:call(Channel, BP, AmqpMsg).

wait_for_evt(CtlQ, Amqp) ->
    receive
	{_, #amqp_msg{props = _Props, payload = Payload}} ->
	    format_log(info, "RESPONDER(~p): Evt recv:~n~p~n", [self(), Payload]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),

	    case get_value(<<"Event-Name">>, Prop) of
		<<"CHANNEL_DESTROY">> ->
		    format_log(info, "RESPONDER(~p): Done~n", [self()]),
		    done;
		_EvtName ->
		    format_log(info, "RESPONDER(~p): Evt: ~p AppMsg: ~p~n"
			       ,[self(), _EvtName, get_value(<<"Application-Response">>, Prop)]),
		    wait_for_evt(CtlQ, Amqp)
	    end;
	_Evt ->
	    format_log(info, "RESPONDER(~p): recv unhandled event: ~n~p~n", [self(), _Evt]),
	    wait_for_evt(CtlQ, Amqp)
    end.

consume_events(_Channel, _Ticket, undefined) ->
    ok;
consume_events(Channel, Ticket, EvtQ) ->
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, EvtQ), self()).
