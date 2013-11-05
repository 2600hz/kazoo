%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_jsonrpc_channel).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([open/1, start_link/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-define(CHANNELID, ?MODULE).

open(Args) ->
    Oid = list_to_binary(rfc4627_jsonrpc:gen_object_name()),
    {ok, Pid} = supervisor:start_child(rabbit_jsonrpc_channel_sup, [Oid, Args]),
    Service = rfc4627_jsonrpc:service(Oid,
				      <<"urn:uuid:b3f82f69-4f63-424b-8dbb-4fa53f63cf06">>,
				      <<"1.2">>,
				      [{<<"poll">>, []},
				       {<<"close">>, []},
				       {<<"call">>, [{"method", str},
						     {"args", arr}]},
				       {<<"cast">>, [{"method", str},
						     {"args", arr},
						     {"content", str},
						     {"props", arr}]}]),
    rfc4627_jsonrpc:register_service(Pid, Service),
    {ok, Oid}.

start_link(Oid, Args) ->
    gen_server:start_link(?MODULE, [Oid, Args], []).

%---------------------------------------------------------------------------

-record(state, {channel,
                connection,
                oid,
                vhost,
                realm,
                ticket,
                timeout_millisec,
                state,
                waiting_rpcs,
                waiting_polls,
                outbound}).

compute_timeout(#state{timeout_millisec = T, waiting_polls = []}) ->
    T;
compute_timeout(#state{timeout_millisec = T}) ->
    T div 2.

check_reply({result, _}) ->
    ok;
check_reply({error, _}) ->
    ok.

noreply(State0) ->
    State = check_outbound(State0),
    {noreply, State, compute_timeout(State)}.

reply(Reply, State0) ->
    check_reply(Reply),
    State = check_outbound(State0),
    {reply, Reply, State, compute_timeout(State)}.

default_param(null, Default) ->
    Default;
default_param(X, _Default) ->
    X.

release_many(_Response, []) ->
    ok;
release_many(Response, [From | Rest]) ->
    gen_server:reply(From, Response),
    release_many(Response, Rest).

release_waiters(Response, State = #state{ waiting_polls = Waiters }) ->
    release_many(Response, Waiters),
    State#state{ waiting_polls = [] }.

pop_outbound(State = #state{ outbound = Outbound }) ->
    case queue:to_list(Outbound) of
        [] ->
            {[], State};
        OutboundList ->
            {OutboundList, State#state{ outbound = queue:new() }}
    end.

empty_poll_result() ->
    {result, []}.

stop_poll_result() ->
    {result, <<"stop">>}.

check_outbound(State = #state{ waiting_polls = [] }) ->
    State;
check_outbound(State0 = #state{ waiting_polls = [LuckyWaiter | Waiting] }) ->
    case pop_outbound(State0) of
        {[], State} ->
            State;
        {OutboundList, State} ->
            gen_server:reply(LuckyWaiter, {result, OutboundList}),
            release_waiters(empty_poll_result(), State#state{ waiting_polls = Waiting })
    end.

close_connection(State = #state { connection = none }) ->
    State;
close_connection(State = #state { connection = Conn, channel = Ch }) ->
    ok = amqp_channel:close(Ch),
    ok = amqp_connection:close(Conn),
    State#state{connection = none, channel = none}.

final_cleanup(RpcResponse, State0) ->
    State = #state{ waiting_rpcs = WaitingRpcs } =
        release_waiters(stop_poll_result(), check_outbound(State0)),
    release_many(RpcResponse, queue:to_list(WaitingRpcs)),
    State#state{ waiting_rpcs = queue:new() }.

method_result(Command) ->
    method_result(Command, []).

method_result(Command, ExtraFields) ->
    [Method | Args] = tuple_to_list(Command),
    {obj, [{"method", list_to_binary(atom_to_list(Method))},
           {"args", Args}
           | ExtraFields]}.

do_receive_reply(Command, State = #state{waiting_rpcs = WaitingRpcs}) ->
    case queue:out(WaitingRpcs) of
	{{value, From}, WaitingRpcsTail} ->
	    gen_server:reply(From, {result, method_result(Command)}),
	    State#state{waiting_rpcs = WaitingRpcsTail};
	{empty, _} ->
	    rabbit_log:error("Unsolicited RPC reply: ~p~n", [Command]),
	    State
    end.

do_receive_async(Method, #amqp_msg{props = Props, payload = Payload},
                 State = #state{outbound = Outbound}) ->
    ['P_basic' | PropValues] = tuple_to_list(Props),
    Result = method_result(Method,
                           [{"content", Payload},
                            {"props", amqp_to_js(PropValues)}]),
    check_outbound(State#state{outbound = queue:in(Result, Outbound)}).

enqueue_waiter(From, State = #state{ waiting_polls = Waiting }) ->
    State#state{ waiting_polls = [From | Waiting] }.

build_props(P = #'P_basic'{}) ->
    P;
build_props(Values) ->
    list_to_tuple(['P_basic' | js_to_amqp(Values)]).

js_to_amqp([]) -> [];
js_to_amqp([null | Rest]) -> [undefined | js_to_amqp(Rest)];
js_to_amqp([{obj, Fs} | Rest]) -> [js_table_to_amqp(Fs) | js_to_amqp(Rest)];
js_to_amqp([V | Rest]) -> [V | js_to_amqp(Rest)].

js_table_to_amqp([]) ->
    [];
js_table_to_amqp([{KeyStr, Value} | Rest]) ->
    case guess_js_table_value(Value) of
	{Type, AmqpValue} ->
	    [{list_to_binary(KeyStr), Type, AmqpValue} | js_table_to_amqp(Rest)];
	unknown ->
	    js_table_to_amqp(Rest)
    end.

guess_js_table_value(X) when is_binary(X) -> {longstr, X}; 
guess_js_table_value(X) when is_integer(X) -> {signedint, X};
guess_js_table_value({obj, Fs}) -> {table, js_table_to_amqp(Fs)};
guess_js_table_value(X) when is_float(X) -> {double, X};
guess_js_table_value(X) when is_boolean(X) -> {bool, X};
guess_js_table_value(_) -> unknown.

amqp_to_js([]) -> [];
amqp_to_js([undefined | Rest]) -> [null | amqp_to_js(Rest)];
amqp_to_js([T | Rest]) when is_list(T) -> [{obj, amqp_table_to_js(T)} | amqp_to_js(Rest)];
amqp_to_js([V | Rest]) -> [V | amqp_to_js(Rest)].

amqp_table_to_js([]) ->
    [];
amqp_table_to_js([{KeyBin, Type, Value} | Rest]) ->
    case guess_amqp_table_value(Type, Value) of
	{ok, V} ->
	    [{binary_to_list(KeyBin), V} | amqp_table_to_js(Rest)];
	unknown ->
	    amqp_table_to_js(Rest)
    end.

guess_amqp_table_value(longstr, X) -> {ok, X};
guess_amqp_table_value(signedint, X) -> {ok, X};
guess_amqp_table_value(table, X) -> {ok, {obj, amqp_table_to_js(X)}};
guess_amqp_table_value(double, X) -> {ok, X};
guess_amqp_table_value(bool, X) -> {ok, X};
guess_amqp_table_value(_, _) -> unknown.

build_content(none, _) ->
    none;
build_content(Bin, Props) ->
    #amqp_msg{props = build_props(Props), payload = Bin}.

cast(Method, Content, State = #state{ channel = Ch }) ->
    ok = amqp_channel:cast(Ch, Method, Content),
    State.

call(Method=#'basic.consume'{}, _Content, State = #state{ channel = Ch }) ->
    Reply = amqp_channel:subscribe(Ch, Method, self()),
    do_receive_reply(Reply, State);
call(Method, Content, State = #state{ channel = Ch }) ->
    Reply = amqp_channel:call(Ch, Method, Content),
    do_receive_reply(Reply, State).

check_invoke(MethodName, Args, Content, Props, StateBad, StateOk, Send, K) ->
    case catch list_to_existing_atom(binary_to_list(MethodName)) of
	{'EXIT', {badarg, _}} ->
	    reply(rfc4627_jsonrpc:error_response(404, "AMQP method not found",
						 {obj, [{"method", MethodName},
							{"args", Args}]}),
		  StateBad);
	'channel.close' ->
	    %% Forbid client-originated channel.close. We wrap
	    %% channel.close in our own close method.
	    reply(rfc4627_jsonrpc:error_response(405, "AMQP method not allowed",
						 {obj, [{"method", MethodName}]}),
		  StateBad);
	MethodAtom ->
            Method = list_to_tuple([MethodAtom | js_to_amqp(Args)]),
            Content1 = build_content(default_param(Content, none),
                                     default_param(Props, #'P_basic'{})),
	    K(Send(Method, Content1, StateOk))
    end.

%---------------------------------------------------------------------------

init([Oid, [Username, Password, SessionTimeout0, VHostPath0]]) ->
    SessionTimeout = default_param(SessionTimeout0, 10),
    {ok, DefaultVHost} = application:get_env(default_vhost),
    VHostPath = default_param(VHostPath0, DefaultVHost),

    rabbit_log:info("HTTP Channel started, timeout ~p~n", [SessionTimeout]),
    SessionTimeoutMs = SessionTimeout * 1000,

    AdapterInfo = #amqp_adapter_info{protocol = {'JSON-RPC', "1.0"}},
    Params = #amqp_params_direct{username     = Username,
                                 password     = Password,
                                 virtual_host = VHostPath,
                                 adapter_info = AdapterInfo},
    {ok, Conn} = amqp_connection:start(Params),
    {ok, Ch} = amqp_connection:open_channel(Conn),
    %% The test suite basic.cancels a tag that does not exist. That is allowed
    %% but we need a default consumer for the cancel_ok.
    ok = amqp_selective_consumer:register_default_consumer(Ch, self()),
    {ok,
     #state{channel = Ch,
            connection = Conn,
            oid = Oid,
            vhost = VHostPath,
            timeout_millisec = SessionTimeoutMs,
            waiting_rpcs = queue:new(),
            waiting_polls = [],
            outbound = queue:new()},
     SessionTimeoutMs}.

handle_call({jsonrpc, <<"poll">>, _RequestInfo, []}, From, State) ->
    noreply(enqueue_waiter(From, State));
handle_call({jsonrpc, <<"close">>, _RequestInfo, []}, _From, State0) ->
    %%% FIXME: somehow propagate some of the args into the close reason given to other callers?
    rabbit_log:info("HTTP Channel closing by request.~n"),
    {OutboundList, State} = pop_outbound(State0),
    {stop, normal, {result, OutboundList}, close_connection(State)};
handle_call({jsonrpc, <<"call">>, _RequestInfo, [Method, Args]}, From,
            State = #state{waiting_rpcs = WaitingRpcs}) ->
    check_invoke(Method, Args, none, #'P_basic'{}, State,
                 State#state{waiting_rpcs = queue:in(From, WaitingRpcs)},
                 fun call/3,
                 fun noreply/1);
handle_call({jsonrpc, <<"cast">>, _RequestInfo, [Method, Args, Content, Props]}, _From, State) ->
    check_invoke(Method, Args, Content, Props, State, State,
                 fun cast/3,
                 fun (State1) -> reply({result, []}, State1) end);
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p", [?MODULE, Request]),
    reply({result, not_supported}, State).

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    noreply(State).

handle_info(timeout, State = #state{waiting_polls = []}) ->
    rabbit_log:info("HTTP Channel timed out, closing.~n"),
    {stop, normal, State};
handle_info(timeout, State) ->
    noreply(release_waiters(empty_poll_result(), State));
handle_info(shutdown, State) ->
    rabbit_log:info("HTTP Channel writer shutdown requested.~n"),
    %% We're going to close pretty soon anyway. No special action needed here.
    noreply(State);
handle_info(#'channel.close'{reply_code = ErrorCode,
                             reply_text = ErrorText,
                             method_id = MethodId}, State) ->
    handle_close(channel, ErrorCode, ErrorText, MethodId, State);
handle_info(#'connection.close'{reply_code = ErrorCode,
                                reply_text = ErrorText,
                                method_id = MethodId}, State) ->
    handle_close(connection, ErrorCode, ErrorText, MethodId, State);
handle_info(#'basic.consume_ok'{}, State) ->
    noreply(State);
handle_info(#'basic.cancel_ok'{}, State) ->
    noreply(State);
handle_info({Method, Content}, State) ->
    noreply(do_receive_async(Method, Content, State));
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p", [?MODULE, Info]),
    noreply(State).

handle_close(Type, ErrorCode, ErrorText, MethodId, State) ->
    Detail = {obj, [{type, Type},
                    {code, list_to_binary(atom_to_list(ErrorCode))},
                    {text, list_to_binary(ErrorText)},
                    {method, list_to_binary(atom_to_list(MethodId))}]},
    {stop, normal, final_cleanup(
                     rfc4627_jsonrpc:error_response(500, "AMQP error", Detail),
                     State)}.

terminate(_Reason, State) ->
    _State1 = final_cleanup(rfc4627_jsonrpc:error_response(504, "Closed", null),
                            close_connection(State)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
