%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Event Subscription module
%%%
%%% Handle client requests for binding to AMQP messages (like call events).
%%%
%%% @end
%%% Created : 13 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(evtsub).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([dispatch_config/0, set_amqp_host/1]).

-export([add/1, rm/1, clear/1, status/1, poll/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_RESPONSE_SIZE, 100).

%% { account_id, queue_name, [{{<<"exchange">>, <<"binding_data">>}, Options}]}
-type client_sub() :: tuple(term(), binary(), list(tuple(tuple(binary(), binary()), term()))).

-record(state, {
	  amqp_host = "" :: string()
	 ,client_subs = [] :: list(client_sub())
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

%% Define this function if you want to dynamically add a dispatch rule to webmachine
%% for calls to webmachine_router:add_route/1
dispatch_config() ->
    {["evtsub", request, '*'], evtsub_resource, []}.

set_amqp_host(H) ->
    gen_server:cast(?SERVER, {set_amqp_host, H}).

%% Exposed API calls can return one of three results:
%% {success | error | fatal, data proplist}
%% {success | error | fatal, data proplist, message}
%% {success | error | fatal, data proplist, message, error code}
%% AKA the crossbar_module_result() type
-spec(add/1 :: (ReqParams :: proplist()) -> crossbar_module_result()).
add(ReqParams) -> gen_server:call(?SERVER, {add, ReqParams}).

-spec(rm/1 :: (ReqParams :: proplist()) -> crossbar_module_result()).
rm(ReqParams) -> gen_server:call(?SERVER, {rm, ReqParams}).

-spec(clear/1 :: (ReqParams :: proplist()) -> crossbar_module_result()).
clear(ReqParams) -> gen_server:call(?SERVER, {clear, ReqParams}).

-spec(status/1 :: (ReqParams :: proplist()) -> crossbar_module_result()).
status(ReqParams) -> gen_server:call(?SERVER, {status, ReqParams}).

-spec(poll/1 :: (ReqParams :: proplist()) -> crossbar_module_result()).
poll(ReqParams) -> gen_server:call(?SERVER, {poll, ReqParams}).

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
-spec(init/1 :: (_) -> tuple(ok, #state{})).
init([]) ->
    amqp_util:module_info(), %% loads all exported function names into the atoms table - for list_to_existing_atom/1
    start_bindings(),
    {ok, #state{}}.

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
handle_call({add, ReqParams}, _, #state{amqp_host=H, client_subs=Subs}=State) ->
    Client = props:get_value(<<"account_id">>, ReqParams),
    {struct, Data} = props:get_value(<<"data">>, ReqParams),
    Exchange = props:get_value(<<"exchange">>, Data),
    Binding = props:get_value(<<"binding">>, Data),
    MaxRespSize = case whistle_util:to_integer(props:get_value(<<"max_response_size">>, Data, ?DEFAULT_RESPONSE_SIZE)) of
		      V when V > ?DEFAULT_RESPONSE_SIZE -> ?DEFAULT_RESPONSE_SIZE;
		      V1 -> V1
		  end,
    ClientSub = case lists:keyfind(Client, 1, Subs) of
		    false -> new_sub(Client, Exchange, Binding, MaxRespSize, H);
		    CSub -> edit_sub(CSub, Exchange, Binding, MaxRespSize, H)
		end,
    Bindings = client_bindings(ClientSub),
    {reply, {success, [{<<"subscribed">>, Bindings}]}, State#state{client_subs=[ClientSub | lists:keydelete(Client, 1, Subs)]}};
handle_call({rm, ReqParams}, _, #state{amqp_host=H, client_subs=Subs}=State) ->
    Client = props:get_value(<<"account_id">>, ReqParams),
    {struct, Data} = props:get_value(<<"data">>, ReqParams),
    Exchange = props:get_value(<<"exchange">>, Data),
    Binding = props:get_value(<<"binding">>, Data),
    case lists:keyfind(Client, 1, Subs) of
	false ->
	    {reply, {error, [], "No client found"}, State};
	ClientSub ->
	    case rm_sub(ClientSub, Exchange, Binding, H) of
		{_, _, []} -> {reply, {success, [], "All bindings cleared"}, State#state{client_subs=lists:keydelete(Client, 1, Subs)}};
		ClientSub1 ->
		    Bindings = client_bindings(ClientSub1),
		    {reply, {success, [{<<"subscribed">>, Bindings}]}, State#state{client_subs=[ClientSub1 | lists:keydelete(Client, 1, Subs) ]}}
	    end
    end;
handle_call({clear, ReqParams}, _, #state{amqp_host=H, client_subs=Subs}=State) ->
    Client = props:get_value(<<"account_id">>, ReqParams),
    case lists:keyfind(Client, 1, Subs) of
	false ->
	    {reply, {error, [], "No client found"}, State};
	ClientSub ->
	    clear_sub(ClientSub, H),
	    {reply, {success, [{<<"subscribed">>, []}], "All bindings cleared"}, State#state{client_subs=lists:keydelete(Client, 1, Subs)}}
    end;
handle_call({status, ReqParams}, _, #state{client_subs=Subs}=State) ->
    Client = props:get_value(<<"account_id">>, ReqParams),
    case lists:keyfind(Client, 1, Subs) of
	false ->
	    {reply, {error, [], "No client found"}, State};
	ClientSub ->
	    Bindings = client_bindings(ClientSub),
	    {reply, {success, [{<<"subscribed">>, Bindings}]}, State}
    end;
handle_call({poll, ReqParams}, _, #state{client_subs=Subs, amqp_host=H}=State) ->
    Client = props:get_value(<<"account_id">>, ReqParams),
    case lists:keyfind(Client, 1, Subs) of
	false ->
	    {reply, {error, [], "No client found"}, State};
	ClientSub ->
	    Events = client_events(ClientSub, H),
	    {reply, {success, [{<<"events">>, Events}]}, State}
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
handle_cast({set_amqp_host, H}, #state{amqp_host=_OH}=State) ->
    {noreply, State#state{amqp_host=H}};
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
handle_info({binding_fired, Pid, <<"evtsub.init">>=Route, Payload}, State) ->
    format_log(info, "EVTSUB(~p): binding: ~p~n", [self(), Route]),
    InitOpts = [{amqp_host, whapp_default_amqp}
		,{content_types_provided, [{to_json, ["application/json", "application/x-json"]}
					   ,{to_html, ["text/html"]}
					   ,{to_text, ["text/plain"]}
					   ,{to_xml, ["application/xml"]}
					  ]
		 }
		,{content_types_accepted, [{from_json, ["application/json", "application/x-json"]}
					   ,{from_html, ["text/html"]}
					   ,{from_text, ["text/plain"]}
					   ,{from_xml, ["application/xml"]}
					  ]
		 }
	       ],
    Opts = lists:umerge(Payload, InitOpts),
    Pid ! {binding_result, true, Opts},
    {noreply, State};
handle_info({binding_fired, Pid, <<"evtsub.authenticate">>=Route, {#session{account_id=Acct}=_S, _Params}=Payload}, State) ->
    format_log(info, "EVTSUB(~p): binding: ~p acct: ~p~n", [self(), Route, Acct]),
    Pid ! {binding_result, (not (Acct =:= <<>>)), Payload},
    {noreply, State};
handle_info({binding_fired, Pid, <<"evtsub.authorize">>=Route, {#session{account_id=Acct}=_S, _Params}=Payload}, State) ->
    format_log(info, "EVTSUB(~p): binding: ~p acct: ~p~n", [self(), Route, Acct]),
    Pid ! {binding_result, true, Payload},
    {noreply, State};
handle_info({binding_fired, Pid, <<"evtsub.validate">>=Route, Payload}, State) ->
    format_log(info, "EVTSUB(~p): binding: ~p~n", [self(), Route]),
    spawn(fun() ->
		  {Result, Payload1} = validate(Payload),
		  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};
handle_info({binding_fired, Pid, <<"session.destroy">>=Route, #session{account_id=Acct}=S}, #state{client_subs=Subs, amqp_host=H}=State) ->
    format_log(info, "EVTSUB(~p): binding: ~p~n", [self(), Route]),
    spawn(fun() -> clear_sub(lists:keyfind(Acct, 1, Subs), H) end), %crashes if Acct isn't found, no biggie
    Pid ! { binding_result, true, S },
    {noreply, State#state{client_subs=lists:keydelete(Acct, 1, Subs)}};
handle_info({binding_fired, Pid, Route, Payload}, State) ->
    format_log(info, "EVTSUB(~p): unhandled binding: ~p~n", [self(), Route]),
    Pid ! {binding_result, true, Payload},
    {noreply, State};
handle_info({binding_flushed, _B}, State) ->
    format_log(info, "EVTSUB(~p): binding ~p flushed~n", [self(), _B]),
    start_bindings(),
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "EVTSUB(~p): unhandled info ~p~n", [self(), _Info]),
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
-spec(start_bindings/0 :: () -> no_return()).
start_bindings() ->
    crossbar_bindings:bind(<<"evtsub.#">>),        %% all evtsub events
    crossbar_bindings:bind(<<"session.destroy">>). %% session destroy events

-spec(validate/1 :: (tuple(string(), proplist())) -> tuple(boolean(), list())).
validate({"add", Params}) ->
    Req = [{<<"auth_token">>, fun erlang:is_binary/1}
	   ,{<<"data">>, [{<<"binding">>, fun erlang:is_binary/1}
			  ,{<<"exchange">>, fun(E) ->
						    try 
							format_log(info, "util fun: ~p~n", [binary_to_list(<<E/binary, "_exchange">>)]),
							F = list_to_existing_atom(binary_to_list(<<E/binary, "_exchange">>)),
							erlang:function_exported(amqp_util, F, 1)
						    catch
							_:_ -> false
						    end
					    end}
			  ,{<<"max_response_size">>, fun(X) -> try whistle_util:to_integer(X), true catch _:_ -> false end end, optional}
			 ]}
	  ],
    validate(Params, Req);
validate({"rm", Params}) ->
    Req = [{<<"auth_token">>, fun erlang:is_binary/1}
	   ,{<<"data">>, [{<<"binding">>, fun erlang:is_binary/1}
			  ,{<<"exchange">>, fun(E) ->
						    F = list_to_existing_atom(binary_to_list(<<E/binary, "_exchange">>)),
						    erlang:function_exported(amqp_util, F, 1)
					    end}
			  ,{<<"flush">>, fun(<<"true">>) -> true; (<<"false">>) -> true; (_) -> false end, optional}
			 ]}
	  ],
    validate(Params, Req);
validate({"clear", Params}) ->
    Req = [{<<"auth_token">>, fun erlang:is_binary/1}
	   ,{<<"data">>, [
			  {<<"flush">>, fun(<<"true">>) -> true; (<<"false">>) -> true; (_) -> false end, optional}
			 ]}
	  ],
    validate(Params, Req);
validate({"status", Params}) ->
    Req = [{<<"auth_token">>, fun erlang:is_binary/1}
	   ,{<<"data">>, fun(_) -> true end}
	  ],
    validate(Params, Req);
validate({"poll", Params}) ->
    Req = [{<<"auth_token">>, fun erlang:is_binary/1}
	   ,{<<"data">>, fun(_) -> true end}
	  ],
    validate(Params, Req);
validate(_) ->
    {false, [<<"unknown_request">>]}.

-spec(validate/2 :: (Params :: proplist(), Req :: proplist()) -> tuple(boolean(), proplist())).
validate(Params, Req) ->
    Scanned = lists:map(fun(R) -> crossbar_util:param_exists(Params, R) end, Req),
    Failed = lists:foldl(fun crossbar_util:find_failed/2, [], Scanned),
    {Failed =:= [], Failed}.

-spec(new_sub/5 :: (Client :: binary(), Exchange :: binary(), Binding :: binary(), MaxRespSize :: integer(), H :: string()) -> client_sub()).
new_sub(Client, Exchange, Binding, MaxRespSize, H) ->
    Q = amqp_util:new_queue(H, <<>>, [{durable, true}]),
    F = list_to_existing_atom(binary_to_list(<<"bind_q_to_", Exchange/binary>>)),
    amqp_util:F(H, Q, Binding), % bind our queue
    {Client, Q, [{{Exchange, Binding}, MaxRespSize}]}.

-spec(edit_sub/5 :: (ClientSub :: client_sub(), Exchange :: binary(), Binding :: binary(), MaxRespSize :: integer(), H :: string()) -> client_sub()).
edit_sub({Client, Q, Bindings}=ClientSub, Exchange, Binding, MaxRespSize, H) ->
    case lists:keyfind({Exchange, Binding}, 1, Bindings) of
	false ->
	    F = list_to_existing_atom(binary_to_list(<<"bind_q_to_", Exchange/binary>>)),
	    amqp_util:F(H, Q, Binding), % bind our queue
	    {Client, Q, [{{Exchange, Binding}, MaxRespSize} | Bindings]};
	{_, _} -> ClientSub
    end.

-spec(rm_sub/4 :: (ClientSub :: client_sub(), Exchange :: binary(), Binding :: binary(), H :: string()) -> client_sub()).
rm_sub({Client, Q, Bindings}=ClientSub, Exchange, Binding, H) ->
    case lists:keyfind({Exchange, Binding}, 1, Bindings) of
	false -> ClientSub;
	{_,_} ->
	    F = list_to_existing_atom(binary_to_list(<<"unbind_q_from_", Exchange/binary>>)),
	    amqp_util:F(H, Q, Binding),
	    {Client, Q, lists:keydelete({Exchange, Binding}, 1, Bindings)}
    end.

-spec(clear_sub/2 :: (ClientSub :: client_sub(), H :: string()) -> no_return()).
clear_sub({_Client, Q, Bindings}=ClientSub, H) ->
    lists:foreach(fun({E,B}) -> rm_sub(ClientSub, E, B, H) end, Bindings),
    amqp_util:delete_queue(H, Q).

-spec(client_bindings/1 :: (client_sub()) -> list(list())).				
client_bindings({_, _, Bindings}) -> lists:map(fun({{E,B},_}) -> [E, B] end, Bindings).

client_events({_,_,[]}, _) -> [];
client_events({_,Q,B}, H) ->
    Self = self(),
    MaxResp = lists:foldl(fun({_,X}, Y) when X > Y -> X; (_, Y) -> Y end, 0, B),
    spawn(fun() ->
		  amqp_util:basic_consume(H, Q),
		  receive_events(Self, MaxResp, 0, [])
		  %% look to basic.qos{prefetch_count} to pull specified # of messages
	  end),
    receive
	{events, Events} -> {ok, Events}
    after
	2000 -> {error, timeout}
    end.

receive_events(Pid, Max, Max, Evts) ->
    Pid ! {events, Evts};
receive_events(Pid, Max, Cur, Evts) ->
    receive
	AmqpMsg -> receive_events(Pid, Max, Cur+1, [AmqpMsg | Evts])
    after
	2000 -> receive_events(Pid, Max, Max, Evts)
    end.
