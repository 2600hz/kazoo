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

-export([add/1, rm/1, clear/1, status/1, poll/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).

%% { auth_token, queue_name, [{<<"route.one">>, Options}, {<<"route.two.#">>, Options}]}
-type client_sub() :: tuple(term(), binary(), list(tuple(binary(), term()))).

-record(state, {client_subs = [] :: list(client_sub())}).

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

-spec(add/1 :: (Request :: list(tuple(binary(), binary()))) -> list(tuple(binary(), term()))).
add(Request) -> [<<"add">>, <<"me">>].
rm(Request) -> "rm".
clear(Request) -> "clear".
status(Request) -> "status".
poll(Request) -> "poll".

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
handle_info({binding_fired, Pid, Route, Payload}, State) ->
    format_log(info, "EVTSUB(~p): binding: ~p~n", [self(), Route]),
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
start_bindings() ->
    crossbar_bindings:bind(<<"evtsub.#">>),        %% all evtsub events
    crossbar_bindings:bind(<<"session.destroy">>). %% session destroy events
