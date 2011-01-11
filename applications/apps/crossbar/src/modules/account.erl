%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(account).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([index/2, create/2, read/2, update/2, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

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

index(Params, RD) ->
    case wrq:method(RD) of
	      'PUT' -> 
		    create(Params, RD);
	      'GET' ->
		    read(Params, RD);
	      'POST' ->
		    update(Params, RD);
	      'DELETE' ->
		    delete(Params, RD);
	      _ ->
		    self() ! [{status, error}, {error, 417}, {message, <<"CRUD!">>}]
    end.

read(Params, RD) ->
    gen_server:cast({local, ?SERVER}, {read, Params, RD, self()}).

create(Params, RD) ->
    gen_server:cast({local, ?SERVER}, {create, Params, RD, self()}).
    
update(Params, RD) ->
    gen_server:cast({local, ?SERVER}, {update, Params, RD, self()}).

delete(Params, RD) ->
    gen_server:cast({local, ?SERVER}, {delete, Params, RD, self()}).

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
    format_log(info, "ACCOUNT(~p): Its alive, alive!!!~n", [self()]),
    bind_to_crossbar(),
    webmachine_router:add_route({["account"], account_resource, []}),
    webmachine_router:add_route({["account", request, '*'], account_resource, []}),
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
handle_cast({read, Params, RD, Pid}, State) ->
    Id = proplists:get_value("id", Params),
    Doc = couch_mgr:open_doc("ts", list_to_binary(Id)),
    Str = couchbeam_util:json_encode({Doc}),
    {struct, Json} = mochijson2:decode(Str),
    Pid ! [{status, success}, {data, Json}];

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
handle_info({binding_fired, Pid, <<"account.content_types_provided">>, _Payload}, State) ->
    Reply = [
	      {to_text, ["text/plain"]}
             ,{to_html, ["text/html"]}
	     ,{to_xml, ["application/xml"]}
             ,{to_json, ["application/json", "application/x-json"]}
	    ],
    Pid ! {binding_result, true, Reply},
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.allowed_methods">>, _Payload}, State) ->
    Reply = ['POST', 'GET', 'PUT'],
    Pid ! {binding_result, true, Reply},
    {noreply, State};

handle_info({binding_fired, Pid, Route, Payload}, State) ->
    format_log(info, "ACCOUNT(~p): unhandled binding: ~p~n", [self(), Route]),
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info(_Info, State) ->
    format_log(info, "ACCOUNT(~p): unhandled info ~p~n", [self(), _Info]),
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
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"account.#">>).
