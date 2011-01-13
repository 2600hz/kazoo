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

-export([http_put/3, http_get/3, http_post/3, http_delete/3]).
-export([create/2, read/2, read/3, update/3, delete/2]).

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

http_put(DocId, Params, RD) ->
    create([{<<"id">>, list_to_binary(DocId)}], RD).

http_get(DocId, Params, RD) ->
    read(DocId, Params, RD).

http_post(DocId, Params, RD) ->
    update(DocId, Params, RD).

http_delete(DocId, Params, RD) ->
    delete(Params, RD).

create(Params, RD) ->
    gen_server:cast(?SERVER, {create, Params, RD, self()}).

read(Params, RD) ->    
    {struct, Data} = proplists:get_value(<<"data">>, Params),
    read(proplists:get_value(<<"_id">>, Data), Params, RD).

read(DocId, _Params, _RD) ->
    gen_server:cast(?SERVER, {read, self(), whistle_util:to_binary(DocId)}).

update(DocID, Params, _RD) ->
    {struct, Data} = proplists:get_value(<<"data">>, Params),
    gen_server:cast(?SERVER, {update, self(), DocID, Data}).

delete(Params, RD) ->
    gen_server:cast(?SERVER, {delete, Params, RD, self()}).

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
    bind_to_crossbar(),
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
handle_cast({create, Params, RD, Pid}, State) ->
    Pid ! [{status, success}, {data, [{method, create}]}],
    {noreply, State};

handle_cast({read, Pid, DocId}, State) ->
    case couch_mgr:open_doc("accounts", whistle_util:to_binary(DocId)) of
	{error, not_found} ->
	    Pid ! [
		    {status, error}
		   ,{data, []}
		   ,{message, "Could not locate account"}
		   ,{error, 404}
		   ];
	{error, db_not_reachable} ->
	    Pid ! [
		    {status, error}
		   ,{data, []}
		   ,{message, "Could not connect to datastore"}
		   ,{error, 504}
		   ];
	Doc ->
	    Str = couchbeam_util:json_encode({Doc}),
	    {struct, Json} = mochijson2:decode(Str),
	    Pid ! [
		    {status, success}
		   ,{data, Json}
		   ]	    
    end,
    {noreply, State};

handle_cast({update, Pid, DocId, Data}, State) ->
    case couch_mgr:save_doc("accounts", Data) of
	{ok, Doc} -> 
	    Pid ! [
		    {status, success}
		   ,{data, Doc}
		   ];
	{error, conflict} ->
	    Pid ! [
		    {status, error}
		   ,{data, []}
		   ,{message, "Document is stale"}
		   ,{error, 409}
		   ]
    end,
    {noreply, State};

handle_cast({delete, Params, RD, Pid}, State) ->
    Pid ! [{status, success}, {data, [{method, delete}]}],
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Unhandled ~p", [_Msg]),
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
handle_info({binding_fired, Pid, <<"account.validate">>, Payload}, State) ->
    spawn(fun() ->                  
		  {Result, Payload1} = validate(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.authenticate">>, Payload}, State) ->
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.authorize">>, Payload}, State) ->
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info({binding_fired, Pid, Route, Payload}, State) ->
    format_log(info, "ACCOUNT(~p): unhandled binding: ~p~n~p~n", [self(), Route, Payload]),
    Pid ! {binding_result, false, []},
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
    crossbar_bindings:bind(<<"account.authenticate">>),
    crossbar_bindings:bind(<<"account.authorize">>),
    crossbar_bindings:bind(<<"account.validate">>).

validate({http_get, [_DocID, _Params]}) ->
    {true, []};

validate({read, [_DocId, _Params]}) ->
    {true, []};

validate({read, [Params]}) when is_list(Params) ->
    io:format("~p~n", [Params]),
    {struct, Data} = proplists:get_value(<<"data">>, Params),
    case proplists:get_value(<<"_id">>, Data) of
	undefined ->			    
	    {false, [<<"_id">>]};
	_ ->                    
	    {true, []}
    end;

validate(_Params) ->
    io:format("~p~n", [_Params]),
    {false, []}.
