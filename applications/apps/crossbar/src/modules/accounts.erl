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
-module(accounts).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.accounts">>, {Params, RD, Context}}, State) ->
    spawn(fun() ->
                  Context1 = validate(wrq:method(RD), Params, Context),
                  Pid ! {binding_result, true, {Params, RD, Context1}}
	  end),
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%--------------------------------------------------------------------
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.validate.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.accounts">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec(validate/3 :: (Verb :: atom(), Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate('GET', [], Context) ->
    Context;
validate('PUT', [], #cb_context{req_data={struct,Data}}=Context) ->
    case is_valid_doc(Data) of
        false ->
            cb_error("Please provide a name", 400, Context);
        true ->
            Context
    end;
validate('GET', [DocId], Context) ->
    case get_public_doc(DocId) of
        {error, not_found} ->
            cb_error("Account not found", 404, Context);
        {error, db_not_reachable} ->
            cb_error("Could not connect", 503, Context);
	Doc ->
            Context#cb_context{doc=Doc, resp_data=Doc}
    end;
validate('POST', [DocId], #cb_context{req_data={struct,Data}}=Context) ->
    io:format("~p~n", [is_valid_doc(Data)]),
    case is_valid_doc(Data) andalso get_public_doc(DocId) of
        false ->
            cb_error("Please provide a name", 400, Context);
        {error, not_found} ->
            cb_error("Account not found", 404, Context);
        {error, db_not_reachable} ->
            cb_error("Could not connect", 503, Context);
	Doc ->
            Context#cb_context{doc=Doc, resp_data=Doc}
    end;
validate('DELETE', [DocId], Context) ->
    case get_public_doc(DocId) of
        {error, not_found} ->
            cb_error("Account not found", 404, Context);
        {error, db_not_reachable} ->
            cb_error("Could not connect", 503, Context);
	Doc ->
            Context#cb_context{doc=Doc, resp_data=Doc}
    end;
validate(_, _, Context) ->
    cb_error("Account failed validation", 500, Context).

is_valid_doc(Data) ->
    proplists:get_value(<<"name">>, Data, false) =/= false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), [])).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, <<"parents">>]) ->
    {true, ['GET', 'DELETE']};
allowed_methods([_, Path]) ->
    Valid = lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>]),
    {Valid, ['GET']};
allowed_methods([_, <<"parents">>, _]) ->
    {true, ['PUT']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists([_, Path]) ->
    Valid = lists:member(Path, [<<"parents">>, <<"ancestors">>, <<"children">>, <<"descendants">>]),
    {Valid, []};
resource_exists([_, <<"parents">>, _]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convience wrapper for setting the error resp fields in the context
%% record
%% @end
%%--------------------------------------------------------------------
-spec(cb_error/2 :: (Msg :: string(), Context :: #cb_context{}) -> #cb_context{}).
cb_error(Msg, Context) ->
    cb_error(Msg, 500, error, Context).
cb_error(Msg, Code, Context) ->
    cb_error(Msg, Code, error, Context).
cb_error(Msg, Code, Error, Context) ->
    Context#cb_context {
         resp_status = Error
	,resp_error_msg = Msg
	,resp_error_code = Code
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the private version of the account doc specified
%% by DocId
%% @end
%%--------------------------------------------------------------------
-spec(get_private_doc/1 :: (DocId :: term()) -> json_object() | tuple(error, string())).
get_private_doc(DocId) when is_binary(DocId) ->
    try
        case couch_mgr:open_doc("accounts", DocId) of
            {error, Error} ->       
                {error, Error};
            Doc ->
                Str = couchbeam_util:json_encode({Doc}),
		{struct, Json} = mochijson2:decode(Str),
		%% return any key that starts with _ or pvt_
                lists:filter(fun({K, _}) -> binary:first(K) == 95 orelse binary:bin_to_list(K, 0, 4) == "pvt_" end, Json)
        end
    catch
        Fault:Reason ->
            {error, {Fault, Reason}}
    end;
get_private_doc(DocId) ->
    get_private_doc(whistle_util:to_binary(DocId)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the public version of the account doc specified
%% by DocId
%% @end
%%--------------------------------------------------------------------
-spec(get_public_doc/1 :: (DocId :: term()) -> json_object() | tuple(error, string())).
get_public_doc(DocId) when is_binary(DocId) ->
    try
	case couch_mgr:open_doc("accounts", DocId) of
	    {error, Error} ->	    
		{error, Error};
	    Doc ->
		Str = couchbeam_util:json_encode({Doc}),
		{struct, Json} = mochijson2:decode(Str),
		%% remove any key that starts with _ or pvt_
		[{<<"id">>, DocId}] ++ lists:filter(fun({K, _}) -> binary:first(K) =/= 95 andalso binary:bin_to_list(K, 0, 4) =/= "pvt_" end, Json)
	end
    catch
	Fault:Reason ->
	    {error, {Fault, Reason}}
    end;
get_public_doc(DocId) ->
    get_public_doc(whistle_util:to_binary(DocId)).