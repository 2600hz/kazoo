%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Account module
%%%
%%% Store/retrieve media files
%%%
%%% @end
%%% Created : Mar 8 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(media).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(BIN_DATA, <<"raw">>).

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
-spec(init/1 :: (_) -> tuple(ok, ok)).
init([]) ->
    bind_to_crossbar(),
    {ok, ok}.

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
handle_call(_, _, S) ->
    {reply, ok, S}.

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
handle_cast(_, S) ->
    {noreply, S}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.media">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.media">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
     	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Pid ! {binding_result, true, [RD, Context, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, _Route, Payload}, State) ->
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info(_Info, State) ->
    format_log(info, "MEDIA(~p): unhandled info ~p~n", [self(), _Info]),
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
-spec(bind_to_crossbar/0 :: () ->  ok | tuple(error, exists)).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.media">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.media">>),
    crossbar_bindings:bind(<<"v1_resource.validate.media">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.media">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/media => Paths == []
%% /account/{AID}/media/{CallID} => Paths = [<<CallID>>]
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_MediaID]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_MediaID, ?BIN_DATA]) ->
    {true, ['GET']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/media => Paths == []
%% /account/{AID}/media/{CallID} => Paths = [<<CallID>>]
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([_MediaID]) ->
    {true, []};
resource_exists([_MediaID, ?BIN_DATA]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec(validate/2 :: (Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    crossbar_util:response([{struct, [{<<"id">>, <<"test1234">>}, {<<"display_name">>, <<"Test 1234">>}, {<<"media_format">>, <<"ogg">>}]}], Context);

validate([], #cb_context{req_verb = <<"put">>, req_data=Data}=Context) ->
    Name = whapps_json:get_value(<<"display_name">>, Data),
    
    case lookup_media_by_name(Name) of
	{error, not_found} ->
	    create_media(Name, Data);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end;

validate([MediaID], #cb_context{req_verb = <<"get">>}=Context) ->
    case lookup_media_by_id(MediaID) of
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(MediaID, Context);
	MediaDoc ->
	    Context#cb_context{resp_status=success, doc=MediaDoc}
    end;

validate([MediaID], #cb_context{req_verb = <<"post">>, req_data=_Data}=Context) ->
    case lookup_media_by_id(MediaID) of
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(MediaID, Context);
	MediaDoc ->
	    %% need to update
	    Context#cb_context{resp_status=success, doc=MediaDoc}
    end;

validate([MediaID], #cb_context{req_verb = <<"delete">>, req_data=_Data}=Context) ->
    case lookup_media_by_id(MediaID) of
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(MediaID, Context);
	_MediaDoc ->
	    %% need to delete doc
	    Context#cb_context{resp_status=success, doc=[]}
    end;

validate([MediaID, ?BIN_DATA], #cb_context{req_verb = <<"get">>, req_data=_Data}=Context) ->
    case lookup_media_by_id(MediaID) of
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(MediaID, Context);
	MediaDoc ->
	    %% need to stream attachment
	    Context#cb_context{resp_status=success, doc=MediaDoc}
    end;

validate(Params, #cb_context{req_verb=Verb, req_nouns=Nouns, req_data=D}=Context) ->
    format_log(info, "Media.validate: P: ~p~nV: ~s Ns: ~p~nData: ~p~nContext: ~p~n", [Params, Verb, Nouns, D, Context]),
    crossbar_util:response_faulty_request(Context).

create_media(_Name, _Data) ->
    ok.

lookup_media_by_id(_MediaID) ->
    ok.

lookup_media_by_name(_MediaName) ->
    ok.
