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
-define(VIEW_FILE, <<"media_doc.json">>).

-define(METADATA_FIELDS, [<<"display_name">>, <<"description">>, <<"media_type">>
			      ,<<"status">>, <<"length">>, <<"size">>
			      ,<<"streamable">>, <<"format">>, <<"sample">>
			]). % until validation is in place

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
    accounts:update_all_accounts(?VIEW_FILE),
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
		  {Context1, Resp} = case Context#cb_context.resp_status =:= success of
					 true -> {crossbar_doc:save(Context), true};
					 false -> {Context, false}
				     end,
		  Pid ! {binding_result, Resp, [RD, Context1, Params]}
     	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  case props:get_value(<<"Location">>, Context#cb_context.resp_headers) of
		      undefined ->
			  {Context1, Resp} = case create_media_meta(Context#cb_context.req_data, Context) of
						 #cb_context{resp_status=success, resp_data=RespData}=Context2 ->
						     DocID = whapps_json:get_value(<<"id">>, RespData),
						     {Context2#cb_context{resp_data=[], resp_headers=[{"Location", DocID} | Context2#cb_context.resp_headers]}, true};
						 Context3 ->
						     format_log(info, "MEDIA.v.PUT: ERROR~n", []),
						     {Context3, false}
					     end,
			  Pid ! {binding_result, Resp, [RD, Context1, Params]};
		      _ ->
			  {Context, true}
		  end
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  crossbar_util:binding_heartbeat(Pid),
		  format_log(info, "MEDIA: del ~p: ~p~n", [Params, Context#cb_context.doc]),
		  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, Context1#cb_context.resp_status =:= success, [RD, Context1, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"accounts.created">>, _}, State) ->
    Pid ! {binding_result, true, ?VIEW_FILE},
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
-spec(bind_to_crossbar/0 :: () ->  no_return()).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.media">>),
    _ = crossbar_bindings:bind(<<"account.created">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/media => Paths == []
%% /account/{AID}/media/{MediaID} => Paths = [<<MediaID>>]
%% /account/{AID}/media/{MediaID}/raw => Paths = [<<"MediaID">>, <<"raw">>]
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
%% /account/{AID}/media/{MediaID} => Paths = [<<<MediaID>>]
%% /account/{AID}/media/{MediaID}/raw => Paths = [<<"MediaID">>, <<"raw">>]
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
    lookup_media(Context);

validate([], #cb_context{req_verb = <<"put">>, req_data=Data}=Context) ->
    Name = whapps_json:get_value(<<"display_name">>, Data),

    case Name =/= undefined andalso lookup_media_by_name(Name, Context) of
	false ->
	    crossbar_util:response_invalid_data([<<"display_name">>], Context);
	#cb_context{resp_status=success, doc=[{struct, _}=Doc|_]}=Context1 ->
	    DocID = whapps_json:get_value(<<"id">>, Doc),
	    Context1#cb_context{resp_headers=[{"Location", DocID} | Context1#cb_context.resp_headers]};
	_ ->
	    Context#cb_context{resp_status=success}
    end;

validate([MediaID], #cb_context{req_verb = <<"get">>}=Context) ->
    get_media_doc(MediaID, Context);

validate([MediaID], #cb_context{req_verb = <<"post">>, req_data=Data}=Context) ->
    case whapps_json:get_value(<<"display_name">>, Data) =/= undefined of
	true ->
	    crossbar_doc:load_merge(MediaID, Data, Context);
	false ->
	    crossbar_util:response_invalid_data([<<"display_name">>], Context)
    end;

validate([MediaID], #cb_context{req_verb = <<"delete">>, req_data=_Data}=Context) ->
    case get_media_doc(MediaID, Context) of
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(MediaID, Context);
	Context1 ->
	    Context1
    end;

validate([MediaID, ?BIN_DATA], #cb_context{req_verb = <<"get">>, req_data=_Data}=Context) ->
    case get_media_doc(MediaID, Context) of
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(MediaID, Context);
	Context1 ->
	    Context1
    end;

validate(Params, #cb_context{req_verb=Verb, req_nouns=Nouns, req_data=D}=Context) ->
    format_log(info, "Media.validate: P: ~p~nV: ~s Ns: ~p~nData: ~p~nContext: ~p~n", [Params, Verb, Nouns, D, Context]),
    crossbar_util:response_faulty_request(Context).

create_media_meta(Data, Context) ->
    Doc1 = lists:foldr(fun(Meta, DocAcc) ->
			       case whapps_json:get_value(Meta, Data) of
				   undefined -> DocAcc;
				   V -> [{Meta, whistle_util:to_binary(V)} | DocAcc]
			       end
		       end, [], ?METADATA_FIELDS),
    crossbar_doc:save(Context#cb_context{doc=[{<<"pvt_type">>, <<"media">>} | Doc1]}).

%% GET /media
-spec(lookup_media/1 :: (Context :: #cb_context{}) -> #cb_context{}).
lookup_media(Context) ->
    Context1 = crossbar_doc:load_view({"media_doc", "listing_by_name"}, [], Context),
    case Context1#cb_context.resp_status =:= success of
	true ->
	    Resp = lists:map(fun(ViewObj) ->
				     whapps_json:get_value(<<"value">>, ViewObj)
			     end, Context1#cb_context.doc),
	    crossbar_util:response(Resp, Context1);
	false ->
	    Context1
    end.

%% GET/POST/DELETE /media/MediaID
-spec(get_media_doc/2 :: (MediaID :: binary(), Context :: #cb_context{}) -> #cb_context{}).
get_media_doc(MediaID, Context) ->
    crossbar_doc:load(MediaID, Context).

%% check for existence of media by display_name
-spec(lookup_media_by_name/2 :: (MediaID :: binary(), Context :: #cb_context{}) -> #cb_context{}).
lookup_media_by_name(MediaName, Context) ->
    crossbar_doc:load_view({"media_doc", "listing_by_name"}, [{<<"key">>, MediaName}], Context).
