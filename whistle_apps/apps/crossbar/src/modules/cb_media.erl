%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Account module
%%%
%%% Store/retrieve media files
%%%
%%% @end
%%% Created : Mar 8 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_media).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(BIN_DATA, <<"raw">>).

-define(MEDIA_MIME_TYPES, ["audio/x-wav", "audio/mpeg", "application/octet-stream"]).

-define(METADATA_FIELDS, [<<"name">>, <<"description">>, <<"media_type">>
			      ,<<"status">>, <<"content_size">>, <<"size">>
			      ,<<"content_type">>, <<"content_length">>
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
init([]) ->
    {ok, ok, 0}.

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
handle_info({binding_fired, Pid, <<"v1_resource.content_types_provided.media">>, {RD, Context, Params}}, State) ->
    spawn(fun() ->
		  Context1 = content_types_provided(Params, Context),
                  Pid ! {binding_result, true, {RD, Context1, Params}}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.content_types_accepted.media">>, {RD, Context, Params}}, State) ->
    spawn(fun() ->
		  Context1 = content_types_accepted(Params, Context),
                  Pid ! {binding_result, true, {RD, Context1, Params}}
	  end),
    {noreply, State};

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
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.media">>, [RD, Context | Params]}, State) ->
    case Params of
	[_MediaID, ?BIN_DATA] ->
	    spawn(fun() ->
                          crossbar_util:put_reqid(Context),
			  Context1 = Context#cb_context{resp_headers = [{<<"Content-Type">>
									     ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
									,{<<"Content-Length">>
									      ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
									| Context#cb_context.resp_headers]},
			  Pid ! {binding_result, true, [RD, Context1, Params]}
		  end);
	_ ->
	    spawn(fun() ->
			  Pid ! {binding_result, true, [RD, Context, Params]}
		  end)
    end,
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.media">>, [RD, #cb_context{req_files=[], resp_status=RespStatus}=Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  #cb_context{resp_status=Resp}=Context1 = case RespStatus =:= success of
							       true -> crossbar_doc:save(Context);
							       false -> Context
							   end,
		  Pid ! {binding_result, Resp, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.media">>, [RD, #cb_context{req_files=[{_, FileObj}]}=Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid, infinity),
		  [MediaID, ?BIN_DATA] = Params,
		  HeadersJObj = wh_json:get_value(<<"headers">>, FileObj),
		  Contents = wh_json:get_value(<<"contents">>, FileObj),

		  #cb_context{resp_status=RespStatus}=Context1 = update_media_binary(MediaID, Contents, Context, HeadersJObj),
		  Pid ! {binding_result, (RespStatus =:= success), [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.media">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  case props:get_value(<<"Location">>, Context#cb_context.resp_headers) of
		      undefined ->
			  {Context1, Resp} = case create_media_meta(Context#cb_context.req_data, Context) of
						 #cb_context{resp_status=success, resp_data=RespData, resp_headers=RHs}=Context2 ->
						     DocID = wh_json:get_value(<<"id">>, RespData),
						     {Context2#cb_context{resp_headers=[{"Location", DocID} | RHs]}, true};
						 Context3 ->
						     ?LOG_SYS("PUT: ERROR"),
						     {Context3, false}
					     end,
			  Pid ! {binding_result, Resp, [RD, Context1, Params]};
		      _ ->
			  {Context, true}
		  end
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.media">>, [RD, Context | Params]}, State) ->
    case Params of
	[MediaID, ?BIN_DATA] ->
	    spawn(fun() ->
                          crossbar_util:put_reqid(Context),
			  crossbar_util:binding_heartbeat(Pid),
			  #cb_context{resp_status=RS}=Context1 = delete_media_binary(MediaID, Context),
			  Pid ! {binding_result, RS =:= success, [RD, Context1, Params]}
		  end);
	[_] ->
	    spawn(fun() ->
                          crossbar_util:put_reqid(Context),
			  crossbar_util:binding_heartbeat(Pid),
			  Context1 = delete_media(Context),
			  Pid ! {binding_result, Context1#cb_context.resp_status =:= success, [RD, Context1, Params]}
		  end)
    end,
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    {noreply, State};

handle_info(_Info, State) ->
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
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.media">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.media">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.media">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
content_types_provided([_MediaID, ?BIN_DATA], #cb_context{req_verb = <<"get">>}=Context) ->
    CTP = [{to_binary, ?MEDIA_MIME_TYPES}],
    Context#cb_context{content_types_provided=CTP};
content_types_provided(_, Context) -> Context.

content_types_accepted([_MediaID, ?BIN_DATA], #cb_context{req_verb = <<"post">>}=Context) ->
    CTA = [{from_binary, ?MEDIA_MIME_TYPES}],
    Context#cb_context{content_types_accepted=CTA};
content_types_accepted(_, Context) -> Context.

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
    {true, ['GET', 'POST']};
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
    Name = wh_json:get_value(<<"name">>, Data),

    case Name =/= undefined andalso lookup_media_by_name(Name, Context) of
	false ->
	    crossbar_util:response_invalid_data([<<"name">>], Context);
	#cb_context{resp_status=success, doc=[{struct, _}=Doc|_], resp_headers=RHs}=Context1 ->
	    DocID = wh_json:get_value(<<"id">>, Doc),
	    Context1#cb_context{resp_headers=[{"Location", DocID} | RHs]};
	_ ->
	    Context#cb_context{resp_status=success}
    end;

validate([MediaID], #cb_context{req_verb = <<"get">>}=Context) ->
    get_media_doc(MediaID, Context);

validate([MediaID], #cb_context{req_verb = <<"post">>, req_data=Data}=Context) ->
    case wh_json:get_value(<<"name">>, Data) =/= undefined of
	true ->
	    crossbar_doc:load_merge(MediaID, Data, Context);
	false ->
	    crossbar_util:response_invalid_data([<<"name">>], Context)
    end;

validate([MediaID], #cb_context{req_verb = <<"delete">>, req_data=_Data}=Context) ->
    get_media_doc(MediaID, Context);

validate([MediaID, ?BIN_DATA], #cb_context{req_verb = <<"get">>}=Context) ->
    get_media_binary(MediaID, Context);

validate([_MediaID, ?BIN_DATA], #cb_context{req_verb = <<"post">>, req_files=[]}=Context) ->
    ?LOG_SYS("No files in request to save attachment"),
    crossbar_util:response_invalid_data([<<"no_files">>], Context);
validate([MediaID, ?BIN_DATA], #cb_context{req_verb = <<"post">>, req_files=[{_Filename, FileObj}]}=Context) ->
    case wh_json:get_value([<<"contents">>], FileObj, <<>>) of
	<<>> ->
	    crossbar_util:response_invalid_data([<<"empty_file">>], Context);
	_ ->
	    lookup_media_by_id(MediaID, Context)
    end;

validate(Params, #cb_context{req_verb=Verb, req_nouns=Nouns, req_data=D}=Context) ->
    ?LOG_SYS("Failed validating request"),
    ?LOG_SYS("Params: ~p", [Params]),
    ?LOG_SYS("Verb: ~s", [Verb]),
    ?LOG_SYS("Nouns: ~p", [Nouns]),
    ?LOG_SYS("Data: ~p", [D]),
    crossbar_util:response_faulty_request(Context).

create_media_meta(Data, Context) ->
    Doc1 = lists:foldr(fun(Meta, DocAcc) ->
			       case wh_json:get_value(Meta, Data) of
				   undefined -> [{Meta, <<>>} | DocAcc];
				   V -> [{Meta, wh_util:to_binary(V)} | DocAcc]
			       end
		       end, [], ?METADATA_FIELDS),
    crossbar_doc:save(Context#cb_context{doc={struct, [{<<"pvt_type">>, <<"media">>} | Doc1]}}).

update_media_binary(MediaID, Contents, Context, HeadersJObj) ->
    CT = wh_json:get_value(<<"content_type">>, HeadersJObj, <<"application/octet-stream">>),
    Opts = [{headers, [{content_type, wh_util:to_list(CT)}]}],

    ?LOG("Setting Content-Type to ~s", [CT]),

    case crossbar_doc:save_attachment(MediaID, attachment_name(MediaID), Contents, Context, Opts) of
	#cb_context{resp_status=success}=Context1 ->
	    ?LOG("Saved attachement successfully"),
	    #cb_context{doc=Doc} = crossbar_doc:load(MediaID, Context),
	    Doc1 = wh_json:set_value(<<"content_type">>, CT, Doc),
	    crossbar_doc:save(Context1#cb_context{doc=Doc1});
	C ->
	    ?LOG("Failed to save attachment"),
	    C
    end.

%% GET /media
-spec(lookup_media/1 :: (Context :: #cb_context{}) -> #cb_context{}).
lookup_media(Context) ->
    case crossbar_doc:load_view(<<"media/crossbar_listing">>, [], Context) of
	#cb_context{resp_status=success, doc=Doc}=Context1 ->
	    Resp = [ wh_json:get_value(<<"value">>, ViewObj) || ViewObj <- Doc ],
	    crossbar_util:response(Resp, Context1);
	C -> C
    end.

%% GET/POST/DELETE /media/MediaID
-spec(get_media_doc/2 :: (MediaID :: binary(), Context :: #cb_context{}) -> #cb_context{}).
get_media_doc(MediaID, Context) ->
    crossbar_doc:load(MediaID, Context).

%% GET/DELETE /media/MediaID/raw
get_media_binary(MediaID, Context) ->
    crossbar_doc:load_attachment(MediaID, attachment_name(MediaID), Context).

%% check for existence of media by name
-spec(lookup_media_by_name/2 :: (MediaID :: binary(), Context :: #cb_context{}) -> #cb_context{}).
lookup_media_by_name(MediaName, Context) ->
    crossbar_doc:load_view(<<"media/listing_by_name">>, [{<<"key">>, MediaName}], Context).

%% check for existence of media by id
-spec(lookup_media_by_id/2 :: (MediaID :: binary(), Context :: #cb_context{}) -> #cb_context{}).
lookup_media_by_id(MediaID, Context) ->
    crossbar_doc:load_view(<<"media/crossbar_listing">>, [{<<"key">>, MediaID}], Context).

delete_media(Context) ->
    crossbar_doc:delete(Context).

delete_media_binary(MediaID, Context) ->
    crossbar_doc:delete_attachment(MediaID, attachment_name(MediaID), Context).

attachment_name(MediaID) ->
    <<MediaID/binary, "-raw">>.
