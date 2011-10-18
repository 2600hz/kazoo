%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Users module
%%%
%%% Handle client requests for user documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_users).

-behaviour(gen_server).

%% API
-export([start_link/0, create_user/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CB_LIST, <<"users/crossbar_listing">>).
-define(GROUP_BY_USERNAME, <<"users/group_by_username">>).

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
init(_) ->
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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.users">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.users">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.users">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.users">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(hash_password(Context)),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.users">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(hash_password(Context)),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.users">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.users">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.users">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.users">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.users">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
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
    load_user_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_user(Context);
validate([UserId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_user(UserId, Context);
validate([UserId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_user(UserId, Context);
validate([UserId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_user(UserId, Context);
validate(_UserId, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec(load_user_summary/1 :: (Context :: #cb_context{}) -> #cb_context{}).
load_user_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new user document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec(create_user/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_user(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            case is_unique_username(undefined, Context) of
                true ->
                    Context#cb_context{
                      doc=wh_json:set_value(<<"pvt_type">>, <<"user">>, JObj)
                      ,resp_status=success
                     };
                false ->
                    crossbar_util:response_invalid_data([<<"username">>], Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a user document from the database
%% @end
%%--------------------------------------------------------------------
-spec(load_user/2 :: (UserId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_user(UserId, Context) ->
    Doc = crossbar_doc:load(UserId, Context),
    case wh_json:get_value(<<"pvt_deleted">>, Doc, false) of
	true ->
	    crossbar_util:response_bad_identifier(UserId, Context);
	false ->
	    Doc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing user document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_user/2 :: (UserId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_user(UserId, #cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            case is_unique_username(UserId, Context) of
                true ->
                    crossbar_doc:load_merge(UserId, JObj, Context);
                false ->
                    crossbar_util:response_invalid_data([<<"username">>], Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (Doc :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTICE: This is very temporary, placeholder until the schema work is
%% complete!
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_doc/1 :: (JObj :: json_object()) -> tuple(boolean(), list(binary()) | [])).
is_valid_doc(JObj) ->
    RequiredFields = [<<"email">>, <<"username">>],
    ErrorFields = [Field || Field <- RequiredFields, not field_exists(Field, JObj) ],

    case ErrorFields of
	[] -> {true, []};
	_ -> {false, ErrorFields}
    end.

-spec(field_exists/2 :: (Field :: binary(), JObj :: json_object()) -> boolean()).
field_exists(Field, JObj) ->
    is_binary(wh_json:get_value(Field, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the password parameter is present
%% and if so create the hashes then remove it.
%% @end
%%--------------------------------------------------------------------
-spec(hash_password/1 :: (Context :: #cb_context{}) -> #cb_context{}).
hash_password(#cb_context{doc=JObj}=Context) ->
    case wh_json:get_value(<<"password">>, JObj) of
        undefined ->
            Context;
        Password ->
            Creds = <<(wh_json:get_value(<<"username">>, JObj, <<>>))/binary, $:, Password/binary>>,
            SHA1 = wh_util:to_binary(wh_util:to_hex(crypto:sha(Creds))),
            MD5 = wh_util:to_binary(wh_util:to_hex(erlang:md5(Creds))),
            JObj1 = wh_json:set_value(<<"pvt_md5_auth">>, MD5, JObj),
            {struct, Props} = wh_json:set_value(<<"pvt_sha1_auth">>, SHA1, JObj1),
            Context#cb_context{doc={struct, props:delete(<<"password">>, Props)}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the username in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec(is_unique_username/2 :: (UserId :: binary()|undefined, Context :: #cb_context{}) -> boolean()).
is_unique_username(UserId, Context) ->
    Username = wh_json:get_value(<<"username">>, Context#cb_context.req_data),
    JObj = case crossbar_doc:load_view(?GROUP_BY_USERNAME, [{<<"key">>, Username}, {<<"reduce">>, <<"true">>}], Context) of
               #cb_context{resp_status=success, doc=[J]} -> J;
               _ -> []
           end,
    Assignments = wh_json:get_value(<<"value">>, JObj, []),
    case UserId of
        undefined ->
            Assignments =:= [];
        Id ->
            Assignments =:= [] orelse Assignments =:= [[Id]]
    end.
