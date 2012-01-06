%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_phone_numbers).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(PVT_FUNS, [fun add_pvt_type/2
                   ,fun set_doc_id/2
                  ]).

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.phone_numbers">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.phone_numbers">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.phone_numbers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.phone_numbers">>, [RD, Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Key = [<<"account">>, format_number(Number)],
                  #cb_context{resp_data=JObj} = Context1 = crossbar_doc:save(Context),
                  Response = crossbar_doc:public_fields(wh_json:get_value(Key, JObj, wh_json:new())),
                  Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data=Response}, [Number]]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.phone_numbers">>, [RD, Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Key = [<<"account">>, format_number(Number)],
                  #cb_context{resp_data=JObj} = Context1 = crossbar_doc:save(Context),
                  Response = crossbar_doc:public_fields(wh_json:get_value(Key, JObj, wh_json:new())),
                  Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data=Response}, [Number]]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.phone_numbers">>, [RD, Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Key = [<<"account">>, format_number(Number)],
                  #cb_context{resp_data=JObj} = Context1 = crossbar_doc:save(Context),
                  Response = crossbar_doc:public_fields(wh_json:get_value(Key, JObj, wh_json:new())),
                  Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data=Response}, [Number]]}
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
-spec bind_to_crossbar/0 :: () ->  no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.phone_numbers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.phone_numbers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.phone_numbers">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.phone_numbers">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: ([ne_binary(),...] | []) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET']};
allowed_methods([_]) ->
    {true, ['GET', 'PUT', 'POST', 'DELETE']};
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
-spec resource_exists/1 :: ([ne_binary(),...] | []) -> {boolean(), []}.
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
-spec validate/2 :: ([ne_binary(),...] | [], #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate([Number], #cb_context{req_verb = <<"get">>}=Context) ->
    read(format_number(Number), Context);
validate([Number], #cb_context{req_verb = <<"put">>}=Context) ->
    create(format_number(Number), Context);
validate([Number], #cb_context{req_verb = <<"post">>}=Context) ->
    update(format_number(Number), Context);
validate([Number], #cb_context{req_verb = <<"delete">>}=Context) ->
    delete(format_number(Number), Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    Numbers = crossbar_doc:public_fields(get_numbers_doc(Context)),
    crossbar_util:response(wh_json:get_keys(Numbers), Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
create(Number, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"phone_numbers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Numbers = get_numbers_doc(Context),
            Key = [<<"account">>, Number],
            case wh_json:get_value(Key, Numbers) of
                undefined ->
                    Context#cb_context{resp_status=success
                                       ,doc=wh_json:set_value(Key, JObj, Numbers)
                                      };
                _Else ->
                    crossbar_util:response_conflicting_docs(Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Number, Context) ->
    JObj = get_numbers_doc(Context),
    case wh_json:get_value([<<"account">>, Number], JObj) of
        undefined -> 
            crossbar_util:response_bad_identifier(Number, Context);
        NumberJObj ->
            crossbar_util:response(crossbar_doc:public_fields(NumberJObj), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Number, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"phone_numbers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Numbers = get_numbers_doc(Context),
            Key = [<<"account">>, Number],
            case wh_json:get_value(Key, Numbers) of
                undefined -> 
                    crossbar_util:response_bad_identifier(Number, Context);
                _Else ->
                    NumberJObj = wh_json:merge_jobjs(crossbar_doc:public_fields(JObj)
                                                     ,wh_json:get_value(Key, Numbers, wh_json:new())),
                    Context#cb_context{resp_status=success
                                       ,doc=wh_json:set_value(Key, NumberJObj, Numbers)
                                      }
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
delete(Number, Context) ->
    Numbers = get_numbers_doc(Context),
    Key = [<<"account">>, Number],
    case wh_json:get_value(Key, Numbers) of
        undefined -> 
            crossbar_util:response_bad_identifier(Number, Context);
        _Else ->
            Context#cb_context{resp_status=success
                               ,doc=wh_json:delete_key(Key, Numbers)
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (json_object(), #cb_context{}) -> json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, <<"phone_numbers">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec set_doc_id/2 :: (json_object(), #cb_context{}) -> json_object().
set_doc_id(JObj, _) ->
    wh_json:set_value(<<"_id">>, <<"phone_numbers">>, JObj).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_numbers_doc/1 :: (#cb_context{}) -> json_object().
get_numbers_doc(#cb_context{db_name=Db}=Context) ->
    case couch_mgr:open_doc(Db, <<"phone_numbers">>) of
        {ok, JObj} -> JObj;
        {error, not_found} ->
            {JObj, _} = lists:foldr(fun(F, {J, C}) ->
                                            {F(J, C), C}
                                    end, {wh_json:new(), Context}, ?PVT_FUNS),
            JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec format_number/1 :: (ne_binary()) -> ne_binary().
format_number(Number) ->
    wh_util:to_e164(wh_util:to_binary(mochiweb_util:unquote(Number))).
