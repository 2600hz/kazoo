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
                  Context1 = update_phone_number(Number, Context),
                  Pid ! {binding_result, true, [RD, Context1, [Number]]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.phone_numbers">>, [RD, Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = update_phone_number(Number, Context),
                  Pid ! {binding_result, true, [RD, Context1, [Number]]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.phone_numbers">>, [RD, Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = update_phone_number(Number, Context),
                  Pid ! {binding_result, true, [RD, Context1, [Number]]}
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
    read(Number, Context);
validate([Number], #cb_context{req_verb = <<"put">>}=Context) ->
    create(Number, Context);
validate([Number], #cb_context{req_verb = <<"post">>}=Context) ->
    update(Number, Context);
validate([Number], #cb_context{req_verb = <<"delete">>}=Context) ->
    delete(Number, Context);
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
summary(#cb_context{account_id=AccountId}=Context) ->
    case crossbar_doc:load(AccountId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            Numbers = wh_json:get_value(get_key(), JObj, wh_json:new()),
            crossbar_util:response(wh_json:get_keys(Numbers), Context1);
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
create(_, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"phone_numbers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=JObj
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Number, #cb_context{account_id=AccountId}=Context) ->
    case crossbar_doc:load(AccountId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            case wh_json:get_value(get_key(Number), JObj) of
                undefined -> 
                    crossbar_util:response_bad_identifier(Number, Context1);
                NumberJObj ->
                    crossbar_util:response(crossbar_doc:public_fields(NumberJObj), Context1)
            end;
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(_, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"phone_numbers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=JObj
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
delete(_, Context) ->
    Context#cb_context{resp_status=success
                       ,doc=undefined
                      }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_phone_number/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
-spec update_phone_number/4 :: (json_object(), ne_binary(), undefined | json_object(), #cb_context{}) -> #cb_context{}.

update_phone_number(Number, #cb_context{account_id=AccountId}=Context) ->
    case crossbar_doc:load(AccountId, Context) of
        #cb_context{resp_status=success, doc=Account} ->
            ExistingJObj = wh_json:get_value(get_key(Number), Account),
            update_phone_number(Account, Number, ExistingJObj, Context);
        Else ->
            Else
    end.

update_phone_number(_, Number, undefined, #cb_context{req_verb = <<"post">>}=Context) ->
    crossbar_util:response_bad_identifier(Number, Context);
update_phone_number(Account, Number, ExistingJObj, #cb_context{doc=NewJObj, req_verb = <<"post">>}=Context) ->
    NumberJObj = wh_json:merge_jobjs(crossbar_doc:private_fields(ExistingJObj), NewJObj),
    case crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(get_key(Number), NumberJObj, Account)}) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            couch_mgr:ensure_saved(?ACCOUNTS_AGG_DB, JObj),
            Context1#cb_context{resp_data=crossbar_doc:public_fields(NumberJObj)};
        Else ->
            Else
    end;
update_phone_number(Account, Number, undefined, #cb_context{doc=NumberJObj, req_verb = <<"put">>}=Context) ->
    case crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(get_key(Number), NumberJObj, Account)}) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            couch_mgr:ensure_saved(?ACCOUNTS_AGG_DB, JObj),
            Context1#cb_context{resp_data=crossbar_doc:public_fields(NumberJObj)};
        Else ->
            Else
    end;
update_phone_number(_, _, _, #cb_context{req_verb = <<"put">>}=Context) ->
    crossbar_util:response_conflicting_docs(Context);
update_phone_number(_, Number, undefined, #cb_context{req_verb = <<"delete">>}=Context) ->
    crossbar_util:response_bad_identifier(Number, Context);        
update_phone_number(Account, Number, _, #cb_context{req_verb = <<"delete">>}=Context) ->
    case crossbar_doc:save(Context#cb_context{doc=wh_json:delete_key(get_key(Number), Account)}) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            couch_mgr:ensure_saved(?ACCOUNTS_AGG_DB, JObj),
            Context1#cb_context{resp_data=[]};
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec format_number/1 :: (ne_binary()) -> ne_binary().
format_number(Number) ->
    Num = case binary:match(Number, <<"%">>) of
              nomatch -> Number;
              _ -> mochiweb_util:unquote(Number)
          end,
    wh_util:to_e164(wh_util:to_binary(Num)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_key/0 :: () -> ne_binary().
-spec get_key/1 :: (ne_binary()) -> [ne_binary(),...].

get_key() ->
    <<"pvt_phone_numbers">>.

get_key(Number) ->
    [get_key(), format_number(Number)].
