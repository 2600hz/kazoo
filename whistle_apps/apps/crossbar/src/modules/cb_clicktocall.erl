%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @author Eduoard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Click to call
%%%
%%% Allow embeddable HTML forms (or other ways to POST to the URL)
%%% and create a call.
%%%
%%% @end
%%% Created : 09 May 2011 by Karl Anderson <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_clicktocall).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, <<"connect">>]}, {<<"accounts">>, [_]}]).

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
    {reply, ok, State}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.clicktocall">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.clicktocall">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Pid ! {binding_result, true, [RD, Context, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.clicktocall">>, [RD, #cb_context{resp_data=HistoryItem}=Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context0 = crossbar_doc:save(Context),
                  Context1 = case Context#cb_context.req_nouns of
                                 ?CONNECT_C2C_URL ->
                                     Context#cb_context{resp_data=HistoryItem};
                                 _Else ->
                                     Context0
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>, req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>, req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, _B, Payload}, State) ->
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
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.clicktocall">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.clicktocall">>).

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
allowed_methods([_,?CONNECT_CALL]) ->
    {true, ['POST']};
allowed_methods([_,?HISTORY]) ->
    {true, ['GET']};
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
resource_exists([_, ?CONNECT_CALL]) ->
    {true, []};
resource_exists([_, ?HISTORY]) ->
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
    load_c2c_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_c2c(Context);

validate([C2CId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_c2c(C2CId, Context);
validate([C2CId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_c2c(C2CId, Context);
validate([C2CId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_c2c(C2CId, Context);

validate([C2CId, ?HISTORY], #cb_context{req_verb = <<"get">>}=Context) ->
    load_c2c_history(C2CId, Context);
validate([C2CId, ?CONNECT_CALL], #cb_context{req_verb = <<"post">>}=Context) ->
    establish_c2c(C2CId, Context);

validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (json_object(), json_objects()) -> json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

load_c2c_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

load_c2c(C2CId, Context) ->
    crossbar_doc:load(C2CId, Context).

load_c2c_history(C2CId, Context) ->
    case crossbar_doc:load(C2CId, Context) of
        #cb_context{doc=JObj, resp_status=success}=Context1 ->
            History = wh_json:get_value(<<"pvt_history">>, JObj, []),
            Context1#cb_context{resp_data=History};
        Else ->
            Else
    end.

create_c2c(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"clicktocall">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{
              doc=wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, wh_json:set_value(<<"pvt_history">>, [], JObj))
              ,resp_status=success
             }
    end.

update_c2c(C2CId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"clicktocall">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(C2CId, JObj, Context)
    end.

establish_c2c(C2CId, Context) ->
    case crossbar_doc:load(C2CId, Context) of
        #cb_context{resp_status=success}=Context1 ->
            originate_call(Context1);
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-------------------------------------------------------------------
-spec originate_call/1 :: (#cb_context{}) -> #cb_context{}.
-spec originate_call/3 :: (ne_binary(), json_object(), ne_binary()) -> {'success', ne_binary()} | {'error', ne_binary()} | {'timeout'}.

originate_call(#cb_context{doc=JObj, req_data=Req, account_id=AccountId}=Context) ->
    case get_c2c_contact(wh_json:get_string_value(<<"contact">>, Req)) of
        undefined ->
            Context#cb_context{resp_status=error, resp_error_msg = <<"invalid contact">>, resp_error_code = 400, resp_data=[]};
        Contact ->
            Status = originate_call(Contact, JObj, AccountId),
            HistoryItem = wh_json:from_list(create_c2c_history_item(Status, Contact)),

            History = wh_json:get_value(<<"pvt_history">>, JObj, []),
            Context#cb_context{doc=wh_json:set_value(<<"pvt_history">>, [HistoryItem | History], JObj)
                               ,resp_data=HistoryItem
                               ,resp_status=get_c2c_resp_status(Status)}
    end.

originate_call(Contact, JObj, AccountId) ->
    Exten = wh_util:to_e164(wh_json:get_binary_value(<<"extension">>, JObj)),
    FriendlyName = wh_json:get_ne_value(<<"name">>, JObj, <<>>),

    ?LOG("attempting clicktocall ~s in account ~s", [FriendlyName, AccountId]),

    Amqp = amqp_util:new_queue(),
    amqp_util:bind_q_to_targeted(Amqp),
    amqp_util:basic_consume(Amqp),

    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Auto-Answer">>, <<"true">>}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Authorizing-ID">>, wh_json:get_value(<<"_id">>, JObj)}
           ],

    Req = [{<<"Msg-ID">>, wh_util:current_tstamp()}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"Invite-Format">>, <<"route">>}
           ,{<<"Route">>, <<"loopback/", Exten/binary, "/context_2">>}
           ,{<<"Outgoing-Callee-ID-Name">>, Exten}
           ,{<<"Outgoing-Callee-ID-Number">>, Exten}
           ,{<<"Outgoing-Caller-ID-Name">>, FriendlyName}
           ,{<<"Outgoing-Caller-ID-Number">>, Contact}
           ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
           ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>]}
           ,{<<"Application-Name">>, <<"transfer">>}
           ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, Contact}])}
           | wh_api:default_headers(Amqp, ?APP_NAME, ?APP_VERSION)],

    ?LOG("pubishing origination request from ~s to ~s", [Contact, Exten]),
    wapi_resource:publish_req(Req),
    wait_for_originate().

-spec wait_for_originate/0 :: () -> {success, binary()} | {error, binary()} | {timeout}.
wait_for_originate() ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            try
                JObj = mochijson2:decode(Payload),
                case whapps_util:get_event_type(JObj) of
                    {<<"resource">>, <<"originate_resp">>} ->
                        true = wapi_resource:resp_v(JObj),
                        whapps_util:put_callid(JObj),
                        {success, wh_json:get_value(<<"Call-ID">>, JObj)};
                    {<<"resource">>, <<"originate_error">>} ->
                        true = wapi_resource:error_v(JObj),
                        {error, wh_json:get_value(<<"Failure-Message">>, JObj, <<>>)};
                    _ ->
                        wait_for_originate()
                end
            catch
                _:_ ->
                    wait_for_originate()
            end;
        _ ->
            wait_for_originate()
    after
        15000 ->
            ?LOG("cannot establish click to call, timeout"),
            {timeout}
    end.

-spec get_c2c_contact/1 :: ('undefined' | nonempty_string() | ne_binary()) -> 'undefined' | ne_binary().
get_c2c_contact(undefined) ->
    undefined;
get_c2c_contact(Contact) when not is_list(Contact) ->
    get_c2c_contact(wh_util:to_list(Contact));
get_c2c_contact(Contact) ->
    Encoded = mochiweb_util:quote_plus(Contact),
    wh_util:to_e164(wh_util:to_binary(Encoded)).

-spec get_c2c_resp_status/1 :: ({'success', ne_binary()} | {'error', ne_binary()} | {'timeout'}) -> 'success' | 'error'.
get_c2c_resp_status({success, _}) -> success;
get_c2c_resp_status(_) -> error.

-spec create_c2c_history_item/2 :: ({'success', ne_binary()} | {'error', ne_binary()} | {'timeout'}, ne_binary()) -> proplist().
create_c2c_history_item({success, CallId}, Contact) ->
    [{<<"timestamp">>, wh_util:current_tstamp()}
     ,{<<"contact">>, Contact}
     ,{<<"call_id">>, CallId}
     ,{<<"result">>, <<"success">>}
    ];
create_c2c_history_item({error, Error}, Contact) ->
    [{<<"timestamp">>, wh_util:current_tstamp()}
     ,{<<"contact">>, Contact}
     ,{<<"result">>, <<"error">>}
     ,{<<"cause">>, Error}
    ];
create_c2c_history_item({timeout}, Contact) ->
    [{<<"timestamp">>, wh_util:current_tstamp()}
     ,{<<"contact">>, Contact}
     ,{<<"result">>, <<"timeout">>}
     ,{<<"cause">>, <<"no response received">>}
    ].
