%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% Click to call
%%%
%%% Allow embeddable HTML forms (or other ways to POST to the URL)
%%% and create a call.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(cb_clicktocall).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,authenticate/1
         ,authorize/1
         ,put/1
         ,post/2, post/3
         ,delete/2
        ]).

-include_lib("crossbar.hrl").

-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, ?CONNECT_CALL]}, {?WH_ACCOUNTS_DB, [_]}]).
-define(SUCCESSFUL_HANGUP_CAUSES, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.clicktocall">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.clicktocall">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.clicktocall">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.clicktocall">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.clicktocall">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.clicktocall">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(_, ?CONNECT_CALL) ->
    ['POST'];
allowed_methods(_, ?HISTORY) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(_) ->
    true.
resource_exists(_, ?CONNECT_CALL) ->
    true;
resource_exists(_, ?HISTORY) ->
    true.

-spec authenticate/1 :: (#cb_context{}) -> 'true'.
authenticate(#cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>}) ->
    lager:debug("authenticating request"),
    true.

-spec authorize/1 :: (#cb_context{}) -> 'true'.
authorize(#cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>}) ->
    lager:debug("authorizing request"),
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_c2c_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_c2c(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    load_c2c(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    update_c2c(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    load_c2c(Id, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?HISTORY) ->
    load_c2c_history(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id, ?CONNECT_CALL) ->
    establish_c2c(Id, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).
post(#cb_context{resp_data=HistoryItem}=Context, _, ?CONNECT_CALL) ->
    Context1 = crossbar_doc:save(Context),
    Context1#cb_context{resp_data=HistoryItem}.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

load_c2c_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

load_c2c(C2CId, Context) ->
    crossbar_doc:load(C2CId, Context).

load_c2c_history(C2CId, Context) ->
    case crossbar_doc:load(C2CId, Context) of
        #cb_context{doc=JObj, resp_status=success}=Context1 ->
            Context1#cb_context{resp_data=wh_json:get_value(<<"pvt_history">>, JObj, [])};
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
-spec originate_call/3 :: (ne_binary(), wh_json:json_object(), ne_binary()) -> {'success', ne_binary()} | {'error', ne_binary()}.

originate_call(#cb_context{doc=JObj, req_data=Req, account_id=AccountId, db_name=Db}=Context) ->
    case get_c2c_contact(wh_json:get_string_value(<<"contact">>, Req)) of
        undefined ->
            lager:debug("click to call document does not define a contact", []),
            E = wh_json:set_value([<<"contact">>, <<"required">>], <<"The contact extension for this click to call has not been set">>, wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        Contact ->
            ReqId = get(callid),
            spawn(fun() ->
                          put(callid, ReqId),
                          Status = originate_call(Contact, JObj, AccountId),
                          HistoryItem = wh_json:from_list(create_c2c_history_item(Status, Contact)),
                          save_history(HistoryItem, JObj, Db)
                  end),
            crossbar_util:response_202(<<"processing request">>, Context)
    end. 

originate_call(Contact, JObj, AccountId) ->
    Exten = wnm_util:to_e164(wh_json:get_binary_value(<<"extension">>, JObj)),
    FriendlyName = wh_json:get_ne_value(<<"name">>, JObj, <<>>),

    lager:debug("attempting clicktocall ~s in account ~s", [FriendlyName, AccountId]),

    Amqp = amqp_util:new_queue(),
    amqp_util:bind_q_to_targeted(Amqp),
    amqp_util:basic_consume(Amqp),

    lager:debug("created click to call AMQP queue ~s", [Amqp]),

    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Auto-Answer">>, <<"true">>}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Authorizing-ID">>, wh_json:get_value(<<"_id">>, JObj)}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, <<"device">>}
            ,{<<"Inception">>, <<"on-net">>}
           ],

    Endpoint = [{<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>,  <<"loopback/", Exten/binary, "/context_2">>}
                ,{<<"To-DID">>, Exten}
               ],

    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:rand_hex_binary(16)),
    Request = [{<<"Application-Name">>, <<"transfer">>}    
               ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, Contact}])}
               ,{<<"Msg-ID">>, MsgId}
               ,{<<"Endpoints">>, [Endpoint]}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
               ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
               ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
               ,{<<"Outgoing-Callee-ID-Name">>, Exten}
               ,{<<"Outgoing-Callee-ID-Number">>, Exten}
               ,{<<"Outgoing-Caller-ID-Name">>, FriendlyName}
               ,{<<"Outgoing-Caller-ID-Number">>, Contact}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
               | wh_api:default_headers(Amqp, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    wh_amqp_mgr:register_return_handler(),
    wapi_resource:publish_originate_req(Request),
    lager:debug("published click to call request ~s", [MsgId]),
    wait_for_originate(MsgId).

-spec wait_for_originate/1 :: (ne_binary()) -> {'success' | 'error', ne_binary()}.
wait_for_originate(MsgId) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case wh_util:get_event_type(JObj) of               
                {<<"resource">>, <<"originate_resp">>} ->
                    AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                                    wh_json:get_value(<<"Hangup-Cause">>, JObj)),
                            lager:debug("click to call attempt returned hangup cause: ~s", [AppResponse]),
                    case lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES) of
                        true -> 
                            {success, wh_json:get_value(<<"Call-ID">>, JObj)};
                        false -> 
                            {error, AppResponse}
                    end;
                {<<"error">>, <<"originate_resp">>} ->
                    Error = wh_json:get_value(<<"Error-Message">>, JObj),
                    lager:debug("click to call attempt returned error: ~s", [Error]),
                    {error, Error};
                _  ->
                    wait_for_originate(MsgId)
            end;
        %% if there are no FS nodes connected (or ecallmgr is down) we get the message
        %% returned so we know...
        {#'basic.return'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case wh_json:get_value(<<"Msg-ID">>, JObj) of
                MsgId -> 
                    lager:debug("there were no resources available to fulfill the click to call request", []),
                    {error, <<"no resources">>};
                _Else -> wait_for_originate(MsgId)
            end;
        _ ->
            wait_for_originate(MsgId)
    end.

-spec get_c2c_contact/1 :: ('undefined' | nonempty_string() | ne_binary()) -> 'undefined' | ne_binary().
get_c2c_contact(undefined) ->
    undefined;
get_c2c_contact(Contact) when not is_list(Contact) ->
    get_c2c_contact(wh_util:to_list(Contact));
get_c2c_contact(Contact) ->
    Encoded = mochiweb_util:quote_plus(Contact),
    wnm_util:to_e164(wh_util:to_binary(Encoded)).

-spec create_c2c_history_item/2 :: ({'success', ne_binary()} | {'error', ne_binary()}, ne_binary()) -> proplist().
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
    ].

-spec save_history/3 :: (wh_json:json_object(), wh_json:json_object(), ne_binary()) -> 'ok'.
save_history(HistoryItem, JObj, Db) ->
    History = wh_json:get_value(<<"pvt_history">>, JObj, []),
    case couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_history">>, [HistoryItem | History], JObj)) of
        {ok, _} -> ok;
        {error, conflict} -> 
            Id = wh_json:get_value(<<"_id">>, JObj),
            case couch_mgr:open_doc(Db, Id) of
                {ok, NewJobj} -> save_history(HistoryItem, NewJobj, Db);
                _Else ->
                    lager:debug("unable to save click-2-call history: ~p", [_Else])
            end;
        _Else ->
            lager:debug("unable to save click-2-call history: ~p", [_Else])
    end.
