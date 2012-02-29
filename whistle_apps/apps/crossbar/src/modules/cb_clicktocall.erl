%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-include_lib("crossbar/include/crossbar.hrl").

-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, ?CONNECT_CALL]}, {?WH_ACCOUNTS_DB, [_]}]).

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
authenticate(#cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>, req_id=ReqId}) ->
    lager:debug("authenticating request"),
    true.

-spec authorize/1 :: (#cb_context{}) -> 'true'.
authorize(#cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>, req_id=ReqId}) ->
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
-spec originate_call/3 :: (ne_binary(), wh_json:json_object(), ne_binary()) -> {'success', ne_binary()} | {'error', ne_binary()} | {'timeout'}.

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
    Exten = wnm_util:to_e164(wh_json:get_binary_value(<<"extension">>, JObj)),
    FriendlyName = wh_json:get_ne_value(<<"name">>, JObj, <<>>),

    lager:debug("attempting clicktocall ~s in account ~s", [FriendlyName, AccountId]),

    Amqp = amqp_util:new_queue(),
    amqp_util:bind_q_to_targeted(Amqp),
    amqp_util:basic_consume(Amqp),

    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Auto-Answer">>, <<"true">>}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Authorizing-ID">>, wh_json:get_value(<<"_id">>, JObj)}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, <<"device">>}
            ,{<<"Inception">>, <<"on-net">>}
           ],

    Req = [{<<"Msg-ID">>, wh_util:current_tstamp()}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"Resource-Minimum">>, <<"1">>}
           ,{<<"Resource-Maximum">>, <<"1">>}
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

    lager:debug("pubishing origination request from ~s to ~s", [Contact, Exten]),
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
            lager:debug("cannot establish click to call, timeout"),
            {timeout}
    end.

-spec get_c2c_contact/1 :: ('undefined' | nonempty_string() | ne_binary()) -> 'undefined' | ne_binary().
get_c2c_contact(undefined) ->
    undefined;
get_c2c_contact(Contact) when not is_list(Contact) ->
    get_c2c_contact(wh_util:to_list(Contact));
get_c2c_contact(Contact) ->
    Encoded = mochiweb_util:quote_plus(Contact),
    wnm_util:to_e164(wh_util:to_binary(Encoded)).

-spec get_c2c_resp_status/1 :: ({'success', ne_binary()} | {'error', ne_binary()} | {'timeout'}) -> 'success' | 'error'.
get_c2c_resp_status({success, _}) -> success;
get_c2c_resp_status(_) -> error.

-spec create_c2c_history_item/2 :: ({'success', ne_binary()} | {'error', ne_binary()} | {'timeout'}, ne_binary()) -> [{ne_binary(), term()},...].
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
