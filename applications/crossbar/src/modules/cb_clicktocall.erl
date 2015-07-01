%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
         ,patch/2
         ,delete/2
         ,maybe_migrate_history/1
        ]).

-include("../crossbar.hrl").

-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(HISTORY_LIST, <<"clicktocall/history_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, ?CONNECT_CALL]}, {?WH_ACCOUNTS_DB, [_]}]).
-define(SUCCESSFUL_HANGUP_CAUSES, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.clicktocall">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.clicktocall">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.clicktocall">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.clicktocall">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.clicktocall">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.clicktocall">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.clicktocall">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_, ?CONNECT_CALL) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_, ?HISTORY) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() ->
    'true'.
resource_exists(_) ->
    'true'.
resource_exists(_, ?CONNECT_CALL) ->
    'true';
resource_exists(_, ?HISTORY) ->
    'true'.

-spec authenticate(cb_context:context()) -> 'true'.
authenticate(Context) ->
    case is_c2c_url(Context, cb_context:req_nouns(Context)) of
        'true' -> maybe_authenticate(Context);
        'false' -> 'false'
    end.

-spec authorize(cb_context:context()) -> 'true'.
authorize(Context) ->
    case is_c2c_url(Context, cb_context:req_nouns(Context)) of
        'true' -> maybe_authorize(Context);
        'false' -> 'false'
    end.

-spec maybe_authenticate(cb_context:context()) -> boolean().
maybe_authenticate(Context) ->
    case is_auth_required(Context) of
        'true' -> 'false';
        'false' ->
            lager:debug("authenticating request"),
            'true'
    end.

-spec maybe_authorize(cb_context:context()) -> boolean().
maybe_authorize(Context) ->
    case is_auth_required(Context) of
        'true' -> 'false';
        'false' ->
            lager:debug("authorizing request"),
            'true'
    end.

-spec is_auth_required(cb_context:context()) -> boolean().
is_auth_required(Context) ->
    Nouns = cb_context:req_nouns(Context),
    [C2CID, _] = props:get_value(<<"clicktocall">>, Nouns),
    JObj = cb_context:doc(crossbar_doc:load(C2CID, Context)),
    wh_json:is_true(<<"auth_required">>, JObj, 'true').

-spec is_c2c_url(cb_context:context(), req_nouns()) -> boolean().
is_c2c_url(Context, ?CONNECT_C2C_URL) ->
    Verb = cb_context:req_verb(Context),
    (Verb =:= ?HTTP_GET) orelse (Verb =:= ?HTTP_POST);
is_c2c_url(_Context, _Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_c2cs(Context, cb_context:req_verb(Context)).

validate_c2cs(Context, ?HTTP_GET) ->
    load_c2c_summary(Context);
validate_c2cs(Context, ?HTTP_PUT) ->
    create_c2c(Context).

validate(Context, Id) ->
    validate_c2c(Context, Id, cb_context:req_verb(Context)).

validate_c2c(Context, Id, ?HTTP_GET) ->
    load_c2c(Id, Context);
validate_c2c(Context, Id, ?HTTP_POST) ->
    update_c2c(Id, Context);
validate_c2c(Context, Id, ?HTTP_PATCH) ->
    validate_patch_c2c(Id, Context);
validate_c2c(Context, Id, ?HTTP_DELETE) ->
    load_c2c(Id, Context).

validate(Context, Id, ?HISTORY) ->
    load_c2c_history(Id, Context);
validate(Context, Id, ?CONNECT_CALL) ->
    establish_c2c(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).
post(Context, _, ?CONNECT_CALL) ->
    Context1 = crossbar_doc:save(Context),
    cb_context:set_resp_data(Context1, cb_context:resp_data(Context)).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
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
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_history_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_history_results(JObj, Acc) ->
    [wh_json:get_value(<<"doc">>, JObj)|Acc].

-spec load_c2c_summary(cb_context:context()) -> cb_context:context().
load_c2c_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST
                           ,[]
                           ,Context
                           ,fun normalize_view_results/2
                          ).

-spec load_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
load_c2c(C2CId, Context) ->
    crossbar_doc:load(C2CId, Context).

-spec load_c2c_history(ne_binary(), cb_context:context()) -> cb_context:context().
load_c2c_history(_C2CId, Context) ->
    crossbar_doc:load_view(?HISTORY_LIST
                           ,['include_docs']
                           ,cb_context:set_account_db(Context, cb_context:account_modb(Context))
                           ,fun normalize_history_results/2
                          ).

-spec create_c2c(cb_context:context()) -> cb_context:context().
create_c2c(Context) ->
    cb_context:validate_request_data(<<"clicktocall">>, Context, fun clear_history_set_type/1).

-spec update_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
update_c2c(C2CId, Context) ->
    OnSuccess = fun(C) -> crossbar_doc:load_merge(C2CId, C) end,
    cb_context:validate_request_data(<<"clicktocall">>, Context, OnSuccess).

-spec validate_patch_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch_c2c(C2CId, Context) ->
    crossbar_doc:patch_and_validate(C2CId, Context, fun update_c2c/2).

-spec establish_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
establish_c2c(C2CId, Context) ->
    Context1 = crossbar_doc:load(C2CId, Context),
    case cb_context:resp_status(Context1) of
        'success' -> originate_call(C2CId, Context1);
        _Status -> Context1
    end.

-spec maybe_migrate_history(ne_binary()) -> 'ok'.
maybe_migrate_history(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    case couch_mgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'ok', C2Cs} -> migrate_histories(AccountId, AccountDb, C2Cs);
        {'error', _} -> 'ok'
    end.

-spec migrate_histories(ne_binary(), ne_binary(), wh_json:objects()) -> 'ok'.
migrate_histories(AccountId, AccountDb, C2Cs) ->
    _ = [migrate_history(AccountId, AccountDb, wh_json:get_value(<<"doc">>, C2C)) || C2C <- C2Cs],
    'ok'.

-spec migrate_history(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
migrate_history(AccountId, AccountDb, C2C) ->
    case wh_json:get_value(<<"pvt_history">>, C2C, []) of
        [] -> 'ok';
        History ->
            Id = wh_doc:id(C2C),
            _ = [save_history_item(AccountId, HistoryItem, Id) || HistoryItem <- History],
            _Resp = couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"pvt_history">>, C2C)),
            lager:debug("removed history from c2c ~s in ~s: ~p", [Id
                                                                  ,AccountId
                                                                  ,_Resp
                                                                 ])
    end.

-spec save_history_item(ne_binary(), wh_json:object(), ne_binary()) -> any().
save_history_item(AccountId, HistoryItem, C2CId) ->
    Timestamp = wh_json:get_integer_value(<<"timestamp">>, HistoryItem, wh_util:current_tstamp()),
    AccountModb = wh_util:format_account_mod_id(AccountId, Timestamp),
    JObj = wh_doc:update_pvt_parameters(wh_json:set_value(<<"pvt_clicktocall_id">>, C2CId, HistoryItem)
                                        ,AccountModb
                                        ,[{'type', <<"c2c_history">>}
                                          ,{'account_id', AccountId}
                                          ,{'created', Timestamp}
                                         ]),
    couch_mgr:save_doc(AccountModb, JObj).

-spec clear_history_set_type(cb_context:context()) -> cb_context:context().
clear_history_set_type(Context) ->
    cb_context:set_doc(Context
                       ,wh_doc:update_pvt_parameters(cb_context:doc(Context)
                                                     ,cb_context:account_db(Context)
                                                     ,[{'type', ?PVT_TYPE}]
                                                    )
                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-------------------------------------------------------------------
-spec originate_call(ne_binary(), cb_context:context()) -> cb_context:context().
-spec originate_call(ne_binary(), cb_context:context(), api_binary()) -> cb_context:context().
originate_call(C2CId, Context) ->
    originate_call(C2CId, Context, get_c2c_contact(cb_context:req_value(Context, <<"contact">>))).

originate_call(_C2CId, Context, 'undefined') ->
    Message = <<"The contact extension for this click to call has not been set">>,
    cb_context:add_validation_error(
      <<"contact">>
      ,<<"required">>
      ,wh_json:from_list([{<<"message">>, Message}])
      ,Context
     );
originate_call(C2CId, Context, Contact) ->
    ReqId = cb_context:req_id(Context),
    AccountId = cb_context:account_id(Context),
    AccountModb = cb_context:account_modb(Context),

    _Pid = wh_util:spawn(
             fun() ->
                     wh_util:put_callid(ReqId),
                     Request = build_originate_req(Contact, Context),
                     Status = exec_originate(Request),
                     lager:debug("got status ~p", [Status]),

                     JObj = wh_json:from_list(
                              [{<<"pvt_clicktocall_id">>, C2CId}
                               | create_c2c_history_item(Status, Contact)
                              ]
                             ),
                     HistoryItem =
                         wh_doc:update_pvt_parameters(JObj
                                                      ,AccountModb
                                                      ,[{'account_id', AccountId}
                                                        ,{'type', <<"c2c_history">>}
                                                       ]),
                     kazoo_modb:save_doc(AccountId, HistoryItem)
             end),
    lager:debug("attempting call in ~p", [_Pid]),
    crossbar_util:response_202(<<"processing request">>, Context).

-spec exec_originate(api_terms()) ->
                            {'success', ne_binary()} |
                            {'error', ne_binary()}.
exec_originate(Request) ->
    handle_originate_resp(
      wh_amqp_worker:call_collect(Request
                                  ,fun wapi_resource:publish_originate_req/1
                                  ,fun is_resp/1
                                  ,20 * ?MILLISECONDS_IN_SECOND
                                 )
     ).

-spec handle_originate_resp({'ok', wh_json:objects()} |
                            {'returned', wh_json:object(), wh_json:object()} |
                            {'error', _} |
                            {'timeout', _}
                           ) ->
                                   {'success', ne_binary()} |
                                   {'error', ne_binary()}.
handle_originate_resp({'ok', [Resp|_]}) ->
    AppResponse = wh_json:get_first_defined([<<"Application-Response">>
                                             ,<<"Hangup-Cause">>
                                             ,<<"Error-Message">>
                                            ], Resp),
    case lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES) of
        'true' ->
            {'success', wh_json:get_value(<<"Call-ID">>, Resp)};
        'false' when AppResponse =:= 'undefined' ->
            {'success', wh_json:get_value(<<"Call-ID">>, Resp)};
        'false' ->
            lager:debug("app response ~s not successful: ~p", [AppResponse, Resp]),
            {'error', AppResponse}
    end;
handle_originate_resp({'returned', _JObj, Return}) ->
    case {wh_json:get_value(<<"code">>, Return)
          ,wh_json:get_value(<<"message">>, Return)
         }
    of
        {312, _Msg} ->
            lager:debug("no resources available to take request: ~s", [_Msg]),
            {'error', <<"no resources">>};
        {_Code, Msg} ->
            lager:debug("failed to publish request: ~p: ~s", [_Code, Msg]),
            {'error', <<"request failed: ", Msg>>}
    end;
handle_originate_resp({'error', _E}) ->
    lager:debug("errored while originating: ~p", [_E]),
    {'error', <<"timed out">>};
handle_originate_resp({'timeout', _T}) ->
    lager:debug("timed out while originating: ~p", [_T]),
    {'error', <<"timed out">>}.

-spec build_originate_req(ne_binary(), cb_context:context()) -> api_terms().
build_originate_req(Contact, Context) ->
    AccountId = cb_context:account_id(Context),
    JObj = cb_context:doc(Context),
    Exten = wnm_util:to_e164(wh_json:get_binary_value(<<"extension">>, JObj)),
    CalleeName = wh_json:get_binary_value(<<"outbound_callee_id_name">>, JObj, Exten),
    CalleeNumber = wnm_util:to_e164(wh_json:get_binary_value(<<"outbound_callee_id_number">>, JObj, Exten)),
    FriendlyName = wh_json:get_ne_value(<<"name">>, JObj, <<>>),
    OutboundNumber = wh_json:get_value(<<"caller_id_number">>, JObj, Contact),
    AutoAnswer = wh_json:is_true(<<"auto_answer">>, cb_context:query_string(Context), 'true'),

    lager:debug("attempting clicktocall ~s in account ~s", [FriendlyName, AccountId]),

    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Auto-Answer">>, AutoAnswer}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Authorizing-ID">>, wh_doc:id(JObj)}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, <<"device">>}
           ],

    {'ok', AccountDoc} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),

    Endpoint = [{<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>,  <<"loopback/", Exten/binary, "/context_2">>}
                ,{<<"To-DID">>, Exten}
                ,{<<"To-Realm">>, wh_json:get_value(<<"realm">>, AccountDoc)}
                ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               ],

    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:rand_hex_binary(16)),
    props:filter_undefined(
      [{<<"Application-Name">>, <<"transfer">>}
       ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, Contact}])}
       ,{<<"Msg-ID">>, MsgId}
       ,{<<"Endpoints">>, [wh_json:from_list(Endpoint)]}
       ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
       ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
       ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
       ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
       ,{<<"Outbound-Callee-ID-Name">>, CalleeName}
       ,{<<"Outbound-Callee-ID-Number">>, CalleeNumber}
       ,{<<"Outbound-Caller-ID-Name">>, FriendlyName}
       ,{<<"Outbound-Caller-ID-Number">>, OutboundNumber}
       ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
       ,{<<"Dial-Endpoint-Method">>, <<"single">>}
       ,{<<"Continue-On-Fail">>, 'true'}
       ,{<<"Custom-SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, JObj)}
       ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
       ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
       | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec is_resp(wh_json:objects() | wh_json:object()) -> boolean().
is_resp([JObj|_]) -> is_resp(JObj);
is_resp(JObj) ->
    wapi_resource:originate_resp_v(JObj)
        orelse wh_api:error_resp_v(JObj).

-spec get_c2c_contact(api_binary()) -> api_binary().
get_c2c_contact('undefined') -> 'undefined';
get_c2c_contact(Contact) ->
    Encoded = mochiweb_util:quote_plus(wh_util:to_list(Contact)),
    wnm_util:to_e164(wh_util:to_binary(Encoded)).

-spec create_c2c_history_item({'success', ne_binary()} | {'error', ne_binary()}, ne_binary()) -> wh_proplist().
create_c2c_history_item({'success', CallId}, Contact) ->
    [{<<"timestamp">>, wh_util:current_tstamp()}
     ,{<<"contact">>, Contact}
     ,{<<"call_id">>, CallId}
     ,{<<"result">>, <<"success">>}
    ];
create_c2c_history_item({'error', Error}, Contact) ->
    [{<<"timestamp">>, wh_util:current_tstamp()}
     ,{<<"contact">>, Contact}
     ,{<<"result">>, <<"error">>}
     ,{<<"cause">>, Error}
    ].
