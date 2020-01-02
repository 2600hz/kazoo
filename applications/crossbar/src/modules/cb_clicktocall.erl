%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Click to call
%%% Allow embedded HTML forms (or other ways to POST to the URL)
%%% and create a call.
%%%
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Edouard Swiac
%%% @author Sponsored by Conversant Ltd, Implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_clicktocall).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,authenticate/1, authenticate/2, authenticate/3
        ,authorize/1, authorize/2, authorize/3
        ,put/1
        ,post/2, post/3
        ,patch/2
        ,delete/2
        ,maybe_migrate_history/1
        ]).

-include("crossbar.hrl").

-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(HISTORY_LIST, <<"clicktocall/history_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, ?CONNECT_CALL]}
                         ,{?KZ_ACCOUNTS_DB, [_]}
                         ]).
-define(SUCCESSFUL_HANGUP_CAUSES, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods.clicktocall">>, 'allowed_methods'}
               ,{<<"*.resource_exists.clicktocall">>, 'resource_exists'}
               ,{<<"*.authenticate.clicktocall">>, 'authenticate'}
               ,{<<"*.authorize.clicktocall">>, 'authorize'}
               ,{<<"*.validate.clicktocall">>, 'validate'}
               ,{<<"*.execute.put.clicktocall">>, 'put'}
               ,{<<"*.execute.post.clicktocall">>, 'post'}
               ,{<<"*.execute.patch.clicktocall">>, 'patch'}
               ,{<<"*.execute.delete.clicktocall">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_C2CId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_C2CId, ?CONNECT_CALL) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_C2CId, ?HISTORY) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, ?CONNECT_CALL) -> 'true';
resource_exists(_, ?HISTORY) -> 'true'.

-spec authenticate(cb_context:context()) -> 'true'.
authenticate(Context) ->
    is_c2c_url(Context, cb_context:req_nouns(Context))
        andalso maybe_authenticate(Context).

-spec authenticate(cb_context:context(), path_token()) -> 'true'.
authenticate(Context, _) ->
    authenticate(Context).

-spec authenticate(cb_context:context(), path_token(), path_token()) -> 'true'.
authenticate(Context, _, _) ->
    authenticate(Context).

-spec authorize(cb_context:context()) -> 'true'.
authorize(Context) ->
    is_c2c_url(Context, cb_context:req_nouns(Context))
        andalso maybe_authorize(Context).

-spec authorize(cb_context:context(), path_token()) -> 'true'.
authorize(Context, _) ->
    authorize(Context).

-spec authorize(cb_context:context(), path_token(), path_token()) -> 'true'.
authorize(Context, _, _) ->
    authorize(Context).

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
    JObj = cb_context:doc(crossbar_doc:load(C2CID, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))),
    kz_json:is_true(<<"auth_required">>, JObj, 'true').

-spec is_c2c_url(cb_context:context(), req_nouns()) -> boolean().
is_c2c_url(Context, ?CONNECT_C2C_URL) ->
    Verb = cb_context:req_verb(Context),
    (Verb =:= ?HTTP_GET)
        orelse (Verb =:= ?HTTP_POST);
is_c2c_url(_Context, _Nouns) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_c2cs(Context, cb_context:req_verb(Context)).

validate_c2cs(Context, ?HTTP_GET) ->
    load_c2c_summary(Context);
validate_c2cs(Context, ?HTTP_PUT) ->
    create_c2c(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
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

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id, ?HISTORY) ->
    load_c2c_history(Id, Context);
validate(Context, Id, ?CONNECT_CALL) ->
    establish_c2c(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _, ?CONNECT_CALL) ->
    Context.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_history_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_history_results(JObj, Acc) ->
    [kz_json:get_value(<<"doc">>, JObj)|Acc].

-spec load_c2c_summary(cb_context:context()) -> cb_context:context().
load_c2c_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST
                          ,[]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec load_c2c(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_c2c(C2CId, Context) ->
    crossbar_doc:load(C2CId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)).

-spec load_c2c_history(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_c2c_history(C2CId, Context) ->
    Options = ['include_docs'
              ,{'startkey', [C2CId]}
              ,{'endkey', [C2CId, kz_json:new()]}
              ],
    crossbar_doc:load_view(?HISTORY_LIST
                          ,Options
                          ,cb_context:set_db_name(Context, cb_context:account_modb(Context))
                          ,fun normalize_history_results/2
                          ).

-spec create_c2c(cb_context:context()) -> cb_context:context().
create_c2c(Context) ->
    cb_context:validate_request_data(<<"clicktocall">>, Context, fun clear_history_set_type/1).

-spec update_c2c(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update_c2c(C2CId, Context) ->
    OnSuccess = fun(C) -> crossbar_doc:load_merge(C2CId, C, ?TYPE_CHECK_OPTION(?PVT_TYPE)) end,
    cb_context:validate_request_data(<<"clicktocall">>, Context, OnSuccess).

-spec validate_patch_c2c(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch_c2c(C2CId, Context) ->
    crossbar_doc:patch_and_validate(C2CId, Context, fun update_c2c/2).

-spec establish_c2c(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
establish_c2c(C2CId, Context) ->
    Context1 = crossbar_doc:load(C2CId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    case cb_context:resp_status(Context1) of
        'success' -> originate_call(C2CId, Context1);
        _Status -> Context1
    end.

-spec maybe_migrate_history(kz_term:ne_binary()) -> 'ok'.
maybe_migrate_history(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    AccountDb = kzs_util:format_account_db(Account),

    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'ok', C2Cs} -> migrate_histories(AccountId, AccountDb, C2Cs);
        {'error', _} -> 'ok'
    end.

-spec migrate_histories(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
migrate_histories(AccountId, AccountDb, C2Cs) ->
    _ = [migrate_history(AccountId, AccountDb, kz_json:get_value(<<"doc">>, C2C)) || C2C <- C2Cs],
    'ok'.

-spec migrate_history(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
migrate_history(AccountId, AccountDb, C2C) ->
    case kz_json:get_list_value(<<"pvt_history">>, C2C, []) of
        [] -> 'ok';
        History ->
            Id = kz_doc:id(C2C),
            _ = [save_history_item(AccountId, HistoryItem, Id) || HistoryItem <- History],
            Update = [{[<<"pvt_history">>], 'null'}],
            UpdateOptions = [{'update', Update}],
            _Resp = kz_datamgr:update_doc(AccountDb, Id, UpdateOptions),
            lager:debug("removed history from c2c ~s in ~s: ~p"
                       ,[Id, AccountId, _Resp]
                       )
    end.

-spec save_history_item(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> any().
save_history_item(AccountId, HistoryItem, C2CId) ->
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, HistoryItem, kz_time:now_s()),
    AccountModb = kzs_util:format_account_mod_id(AccountId, Timestamp),
    JObj = kz_doc:update_pvt_parameters(kz_json:set_value(<<"pvt_clicktocall_id">>, C2CId, HistoryItem)
                                       ,AccountModb
                                       ,[{'type', <<"c2c_history">>}
                                        ,{'account_id', AccountId}
                                        ,{'created', Timestamp}
                                        ]),
    kz_datamgr:save_doc(AccountModb, JObj).

-spec clear_history_set_type(cb_context:context()) -> cb_context:context().
clear_history_set_type(Context) ->
    cb_context:set_doc(Context
                      ,kz_doc:update_pvt_parameters(cb_context:doc(Context)
                                                   ,cb_context:db_name(Context)
                                                   ,[{'type', ?PVT_TYPE}]
                                                   )
                      ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec originate_call(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
originate_call(C2CId, Context) ->
    originate_call(C2CId, Context, get_c2c_contact(cb_context:req_value(Context, <<"contact">>))).

-spec originate_call(kz_term:ne_binary(), cb_context:context(), kz_term:api_binary()) -> cb_context:context().
originate_call(_C2CId, Context, 'undefined') ->
    Message = <<"The contact extension for this click to call has not been set">>,
    cb_context:add_validation_error(<<"contact">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, Message}])
                                   ,Context
                                   );
originate_call(C2CId, Context, Contact) ->
    JObj = cb_context:doc(Context),
    case kz_json:get_ne_value(<<"whitelist">>, JObj, []) of
        [] -> originate_call(C2CId, Context, Contact, 'true');
        Whitelist -> originate_call(C2CId, Context, Contact, match_regexps(Whitelist, Contact))
    end.

-spec originate_call(kz_term:ne_binary(), cb_context:context(), kz_term:api_binary(), boolean()) -> cb_context:context().
originate_call(_C2CId, Context, Contact, 'false') ->
    crossbar_util:response_400(<<"Contact doesn't match whitelist">>, Contact, Context);
originate_call(C2CId, Context, Contact, 'true') ->
    Request = build_originate_req(Contact, Context),
    do_originate_call(C2CId, Context, Contact, Request, cb_context:req_value(Context, <<"blocking">>, 'false')).

do_originate_call(C2CId, Context, Contact, Request, 'false') ->
    _Pid = kz_process:spawn(fun() -> do_originate_call(C2CId, Context, Contact, Request) end),
    JObj = kz_json:normalize(kz_json:from_list(kz_api:remove_defaults(Request))),
    lager:debug("attempting call in ~p", [JObj]),
    crossbar_util:response_202(<<"processing request">>, JObj, cb_context:set_resp_data(Context, Request));
do_originate_call(C2CId, Context, Contact, Request, 'true') ->
    Resp = exec_originate(Request),
    case handle_response(C2CId, Context, Contact, Resp) of
        {'ok', HistoryItem} -> crossbar_doc:handle_json_success(HistoryItem, Context);
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, C2CId, Context)
    end.

handle_response(C2CId, Context, Contact, Resp) ->
    AccountId = cb_context:account_id(Context),
    Modb = cb_context:account_modb(Context),

    JObj = kz_json:from_list(create_c2c_history_item(Resp, C2CId, Contact)),
    Options = [{'account_id', cb_context:account_id(Context)}
              ,{'type', <<"c2c_history">>}
              ],
    HistoryItem = kz_doc:update_pvt_parameters(crossbar_doc:update_pvt_parameters(JObj, Context)
                                              ,Modb
                                              ,Options
                                              ),
    _ = kazoo_modb:save_doc(AccountId, HistoryItem).

-spec do_originate_call(kz_term:ne_binary(), cb_context:context(), kz_term:api_binary(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          kz_datamgr:data_error().
do_originate_call(C2CId, Context, Contact, Request) ->
    ReqId = cb_context:req_id(Context),
    kz_log:put_callid(ReqId),

    Resp = exec_originate(Request),
    lager:debug("got status for ~p", [Resp]),

    handle_response(C2CId, Context, Contact, Resp).

-spec match_regexps(kz_term:binaries(), kz_term:ne_binary()) -> boolean().
match_regexps([Pattern | Rest], Number) ->
    case re:run(Number, Pattern) of
        {'match', _} -> 'true';
        'nomatch' -> match_regexps(Rest, Number)
    end;
match_regexps([], _Number) -> 'false'.

-spec exec_originate(kz_term:api_terms()) ->
          {'success', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
exec_originate(Request) ->
    Resp = kz_amqp_worker:call_collect(Request
                                      ,fun kapi_resource:publish_originate_req/1
                                      ,fun is_resp/1
                                      ,20 * ?MILLISECONDS_IN_SECOND
                                      ),
    handle_originate_resp(Resp).

-spec handle_originate_resp({'ok', kz_json:objects()} |
                            {'returned', kz_json:object(), kz_json:object()} |
                            {'error', _} |
                            {'timeout', _}
                           ) ->
          {'success', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
handle_originate_resp({'ok', [Resp|_]}) ->
    AppResponse = kz_json:get_first_defined([<<"Application-Response">>
                                            ,<<"Hangup-Cause">>
                                            ,<<"Error-Message">>
                                            ]
                                           ,Resp
                                           ),
    case lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES) of
        'true' ->
            {'success', Resp};
        'false' when AppResponse =:= 'undefined' ->
            {'success', Resp};
        'false' ->
            lager:debug("app response ~s not successful: ~p", [AppResponse, Resp]),
            {'error', AppResponse}
    end;
handle_originate_resp({'returned', _JObj, Return}) ->
    case {kz_json:get_value(<<"code">>, Return)
         ,kz_json:get_value(<<"message">>, Return)
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

-record(contact, {route :: kz_term:ne_binary()
                 ,number :: kz_term:ne_binary()
                 ,name :: kz_term:ne_binary()
                 }).

-spec build_originate_req(kz_term:ne_binary(), cb_context:context()) -> kz_term:proplist().
build_originate_req(Contact, Context) ->
    AccountId = cb_context:account_id(Context),
    C2CDoc = cb_context:doc(Context),
    Exten = knm_converters:normalize(kzd_clicktocall:extension(C2CDoc)),
    CalleeName = kzd_clicktocall:outbound_callee_id_name(C2CDoc, Exten),
    CalleeNumber = knm_converters:normalize(kzd_clicktocall:outbound_callee_id_number(C2CDoc, Exten)),

    FriendlyName = kzd_clicktocall:name(C2CDoc, <<>>),
    OutboundNumber = kzd_clicktocall:caller_id_number(C2CDoc, Contact),
    AutoAnswer = kz_json:is_true(<<"auto_answer">>, cb_context:query_string(Context), 'true'),

    DialFirst = kzd_clicktocall:dial_first(C2CDoc, <<"extension">>),

    {Caller, Callee} = get_caller_callee(DialFirst
                                        ,#contact{number = OutboundNumber
                                                 ,name = FriendlyName
                                                 ,route = Contact
                                                 }
                                        ,#contact{number = CalleeNumber
                                                 ,name = CalleeName
                                                 ,route = Exten
                                                 }
                                        ),

    lager:debug("attempting clicktocall ~s in account ~s", [FriendlyName, AccountId]),
    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    AccountRealm = kzd_accounts:realm(AccountDoc),

    CCVs = [{<<"Account-ID">>, AccountId}
           ,{<<"Auto-Answer-Loopback">>, AutoAnswer}
           ,{<<"Authorizing-ID">>, kz_doc:id(C2CDoc)}
           ,{<<"Authorizing-Type">>, <<"clicktocall">>}
           ,{<<"Loopback-Request-URI">>, <<OutboundNumber/binary, "@", AccountRealm/binary>>}
           ,{<<"From-URI">>, <<CalleeNumber/binary, "@", AccountRealm/binary>>}
           ,{<<"Request-URI">>, <<OutboundNumber/binary, "@", AccountRealm/binary>>}
           ,{<<"Inherit-Codec">>, 'false'}
           ,{<<"Retain-CID">>, 'true'}
           ],
    CAVs = cb_modules_util:cavs_from_context(Context),

    Endpoint = [{<<"Invite-Format">>, <<"loopback">>}
               ,{<<"Route">>,  Callee#contact.route}
               ,{<<"To-DID">>, Callee#contact.route}
               ,{<<"To-Realm">>, AccountRealm}
               ],

    MsgId = kz_binary:rand_hex(8),
    CallId = kz_binary:join([<<"c2c">>, kz_doc:id(C2CDoc), kz_binary:rand_hex(5)], <<"-">>),

    props:filter_undefined(
      [{<<"Application-Name">>, <<"transfer">>}
      ,{<<"Application-Data">>, kz_json:from_list([{<<"Route">>, Caller#contact.route}])}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Msg-ID">>, MsgId}
      ,{<<"Endpoints">>, [kz_json:from_list(Endpoint)]}
      ,{<<"Timeout">>, kzd_clicktocall:timeout(C2CDoc, 30)}
      ,{<<"Ignore-Early-Media">>, get_ignore_early_media(C2CDoc)}
      ,{<<"Bypass-Media">>, kzd_clicktocall:bypass_media(C2CDoc, 'undefined')}
      ,{<<"Hold-Media">>, kzd_clicktocall:music_on_hold_media_id(C2CDoc)}
      ,{<<"Presence-ID">>, kzd_clicktocall:presence_id(C2CDoc)}
      ,{<<"Outbound-Callee-ID-Name">>, Callee#contact.name}
      ,{<<"Outbound-Callee-ID-Number">>, Callee#contact.number}
      ,{<<"Outbound-Caller-ID-Name">>, Caller#contact.name}
      ,{<<"Outbound-Caller-ID-Number">>, Caller#contact.number}
      ,{<<"Outbound-Call-ID">>, CallId}
      ,{<<"Origination-Call-ID">>, CallId}
      ,{<<"Ringback">>, kzd_clicktocall:ringback(C2CDoc)}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{<<"Continue-On-Fail">>, 'true'}
      ,{<<"Custom-SIP-Headers">>, kzd_clicktocall:custom_sip_headers(C2CDoc)}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
      ,{<<"Custom-Application-Vars">>, kz_json:from_list(CAVs)}
      ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>
                                          ,<<"Loopback-Request-URI">>
                                          ,<<"From-URI">>, <<"Request-URI">>
                                          ]
       }
      ,{<<"Simplify-Loopback">>, 'false'}
      ,{<<"Loopback-Bowout">>, 'false'}
      ,{<<"Start-Control-Process">>, 'false'}
       | kz_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec get_caller_callee(kz_term:ne_binary(), #contact{}, #contact{}) -> {#contact{}, #contact{}}.
get_caller_callee(<<"extension">>, Contact, Extension) -> {Contact, Extension};
get_caller_callee(<<"contact">>, Contact, Extension) -> {Extension, Contact}.

-spec get_ignore_early_media(kzd_clicktocall:doc()) -> boolean().
get_ignore_early_media(C2CDoc) ->
    kzd_clicktocall:media_ignore_early_media(C2CDoc, 'true').

-spec is_resp(kz_json:objects() | kz_json:object()) -> boolean().
is_resp([JObj|_]) -> is_resp(JObj);
is_resp(JObj) ->
    kapi_resource:originate_resp_v(JObj)
        orelse kz_api:error_resp_v(JObj).

-spec get_c2c_contact(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
get_c2c_contact('undefined') -> 'undefined';
get_c2c_contact(Contact) ->
    knm_converters:normalize(kz_http_util:urlencode(Contact)).

-spec create_c2c_history_item({'success' | 'error', kz_json:object()}, kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
create_c2c_history_item({'success', Resp}, C2CId, Contact) ->
    [{<<"timestamp">>, kz_time:now_s()}
    ,{<<"contact">>, Contact}
    ,{<<"call_id">>, kz_api:call_id(Resp)}
    ,{<<"result">>, <<"success">>}
    ,{<<"pvt_clicktocall_id">>, C2CId}
    ];
create_c2c_history_item({'error', Error}, C2CId, Contact) ->
    [{<<"timestamp">>, kz_time:now_s()}
    ,{<<"contact">>, Contact}
    ,{<<"result">>, <<"error">>}
    ,{<<"cause">>, Error}
    ,{<<"pvt_clicktocall_id">>, C2CId}
    ].
