%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
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
%%%   KAZOO-4175: Sponsored by Conversant Ltd,
%%%       implemented by SIPLABS, LLC (Ilya Ashchepkov)
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

-include("crossbar.hrl").

-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(HISTORY_LIST, <<"clicktocall/history_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, ?CONNECT_CALL]}, {?KZ_ACCOUNTS_DB, [_]}]).
-define(SUCCESSFUL_HANGUP_CAUSES, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.clicktocall">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.clicktocall">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.clicktocall">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.clicktocall">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.clicktocall">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.clicktocall">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.clicktocall">>, ?MODULE, 'delete'),
    ok.

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
allowed_methods(_C2CId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_C2CId, ?CONNECT_CALL) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_C2CId, ?HISTORY) ->
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
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?CONNECT_CALL) -> 'true';
resource_exists(_, ?HISTORY) -> 'true'.

-spec authenticate(cb_context:context()) -> 'true'.
authenticate(Context) ->
    is_c2c_url(Context, cb_context:req_nouns(Context))
        andalso maybe_authenticate(Context).

-spec authorize(cb_context:context()) -> 'true'.
authorize(Context) ->
    is_c2c_url(Context, cb_context:req_nouns(Context))
        andalso maybe_authorize(Context).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
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

-spec load_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
load_c2c(C2CId, Context) ->
    crossbar_doc:load(C2CId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)).

-spec load_c2c_history(ne_binary(), cb_context:context()) -> cb_context:context().
load_c2c_history(C2CId, Context) ->
    Options = ['include_docs'
              ,{'startkey', [C2CId]}
              ,{'endkey', [C2CId, kz_json:new()]}
              ],
    crossbar_doc:load_view(?HISTORY_LIST
                          ,Options
                          ,cb_context:set_account_db(Context, cb_context:account_modb(Context))
                          ,fun normalize_history_results/2
                          ).

-spec create_c2c(cb_context:context()) -> cb_context:context().
create_c2c(Context) ->
    cb_context:validate_request_data(<<"clicktocall">>, Context, fun clear_history_set_type/1).

-spec update_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
update_c2c(C2CId, Context) ->
    OnSuccess = fun(C) -> crossbar_doc:load_merge(C2CId, C, ?TYPE_CHECK_OPTION(?PVT_TYPE)) end,
    cb_context:validate_request_data(<<"clicktocall">>, Context, OnSuccess).

-spec validate_patch_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch_c2c(C2CId, Context) ->
    crossbar_doc:patch_and_validate(C2CId, Context, fun update_c2c/2).

-spec establish_c2c(ne_binary(), cb_context:context()) -> cb_context:context().
establish_c2c(C2CId, Context) ->
    Context1 = crossbar_doc:load(C2CId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    case cb_context:resp_status(Context1) of
        'success' -> originate_call(C2CId, Context1);
        _Status -> Context1
    end.

-spec maybe_migrate_history(ne_binary()) -> 'ok'.
maybe_migrate_history(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),

    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'ok', C2Cs} -> migrate_histories(AccountId, AccountDb, C2Cs);
        {'error', _} -> 'ok'
    end.

-spec migrate_histories(ne_binary(), ne_binary(), kz_json:objects()) -> 'ok'.
migrate_histories(AccountId, AccountDb, C2Cs) ->
    _ = [migrate_history(AccountId, AccountDb, kz_json:get_value(<<"doc">>, C2C)) || C2C <- C2Cs],
    'ok'.

-spec migrate_history(ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
migrate_history(AccountId, AccountDb, C2C) ->
    case kz_json:get_value(<<"pvt_history">>, C2C, []) of
        [] -> 'ok';
        History ->
            Id = kz_doc:id(C2C),
            _ = [save_history_item(AccountId, HistoryItem, Id) || HistoryItem <- History],
            _Resp = kz_datamgr:ensure_saved(AccountDb, kz_json:delete_key(<<"pvt_history">>, C2C)),
            lager:debug("removed history from c2c ~s in ~s: ~p", [Id
                                                                 ,AccountId
                                                                 ,_Resp
                                                                 ])
    end.

-spec save_history_item(ne_binary(), kz_json:object(), ne_binary()) -> any().
save_history_item(AccountId, HistoryItem, C2CId) ->
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, HistoryItem, kz_util:current_tstamp()),
    AccountModb = kz_util:format_account_mod_id(AccountId, Timestamp),
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
-spec originate_call(ne_binary(), cb_context:context(), api_binary(), boolean()) -> cb_context:context().
originate_call(C2CId, Context) ->
    originate_call(C2CId, Context, get_c2c_contact(cb_context:req_value(Context, <<"contact">>))).

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

originate_call(_C2CId, Context, Contact, 'false') ->
    crossbar_util:response_400(<<"Contact doesnt match whitelist">>, Contact, Context);
originate_call(C2CId, Context, Contact, 'true') ->
    ReqId = cb_context:req_id(Context),
    AccountId = cb_context:account_id(Context),
    AccountModb = cb_context:account_modb(Context),
    Request = build_originate_req(Contact, Context),
    _Pid = kz_util:spawn(
             fun() ->
                     kz_util:put_callid(ReqId),
                     Status = exec_originate(Request),
                     lager:debug("got status ~p", [Status]),

                     JObj = kz_json:from_list(
                              [{<<"pvt_clicktocall_id">>, C2CId}
                               | create_c2c_history_item(Status, Contact)
                              ]
                             ),
                     HistoryItem =
                         kz_doc:update_pvt_parameters(JObj
                                                     ,AccountModb
                                                     ,[{'account_id', AccountId}
                                                      ,{'type', <<"c2c_history">>}
                                                      ]
                                                     ),
                     kazoo_modb:save_doc(AccountId, HistoryItem)
             end),
    JObj = kz_json:normalize(kz_json:from_list(kz_api:remove_defaults(Request))),
    lager:debug("attempting call in ~p", [JObj]),
    crossbar_util:response_202(<<"processing request">>, JObj, cb_context:set_resp_data(Context, Request)).

-spec match_regexps(binaries(), ne_binary()) -> boolean().
match_regexps([Pattern | Rest], Number) ->
    case re:run(Number, Pattern) of
        {'match', _} -> 'true';
        'nomatch' -> match_regexps(Rest, Number)
    end;
match_regexps([], _Number) -> 'false'.

-spec exec_originate(api_terms()) ->
                            {'success', ne_binary()} |
                            {'error', ne_binary()}.
exec_originate(Request) ->
    handle_originate_resp(
      kz_amqp_worker:call_collect(Request
                                 ,fun kapi_resource:publish_originate_req/1
                                 ,fun is_resp/1
                                 ,20 * ?MILLISECONDS_IN_SECOND
                                 )
     ).

-spec handle_originate_resp({'ok', kz_json:objects()} |
                            {'returned', kz_json:object(), kz_json:object()} |
                            {'error', _} |
                            {'timeout', _}
                           ) ->
                                   {'success', ne_binary()} |
                                   {'error', ne_binary()}.
handle_originate_resp({'ok', [Resp|_]}) ->
    AppResponse = kz_json:get_first_defined([<<"Application-Response">>
                                            ,<<"Hangup-Cause">>
                                            ,<<"Error-Message">>
                                            ], Resp),
    case lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES) of
        'true' ->
            {'success', kz_json:get_value(<<"Call-ID">>, Resp)};
        'false' when AppResponse =:= 'undefined' ->
            {'success', kz_json:get_value(<<"Call-ID">>, Resp)};
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

-record(contact, {route, number, name}).
-spec build_originate_req(ne_binary(), cb_context:context()) -> api_terms().
build_originate_req(Contact, Context) ->
    AccountId = cb_context:account_id(Context),
    JObj = cb_context:doc(Context),
    Exten = knm_converters:normalize(kz_json:get_binary_value(<<"extension">>, JObj)),
    CalleeName = kz_json:get_binary_value(<<"outbound_callee_id_name">>, JObj, Exten),
    CalleeNumber = knm_converters:normalize(kz_json:get_binary_value(<<"outbound_callee_id_number">>, JObj, Exten)),
    FriendlyName = kz_json:get_ne_value(<<"name">>, JObj, <<>>),
    OutboundNumber = kz_json:get_value(<<"caller_id_number">>, JObj, Contact),
    AutoAnswer = kz_json:is_true(<<"auto_answer">>, cb_context:query_string(Context), 'true'),
    {Caller, Callee} = get_caller_callee(kz_json:get_value(<<"dial_first">>, JObj, <<"extension">>)
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
    {'ok', AccountDoc} = kz_account:fetch(AccountId),

    CCVs = [{<<"Account-ID">>, AccountId}
           ,{<<"Auto-Answer-Loopback">>, AutoAnswer}
           ,{<<"Authorizing-ID">>, kz_doc:id(JObj)}
           ,{<<"Inherit-Codec">>, <<"false">>}
           ,{<<"Authorizing-Type">>, <<"device">>}
           ,{<<"Loopback-Request-URI">>, <<OutboundNumber/binary, "@", (kz_account:realm(AccountDoc))/binary>>}
           ,{<<"From-URI">>, <<CalleeNumber/binary, "@", (kz_account:realm(AccountDoc))/binary>>}
           ,{<<"Request-URI">>, <<OutboundNumber/binary, "@", (kz_account:realm(AccountDoc))/binary>>}
           ],

    Endpoint = [{<<"Invite-Format">>, <<"loopback">>}
               ,{<<"Route">>,  Callee#contact.route}
               ,{<<"To-DID">>, Callee#contact.route}
               ,{<<"To-Realm">>, kz_account:realm(AccountDoc)}
               ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
               ],

    MsgId = kz_json:get_value(<<"msg_id">>, JObj, kz_util:rand_hex_binary(16)),
    CallId = <<(kz_util:rand_hex_binary(18))/binary, "-clicktocall">>,

    props:filter_undefined(
      [{<<"Application-Name">>, <<"transfer">>}
      ,{<<"Application-Data">>, kz_json:from_list([{<<"Route">>, Caller#contact.route}])}
      ,{<<"Msg-ID">>, MsgId}
      ,{<<"Endpoints">>, [kz_json:from_list(Endpoint)]}
      ,{<<"Timeout">>, kz_json:get_value(<<"timeout">>, JObj, 30)}
      ,{<<"Ignore-Early-Media">>, get_ignore_early_media(JObj)}
      ,{<<"Media">>, kz_json:get_value(<<"media">>, JObj)}
      ,{<<"Hold-Media">>, kz_json:get_value([<<"music_on_hold">>, <<"media_id">>], JObj)}
      ,{<<"Presence-ID">>, kz_json:get_value(<<"presence_id">>, JObj)}
      ,{<<"Outbound-Callee-ID-Name">>, Callee#contact.name}
      ,{<<"Outbound-Callee-ID-Number">>, Callee#contact.number}
      ,{<<"Outbound-Caller-ID-Name">>, Caller#contact.name}
      ,{<<"Outbound-Caller-ID-Number">>, Caller#contact.number}
      ,{<<"Outbound-Call-ID">>, CallId}
      ,{<<"Ringback">>, kz_json:get_value(<<"ringback">>, JObj)}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{<<"Continue-On-Fail">>, 'true'}
      ,{<<"Custom-SIP-Headers">>, kz_json:get_value(<<"custom_sip_headers">>, JObj)}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
      ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>
                                          ,<<"Auto-Answer-Loopback">>, <<"Loopback-Request-URI">>
                                          ,<<"From-URI">>, <<"Request-URI">>
                                          ]
       }
      ,{<<"Simplify-Loopback">>, <<"false">>}
      ,{<<"Loopback-Bowout">>, <<"false">>}
      ,{<<"Start-Control-Process">>, <<"false">>}
       | kz_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec get_caller_callee(ne_binary(), #contact{}, #contact{}) -> {#contact{}, #contact{}}.
get_caller_callee(<<"extension">>, Contact, Extension) -> {Contact, Extension};
get_caller_callee(<<"contact">>, Contact, Extension) -> {Extension, Contact}.

-spec get_ignore_early_media(kz_json:object()) -> boolean().
get_ignore_early_media(JObj) ->
    kz_util:is_true(kz_json:get_value([<<"media">>, <<"ignore_early_media">>], JObj, 'true')).

-spec is_resp(kz_json:objects() | kz_json:object()) -> boolean().
is_resp([JObj|_]) -> is_resp(JObj);
is_resp(JObj) ->
    kapi_resource:originate_resp_v(JObj)
        orelse kz_api:error_resp_v(JObj).

-spec get_c2c_contact(api_binary()) -> api_binary().
get_c2c_contact('undefined') -> 'undefined';
get_c2c_contact(Contact) ->
    knm_converters:normalize(kz_http_util:urlencode(Contact)).

-spec create_c2c_history_item({'success', ne_binary()} | {'error', ne_binary()}, ne_binary()) -> kz_proplist().
create_c2c_history_item({'success', CallId}, Contact) ->
    [{<<"timestamp">>, kz_util:current_tstamp()}
    ,{<<"contact">>, Contact}
    ,{<<"call_id">>, CallId}
    ,{<<"result">>, <<"success">>}
    ];
create_c2c_history_item({'error', Error}, Contact) ->
    [{<<"timestamp">>, kz_util:current_tstamp()}
    ,{<<"contact">>, Contact}
    ,{<<"result">>, <<"error">>}
    ,{<<"cause">>, Error}
    ].
