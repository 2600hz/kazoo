%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Devices module
%%% Handle client requests for device documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_quickcall).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,validate/2
        ,post/2
        ]).

-include("crossbar.hrl").

-define(QCALL_NUMBER_FILTER, [<<" ">>, <<",">>, <<".">>, <<"-">>, <<"(">>, <<")">>]).

-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"v2_resource.allowed_methods.quickcall">>, 'allowed_methods'}
               ,{<<"v2_resource.resource_exists.quickcall">>, 'resource_exists'}
               ,{<<"v2_resource.validate.quickcall">>, 'validate'}
               ,{<<"v2_resource.execute.post.quickcall">>, 'post'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_Number) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_Number) -> 'true'.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, _Number) ->
    validate_quickcall(Context, cb_context:req_nouns(Context)).

validate_quickcall(Context, ?DEVICES_QCALL_NOUNS(DeviceId, _Number)) ->
    validate_quickcall(load_endpoint(Context, DeviceId, kzd_devices:type()));
validate_quickcall(Context, ?USERS_QCALL_NOUNS(UserId, _Number)) ->
    validate_quickcall(load_endpoint(Context, UserId, kzd_users:type())).

-spec validate_quickcall(cb_context:context()) -> cb_context:context().
validate_quickcall(Context) ->
    Context1 = maybe_validate_quickcall(Context),
    case cb_context:has_errors(Context1) of
        'true' ->
            lager:info("validation of quickcall failed"),
            Context1;
        'false' -> maybe_originate(Context1, cb_context:req_verb(Context))
    end.

-spec maybe_originate(cb_context:context(), http_method()) -> cb_context:context().
maybe_originate(Context, ?HTTP_GET) ->
    maybe_originate_quickcall(Context);
maybe_originate(Context, ?HTTP_POST) ->
    Context.

-spec load_endpoint(cb_context:context(), path_token(), kz_term:ne_binary()) -> cb_context:context().
load_endpoint(Context, EndpointId, EndpointType) ->
    crossbar_doc:load(EndpointId, Context, ?TYPE_CHECK_OPTION(EndpointType)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Number) ->
    maybe_originate_quickcall(Context).

-spec maybe_validate_quickcall(cb_context:context()) ->
          cb_context:context().
maybe_validate_quickcall(Context) ->
    case kz_buckets:consume_tokens(?APP_NAME
                                  ,cb_modules_util:bucket_name(Context)
                                  ,cb_modules_util:token_cost(Context, 1, [?QUICKCALL_PATH_TOKEN])
                                  )
    of
        'false' -> cb_context:add_system_error('too_many_requests', Context);
        'true' -> maybe_validate_quickcall(Context, cb_context:resp_status(Context))
    end.

-spec maybe_validate_quickcall(cb_context:context(), crossbar_status()) ->
          cb_context:context().
maybe_validate_quickcall(Context, 'success') ->
    AllowAnon = kz_json:is_true(<<"allow_anonymous_quickcalls">>, cb_context:doc(Context)),

    case AllowAnon
        orelse cb_context:is_authenticated(Context)
        orelse kapps_config:get_is_true(?CONFIG_CAT, <<"default_allow_anonymous_quickcalls">>, 'true')
    of
        'false' -> cb_context:add_system_error('invalid_credentials', Context);
        'true' -> Context
    end;
maybe_validate_quickcall(Context, _Status) ->
    lager:info("quickcall failed to validate, status ~p, not proceeding", [_Status]),
    Context.

-spec maybe_originate_quickcall(cb_context:context()) -> cb_context:context().
maybe_originate_quickcall(Context) ->
    Call = create_call_from_context(Context),
    case get_endpoints(Call, Context) of
        [] ->
            lager:info("no endpoints found"),
            cb_context:add_system_error('unspecified_fault', Context);
        Endpoints ->
            originate_quickcall(Endpoints, Call, Context)
    end.

-spec create_call_from_context(cb_context:context()) -> kapps_call:call().
create_call_from_context(Context) ->
    Routines =
        [{F, V}
         || {F, V} <- [{fun kapps_call:set_account_db/2, cb_context:db_name(Context)}
                      ,{fun kapps_call:set_account_id/2, cb_context:account_id(Context)}
                      ,{fun kapps_call:set_resource_type/2, <<"audio">>}
                      ,{fun kapps_call:set_owner_id/2, kz_json:get_ne_binary_value(<<"owner_id">>, cb_context:doc(Context))}
                       | request_specific_extraction_funs(Context)
                      ],
            'undefined' =/= V
        ],
    kapps_call:exec(Routines, kapps_call:new()).

-spec request_specific_extraction_funs(cb_context:context()) -> kapps_call:exec_funs().
request_specific_extraction_funs(Context) ->
    request_specific_extraction_funs_from_nouns(Context, cb_context:req_nouns(Context)).

-spec request_specific_extraction_funs_from_nouns(cb_context:context(), req_nouns()) ->
          kapps_call:exec_funs().
request_specific_extraction_funs_from_nouns(Context, ?DEVICES_QCALL_NOUNS(DeviceId, Number)) ->
    NumberURI = build_number_uri(Context, Number),
    [{fun kapps_call:set_authorizing_id/2, DeviceId}
    ,{fun kapps_call:set_authorizing_type/2, <<"device">>}
    ,{fun kapps_call:set_request/2, NumberURI}
    ,{fun kapps_call:set_to/2, NumberURI}
    ];
request_specific_extraction_funs_from_nouns(Context, ?USERS_QCALL_NOUNS(UserId, Number)) ->
    NumberURI = build_number_uri(Context, Number),
    [{fun kapps_call:set_authorizing_id/2, UserId}
    ,{fun kapps_call:set_authorizing_type/2, <<"user">>}
    ,{fun kapps_call:set_request/2, NumberURI}
    ,{fun kapps_call:set_to/2, NumberURI}
    ];
request_specific_extraction_funs_from_nouns(_Context, _ReqNouns) ->
    [].

-spec filter_number_regex(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
filter_number_regex(Number, Regex) ->
    case re:run(Number, Regex, [{'capture', 'all_but_first', 'binary'}]) of
        {'match', [Match|_]} ->
            lager:info("filtered number using regex ~p, result: ~p", [Regex, Match]),
            Match;

        _NotMatching ->
            lager:warning("tried to filter number ~p with regex ~p, but no match found", [Number, Regex]),
            Number
    end.

-spec build_number_uri(cb_context:context(), kz_term:ne_binary()) -> kz_term:ne_binary().
build_number_uri(Context, Number) ->
    QueryStr  = cb_context:query_string(Context),
    FilterVal = kz_json:get_value(<<"number_filter">>, QueryStr, <<"true">>),

    UseNumber = case FilterVal of
                    <<"false">> -> Number;
                    <<"true">>  -> binary:replace(Number, ?QCALL_NUMBER_FILTER, <<>>, ['global']);
                    FilterRegex -> filter_number_regex(Number, FilterRegex)
                end,

    Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    <<UseNumber/binary, "@", Realm/binary>>.

-spec get_endpoints(kapps_call:call(), cb_context:context()) -> kz_json:objects().
get_endpoints(Call, Context) ->
    get_endpoints(Call, Context, cb_context:req_nouns(Context)).

-spec get_endpoints(kapps_call:call(), cb_context:context(), req_nouns()) -> kz_json:objects().
get_endpoints(Call, _Context, ?DEVICES_QCALL_NOUNS(DeviceId, Number)) ->
    Properties = kz_json:from_list([{<<"can_call_self">>, 'true'}
                                   ,{<<"suppress_clid">>, 'true'}
                                   ,{<<"source">>, <<"cb_devices">>}
                                   ]),
    case kz_endpoint:build(DeviceId, Properties, aleg_cid(Number, Call)) of
        {'error', _} -> [];
        {'ok', Endpoints} -> Endpoints
    end;
get_endpoints(Call, _Context, ?USERS_QCALL_NOUNS(_UserId, Number)) ->
    Properties = kz_json:from_list([{<<"can_call_self">>, 'true'}
                                   ,{<<"suppress_clid">>, 'true'}
                                   ,{<<"source">>, <<"cb_users">>}
                                   ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case kz_endpoint:build(EndpointId, Properties, aleg_cid(Number, Call)) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end
               ,[]
               ,kz_attributes:owned_by(_UserId, <<"device">>, Call)
               );
get_endpoints(_Call, _Context, _ReqNouns) ->
    [].

-spec aleg_cid(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
aleg_cid(Number, Call) ->
    Routines = [{fun kapps_call:set_custom_channel_var/3, <<"Retain-CID">>, <<"true">>}
               ,{fun kapps_call:set_caller_id_name/2, <<"QuickCall">>}
               ,{fun kapps_call:set_caller_id_number/2, kz_term:to_binary(Number)}
               ],
    kapps_call:exec(Routines, Call).

-spec originate_quickcall(kz_json:objects(), kapps_call:call(), cb_context:context()) ->
          cb_context:context().
originate_quickcall(Endpoints, Call, Context) ->
    AutoAnswer = kz_json:is_true(<<"auto_answer">>, cb_context:query_string(Context), 'true'),
    CCVs = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Inherit-Codec">>, <<"false">>}
           ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
           ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
            | maybe_retain_cid(Context)
           ],

    CAVs = cb_modules_util:cavs_from_context(Context),

    MsgId = case kz_term:is_empty(cb_context:req_id(Context)) of
                'true' -> kz_binary:rand_hex(16);
                'false' -> cb_context:req_id(Context)
            end,

    Number = kapps_call:request_user(Call),
    AccountId = cb_context:account_id(Context),
    CIDType = case knm_converters:is_reconcilable(Number, AccountId) of
                  'true' -> <<"external">>;
                  'false' -> <<"internal">>
              end,
    {DefaultCIDNumber, DefaultCIDName} = kz_attributes:caller_id(CIDType, Call),
    lager:debug("quickcall default cid ~s : ~s : ~s", [CIDType, DefaultCIDNumber, DefaultCIDName]),

    CallTimeoutS = get_timeout(Context),

    Request =
        kz_json:from_list(
          [{<<"Application-Name">>, <<"transfer">>}
          ,{<<"Application-Data">>, get_application_data(Context)}
          ,{<<"Continue-On-Fail">>, 'false'}
          ,{<<"Custom-Application-Vars">>, kz_json:from_list(CAVs)}
          ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
          ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
          ,{<<"Endpoints">>, update_quickcall_endpoints(AutoAnswer, Endpoints)}
          ,{<<"Existing-Call-ID">>, cb_context:req_value(Context, <<"target_call_id">>)}
          ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                              ,<<"Authorizing-ID">>
                                              ,<<"Authorizing-Type">>
                                              ,<<"Outbound-Callee-ID-Name">>
                                              ,<<"Outbound-Callee-ID-Number">>
                                              ,<<"Retain-CID">>
                                              ]}
          ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Context)}
          ,{<<"Media">>, get_media(Context)}
          ,{<<"Msg-ID">>, MsgId}
          ,{<<"Outbound-Callee-ID-Name">>, get_cid_name(Context, DefaultCIDName)}
          ,{<<"Outbound-Callee-ID-Number">>, get_cid_number(Context, DefaultCIDNumber)}
          ,{<<"Outbound-Caller-ID-Name">>, <<"Device QuickCall">>}
          ,{<<"Outbound-Caller-ID-Number">>, kapps_call:request_user(Call)}
          ,{<<"Timeout">>, CallTimeoutS}
           | kz_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
          ]),

    originate(Request, Context, cb_context:req_verb(Context), CallTimeoutS).

originate(Request, Context, ?HTTP_GET, _CallTimeoutS) ->
    _ = kz_amqp_worker:cast(Request, fun kapi_resource:publish_originate_req/1),
    JObj = kz_json:normalize(kz_api:remove_defaults(Request)),
    crossbar_util:response_202(<<"quickcall initiated">>, JObj, cb_context:set_resp_data(Context, Request));
originate(Request, Context, ?HTTP_POST, CallTimeoutS) ->
    case kz_amqp_worker:call_collect(Request
                                    ,fun kapi_resource:publish_originate_req/1
                                    ,fun([Resp | _Resps]) ->
                                             kapi_resource:originate_resp_v(Resp)
                                                 orelse kz_api:error_resp_v(Resp)
                                     end
                                    ,CallTimeoutS * ?MILLISECONDS_IN_SECOND
                                    )
    of
        {'ok', JObjs} ->
            handle_originate_resp(Request, Context, JObjs);
        {'error', E} ->
            lager:info("error starting quickcall: ~p", [E]),
            cb_context:add_system_error(E, Context)
    end.

-spec handle_originate_resp(kz_json:object(), cb_context:context(), kz_json:objects()) ->
          cb_context:context().
handle_originate_resp(Request, Context, JObjs) ->
    case lists:filter(fun kapi_resource:originate_resp_v/1, JObjs) of
        [Resp] -> send_originate_resp(Request, Context, Resp);
        [] -> send_error_resp(Context, lists:filter(fun kz_api:error_resp_v/1, JObjs))
    end.

-spec send_originate_resp(kz_json:object(), cb_context:context(), kz_json:object()) ->
          cb_context:context().
send_originate_resp(Request, Context, Response) ->
    RequestJObj = kz_json:normalize(kz_api:remove_defaults(Request)),
    ResponseJObj = kz_json:normalize(kz_api:remove_defaults(Response)),
    APIResponse = kz_json:merge(RequestJObj, ResponseJObj),
    crossbar_util:response_202(<<"quickcall initiated">>, APIResponse, cb_context:set_resp_data(Context, APIResponse)).

-spec send_error_resp(cb_context:context(), kz_json:objects()) ->
          cb_context:context().
send_error_resp(Context, []) ->
    crossbar_util:response('error'
                          ,<<"quickcall initiation failed">>
                          ,500
                          ,Context
                          );
send_error_resp(Context, [Error]) ->
    ErrorJObj = kz_json:normalize(kz_api:remove_defaults(Error)),
    crossbar_util:response('error'
                          ,<<"quickcall initiation failed">>
                          ,500
                          ,ErrorJObj
                          ,cb_context:set_resp_data(Context, ErrorJObj)
                          ).

-spec update_quickcall_endpoints(boolean(), kz_json:objects()) -> kz_json:objects().
update_quickcall_endpoints(AutoAnswer, [Endpoint]) ->
    WithAA = kz_json:set_value([<<"Custom-Channel-Vars">>, <<"Auto-Answer">>], AutoAnswer, Endpoint),
    [set_quickcall_outbound_call_id(WithAA)];
update_quickcall_endpoints(_AutoAnswer, Endpoints) ->
    [set_quickcall_outbound_call_id(Endpoint) || Endpoint <- Endpoints].

-spec set_quickcall_outbound_call_id(kz_json:object()) -> kz_json:object().
set_quickcall_outbound_call_id(Endpoint) ->
    CallId = <<(kz_binary:rand_hex(18))/binary, "-quickcall">>,
    kz_json:set_value(<<"Outbound-Call-ID">>, CallId, Endpoint).

-spec get_application_data(cb_context:context()) -> kz_json:object().
get_application_data(Context) ->
    get_application_data_from_nouns(cb_context:req_nouns(Context)).

-spec get_application_data_from_nouns(req_nouns()) -> kz_json:object().
get_application_data_from_nouns(?DEVICES_QCALL_NOUNS(_DeviceId, Number)) ->
    kz_json:from_list([{<<"Route">>, Number}]);
get_application_data_from_nouns(?USERS_QCALL_NOUNS(_UserId, Number)) ->
    kz_json:from_list([{<<"Route">>, Number}]);
get_application_data_from_nouns(_Nouns) ->
    kz_json:from_list([{<<"Route">>, <<"0">>}]).

-define(DEFAULT_TIMEOUT_S, 30).
-spec get_timeout(cb_context:context()) -> pos_integer().
get_timeout(Context) ->
    try kz_term:to_integer(cb_context:req_value(Context, <<"timeout">>, ?DEFAULT_TIMEOUT_S)) of
        Timeout when is_integer(Timeout), Timeout > 3 -> Timeout;
        _ -> ?DEFAULT_TIMEOUT_S
    catch
        _:_ -> ?DEFAULT_TIMEOUT_S
    end.

-spec get_ignore_early_media(cb_context:context()) -> boolean().
get_ignore_early_media(Context) ->
    kz_term:is_true(cb_context:req_value(Context, <<"ignore-early-media">>, 'true')).

-spec get_media(cb_context:context()) -> kz_term:ne_binary().
get_media(Context) ->
    case cb_context:req_value(Context, <<"media">>) of
        <<"bypass">> -> <<"bypass">>;
        _Else -> <<"process">>
    end.

-spec get_cid_name(cb_context:context(), kz_term:api_binary()) -> kz_term:api_binary().
get_cid_name(Context, Default) ->
    case cb_context:req_value(Context, <<"cid-name">>, Default) of
        'undefined' -> 'undefined';
        CIDName -> kz_http_util:urldecode(CIDName)
    end.

-spec get_cid_number(cb_context:context(), kz_term:api_binary()) -> kz_term:api_binary().
get_cid_number(Context, Default) ->
    case cb_context:req_value(Context, <<"cid-number">>, Default) of
        'undefined' -> 'undefined';
        CIDNumber -> kz_http_util:urldecode(CIDNumber)
    end.

-spec maybe_retain_cid(cb_context:context()) -> kz_term:proplist().
maybe_retain_cid(Context) ->
    case cb_context:req_value(Context, <<"cid-number">>) of
        'undefined' -> [{<<"Retain-CID">>, <<"false">>}];
        _Found -> [{<<"Retain-CID">>, <<"true">>}]
    end.
