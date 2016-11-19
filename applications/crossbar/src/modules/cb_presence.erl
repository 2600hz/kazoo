%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors

%%%-------------------------------------------------------------------
-module('cb_presence').

-export([init/0
        ,authenticate/1
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/2
        ,validate/1, validate/2
        ,post/1, post/2
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".presence">>).

-define(PRESENTITY_KEY, <<"include_presentity">>).
-define(PRESENTITY_CFG_KEY, <<"query_include_presentity">>).

-define(PRESENCE_QUERY_TIMEOUT_KEY, <<"query_presence_timeout">>).
-define(PRESENCE_QUERY_DEFAULT_TIMEOUT, 1000).
-define(PRESENCE_QUERY_TIMEOUT, kapps_config:get_integer(?MOD_CONFIG_CAT
                                                        ,?PRESENCE_QUERY_TIMEOUT_KEY
                                                        ,?PRESENCE_QUERY_DEFAULT_TIMEOUT
                                                        )
       ).
-define(REPORT_CONTENT_TYPE, [{'send_file', [{<<"application">>, <<"json">>}]}]).
-define(REPORT_PREFIX, "report-").
-define(MATCH_REPORT_PREFIX(ReportId), <<?REPORT_PREFIX, ReportId/binary>>).
-define(MATCH_REPORT_PREFIX, <<?REPORT_PREFIX, _ReportId/binary>>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    Bindings = [{<<"*.allowed_methods.presence">>, 'allowed_methods'}
               ,{<<"*.authenticate">>, 'authenticate'}
               ,{<<"*.authorize">>, 'authorize'}
               ,{<<"*.resource_exists.presence">>, 'resource_exists'}
               ,{<<"*.content_types_provided.presence">>, 'content_types_provided'}
               ,{<<"*.validate.presence">>, 'validate'}
               ,{<<"*.execute.post.presence">>, 'post'}
               ],
    _ = cb_modules_util:bind(?MODULE, Bindings),
    ok.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authenticate(cb_context:context(), req_nouns(), http_method()) -> boolean().
authenticate(Context, [{<<"presence">>,[?MATCH_REPORT_PREFIX]}], ?HTTP_GET) ->
    cb_context:magic_pathed(Context);
authenticate(_Context, _Nouns, _Verb) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), req_nouns(), http_method()) -> boolean().
authorize(Context, [{<<"presence">>,[?MATCH_REPORT_PREFIX]}], ?HTTP_GET) ->
    cb_context:magic_pathed(Context);
authorize(_Context, _Nouns, _Verb) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(?MATCH_REPORT_PREFIX) ->
    [?HTTP_GET];
allowed_methods(_Extension) ->
    [?HTTP_POST].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_Extension) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function allows report to be downloaded.
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, ?MATCH_REPORT_PREFIX(Report)) ->
    content_types_provided_for_report(Context, Report);
content_types_provided(Context, _) -> Context.

-spec content_types_provided_for_report(cb_context:context(), ne_binary()) -> cb_context:context().
content_types_provided_for_report(Context, Report) ->
    File = <<"/tmp/", Report/binary, ".json">>,
    case filelib:is_file(File) of
        'false' -> Context;
        'true' -> cb_context:set_content_types_provided(Context, ?REPORT_CONTENT_TYPE)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
validate(Context) ->
    validate_thing(Context, cb_context:req_verb(Context)).

validate(Context, ?MATCH_REPORT_PREFIX(Report)) ->
    load_report(Context, Report);
validate(Context, _Extension) ->
    cb_context:set_resp_status(Context, 'success').

-spec validate_thing(cb_context:context(), http_method()) ->
                            cb_context:context().
validate_thing(Context, ?HTTP_GET) ->
    validate_search(Context);
validate_thing(Context, ?HTTP_POST) ->
    validate_thing_reset(Context, cb_context:req_nouns(Context)).

-spec validate_search(cb_context:context()) -> cb_context:context().
validate_search(Context) ->
    Funs = [fun search_req/1
            | maybe_include_presentities(Context)
           ],
    search(Context, Funs).


-spec should_include_presentity(ne_binary()) -> boolean().
should_include_presentity(AccountId) ->
    kz_util:is_true(kapps_account_config:get_global(AccountId, ?MOD_CONFIG_CAT, ?PRESENTITY_CFG_KEY, 'false')).

-spec maybe_include_presentities(cb_context:context()) -> list().
maybe_include_presentities(Context) ->
    Default = should_include_presentity(cb_context:account_id(Context)),
    case kz_util:is_true(cb_context:req_param(Context, ?PRESENTITY_KEY, Default)) of
        'true' -> [fun presentity_search_req/1];
        'false' -> []
    end.

-spec search(cb_context:context(), list()) -> cb_context:context().
search(Context, Funs) ->
    Self = self(),
    lists:foreach(fun(Fun) -> search_spawn(Self, Fun, Context) end, Funs),
    search_collect(Context, kz_json:new(), length(Funs)).

-spec search_spawn(pid(), fun(), cb_context:context()) -> any().
search_spawn(Pid, Fun, Context) ->
    F = fun() -> Pid ! Fun(Context) end,
    kz_util:spawn(F).

-spec search_collect(cb_context:context(), kz_json:object(), integer()) -> cb_context:context().
search_collect(Context, JObj, 0) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]
                      );
search_collect(Context, JObj, N) ->
    receive
        {'ok', Reply} -> search_collect(Context, kz_json:merge_jobjs(Reply, JObj), N - 1);
        {'error', Reason} ->
            lager:debug("error collecting responses from presence : ~p", [Reason]),
            search_collect(Context, JObj, N - 1)
    after ?PRESENCE_QUERY_TIMEOUT ->
            lager:debug("timeout (~B) collecting responses from presence", [?PRESENCE_QUERY_TIMEOUT]),
            search_collect(Context, JObj, N - 1)
    end.

-spec search_req(cb_context:context()) ->
                        {'ok', kz_json:object()} |
                        {'error', any()}.
search_req(Context) ->
    Req = [{<<"Realm">>, cb_context:account_realm(Context)}
          ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
          ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_presence:publish_search_req/1
                                    ,{'omnipresence', 'true', 'true'}
                                    )
    of
        {'error', _R}=Err -> Err;
        {'ok', JObjs} ->
            process_search_responses(JObjs);
        {'timeout', JObjs} ->
            process_search_responses(JObjs, 'true')
    end.

-spec process_search_responses(kz_json:objects()) ->
                                      {'ok', kz_json:object()}.
-spec process_search_responses(kz_json:objects(), api_boolean()) ->
                                      {'ok', kz_json:object()}.
process_search_responses(JObjs) ->
    process_search_responses(JObjs, 'undefined').

process_search_responses(JObjs, Timeout) ->
    Subscriptions = extract_subscriptions_from_results(JObjs),
    {'ok'
    ,kz_json:from_list(
       props:filter_undefined(
         [{<<"subscriptions">>, Subscriptions}
         ,{<<"timeout">>, Timeout}
         ]
        )
      )
    }.

-spec extract_subscriptions_from_results(kz_json:objects()) ->
                                                kz_json:object().
extract_subscriptions_from_results(JObjs) ->
    lists:foldl(fun extract_subscriptions_from_result/2, kz_json:new(), JObjs).

-spec extract_subscriptions_from_result(kz_json:object(), kz_json:object()) ->
                                               kz_json:object().
extract_subscriptions_from_result(JObj, Acc) ->
    Subscriptions = kz_json:get_value(<<"Subscriptions">>, JObj, []),
    lists:foldl(fun extract_subscription/2, Acc, Subscriptions).

-spec extract_subscription(kz_json:object(), kz_json:object()) ->
                                  kz_json:object().
extract_subscription(Subscription, Acc) ->
    Key = [kz_json:get_value(<<"username">>, Subscription)
          ,kz_json:get_value(<<"event">>, Subscription)
          ,kz_json:get_value(<<"call_id">>, Subscription)
          ],
    case kz_json:get_value(Key, Acc) of
        'undefined' ->
            add_subscription(Subscription, Acc, Key);
        _Sub -> Acc
    end.

-spec add_subscription(kz_json:object(), kz_json:object(), kz_json:path()) ->
                              kz_json:object().
add_subscription(Subscription, Acc, Key) ->
    kz_json:set_value(Key
                     ,kz_json:delete_keys([<<"username">>
                                          ,<<"user">>
                                          ,<<"event">>
                                          ,<<"realm">>
                                          ,<<"protocol">>
                                          ,<<"contact">>
                                          ,<<"call_id">>
                                          ]
                                         ,Subscription
                                         )
                     ,Acc
                     ).

-spec presentity_search_req(cb_context:context()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
presentity_search_req(Context) ->
    Req = [{<<"Realm">>, cb_context:account_realm(Context)}
          ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
          ,{<<"Scope">>, <<"presentity">>}
          ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Count = kz_nodes:whapp_count(<<"kamailio">>, 'true'),

    lager:debug("attempting presentity search from ~p servers", [Count]),

    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_omnipresence:publish_search_req/1
                                    ,{fun collect_presentities/2, {0, Count}}
                                    )
    of
        {'error', _E}=Err -> Err;
        {'ok', JObjs} ->
            process_presentity_responses(JObjs);
        {'timeout', JObjs} ->
            process_presentity_responses(JObjs, 'true')
    end.

-spec collect_presentities(kz_json:objects(), {integer(), integer()}) ->
                                  'true' |
                                  {'false', {integer(), integer()}}.
collect_presentities([Response | _], {Count, Max}) ->
    case Count + resp_value(Response) of
        Max -> 'true';
        V -> {'false', {V, Max}}
    end.

-spec resp_value(kz_json:object()) -> 0..1.
resp_value(Response) ->
    case kz_api:event_name(Response) of
        <<"search_resp">> -> 1;
        _EventName -> 0
    end.

-spec process_presentity_responses(kz_json:objects()) ->
                                          {'ok', kz_json:object()}.
-spec process_presentity_responses(kz_json:objects(), api_boolean()) ->
                                          {'ok', kz_json:object()}.
process_presentity_responses(JObjs) ->
    process_presentity_responses(JObjs, 'undefined').

process_presentity_responses(JObjs, Timeout) ->
    Presentities = extract_presentities_from_responses(JObjs),
    {'ok'
    ,kz_json:from_list(
       props:filter_undefined(
         [{<<"presentities">>, Presentities}
         ,{<<"timeout">>, Timeout}
         ]
        )
      )
    }.

extract_presentities_from_responses(JObjs) ->
    lists:foldl(fun extract_presentities_from_response/2, kz_json:new(), JObjs).

extract_presentities_from_response(JObj, Acc) ->
    extract_presentities_from_response(JObj, Acc, kz_api:event_name(JObj)).
extract_presentities_from_response(JObj, Acc, <<"search_partial_resp">>) ->
    process_partial_response(JObj, Acc);
extract_presentities_from_response(_JObj, Acc, _EventName) ->
    Acc.

process_partial_response(JObj, Acc) ->
    SubscriptionKeys = kz_json:get_keys(<<"Subscriptions">>, JObj),
    Node = kz_api:node(JObj),

    lists:foldl(fun(Key, Acc1) ->
                        process_partial_response(Key, Acc1, JObj, Node)
                end
               ,Acc
               ,SubscriptionKeys
               ).

process_partial_response(Key, Acc, JObj, Node) ->
    kz_json:set_value([Key, Node]
                     ,kz_json:get_value([<<"Subscriptions">>, Key], JObj)
                     ,Acc
                     ).

-spec validate_thing_reset(cb_context:context(), req_nouns()) ->
                                  cb_context:context().
validate_thing_reset(Context, [{<<"presence">>, []}
                              ,{<<"devices">>, [DeviceId]}
                              ,{<<"accounts">>, [_AccountId]}
                              ]) ->
    maybe_load_thing(Context, DeviceId);
validate_thing_reset(Context, [{<<"presence">>, []}
                              ,{<<"users">>, [UserId]}
                              ,{<<"accounts">>, [_AccountId]}
                              ]) ->
    case is_reset_request(Context) of
        'true' -> validate_user_reset(Context, UserId);
        'false' -> reset_validation_error(Context)
    end;
validate_thing_reset(Context, _ReqNouns) ->
    crossbar_util:response_faulty_request(Context).

-spec validate_user_reset(cb_context:context(), ne_binary()) -> cb_context:context().
validate_user_reset(Context, UserId) ->
    Context1 = crossbar_doc:load(UserId, Context, ?TYPE_CHECK_OPTION(kzd_user:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_load_user_devices(Context1);
        _Status ->
            Context
    end.

-spec maybe_load_user_devices(cb_context:context()) -> cb_context:context().
maybe_load_user_devices(Context) ->
    User = cb_context:doc(Context),
    case kzd_user:presence_id(User) of
        'undefined' -> load_user_devices(Context);
        _PresenceId -> Context
    end.

-spec load_user_devices(cb_context:context()) -> cb_context:context().
load_user_devices(Context) ->
    User = cb_context:doc(Context),
    Devices = kzd_user:devices(User),
    cb_context:set_doc(Context, Devices).

-spec maybe_load_thing(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_load_thing(Context, ThingId) ->
    case is_reset_request(Context) of
        'true' -> crossbar_doc:load(ThingId, Context, ?TYPE_CHECK_OPTION(kz_device:type())); %%validating device
        'false' ->
            lager:debug("user failed to include reset=true"),
            reset_validation_error(Context)
    end.

-spec is_reset_request(cb_context:context()) -> boolean().
is_reset_request(Context) ->
    kz_util:is_true(cb_context:req_value(Context, <<"reset">>)).

-spec reset_validation_error(cb_context:context()) -> cb_context:context().
reset_validation_error(Context) ->
    cb_context:add_validation_error(<<"reset">>
                                   ,<<"required">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Field must be set to true">>}
                                      ,{<<"target">>, <<"required">>}
                                      ]
                                     )
                                   ,Context
                                   ).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    Things = cb_context:doc(Context),
    _ = collect_report(Context, Things),
    send_reset(Context, Things).

-spec post(cb_context:context(), ne_binary()) -> cb_context:context().
post(Context, Extension) ->
    _ = collect_report(Context, Extension),
    publish_presence_reset(cb_context:account_realm(Context), Extension),
    crossbar_util:response_202(<<"reset command sent for extension ", Extension/binary>>, Context).

-spec send_reset(cb_context:context(), kz_json:object() | kz_json:objects()) ->
                        cb_context:context().
send_reset(Context, []) ->
    lager:debug("nothing to reset"),
    crossbar_util:response(<<"nothing to reset">>, Context);
send_reset(Context, [_|_]=Things) ->
    publish_reset(cb_context:account_realm(Context), Things),
    crossbar_util:response_202(<<"reset commands sent">>, Context);
send_reset(Context, Thing) ->
    send_reset(Context, [Thing]).

-spec publish_reset(ne_binary(), kz_json:objects()) -> 'ok'.
publish_reset(Realm, Things) ->
    F = fun (Thing) -> publish_presence_reset(Realm, find_presence_id(Thing)) end,
    lists:foreach(F, Things).

-spec publish_presence_reset(ne_binary(), api_binary()) -> 'ok'.
publish_presence_reset(_Realm, 'undefined') -> 'ok';
publish_presence_reset(Realm, PresenceId) ->
    lager:debug("resetting ~s @ ~s", [PresenceId, Realm]),
    API = [{<<"Realm">>, Realm}
          ,{<<"Username">>, PresenceId}
          ,{<<"Msg-ID">>, kz_util:get_callid()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(API, fun kapi_presence:publish_reset/1).

-spec find_presence_id(kz_json:object()) -> api_binary().
find_presence_id(JObj) ->
    case kz_device:is_device(JObj) of
        'true' -> kz_device:presence_id(JObj);
        'false' -> kzd_user:presence_id(JObj)
    end.

-spec load_report(cb_context:context(), path_token()) -> cb_context:context().
load_report(Context, Report) ->
    File = <<"/tmp/", Report/binary, ".json">>,
    case filelib:is_file(File) of
        'true' -> set_report(Context, File);
        'false' ->
            lager:error("invalid file while fetching report file ~s", [File]),
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, Report}]), Context)
    end.

-spec set_report(cb_context:context(), ne_binary()) -> cb_context:context().
set_report(Context, File) ->
    Name = kz_util:to_binary(filename:basename(File)),
    Headers = [{<<"Content-Disposition">>, <<"attachment; filename=", Name/binary>>}],
    cb_context:setters(Context,
                       [{fun cb_context:set_resp_file/2, File}
                       ,{fun cb_context:set_resp_etag/2, 'undefined'}
                       ,{fun cb_context:add_resp_headers/2, Headers}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]
                      ).

-spec collect_report(cb_context:context(), ne_binary() | kz_json:object() | kz_json:objects()) -> any().
collect_report(_Context, []) -> 'ok';
collect_report(Context, Param) ->
    Funs = [fun search_req/1
           ,fun presentity_search_req/1
           ],
    Ctx = search(Context, Funs),
    kz_util:spawn(fun send_report/2, [Ctx, Param]).

-spec send_report(cb_context:context(), ne_binary() | kz_json:object() | kz_json:objects()) -> 'ok'.
send_report(Context, Extension)
  when is_binary(Extension) ->
    Msg = <<"presence reset received for extension ", Extension/binary>>,
    format_and_send_report(Context, Msg);
send_report(Context, Things)
  when is_list(Things) ->
    Format = "presence reset received for user_id ~s~n~ndevices:~p",
    Ids = list_to_binary([io_lib:format("~n~s", [kz_doc:id(Thing)])  || Thing <- Things]),
    Msg = io_lib:format(Format, [cb_context:user_id(Context), Ids]),
    format_and_send_report(Context, Msg);
send_report(Context, Thing) ->
    Format = "presence reset received for ~s : ~s",
    Msg = io_lib:format(Format, [kz_doc:type(Thing), kz_doc:id(Thing)]),
    format_and_send_report(Context, Msg).

-spec format_and_send_report(cb_context:context(), iodata()) -> 'ok'.
format_and_send_report(Context, Msg) ->
    {ReportId, URL} = save_report(Context),
    Subject = io_lib:format("presence reset for account ~s", [cb_context:account_id(Context)]),
    Props = [{<<"Report-ID">>, ReportId}
            ,{<<"Node">>, node()}
            ],
    Headers = [{<<"Attachment-URL">>, URL}
              ,{<<"Account-ID">>, cb_context:account_id(Context)}
              ,{<<"Request-ID">>, cb_context:req_id(Context)}
              ],
    kz_notify:detailed_alert(Subject, Msg, Props, Headers).

-spec save_report(cb_context:context()) -> {ne_binary(), ne_binary()}.
save_report(Context) ->
    JObj = kz_json:encode(cb_context:resp_data(Context)),
    Report = kz_util:rand_hex_binary(16),
    File = <<"/tmp/", Report/binary, ".json">>,
    'ok' = file:write_file(File, JObj),
    Args = [cb_context:api_version(Context)
           ,cb_context:account_id(Context)
           ,?MATCH_REPORT_PREFIX(Report)
           ],
    Path = io_lib:format("/~s/accounts/~s/presence/~s", Args),
    MagicPath = kapps_util:to_magic_hash(Path),
    HostURL = cb_context:host_url(Context),
    URL = <<HostURL/binary, "/", MagicPath/binary>>,
    {Report, URL}.
