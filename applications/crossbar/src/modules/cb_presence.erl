%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module('cb_presence').

-export([init/0
        ,authenticate/1, authenticate/2
        ,authorize/1, authorize/2
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
-define(PRESENCE_QUERY_DEFAULT_TIMEOUT, 5000).
-define(PRESENCE_QUERY_TIMEOUT, kapps_config:get_integer(?MOD_CONFIG_CAT
                                                        ,?PRESENCE_QUERY_TIMEOUT_KEY
                                                        ,?PRESENCE_QUERY_DEFAULT_TIMEOUT
                                                        )
       ).
-define(REPORT_CONTENT_TYPE, [{'send_file', [{<<"application">>, <<"json">>, '*'}]}]).
-define(REPORT_PREFIX, "report-").
-define(MATCH_REPORT_PREFIX(ReportId), <<?REPORT_PREFIX, ReportId/binary>>).
-define(MATCH_REPORT_PREFIX, <<?REPORT_PREFIX, _ReportId/binary>>).

-define(MANUAL_PRESENCE_DOC, <<"manual_presence">>).

-define(CONFIRMED, <<"confirmed">>).
-define(EARLY, <<"early">>).
-define(TERMINATED, <<"terminated">>).
-define(PRESENCE_STATES, [?CONFIRMED, ?EARLY, ?TERMINATED]).

-type search_result() :: {'ok', kz_json:object()} | {'error', any()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    Bindings = [{<<"*.allowed_methods.presence">>, 'allowed_methods'}
               ,{<<"*.authenticate.presence">>, 'authenticate'}
               ,{<<"*.authorize.presence">>, 'authorize'}
               ,{<<"*.resource_exists.presence">>, 'resource_exists'}
               ,{<<"*.content_types_provided.presence">>, 'content_types_provided'}
               ,{<<"*.validate.presence">>, 'validate'}
               ,{<<"*.execute.post.presence">>, 'post'}
               ],
    _ = cb_modules_util:bind(?MODULE, Bindings),
    ok.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _) ->
    authenticate_nouns(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authenticate_nouns(cb_context:context(), req_nouns(), http_method()) -> boolean().
authenticate_nouns(Context, [{<<"presence">>,[?MATCH_REPORT_PREFIX]}], ?HTTP_GET) ->
    cb_context:magic_pathed(Context);
authenticate_nouns(_Context, _Nouns, _Verb) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, _) ->
    authorize_nouns(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize_nouns(cb_context:context(), req_nouns(), http_method()) -> boolean().
authorize_nouns(Context, [{<<"presence">>,[?MATCH_REPORT_PREFIX]}], ?HTTP_GET) ->
    cb_context:magic_pathed(Context);
authorize_nouns(_Context, _Nouns, _Verb) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?MATCH_REPORT_PREFIX) ->
    [?HTTP_GET];
allowed_methods(_Extension) ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_Extension) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function allows report to be downloaded.
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, ?MATCH_REPORT_PREFIX(Report)) ->
    content_types_provided_for_report(Context, Report);
content_types_provided(Context, _) -> Context.

-spec content_types_provided_for_report(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
content_types_provided_for_report(Context, Report) ->
    File = <<"/tmp/", Report/binary, ".json">>,
    case filelib:is_file(File) of
        'false' -> Context;
        'true' -> cb_context:set_content_types_provided(Context, ?REPORT_CONTENT_TYPE)
    end.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) ->
          cb_context:context().
validate(Context) ->
    validate_thing(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) ->
          cb_context:context().
validate(Context, ?MATCH_REPORT_PREFIX(Report)) ->
    load_report(Context, Report);
validate(Context, Extension) ->
    search_detail(Context, Extension).

-spec validate_thing(cb_context:context(), http_method()) ->
          cb_context:context().
validate_thing(Context, ?HTTP_GET) ->
    search_summary(Context);
validate_thing(Context, ?HTTP_POST) ->
    validate_presence_thing(Context).

-spec search_summary(cb_context:context()) -> cb_context:context().
search_summary(Context) ->
    search_result(Context, search_req(Context, <<"summary">>)).

-spec search_detail(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
search_detail(Context, Extension) ->
    search_result(Context, search_req(Context, <<"detail">>, Extension)).

-spec search_result(cb_context:context(), search_result()) -> cb_context:context().
search_result(Context, {'ok', JObj}) ->
    Routines = [{fun cb_context:set_resp_data/2, JObj}
               ,{fun cb_context:set_resp_status/2, 'success'}
               ],
    cb_context:setters(Context, Routines);
search_result(Context, {'error', Error}) ->
    cb_context:add_system_error(Error, Context).

-spec search_req(cb_context:context(), kz_term:ne_binary()) -> search_result().
search_req(Context, SearchType) ->
    search_req(Context, SearchType, 'undefined').

-spec search_req(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) -> search_result().
search_req(Context, SearchType, Username) ->
    Req = [{<<"Realm">>, cb_context:account_realm(Context)}
          ,{<<"Username">>, Username}
          ,{<<"Search-Type">>, SearchType}
          ,{<<"Event-Package">>, cb_context:req_param(Context, <<"event">>)}
          ,{<<"System-Log-ID">>, cb_context:req_id(Context)}
          ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Count = kz_nodes:node_role_count(<<"Presence">>, 'true'),
    lager:debug("requesting presence ~s from ~B servers", [SearchType, Count]),
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_presence:publish_search_req/1
                                    ,{fun collect_results/2, {0, Count}}
                                    )
    of
        {'error', _R}=Err -> Err;
        {'ok', JObjs} when is_list(JObjs) -> process_responses(JObjs, SearchType, 'null');
        {'timeout', JObjs} when is_list(JObjs) -> process_responses(JObjs, SearchType, 'true')
    end.

-type collect_params() :: {integer(), integer()}.
-type collect_result() :: 'true' | {'false', collect_params()}.

-spec collect_results(kz_json:objects(), collect_params()) -> collect_result().
collect_results([Response | _], {Count, Max}) ->
    case Count + search_resp_value(kz_api:event_name(Response)) of
        Max -> 'true';
        V -> {'false', {V, Max}}
    end.

-spec search_resp_value(kz_term:ne_binary()) -> 0..1.
search_resp_value(<<"search_resp">>) -> 1;
search_resp_value(_) -> 0.

-type acc_function() :: fun((kz_json:object(), kz_json:object()) -> kz_json:object()).

-spec accumulator_fun(kz_term:ne_binary()) -> acc_function().
accumulator_fun(<<"summary">>) -> fun kz_json:sum/2;
accumulator_fun(<<"detail">>) -> fun kz_json:merge/2.

-spec process_responses(kz_json:objects(), kz_term:ne_binary(), atom()) -> {'ok', kz_json:object()}.
process_responses(JObjs, SearchType, Timeout) ->
    Fun = accumulator_fun(SearchType),
    Subscriptions = extract_subscriptions(JObjs, Fun),
    {'ok', kz_json:set_value(<<"timeout">>, Timeout, Subscriptions)}.

-spec extract_subscriptions(kz_json:objects(), acc_function()) -> kz_json:object().
extract_subscriptions(JObjs, Fun) ->
    lists:foldl(fun(JObj, Acc) -> Fun(kz_api:remove_defaults(JObj), Acc) end, kz_json:new(), JObjs).

-spec validate_presence_thing(cb_context:context()) -> cb_context:context().
validate_presence_thing(Context) ->
    validate_presence_thing(Context, cb_context:req_nouns(Context)).

-spec validate_presence_thing(cb_context:context(), req_nouns()) -> cb_context:context().
validate_presence_thing(Context, [{<<"presence">>, _}
                                 ,{<<"devices">>, [DeviceId]}
                                 ,{<<"accounts">>, [_AccountId]}
                                 ]) ->
    validate_action(load_device(Context, DeviceId));
validate_presence_thing(Context, [{<<"presence">>, _}
                                 ,{<<"users">>, [UserId]}
                                 ,{<<"accounts">>, [_AccountId]}
                                 ]) ->
    validate_action(load_presence_for_user(Context, UserId));
validate_presence_thing(Context, _ReqNouns) ->
    crossbar_util:response_faulty_request(Context).

-spec load_device(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_device(Context, ThingId) ->
    %% validating device
    crossbar_doc:load(ThingId, Context, ?TYPE_CHECK_OPTION(kzd_devices:type())).

-spec load_presence_for_user(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_presence_for_user(Context, UserId) ->
    %% load the user_doc if it has a presence_id set, otherwise load all the user's devices
    Context1 = crossbar_doc:load(UserId, Context, ?TYPE_CHECK_OPTION(kzd_users:type())),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_load_user_devices(Context1);
        _ -> Context
    end.

-spec maybe_load_user_devices(cb_context:context()) -> cb_context:context().
maybe_load_user_devices(Context) ->
    User = cb_context:doc(Context),
    case kzd_users:presence_id(User) of
        'undefined' -> load_user_devices(Context);
        _PresenceId -> Context
    end.

-spec load_user_devices(cb_context:context()) -> cb_context:context().
load_user_devices(Context) ->
    User = cb_context:doc(Context),
    Devices = kz_attributes:owned_by_docs(kz_doc:id(User), cb_context:account_id(Context)),
    cb_context:set_doc(Context, Devices).

-spec validate_action(cb_context:context()) -> cb_context:context().
validate_action(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> validate_action(Context, cb_context:req_data(Context));
        _ -> Context
    end.
validate_action(Context, ReqData) ->
    DeprecatedReset = case kz_json:get_value(<<"reset">>, ReqData, 'false') of
                          'true' -> <<"reset">>;
                          'false' -> 'undefined'
                      end,
    validate_action(Context, ReqData, kz_json:get_value(<<"action">>, ReqData, DeprecatedReset)).
validate_action(Context, _ReqData, <<"reset">>) ->
    Context;
validate_action(Context, ReqData, <<"set">>) ->
    State = kz_json:get_value(<<"state">>, ReqData),
    case lists:member(State, ?PRESENCE_STATES) of
        'true' -> Context;
        'false' -> invalid_state(Context)
    end;
validate_action(Context, _ReqData, _InvalidAction) ->
    invalid_action(Context).

-spec invalid_action(cb_context:context()) -> cb_context:context().
invalid_action(Context) ->
    cb_context:add_validation_error(<<"action">>
                                   ,<<"invalid">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Field must be set to a valid action">>}
                                      ,{<<"target">>, <<"invalid">>}
                                      ]
                                     )
                                   ,Context
                                   ).

-spec invalid_state(cb_context:context()) -> cb_context:context().
invalid_state(Context) ->
    cb_context:add_validation_error(<<"state">>
                                   ,<<"invalid">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Field must be set to a valid state">>}
                                      ,{<<"target">>, <<"invalid">>}
                                      ]
                                     )
                                   ,Context
                                   ).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    ReqData = cb_context:req_data(Context),
    Things = cb_context:doc(Context),
    post_things(Context, Things, kz_json:get_value(<<"action">>, ReqData)).

-spec post(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
post(Context, Extension) ->
    _ = collect_report(Context, Extension),
    publish_presence_reset(Context, Extension),
    crossbar_util:response_202(<<"reset command sent for extension ", Extension/binary>>, Context).

-spec post_things(cb_context:context(), kz_json:object() | kz_json:objects(), kz_term:ne_binary()) ->
          cb_context:context().
post_things(Context, Things, <<"reset">>) ->
    _ = collect_report(Context, Things),
    send_command(Context, fun publish_presence_reset/2, Things);
post_things(Context, Things, <<"set">>) ->
    State = kz_json:get_value(<<"state">>, cb_context:req_data(Context)),
    send_command(Context, fun(C, Id) -> publish_presence_update(C, Id, State) end, Things).

-type presence_command_fun() :: fun((cb_context:context(), kz_term:api_binary()) -> any()).
-spec send_command(cb_context:context(), presence_command_fun(), kz_json:object() | kz_json:objects()) ->
          cb_context:context().
send_command(Context, _CommandFun, []) ->
    lager:debug("nothing to send command to"),
    crossbar_util:response(<<"nothing to send command to">>, Context);
send_command(Context, CommandFun, [_|_]=Things) ->
    lists:foreach(fun(Thing) -> CommandFun(Context, find_presence_id(Thing)) end, Things),
    crossbar_util:response_202(<<"command sent">>, Context);
send_command(Context, CommandFun, Thing) ->
    send_command(Context, CommandFun, [Thing]).

-spec publish_presence_reset(cb_context:context(), kz_term:api_binary()) -> 'ok'.
publish_presence_reset(_Context, 'undefined') -> 'ok';
publish_presence_reset(Context, PresenceId) ->
    Realm = cb_context:account_realm(Context),
    lager:debug("resetting ~s@~s", [PresenceId, Realm]),
    API = [{<<"Realm">>, Realm}
          ,{<<"Username">>, PresenceId}
          ,{<<"Msg-ID">>, kz_log:get_callid()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(API, fun kapi_presence:publish_reset/1).

-spec publish_presence_update(cb_context:context(), kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
publish_presence_update(_Context, 'undefined', _PresenceState) -> 'ok';
publish_presence_update(Context, PresenceId, PresenceState) ->
    Realm = cb_context:account_realm(Context),
    AccountDb = cb_context:db_name(Context),
    lager:debug("updating presence for ~s@~s to state ~s", [PresenceId, Realm, PresenceState]),
    %% persist presence setting
    Update = [{PresenceId, PresenceState}],
    UpdateOptions = [{'update', Update}],
    {'ok', _} = kz_datamgr:update_doc(AccountDb, ?MANUAL_PRESENCE_DOC, UpdateOptions),
    PresenceString = <<PresenceId/binary, "@", Realm/binary>>,
    kapps_call_command:presence(PresenceState, PresenceString, kz_term:to_hex_binary(crypto:hash('md5', PresenceString))).

-spec find_presence_id(kz_json:object()) -> kz_term:api_binary().
find_presence_id(JObj) ->
    case kzd_devices:is_device(JObj) of
        'true' -> kzd_devices:presence_id(JObj);
        'false' -> kzd_users:presence_id(JObj)
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

-spec set_report(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
set_report(Context, File) ->
    Name = kz_term:to_binary(filename:basename(File)),
    Headers = #{<<"Content-Disposition">> => <<"attachment; filename=", Name/binary>>},
    cb_context:setters(Context,
                       [{fun cb_context:set_resp_file/2, File}
                       ,{fun cb_context:set_resp_etag/2, 'undefined'}
                       ,{fun cb_context:add_resp_headers/2, Headers}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]
                      ).

-spec collect_report(cb_context:context(), kz_term:ne_binary() | kz_json:object() | kz_json:objects()) -> any().
collect_report(_Context, []) ->
    lager:debug("nothing to collect");
collect_report(Context, Param) ->
    lager:debug("collecting report for ~s", [Param]),
    kz_process:spawn(fun send_report/2, [search_detail(Context, Param), Param]).

-spec send_report(cb_context:context(), kz_term:ne_binary() | kz_json:object() | kz_json:objects()) -> 'ok'.
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
    Format = "presence reset received for ~s: ~s",
    Msg = io_lib:format(Format, [kz_doc:type(Thing), kz_doc:id(Thing)]),
    format_and_send_report(Context, Msg).

-spec format_and_send_report(cb_context:context(), iodata()) -> 'ok'.
format_and_send_report(Context, Msg) ->
    lager:debug("formatting and sending report"),
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

-spec save_report(cb_context:context()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
save_report(Context) ->
    JObj = kz_json:encode(cb_context:resp_data(Context)),
    Report = kz_binary:rand_hex(16),
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
