%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_alerts).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,delete/2
        ]).

-ifdef(TEST).
-export([check_port_request_status/1
        ,check_port_request_comments/1
        ,check_low_balance/1
        ]).
-endif.

-include("crossbar.hrl").

-define(AVAILABLE_LIST, <<"alerts/available">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.alerts">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.alerts">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.alerts">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.alerts">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.alerts">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.alerts">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_AlertId) ->
    [?HTTP_GET, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /alerts => []
%%    /alerts/foo => [<<"foo">>]
%%    /alerts/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /alerts might load a list of alert objects
%% /alerts/123 might load the alert object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_alerts(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_alert(Context, Id, cb_context:req_verb(Context)).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_ALERTS_DB),
    crossbar_doc:save(Context1).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_alerts(cb_context:context(), http_method()) -> cb_context:context().
validate_alerts(Context, ?HTTP_GET) ->
    summary(Context);
validate_alerts(Context, ?HTTP_PUT) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> create(Context);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_alert(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_alert(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_alert(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    Props = kz_json:to_proplist(cb_context:req_data(Context)),

    Title = props:get_value(kzd_alert:title(), Props),
    Msg = props:get_value(kzd_alert:message(), Props),
    From = props:get_value(kzd_alert:from(), Props),
    To = props:get_value(kzd_alert:to(), Props),

    case kapps_alert:create(Title, Msg, From, To, Props) of
        {'required', Item} ->
            cb_context:add_validation_error(Item
                                           ,<<"required">>
                                           ,<<"missing property">>
                                           ,Context
                                           );
        {'error', 'disabled'} ->
            cb_context:add_system_error('disabled', Context);
        {'ok', JObj} ->
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_doc/2, JObj}
                      ],
            cb_context:setters(Context, Setters)
    end.

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_ALERTS_DB),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(<<"alert">>)).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = load_summary(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            fix_envelope(Context1);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec load_summary(cb_context:context()) -> cb_context:context().
load_summary(Context) ->
    AccountDb = cb_context:account_db(Context),
    Context1 =
        crossbar_doc:load_view(?AVAILABLE_LIST
                              ,[{'keys', view_keys(Context)}]
                              ,cb_context:set_account_db(Context, ?KZ_ALERTS_DB)
                              ,fun normalize_view_results/2
                              ),
    Routines = [%fun check_credit_card/1
              % ,
               fun check_port_request_status/1
               ,fun check_port_request_comments/1
               ,fun check_low_balance/1
               ],
    Fun = fun(Routine, Cntxt) -> Routine(Cntxt) end,
    lists:foldl(Fun, cb_context:set_account_db(Context1, AccountDb), Routines).

%-spec check_credit_card(cb_context:context()) -> cb_context:context().
%check_credit_card(Context) ->
%    %% TODO: check implementation once kz_services:is_good_standing/1 gets created.
%    Resp = kz_services:is_good_standing(kz_services:fetch(cb_context:account_id(Context))),
%    check_credit_card(Context, Resp).
%
%-spec check_credit_card(cb_context:context(), atom() | {'error', atom()}) -> cb_context:context().
%check_credit_card(Context, 'true') ->
%    Context;
%check_credit_card(Context, {'error', Error}) ->
%    JObj = check_result_to_jobj(<<"credit_about_to_expire">>, Error),
%    cb_context:set_resp_data(Context, [AlertJObj | context_resp_data(Context)]).

-spec check_port_request_status(cb_context:context()) -> cb_context:context().
check_port_request_status(Context) ->
    ActivePortReqs = knm_port_request:account_active_ports(cb_context:account_id(Context)),
    check_port_request_status(Context, ActivePortReqs).

-spec check_port_request_status(cb_context:context(), {'ok', kz_json:objects()} | {'error', 'not_found'}) ->
    cb_context:context().
check_port_request_status(Context, {'error', 'not_found'}) ->
    Context;
check_port_request_status(Context, {'ok', Ports}) ->
    case lists:filter(fun is_rejected_or_scheduled/1, Ports) of
        [] -> Context;
        Data ->
            Alerts = check_port_request_status_results_to_alerts(Data),
            cb_context:set_resp_data(Context, Alerts ++ context_resp_data(Context))
    end.

-spec is_rejected_or_scheduled(kz_json:object()) -> boolean().
is_rejected_or_scheduled(Port) ->
    knm_port_request:current_state(Port) =:= <<"rejected">>
    orelse knm_port_request:current_state(Port) =:= <<"scheduled">>.

-spec check_port_request_status_results_to_alerts(kz_json:objects()) -> kz_json:objects() | [].
check_port_request_status_results_to_alerts(Ports) ->
    Type = <<"rejected_or_scheduled_port_requests">>,
    AlertsList = [[{<<"id">>, kz_doc:id(Port)}
                  ,{<<"name">>, kz_json:get_value(<<"name">>, Port)}
                  ,{<<"state">>, knm_port_request:current_state(Port)}
                  ,{<<"type">>, Type}
                  ] || Port <- Ports],
    [kz_json:from_list(Alert) || Alert <- AlertsList].

-spec check_port_request_comments(cb_context:context()) -> cb_context:context().
check_port_request_comments(Context) ->
    ActivePortReqs = knm_port_request:account_active_ports(cb_context:account_id(Context)),
    check_port_request_comments(Context, ActivePortReqs).

-spec check_port_request_comments(cb_context:context(), {'ok', kz_json:objects()} | {'error', 'not_found'}) ->
    cb_context:context().
check_port_request_comments(Context, {'error', 'not_found'}) ->
    Context;
check_port_request_comments(Context, {'ok', Ports}) ->
    case lists:filter(fun is_last_comment_waiting_for_reply/1, Ports) of
        [] -> Context;
        Data ->
            Alerts = check_port_request_comments_results_to_alerts(Data),
            cb_context:set_resp_data(Context, Alerts ++ context_resp_data(Context))
    end.

-spec is_last_comment_waiting_for_reply(kz_json:object()) -> boolean().
is_last_comment_waiting_for_reply(Port) ->
    check_last_comment(port_request_last_comment(Port)).

-spec port_request_last_comment(kz_json:object()) -> kz_json:object() | 'undefined'.
port_request_last_comment(Port) ->
    case kz_json:get_value(<<"comments">>, Port, []) of
        [] -> 'undefined';
        Comments -> hd(lists:reverse(Comments))
    end.

-spec check_last_comment(kz_json:object() | 'undefined') -> boolean().
check_last_comment('undefined') ->
    'false';
check_last_comment(Comment) ->
    kz_json:get_boolean_value(<<"waiting_for_reply">>, Comment, 'false').

-spec check_port_request_comments_results_to_alerts(kz_json:objects()) -> kz_json:objects().
check_port_request_comments_results_to_alerts(Ports) ->
    Type = <<"port_request_with_waiting_for_reply_comment">>,
    AlertsList = [[{<<"id">>, kz_doc:id(Port)}
                  ,{<<"name">>, kz_json:get_value(<<"name">>, Port)}
                  ,{<<"comment">>, kz_json:get_value(<<"content">>, port_request_last_comment(Port))}
                  ,{<<"type">>, Type}
                  ] || Port <- Ports],
    [kz_json:from_list(Alert) || Alert <- AlertsList].

-spec check_low_balance(cb_context:context()) -> cb_context:context().
check_low_balance(Context) ->
    check_low_balance(Context, 3).

-spec check_low_balance(cb_context:context(), 0..3) -> cb_context:context().
check_low_balance(Context, 0) ->
    AccountId = cb_context:account_id(Context),
    lager:debug("Max retries to get account ~s current balance", [AccountId]),
    Context;
check_low_balance(Context, Loop) when Loop > 0 ->
    AccountId = cb_context:account_id(Context),
    ThresholdUSD = kzd_accounts:low_balance_threshold(AccountId),
    case kz_currency:available_dollars(AccountId) of
        {'error', 'timeout'} ->
            check_low_balance(Context, Loop - 1);
        {'error', _R} = _Err ->
            lager:debug("Skipping low_balance check for account ~p. " ++
                        "Got error trying to get balance: ~p", [AccountId, _Err]),
            Context;
        {'ok', AvailableUnitsUSD} when AvailableUnitsUSD =< ThresholdUSD ->
            maybe_topup_account(Context, kz_currency:dollars_to_units(AvailableUnitsUSD));
        {'ok', _AvailableUnitsUSD} -> %% AvailableUnitsUSD > ThresholdUSD
            Context
    end.

-spec maybe_topup_account(cb_context:context(), kz_currency:units()) -> cb_context:context().
maybe_topup_account(Context, AvailableUnits) ->
    AccountId = cb_context:account_id(Context),
    lager:info("checking topup for account ~s with balance $~w"
              ,[AccountId, kz_currency:units_to_dollars(AvailableUnits)]),
    case kz_services_topup:maybe_topup(AccountId, AvailableUnits) of
        {'ok', _Transaction, _Ledger} ->
            lager:info("topup successful for ~s", [AccountId]),
            Context;
        {'error', _Error} ->
            lager:debug("topup failed for ~s: ~p", [AccountId, _Error]),
            maybe_alert_low_balance(Context, AvailableUnits)
    end.

-spec maybe_alert_low_balance(cb_context:context(), kz_currency:units()) -> cb_context:context().
maybe_alert_low_balance(Context, AvailableUnits) ->
    AccountId = cb_context:account_id(Context),
    ThresholdUSD = kzd_accounts:low_balance_threshold(AccountId),
    AvailableUnitsUSD = kz_currency:units_to_dollars(AvailableUnits),
    lager:info("checking if account ~s balance $~w is below notification threshold $~w"
              ,[AccountId, AvailableUnitsUSD, ThresholdUSD]),
    case AvailableUnitsUSD =< ThresholdUSD of
        'false' -> Context;
        'true' -> check_low_balance_result_to_alert(Context, AvailableUnitsUSD, ThresholdUSD)
    end.

-spec check_low_balance_result_to_alert(cb_context:context(), kz_currency:dollars(), kz_currency:dollars()) -> cb_context:context().
check_low_balance_result_to_alert(Context, AvailableUnitsUSD, ThresholdUSD) ->
    Alert = [{<<"available_dollars">>, AvailableUnitsUSD}
            ,{<<"threshold">>, ThresholdUSD}
            ,{<<"type">>, <<"balance_is_below_zero">>}
            ],
    cb_context:set_resp_data(Context, [kz_json:from_list(Alert) | context_resp_data(Context)]).

-spec context_resp_data(cb_context:context()) -> kz_json:objects().
context_resp_data(Context) ->
    case cb_context:resp_data(Context) of
        'undefined' -> [];
        RespData -> RespData
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    RespData = cb_context:resp_data(Context),
    RespEnv = cb_context:resp_envelope(Context),

    Alerts = filter_alerts(RespData),

    Setters = [{fun cb_context:set_resp_data/2, Alerts}
              ,{fun cb_context:set_resp_envelope/2
               ,kz_json:set_value(<<"page_size">>, erlang:length(Alerts), RespEnv)
               }
              ],
    cb_context:setters(Context, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_alerts(kz_json:objects()) -> kz_json:objects().
filter_alerts(Alerts) ->
    [Alert || Alert <- lists:usort(Alerts),
              should_filter_alert(Alert)
    ].

-spec should_filter_alert(kzd_alert:doc()) -> boolean().
should_filter_alert(Alert) ->
    case kzd_alert:expired(Alert) of
        'false' -> 'true';
        'true' ->
            _ = kz_util:spawn(fun kapps_alert:delete/1, [kzd_alert:id(Alert)]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec view_keys(cb_context:context()) -> list().
view_keys(Context) ->
    AuthDoc = cb_context:auth_doc(Context),

    AccountId = cb_context:account_id(Context),
    OwnerId = kz_json:get_value(<<"owner_id">>, AuthDoc),

    IsAdmin = kzd_users:is_account_admin(AccountId, OwnerId),

    Routines = [fun(K) ->
                        [[<<"all">>, <<"all">>]
                        ,[<<"all">>, <<"users">>]
                         |K
                        ]
                end
               ,fun(K) -> [[AccountId, <<"all">>], [AccountId, <<"users">>]|K] end
               ,fun(K) when OwnerId =:= 'undefined' -> K;
                   (K) -> [[AccountId, OwnerId]|K]
                end
               ,fun(K) ->
                        case kz_services_reseller:is_reseller(AccountId) of
                            'false' -> K;
                            'true' -> [[<<"resellers">>, <<"all">>], [<<"resellers">>, <<"users">>]|K]
                        end
                end
               ,fun(K) when IsAdmin ->
                        [[<<"all">>, <<"admins">>]
                        ,[AccountId, <<"admins">>]
                        ,[<<"resellers">>, <<"admins">>]
                         |K
                        ];
                   (K) -> K
                end
               ,fun(K) ->
                        lists:foldl(fun(Descendant, Acc) ->
                                            add_descendants(Descendant, IsAdmin, Acc)
                                    end
                                   ,K
                                   ,crossbar_util:get_descendants(AccountId)
                                   )
                end
               ],
    lists:foldl(fun(F, Keys) -> F(Keys) end, [], Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_descendants(kz_term:ne_binary(), boolean(), list()) -> list().
add_descendants(Descendant, 'false', Keys) ->
    [[<<"descendants">>, [Descendant, <<"all">>]]
    ,[<<"descendants">>, [Descendant, <<"users">>]]
     | Keys
    ];
add_descendants(Descendant, 'true', Keys) ->
    [[<<"descendants">>, [Descendant, <<"all">>]]
    ,[<<"descendants">>, [Descendant, <<"users">>]]
    ,[<<"descendants">>, [Descendant, <<"admins">>]]
     | Keys
    ].

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
