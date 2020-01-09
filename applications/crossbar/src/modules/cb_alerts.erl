%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Crossbar API for alerts.
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
-export([do_check_port_requests/2
        ,maybe_check_financials/1
        ,check_low_balance/1
        ,check_payment_token/1
        ]).
-endif.

-include("crossbar.hrl").
-include_lib("kazoo_numbers/include/knm_port_request.hrl").

-define(AVAILABLE_LIST, <<"alerts/available">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".alerts">>).

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
    Context1 = cb_context:set_db_name(Context, ?KZ_ALERTS_DB),
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
    Context1 = cb_context:set_db_name(Context, ?KZ_ALERTS_DB),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(<<"alert">>)).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = load_summary(cb_port_requests:set_port_authority(Context)),
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
    Routines = [fun maybe_check_port_requests/1
               ,fun maybe_check_financials/1
               ,fun maybe_check_system_alerts/1
               ,fun set_success_resp_status/1
               ],
    lists:foldl(fun(F, C) -> F(C) end
               ,cb_context:set_resp_data(Context, [])
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_success_resp_status(cb_context:context()) -> cb_context:context().
set_success_resp_status(Context) ->
    cb_context:set_resp_status(Context, 'success').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_port_requests(cb_context:context()) -> cb_context:context().
maybe_check_port_requests(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_port_requests">>, 'true') of
        'false' -> Context;
        'true' ->
            maybe_check_port_requests(Context, cb_context:account_id(Context))
    end.

-spec maybe_check_port_requests(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_check_port_requests(Context, AccountId) ->
    MasterId = cb_context:master_account_id(Context),
    AuthId = cb_context:auth_account_id(Context),
    case AuthId =:= MasterId of
        'true' ->
            %% Authenticated account is master
            %% show only masqueraded account's port alerts
            do_check_port_requests(fetch_account_active_ports(AccountId), Context);
        'false' ->
            IsReseller = kz_services_reseller:is_reseller(AccountId),
            IsPortAuthority = cb_context:fetch(Context, 'is_port_authority'),
            maybe_check_port_requests(Context, IsReseller, IsPortAuthority)
    end.

-spec maybe_check_port_requests(cb_context:context(), boolean(), boolean()) -> cb_context:context().
maybe_check_port_requests(Context, 'true', 'true') ->
    %% Authenticated account is reseller AND port authority
    %% so show only masqueraded account's port alerts
    %% (because reseller is port authority, this is whose rejected ports or made action required
    %% comments on the ports, so it shouldn't get alerted for its own doing!)
    do_check_port_requests(fetch_account_active_ports(cb_context:account_id(Context)), Context);
maybe_check_port_requests(Context, 'true', 'false') ->
    %% Authenticated account is reseller lets always show alerts
    %% from its own account and all sub-accounts (no matter what port app is hidden or not)
    ResellerId = cb_context:reseller_id(Context),
    Ports = fetch_account_active_ports(ResellerId)
        ++ get_active_ports(knm_port_request:descendant_active_ports(ResellerId)),
    do_check_port_requests(Ports, Context);
maybe_check_port_requests(Context, 'false', _) ->
    %% Authenticated account is neither master or reseller
    %% show alerts if its reseller is not hiding port app
    AccountId = cb_context:account_id(Context),
    ResellerId = cb_context:reseller_id(Context),
    case should_hide_port(ResellerId) of
        'true' -> Context;
        'false' -> do_check_port_requests(fetch_account_active_ports(AccountId), Context)
    end.

-spec should_hide_port(kz_term:api_ne_binary() | {'ok', kz_json:object()} | kz_datamgr:data_error()) -> boolean().
should_hide_port('undefined') ->
    'false';
should_hide_port(<<ResellerId/binary>>) ->
    should_hide_port(kzd_whitelabel:fetch(ResellerId));
should_hide_port({'ok', Whitelabel}) ->
    kzd_whitelabel:hide_port(Whitelabel);
should_hide_port({'error', _}) ->
    'false'.

-spec fetch_account_active_ports(kz_term:api_ne_binary()) -> kz_json:objects().
fetch_account_active_ports('undefined') -> [];
fetch_account_active_ports(AccountId) ->
    get_active_ports(knm_port_request:account_active_ports(AccountId))
        ++ get_active_ports(knm_port_request:account_ports_by_state(AccountId, ?PORT_UNCONFIRMED)).

-spec get_active_ports({'ok', kz_json:objects()} | kz_datamgr:data_error()) -> kz_json:objects().
get_active_ports({'ok', Active}) ->
    Active;
get_active_ports(_) ->
    [].

-spec do_check_port_requests(kz_json:objects(), cb_context:context()) -> cb_context:context().
do_check_port_requests([], Context) ->
    Context;
do_check_port_requests([PortRequest|PortRequests], Context) ->
    Routines = [fun maybe_check_port_action_required/2
               ,fun maybe_check_port_suspended/2
               ],
    Context1 = lists:foldl(fun(F, C) -> F(PortRequest, C) end
                          ,Context
                          ,Routines
                          ),
    do_check_port_requests(PortRequests, Context1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_port_action_required(kzd_port_requests:doc(), cb_context:context()) ->
          cb_context:context().
maybe_check_port_action_required(PortRequest, Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_port_action_required">>, 'true') of
        'false' -> Context;
        'true' -> check_port_action_required(PortRequest, Context)
    end.

-spec check_port_action_required(kzd_port_requests:doc(), cb_context:context()) ->
          cb_context:context().
check_port_action_required(PortRequest, Context) ->
    LastComment = port_request_last_comment(Context, PortRequest),
    case kzd_comment:action_required(LastComment) of
        'false' -> Context;
        'true' ->
            Metadata = kz_json:from_list(
                         [{<<"name">>, kzd_port_requests:name(PortRequest)}
                         ,{<<"state">>, kzd_port_requests:pvt_port_state(PortRequest)}
                         ]
                        ),
            From = kz_json:from_list(
                     [{<<"type">>, <<"account">>}
                     ,{<<"value">>, kz_doc:account_id(PortRequest)}
                     ]
                    ),
            PortAlert = kz_json:from_list(
                          [{<<"id">>, kz_doc:id(PortRequest)}
                          ,{<<"title">>, <<"Port request requires action">>}
                          ,{<<"message">>, LastComment}
                          ,{<<"metadata">>, Metadata}
                          ,{<<"category">>, <<"port_action_required">>}
                          ,{<<"from">>, [From]}
                          ,{<<"clearable">>, 'false'}
                          ]
                         ),
            append_alert(Context, PortAlert)
    end.

-spec port_request_last_comment(cb_context:context(), kzd_port_requests:doc()) -> kz_json:object().
port_request_last_comment(Context, PortRequest) ->
    JObj = cb_port_requests:filter_private_comments(Context, PortRequest),
    case kzd_port_requests:comments(JObj, []) of
        [] -> kz_json:new();
        Comments -> lists:last(Comments)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_port_suspended(kzd_port_requests:doc(), cb_context:context()) ->
          cb_context:context().
maybe_check_port_suspended(PortRequest, Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_port_suspended">>, 'true') of
        'false' -> Context;
        'true' -> check_port_suspended(PortRequest, Context)
    end.

-spec check_port_suspended(kzd_port_requests:doc(), cb_context:context()) ->
          cb_context:context().
check_port_suspended(PortRequest, Context) ->
    State = kzd_port_requests:pvt_port_state(PortRequest),
    Transition = kzd_port_requests:get_transition(PortRequest, State),
    case lists:member(State, ?PORT_SUSPENDED_STATES)
        andalso Transition =/= []
    of
        'false' -> Context;
        'true' ->
            Metadata = kz_json:from_list(
                         [{<<"name">>, kzd_port_requests:name(PortRequest)}
                         ,{<<"state">>, State}
                         ,{<<"transition">>, hd(Transition)}
                         ]
                        ),
            From = kz_json:from_list(
                     [{<<"type">>, <<"account">>}
                     ,{<<"value">>, kz_doc:account_id(PortRequest)}
                     ]
                    ),
            PortAlert = kz_json:from_list(
                          [{<<"id">>, kz_doc:id(PortRequest)}
                          ,{<<"title">>, <<"Port request requires action">>}
                          ,{<<"message">>, check_port_suspended_message(State)}
                          ,{<<"metadata">>, Metadata}
                          ,{<<"category">>, <<"port_suspended">>}
                          ,{<<"from">>, [From]}
                          ,{<<"clearable">>, 'false'}
                          ]
                         ),
            append_alert(Context, PortAlert)
    end.

-spec check_port_suspended_message(kz_term:ne_binary()) -> kz_term:ne_binary().
check_port_suspended_message(?PORT_REJECTED) ->
    <<"The port request has been rejected, please update the port or cancel.">>;
check_port_suspended_message(?PORT_UNCONFIRMED) ->
    <<"The port request has not been submitted, until it is submitted it will not be processed.">>;
check_port_suspended_message(_) ->
    <<"The port request requires you attention to continue.">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_financials(cb_context:context()) -> cb_context:context().
maybe_check_financials(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_financials">>, 'true') of
        'false' -> Context;
        'true' -> check_financials(Context)
    end.

-spec check_financials(cb_conteext:context()) -> cb_context:context().
check_financials(Context) ->
    Services = kz_services:fetch(cb_context:account_id(Context)),
    case kz_services_plans:is_empty(kz_services:plans(Services)) of
        'true' -> Context;
        'false' ->
            Routines = [fun maybe_check_low_balance/1
                       ,fun maybe_check_payment_token/1
                       ],
            lists:foldl(fun(F, C) -> F(C) end
                       ,Context
                       ,Routines
                       )
    end.

%%------------------------------------------------------------------------------
%% @doc Return a low_balance alert when any of the following scenarios is met:
%% - If threshold is configured then create an alert if their balance is below that amount.
%% - If not threshold configured and post pay is not enabled create an alert if their
%%   balance is less than or equal to 0.
%% - If not threshold configured and post pay is enabled create an alert if their balance
%%   is less than or equal to the maximum post pay amount.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_low_balance(cb_context:context()) -> cb_context:context().
maybe_check_low_balance(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_low_balance">>, 'true') of
        'false' -> Context;
        'true' -> check_low_balance(Context)
    end.

-spec check_low_balance(cb_context:context()) -> cb_context:context().
check_low_balance(Context) ->
    AccountId = cb_context:account_id(Context),
    AvailableDollars = kz_currency:available_dollars(AccountId),
    ThresholdDollars = kzd_accounts:low_balance_threshold(AccountId, 'undefined'),
    check_low_balance(Context, AvailableDollars, ThresholdDollars).

-spec check_low_balance(cb_context:context(), kz_currency:available_dollars_return(), float() | 'undefined') ->
          cb_context:context().
check_low_balance(Context, {'error', _R}, _Threshold) ->
    lager:debug("unable to get current balance: ~p", [_R]),
    Context;
check_low_balance(Context, {'ok', AvailableDollars}, 'undefined') ->
    Threshold = 0,
    Limits = kz_services_limits:fetch(cb_context:account_id(Context)),
    PostPayAmountUnits = kz_json:get_value(<<"pvt_max_postpay_amount">>, Limits, 0),
    PostPayAmountDollars = kz_currency:units_to_dollars(PostPayAmountUnits),

    case kz_json:get_value(<<"pvt_allow_postpay">>, Limits, 'false') of
        'false' when AvailableDollars =< Threshold ->
            low_balance_alert(Context, AvailableDollars, Threshold);
        'true' when AvailableDollars =< PostPayAmountDollars ->
            low_balance_alert(Context, AvailableDollars, PostPayAmountDollars);
        _ ->
            %% Current balance is ok.
            Context
    end;
check_low_balance(Context, {'ok', AvailableDollars}, ThresholdDollars)
  when AvailableDollars < ThresholdDollars ->
    low_balance_alert(Context, AvailableDollars, ThresholdDollars);
check_low_balance(Context, {'ok', _AvailableDollars}, _ThresholdDollars) ->
    Context.

-spec low_balance_alert(cb_context:context(), kz_currency:dollars(), number() | 'undefined') -> cb_context:context().
low_balance_alert(Context, AvailableDollars, ThresholdDollars) ->
    AccountId = cb_context:account_id(Context),
    Metadata = kz_json:from_list(
                 [{<<"available">>, AvailableDollars}
                 ,{<<"threshold">>, ThresholdDollars}
                 ]
                ),
    From = kz_json:from_list(
             [{<<"type">>, <<"account">>}
             ,{<<"value">>, AccountId}
             ]
            ),
    BalanceAlert = kz_json:from_list(
                     [{<<"id">>, AccountId}
                     ,{<<"title">>, <<"Balance below threshold">>}
                     ,{<<"message">>, <<"Please add credit to your account to avoid service interruption.">>}
                     ,{<<"metadata">>, Metadata}
                     ,{<<"category">>, <<"low_balance">>}
                     ,{<<"from">>, [From]}
                     ,{<<"clearable">>, 'false'}
                     ]
                    ),
    append_alert(Context, BalanceAlert).

%%------------------------------------------------------------------------------
%% @doc Return a payment token alert when any of the following scenarios is met:
%% - If the account does not have service plans assigned, then ignore this check.
%% - If the account does have service plans assigned and has no default tokens trigger an
%%   alert for ‘no_payment_token’.
%% - If the account does have service plans assigned trigger an alert for any default
%%   token with an expiration that has expired or will expire within the 60 days.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_payment_token(cb_context:context()) -> cb_context:context().
maybe_check_payment_token(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_payment_token">>, 'true') of
        'false' -> Context;
        'true' -> check_payment_token(Context)
    end.

-spec check_payment_token(cb_context:context()) -> cb_context:context().
check_payment_token(Context) ->
    Services = kz_services:fetch(cb_context:account_id(Context)),
    DefaultTokens = kz_json:values(kz_services_payment_tokens:defaults(Services)),
    check_payment_token(Context, DefaultTokens).

-spec check_payment_token(cb_context:context(), kz_json:objects()) -> cb_context:context().
check_payment_token(Context, []) ->
    %% No default payment tokens.
    payment_token_alert('no_payment_token', Context);
check_payment_token(Context, DefaultTokens) ->
    SixtyDaysFromNow = kz_time:now_s() + (?SECONDS_IN_DAY * 60),
    case [T || T <- DefaultTokens,
               Expiration <- [kz_json:get_integer_value(<<"expiration">>, T)],
               Expiration =/= 'undefined',
               Expiration =< SixtyDaysFromNow
         ]
    of
        [] ->
            %% No default payment tokens expired found.
            Context;
        DefaultTokensExpired ->
            %% Default payment tokens are expired or will expire within the next 60 days.
            lists:foldl(fun payment_token_alert/2, Context, DefaultTokensExpired)
    end.

-spec payment_token_alert(kz_json:object() | 'no_payment_token', cb_context:context()) -> cb_context:context().
payment_token_alert('no_payment_token', Context) ->
    AccountId = cb_context:account_id(Context),
    From = kz_json:from_list(
             [{<<"type">>, <<"account">>}
             ,{<<"value">>, AccountId}
             ]
            ),
    BalanceAlert = kz_json:from_list(
                     [{<<"id">>, AccountId}
                     ,{<<"title">>, <<"No payment token configured">>}
                     ,{<<"message">>, <<"Please add a payment token to avoid service interruption.">>}
                     ,{<<"metadata">>, kz_json:new()}
                     ,{<<"category">>, <<"no_payment_token">>}
                     ,{<<"from">>, [From]}
                     ,{<<"clearable">>, 'false'}
                     ]
                    ),
    append_alert(Context, BalanceAlert);
payment_token_alert(ExpiredToken, Context) ->
    From = kz_json:from_list(
             [{<<"type">>, <<"account">>}
             ,{<<"value">>, cb_context:account_id(Context)}
             ]
            ),
    BalanceAlert = kz_json:from_list(
                     [{<<"id">>, kz_json:get_integer_value(<<"id">>, ExpiredToken)}
                     ,{<<"title">>, <<"Payment method expired or about to expire">>}
                     ,{<<"message">>, <<"Please update your payment tokens to avoid service interruption.">>}
                     ,{<<"metadata">>, ExpiredToken}
                     ,{<<"category">>, <<"expired_payment_token">>}
                     ,{<<"from">>, [From]}
                     ,{<<"clearable">>, 'false'}
                     ]
                    ),
    append_alert(Context, BalanceAlert).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_check_system_alerts(cb_context:context()) -> cb_context:context().
maybe_check_system_alerts(Context) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"check_system_alerts">>, 'true') of
        'false' -> Context;
        'true' -> check_system_alerts(Context)
    end.

-spec check_system_alerts(cb_context:context()) -> cb_context:context().
check_system_alerts(Context) ->
    ViewOptions = [{'keys', view_keys(Context)}],
    case kz_datamgr:get_results(?KZ_ALERTS_DB, ?AVAILABLE_LIST, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get manual alerts: ~p", [_R]),
            Context;
        {'ok', []} ->
            Context;
        {'ok', JObjs} ->
            Alerts = [kz_json:set_value(<<"clearable">>, 'true', JObj)
                      || JObj <- JObjs
                     ],
            append_alerts(Context, Alerts)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec append_alert(cb_context:context(), kz_json:object()) -> cb_context:context().
append_alert(Context, Alert) ->
    CurrentAlerts = cb_context:resp_data(Context),
    cb_context:set_resp_data(Context, [Alert|CurrentAlerts]).

-spec append_alerts(cb_context:context(), kz_json:objects()) -> cb_context:context().
append_alerts(Context, Alerts) ->
    CurrentAlerts = cb_context:resp_data(Context),
    cb_context:set_resp_data(Context, Alerts ++ CurrentAlerts).

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
            _ = kz_process:spawn(fun kapps_alert:delete/1, [kzd_alert:id(Alert)]),
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
