%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_activation_charges).

-export([fetch/1]).
-export([commit/3
        ,commit/4
        ]).

-include("services.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(EXECUTOR_MODULE, kz_term:to_binary(?MODULE)).
-define(SOURCE_SERVICE, <<"payments">>).

-type error_details() :: #{message := kz_term:ne_binary()
                          ,reason := kz_term:ne_binary()
                          }.
-type validate_return() :: {boolean(), kz_json:object() | error_details()}.
-type ledger_return() :: {'ok', kz_transaction:transaction() | 'undefined', kz_ledger:ledger()} |
                         {'error', error_details(), kz_transaction:transaction() | 'undefined', kz_ledger:ledger() | 'undefined'}.
-type activation_return() :: {boolean(), ledger_return() | error_details()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> activation_return().
fetch(?NE_BINARY = AccountId) ->
    FetchOptions = ['hydrate_plans'],
    fetch(kz_services:fetch(AccountId, FetchOptions));
fetch(Services) ->
    AppsDict = kz_services_plans:foldl(fun fetch_foldl/3
                                      ,#{}
                                      ,kz_services:plans(Services)
                                      ),
    kz_json:from_list(maps:to_list(AppsDict)).

-spec fetch_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), map()) -> map().
fetch_foldl(_BookkeeperHash, [], Activations) ->
    Activations;
fetch_foldl(_BookkeeperHash, PlansList, Activations) ->
    Plan = kz_services_plans:merge(PlansList),
    kz_json:foldl(fun(Name, Val, Acts) ->
                          Acts#{Name => Val}
                  end
                 ,Activations
                 ,kz_services_plan:activation_charges(Plan)
                 ).

%% @equiv commit(Services, Current, Proposed, kz_json:new())
-spec commit(kz_services:services()
            ,kz_services_quantities:billables() | kz_services_quantities:billable()
            ,kz_services_quantities:billables() | kz_services_quantities:billable()
            ) -> activation_return().
commit(Services, Current, Proposed) ->
    commit(Services, Current, Proposed, kz_json:new()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec commit(kz_services:services()
            ,kz_services_quantities:billables() | kz_services_quantities:billable()
            ,kz_services_quantities:billables() | kz_services_quantities:billable()
            ,kz_json:object()
            ) -> activation_return().
commit(Services, 'undefined', Proposed, AuditLog) ->
    lager:debug("calculating service activation charges for addition(s)"),
    commit(Services, [], Proposed, AuditLog);
commit(Services, _, Proposed, _)
  when Proposed =:= 'undefined';
       Proposed =:= [] ->
    lager:debug("not calculating activation charges for removal(s)", []),
    Services;

commit(Services, Current, Proposed, AuditLog)
  when not is_list(Current) ->
    commit(Services, [Current], Proposed, AuditLog);
commit(Services, Current, Proposed, AuditLog)
  when not is_list(Proposed) ->
    commit(Services, Current, [Proposed], AuditLog);

commit(Services, CurrentJObjs, ProposedJObjs, AuditLog) ->
    ActivationCharges = kz_json:values(fetch(Services)),
    maybe_commit(Services
                ,AuditLog
                ,validate(Services
                         ,ActivationCharges
                         ,CurrentJObjs
                         ,ProposedJObjs
                         )
                ).

-spec maybe_commit(kz_services:services(), kz_json:object(), validate_return()) ->
                          activation_return().
maybe_commit(_, _, {'false', _}=NotValid) ->
    NotValid;
maybe_commit(Services, AuditLog, {'true', Activation}) ->
    AccountId = kz_services:account_id(Services),
    Transaction = create_activation_transaction(AccountId, AuditLog, Activation),
    case kz_transaction:sale(Transaction) of
        {'error', 'invalid_bookkeeper'} ->
            {'true', create_invalid_bookkeeper_ledger(AccountId, AuditLog, Activation)};
        {'error', Reason} ->
            DbError = kz_term:safe_cast(Reason, <<"unknown">>, fun kz_term:to_binary/1),
            Msg = <<"'", DbError/binary, "' error occurred during creating transaction">>,
            ErrorDetails = #{<<"reason">> => <<"datastore_fault">>
                            ,<<"message">> => Msg
                            },
            {'true', {'error', ErrorDetails, 'undefined', 'undefined'}};
        {'ok', Transaction} ->
            {'true', maybe_create_ledger(Transaction, Activation)}
    end.

%%%=============================================================================
%%% Transaction/Ledger functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_activation_transaction(kz_term:ne_binary(), kz_json:object(), kz_json:object()) ->
                                           kz_transaction:transaction().
create_activation_transaction(AccountId, AuditLog, Activation) ->
    ActivationAmount = kz_json:get_integer_value(<<"rate">>, Activation, 0),
    Metadata = kz_json:from_list([{<<"automatic_description">>, 'true'}]),
    Setters =
        props:filter_empty(
          [{fun kz_transaction:set_account/2, AccountId}
          ,{fun kz_transaction:set_description/2, get_description(Activation)}
          ,{fun kz_transaction:set_executor_trigger/2, <<"automatic">>}
          ,{fun kz_transaction:set_executor_module/2, ?EXECUTOR_MODULE}
          ,{fun kz_transaction:set_audit/2, AuditLog}
          ,{fun kz_transaction:set_unit_amount/2, kz_currency:dollars_to_units(ActivationAmount)}
          ,{fun kz_transaction:set_metadata/2, Metadata}
          ]
         ),
    kz_transaction:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_create_ledger(kz_transaction:transaction(), kz_json:object()) -> ledger_return().
maybe_create_ledger(Transaction, Activation) ->
    case kz_transaction:status_completed(Transaction) of
        'false' ->
            ErrorDetails = #{<<"reason">> => <<"transaction_incomplete">>
                            ,<<"message">> => <<"transaction_incomplete">>
                            },
            {'error', ErrorDetails, Transaction, 'undefined'};
        'true' -> create_ledger(Transaction, Activation)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_ledger(kz_transaction:transaction(), kz_json:object()) -> ledger_return().
create_ledger(Transaction, Activation) ->
    AccountId = kz_transaction:account_id(Transaction),
    ActivationAmount = kz_transaction:unit_amount(Transaction),
    AuditLog = kz_transaction:audit(Transaction),
    Props = [{[<<"transaction">>, <<"id">>], kz_transaction:id(Transaction)}
            ,{[<<"transaction">>, <<"created">>], 'true'}
            ,{<<"automatic_description">>, 'true'}
            ],
    Metadata = kz_json:set_values(Props, kz_json:new()),
    case create_ledger(AccountId, ActivationAmount, Activation, AuditLog, Metadata) of
        {'error', Message} ->
            ErrorDetails = #{<<"reason">> => <<"ledger_error">>
                            ,<<"message">> => Message
                            },
            {'error', ErrorDetails, Transaction, 'undefined'};
        {'ok', Ledger} ->
            {'ok', Transaction, Ledger}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_invalid_bookkeeper_ledger(kz_term:ne_binary(), kz_json:object(), kz_json:object()) ->
                                              ledger_return().
create_invalid_bookkeeper_ledger(AccountId, AuditLog, Activation) ->
    ActivationAmount =
        kz_currency:dollars_to_units(kz_json:get_integer_value(<<"rate">>, Activation, 0)),
    Metadata = kz_json:from_list([{<<"automatic_description">>, 'true'}]),
    case create_ledger(AccountId, ActivationAmount, Activation, AuditLog, Metadata) of
        {'error', Message} ->
            ErrorDetails = #{<<"reason">> => <<"ledger_error">>
                            ,<<"message">> => Message
                            },
            {'error', ErrorDetails, 'undefined', 'undefined'};
        {'ok', Ledger} ->
            {'ok', 'undefined', Ledger}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_ledger(kz_term:ne_binary(), kz_currency:units(), kz_json:object(), kz_json:object(), kz_json:object()) ->
                           {'ok', kz_ledger:ledger()} | {'error', kz_term:ne_binary()}.
create_ledger(AccountId, Units, Activation, Audit, Metadata) ->
    SourceId = kz_json:get_ne_binary_value([<<"transaction">>, <<"id">>]
                                          ,Metadata
                                          ,kz_binary:rand_hex(5)
                                          ),
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, AccountId}
          ,{fun kz_ledger:set_source_id/2, SourceId}
          ,{fun kz_ledger:set_description/2, get_description(Activation)}
          ,{fun kz_ledger:set_metadata/2, Metadata}
          ,{fun kz_ledger:set_executor_trigger/2, <<"automatic">>}
          ,{fun kz_ledger:set_executor_module/2, ?EXECUTOR_MODULE}
          ,{fun kz_ledger:set_audit/2, Audit}
          ,{fun kz_ledger:set_unit_amount/2, Units}
          ,{fun kz_ledger:set_period_start/2, kz_time:now_s()}
          ,{fun kz_ledger:set_source_service/2, ?SOURCE_SERVICE}
          ]
         ),
    case kz_ledger:credit(kz_ledger:setters(Setters)) of
        {'ok', _}=OK -> OK;
        {'error', Reason} ->
            DbError = kz_term:safe_cast(Reason, <<"unknown">>, fun kz_term:to_binary/1),
            Msg = <<"'", DbError/binary, "' error occurred during creating ledger">>,
            {'error', Msg}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_description(kz_json:object()) -> kz_term:ne_binary().
get_description(Activation) ->
    Name = kz_json:get_ne_binary_value(<<"name">>, Activation, <<"Object">>),
    kz_json:get_ne_binary_value(<<"description">>, Activation, <<Name/binary, " activation charge">>).

%%%=============================================================================
%%% Validation functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate(kz_services:services(), kz_json:objects(), kz_json:objects(), kz_json:objects()) ->
                      validate_return().
validate(_, [], _, _) ->
    {'false'
    ,#{<<"reason">> => <<"no_activation_charges">>
      ,<<"message">> => <<"no activation charges is defined">>
      }
    };
validate(_, _, [], []) ->
    {'false'
    ,#{<<"reason">> => <<"no_changes">>
      ,<<"message">> => <<"no changes">>
      }
    };
validate(_Services, Activations, [], [ProposedJObj | _]) ->
    validate_activations(<<"proposed">>, Activations, ProposedJObj);
validate(_Services, Activations, [CurrentJObj | _], [ProposedJObj | _]) ->
    case validate_activations(<<"current">>, Activations, CurrentJObj) of
        {'true', Activation} ->
            validate_activation(<<"proposed">>, Activation, ProposedJObj);
        {'false', _}=NotValid ->
            NotValid
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_activations(kz_term:ne_binary(), kz_json:objects(), kz_json:object()) ->
                                  validate_return().
validate_activations(Type, [], _) ->
    {'false'
    ,#{<<"reason">> => <<"no_match">>
      ,<<"message">> => <<"no activation charges matched for ", Type/binary, " object">>
      }
    };
validate_activations(Type, [Activation | Activations], JObj) ->
    case validate_activation(Type, Activation, JObj) of
        {'ok', ValidActivation} -> {'true', ValidActivation};
        {'error', _} ->
            validate_activations(Type, Activations, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_activation(kz_term:ne_binary(), kz_json:object(), kz_json:object()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', [jesse_error:error_reason()]}.
validate_activation(Type, Activation, JObj) ->
    Options = [], %% use [{'external_validator', [{'external_validator', Fun}]}] or kz_json_schema:setup_extra_validator
    SchemaJObj = kz_json:get_ne_binary_value(Type, Activation, kz_json:new()),
    try kz_json_schema:validate(SchemaJObj, JObj, Options) of
        {'ok', _ValidJObj} ->
            lager:debug("validation passed"),
            {'ok', Activation};
        {'error', Errors} ->
            {'error', Errors}
    catch
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            lager:debug("function clause failure"),
            kz_util:log_stacktrace(ST),
            {'error', <<"function_clause">>}
    end.
