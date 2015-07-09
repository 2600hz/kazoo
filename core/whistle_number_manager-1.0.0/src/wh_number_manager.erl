%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_number_manager).

-export([find/1, find/2, find/3]).
-export([check/1, check/2]).
-export([lookup_account_by_number/1]).
-export([ported/1]).
-export([create_number/3, create_number/4, create_number/5, create_number/6]).
-export([port_in/3
         ,port_in/4
         ,port_in/5
        ]).
-export([reconcile_number/3]).
-export([free_numbers/1]).
-export([reserve_number/3, reserve_number/4, reserve_number/5]).
-export([assign_number_to_account/3, assign_number_to_account/4, assign_number_to_account/5]).
-export([release_number/2]).
-export([list_attachments/2]).
-export([fetch_attachment/3]).
-export([put_attachment/5]).
-export([delete_attachment/3]).
-export([get_public_fields/2, set_public_fields/3, set_public_fields/4]).
-export([track_assignment/2]).

-include("wnm.hrl").

-export_type([number_property/0
              ,number_properties/0
             ]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for available numbers.
%% force leading +
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary()) -> ne_binaries().
-spec find(ne_binary(), ne_binary()) -> ne_binaries().
-spec find(ne_binary(), ne_binary(), wh_proplist()) -> ne_binaries().

find(Number) ->
    find(Number, <<"1">>).

find(Number, Quantity) ->
    find(Number, Quantity, []).

find(Number, Quantity, Opts) ->
    AccountId = props:get_value(<<"Account-ID">>, Opts),
    Num = wnm_util:normalize_number(Number),
    lager:info("attempting to find ~p numbers with prefix '~s' for account ~p"
               ,[Quantity, Number, AccountId]
              ),

    Results = [{Module, catch(Module:find_numbers(Num, Quantity, Opts))}
               || Module <- wnm_util:list_carrier_modules()
              ],
    NewOpts = [{<<"classification">>, wnm_util:classify_number(Num)}
               ,{<<"account-id">>, AccountId}
               ,{<<"services">>, maybe_get_services(AccountId)}
               | Opts
              ],
    prepare_find_results(Results, [], NewOpts).

-spec maybe_get_services(api_binary()) -> api_binary().
maybe_get_services('undefined') -> 'undefined';
maybe_get_services(AccountId) ->
    wh_services:fetch(AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for available numbers.
%% force leading +
%% @end
%%--------------------------------------------------------------------
-spec check(ne_binaries()) -> wh_json:object().
-spec check(ne_binaries(), wh_proplist()) -> wh_json:object().
check(Numbers) ->
    check(Numbers, []).

check(Numbers, Opts) ->
    FormattedNumbers = [wnm_util:normalize_number(Number) || Number <- Numbers],
    lager:info("attempting to check ~p ", [FormattedNumbers]),
    Results = [{Module, catch(Module:check_numbers(FormattedNumbers, Opts))}
               || Module <- wnm_util:list_carrier_modules()
              ],
    prepare_check_result(Results).

-spec prepare_check_result(wh_json:objects()) -> wh_json:object().
-spec prepare_check_result(wh_json:objects(), wh_json:object()) -> wh_json:object().
prepare_check_result(Results) ->
    prepare_check_result(Results, wh_json:new()).

prepare_check_result([], JObj) -> JObj;
prepare_check_result([{'wnm_other', {'ok', ModuleResults}}|Results], JObj) ->
    JObj1 = lists:foldl(
                fun({Num, Status}, Acc) ->
                    Number = wnm_util:normalize_number(Num),
                    wh_json:set_value(Number, Status, Acc)
                end
                ,JObj
                ,wh_json:to_proplist(ModuleResults)
            ),
    prepare_check_result(Results, JObj1);
prepare_check_result([_|Results], JObj) ->
    prepare_check_result(Results, JObj).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to find the number, and if sucessful return the account
%% assignment
%% @end
%%--------------------------------------------------------------------
-type lookup_errors() ::'not_reconcilable' |
                        'unassigned' |
                        'not_found' |
                        {wnm_failures(), api_object() | ne_binary()}.
-spec lookup_account_by_number(api_binary()) ->
                                      {'ok', ne_binary(), wh_proplist()} |
                                      {'error', lookup_errors()}.
lookup_account_by_number('undefined') ->
    {'error', 'not_reconcilable'};
lookup_account_by_number(Num) ->
    Number = wnm_util:to_e164(Num),
    Key = {'account_lookup', Number},
    case wh_cache:peek_local(?WNM_NUMBER_CACHE, Key) of
        {'ok', Ok} -> Ok;
        {'error', 'not_found'} ->
            case fetch_account_by_number(Number) of
                {'ok', _, _}=Ok ->
                    CacheProps = [{'origin', [{'db', wnm_util:number_to_db_name(Number), Number}]}],
                    wh_cache:store_local(?WNM_NUMBER_CACHE, Key, Ok, CacheProps),
                    Ok;
                Else -> Else
            end
    end.

-spec fetch_account_by_number(api_binary()) ->
                                     {'ok', ne_binary(), wh_proplist()} |
                                     {'error', lookup_errors()}.
fetch_account_by_number(Number) ->
    try wnm_number:get(Number) of
        Number1 -> maybe_check_account(Number1)
    catch
        'throw':{Error, #number{}} ->
            lookup_account_in_ports(Number, {'error', Error})
    end.

-spec lookup_account_in_ports(ne_binary(), {'error', lookup_errors()}) ->
                                     {'ok', ne_binary(), wh_proplist()} |
                                     {'error', lookup_errors()}.
lookup_account_in_ports(N, Error) ->
    Number = wnm_util:to_e164(N),
    case couch_mgr:get_results(?KZ_PORT_REQUESTS_DB
                               ,<<"port_requests/port_in_numbers">>
                               ,[{'key', Number}]
                              )
    of
        {'ok', []} ->
            lager:debug("no port for ~s: ~p", [Number, Error]),
            Error;
        {'ok', [Port]} ->
            AccountId = wh_json:get_value(<<"value">>, Port),
            {'ok', AccountId
             ,[{'force_outbound', 'true'}
               ,{'pending_port', 'true'}
               ,{'local', 'true'}
               ,{'inbound_cnam', 'false'}
               ,{'number', Number}
               ,{'account_id', AccountId}
              ]
            };
        {'error', 'not_found'}=E ->
            lager:debug("port number ~s not found", [Number]),
            E;
        {'error', _E} ->
            lager:debug("failed to query for port number '~s': ~p", [Number, _E]),
            Error
    end.

-spec maybe_check_account(wnm_number()) ->
                                 {'ok', ne_binary(), wh_proplist()} |
                                 {'error', _}.
maybe_check_account(#number{assigned_to='undefined'
                            ,number=_Number
                           }) ->
    lager:debug("number ~p not assigned to an account", [_Number]),
    {'error', 'unassigned'};
maybe_check_account(#number{assigned_to=AssignedTo
                            ,state = ?NUMBER_STATE_PORT_IN
                            ,number=_Number
                           }=N) ->
    lager:debug("number ~p is assigned to ~p in state port_in", [_Number, AssignedTo]),
    check_account(N);
maybe_check_account(#number{assigned_to=AssignedTo
                            ,state = ?NUMBER_STATE_IN_SERVICE
                            ,number=_Number
                           }=N) ->
    lager:debug("number ~p is assigned to ~p in state in_service", [_Number, AssignedTo]),
    check_account(N);
maybe_check_account(#number{assigned_to=AssignedTo
                            ,state = ?NUMBER_STATE_PORT_OUT
                            ,number=_Number
                           }=N) ->
    lager:debug("number ~p is assigned to ~p in state port_in", [_Number, AssignedTo]),
    check_account(N);
maybe_check_account(#number{assigned_to=AssignedTo
                            ,state=State
                            ,number=_Number
                           }) ->
    lager:debug("number ~p assigned to account id ~p but in state ~p", [_Number, AssignedTo, State]),
    {'error', {'not_in_service', AssignedTo}}.

-spec check_account(wnm_number()) ->
                           {'ok', ne_binary(), wh_proplist()} |
                           {'error', _}.
check_account(#number{assigned_to=AssignedTo}=N) ->
    case wh_util:is_account_enabled(AssignedTo) of
        'false' -> {'error', {'account_disabled', AssignedTo}};
        'true' -> {'ok', AssignedTo, number_options(N)}
    end.

-spec number_options(wnm_number()) -> number_properties().
number_options(#number{state=State
                       ,features=Features
                       ,module_name=Module
                       ,number=Num
                       ,assigned_to=AssignedTo
                      }=Number) ->
    [{'force_outbound', should_force_outbound(Number)}
     ,{'pending_port', State =:= ?NUMBER_STATE_PORT_IN}
     ,{'local', Module =:= 'wnm_local'}
     ,{'inbound_cnam'
       ,sets:is_element(<<"inbound_cnam">>, Features)
       andalso should_lookup_cnam(Module)
      }
     ,{'ringback_media', find_early_ringback(Number)}
     ,{'transfer_media', find_transfer_ringback(Number)}
     ,{'number', Num}
     ,{'account_id', AssignedTo}
     ,{'prepend', prepend(Number)}
    ].

%% Checks the carrier module for whether to lookup CNAM on this number
-spec should_lookup_cnam(atom()) -> boolean().
should_lookup_cnam(Module) ->
    try Module:should_lookup_cnam() of
        Boolean -> wh_util:is_true(Boolean)
    catch
        _E:_R -> 'true'
    end.

-spec should_force_outbound(wnm_number()) -> boolean().
should_force_outbound(#number{state = ?NUMBER_STATE_PORT_IN
                              ,module_name=Mod
                             }) ->
    whapps_config:get_is_true(?WNM_CONFIG_CAT, <<"force_port_in_outbound">>, 'true')
        orelse force_wnm_module_outbound(Mod);
should_force_outbound(#number{state = ?NUMBER_STATE_PORT_OUT
                              ,module_name=Mod
                             }) ->
    whapps_config:get_is_true(?WNM_CONFIG_CAT, <<"force_port_out_outbound">>, 'true')
        orelse force_wnm_module_outbound(Mod);
should_force_outbound(#number{module_name='wnm_local'}) ->
    force_wnm_local_outbound();
should_force_outbound(#number{number_doc=JObj}) ->
    case wh_json:get_ne_value(<<"force_outbound">>, JObj) of
        'undefined' -> default_force_outbound();
        ForceOutbound -> wh_util:is_true(ForceOutbound)
    end.

-spec force_wnm_module_outbound(atom()) -> boolean().
force_wnm_module_outbound('wnm_local') ->
    force_wnm_local_outbound();
force_wnm_module_outbound(_Mod) ->
    'false'.

-spec force_wnm_local_outbound() -> boolean().
force_wnm_local_outbound() ->
    whapps_config:get_is_true(?WNM_CONFIG_CAT, <<"force_wnm_local_outbound">>, 'true').

-spec default_force_outbound() -> boolean().
default_force_outbound() ->
    whapps_config:get_is_true(?WNM_CONFIG_CAT, <<"default_force_outbound">>, 'false').

-spec find_early_ringback(wnm_number()) -> api_binary().
find_early_ringback(#number{number_doc=JObj}) ->
    wh_json:get_ne_value([<<"ringback">>, <<"early">>], JObj).

-spec find_transfer_ringback(wnm_number()) -> api_binary().
find_transfer_ringback(#number{number_doc=JObj}) ->
    wh_json:get_ne_value([<<"ringback">>, <<"transfer">>], JObj).

-spec prepend(wnm_number()) -> api_binary().
prepend(#number{number_doc=JObj
                ,features=Features
               }) ->
    case
        sets:is_element(<<"prepend">>, Features)
        andalso wh_json:is_true([<<"prepend">>, <<"enabled">>], JObj)
    of
        'false' -> 'undefined';
        'true' ->
            wh_json:get_ne_value([<<"prepend">>, <<"name">>], JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Move a port_in number to in_service
%% @end
%%--------------------------------------------------------------------
-spec ported(ne_binary()) -> operation_return().
ported(Number) ->
    Routines = [fun({'not_found', #number{}=N}) ->
                        lager:debug("number ~s not found, checking ports", [Number]),
                        check_ports(N);
                   ({_, #number{}}=E) -> E;
                   (#number{state = ?NUMBER_STATE_PORT_IN
                            ,assigned_to=AssignedTo
                           }=N) ->
                        lager:debug("attempting to move port_in number ~s to in_service for account ~s", [Number, AssignedTo]),
                        N#number{auth_by='system'
                                 ,assign_to=AssignedTo
                                };
                   (#number{}=N) ->
                        wnm_number:error_unauthorized(N)
                end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:in_service(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:simple_save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("ported number update prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number_doc=JObj}) ->
                         lager:debug("ported number update successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end
                ,catch wnm_number:get(Number)
                ,Routines
               ).

-spec check_ports(wnm_number()) ->
                         wnm_number() |
                         {'not_found', wnm_number()}.
check_ports(#number{number=MaybePortNumber}=Number) ->
    case wnm_number:find_port_in_number(Number) of
        {'ok', PortDoc} ->
            lager:debug("found port doc with number ~s for account ~s"
                        ,[MaybePortNumber, wh_doc:account_id(PortDoc)]),
            wnm_number:number_from_port_doc(Number, PortDoc);
        {'error', 'not_found'} ->
            lager:debug("number not found in ports"),
            {'not_found', Number}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add and reserve a number for an account
%% @end
%%--------------------------------------------------------------------
-spec create_number(ne_binary(), api_binary(), ne_binary() | 'system') ->
                           operation_return().
-spec create_number(ne_binary(), api_binary(), ne_binary() | 'system', wh_json:object()) ->
                           operation_return().
-spec create_number(ne_binary(), ne_binary(), ne_binary() | 'system', wh_json:object(), boolean()) ->
                           operation_return().
-spec create_number(ne_binary(), ne_binary(), ne_binary() | 'system', wh_json:object(), boolean(), api_binary()) ->
                           operation_return().

create_number(Number, AssignTo, AuthBy) ->
    create_number(Number, AssignTo, AuthBy, wh_json:new()).

create_number(Number, AssignTo, AuthBy, PublicFields) ->
    create_number(Number, AssignTo, AuthBy, PublicFields, 'false').

create_number(Number, AssignTo, AuthBy, PublicFields, DryRun) ->
    create_number(Number, AssignTo, AuthBy, PublicFields, DryRun, 'undefined').

create_number(Number, AssignTo, AuthBy, PublicFields, DryRun, ModuleName) ->
    lager:debug("attempting to create number ~s for account ~s", [Number, AssignTo]),
    Routines = [fun(_) -> wnm_number:get(Number, PublicFields) end
                ,fun({'not_found', #number{}=N}) ->
                         lager:debug("try to create not_found number ~s", [Number]),
                         create_not_found_number(Number, AssignTo, AuthBy, PublicFields, N, ModuleName);
                    ({_, #number{}}=E) -> E;
                    (#number{current_state = ?NUMBER_STATE_AVAILABLE}=N) -> N;
                    (#number{}=N) -> wnm_number:error_number_exists(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> N#number{dry_run=DryRun}
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) ->
                         wnm_number:reserved(N#number{assign_to=AssignTo
                                                      ,auth_by=AuthBy
                                                     })
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{dry_run='true'
                             ,services=Services
                             ,activations=ActivationCharges
                            }) ->
                         {'dry_run', [{'services', Services}
                                      ,{'activation_charges', ActivationCharges}
                                     ]};
                    (#number{number_doc=JObj}) ->
                         lager:debug("create number successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

-spec create_not_found_number(ne_binary(), api_binary(), 'system' | ne_binary(), wh_json:object(), wnm_number(), api_binary()) ->
                                     wnm_number().
create_not_found_number(Number, AssignTo, AuthBy, PublicFields, N, ModuleName) ->
    case account_can_create_number(AuthBy) of
        'true' ->
            lager:debug("number doesnt exist but account ~s is authorized to create it", [AuthBy]),
            case is_number_porting(N) of
                'true' ->
                    lager:debug("number is being ported in, not creating"),
                    wnm_number:error_number_is_porting(N);
                'false' ->
                    lager:debug("number is not in a port request"),
                    NewNumber = N#number{number=Number
                                         ,assign_to=AssignTo
                                         ,auth_by=AuthBy
                                         ,number_doc=PublicFields
                                         ,module_name=ModuleName
                                        },
                    wnm_number:create_available(NewNumber)
            end;
        'false' ->
            lager:debug("account is not authorized to create a new number"),
            wnm_number:error_unauthorized(N)
    end.

-spec is_number_porting(wnm_number()) -> boolean().
is_number_porting(N) ->
    case wnm_number:find_port_in_number(N) of
        {'ok', _Doc} -> 'true';
        {'error', 'not_found'} -> 'false'
    end.

-spec account_can_create_number(ne_binary() | 'system') -> boolean().
account_can_create_number('system') -> 'true';
account_can_create_number(Account) ->
    {'ok', JObj} = kz_account:fetch(Account),
    kz_account:allow_number_additions(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a port in number for an account
%% @end
%%--------------------------------------------------------------------
-spec port_in(ne_binary(), ne_binary(), ne_binary()) ->
                     operation_return().
-spec port_in(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) ->
                     operation_return().
-spec port_in(ne_binary(), ne_binary(), ne_binary(), wh_json:object(), boolean()) ->
                     operation_return().

port_in(Number, AssignTo, AuthBy) ->
    port_in(Number, AssignTo, AuthBy, wh_json:new()).

port_in(Number, AssignTo, AuthBy, PublicFields) ->
    port_in(Number, AssignTo, AuthBy, PublicFields, 'false').

port_in(Number, AssignTo, AuthBy, PublicFields, DryRun) ->
    lager:debug("attempting to port_in number ~s for account ~s", [Number, AssignTo]),
    Routines = [fun({'not_found', #number{}=N}) ->
                        NewNumber = N#number{number=Number
                                             ,assign_to=AssignTo
                                             ,auth_by=AuthBy
                                             ,number_doc=PublicFields
                                            },
                        wnm_number:create_port_in(NewNumber);
                   ({_, #number{}}=E) -> E;
                   (#number{}=N) -> wnm_number:error_number_exists(N)
                end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) ->
                         N#number{dry_run=DryRun}
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{dry_run='true'
                             ,services=Services
                             ,activations=ActivationCharges
                            }) ->
                            {'dry_run', [{'services', Services}
                                         ,{'activation_charges', ActivationCharges}
                                        ]};
                    (#number{number_doc=JObj}) ->
                         lager:debug("port in number successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, catch wnm_number:get(Number, PublicFields), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, creating it as a local number if
%% missing
%% @end
%%--------------------------------------------------------------------
-spec reconcile_number(ne_binary(), ne_binary(), ne_binary()) -> operation_return().
reconcile_number(Number, AssignTo, AuthBy) ->
    Routines = [fun({'not_found', #number{}=N}) when is_binary(AssignTo) ->
                        NewNumber = N#number{number=Number
                                             ,assign_to=AssignTo
                                             ,auth_by=wh_util:to_binary(AuthBy)
                                            },
                        wnm_number:create_available(NewNumber);
                   ({_, #number{}}=E) -> E;
                   (#number{}=N) when is_binary(AssignTo) ->
                        N#number{assign_to=AssignTo};
                   (#number{assigned_to='undefined'}=N)  ->
                        wnm_number:error_unauthorized(N);
                   (#number{assigned_to=AssignedTo}=N) ->
                        N#number{assign_to=AssignedTo}
                end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) when is_binary(AuthBy) ->
                         N#number{auth_by=AuthBy};
                    (#number{assign_to=ATo}=N) ->
                         N#number{auth_by=ATo}
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{assign_to=Assign}=N) ->
                         lager:debug("attempting to reconcile number ~s with account ~s", [Number, Assign]),
                         wnm_number:in_service(N)
                 end
                ,fun({'no_change_required', #number{assigned_to='undefined'}}=E) ->
                         E;
                    ({'no_change_required', #number{}=N}) ->
                         wnm_number:save_phone_number_docs(N);
                    ({'unauthorized', #number{auth_by='system'}=N}) ->
                         wnm_number:save_phone_number_docs(N);
                    ({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number_doc=JObj}) ->
                         lager:debug("reconcile number successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, catch wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release all numbers currently assigned to an account
%% @end
%%--------------------------------------------------------------------
-spec free_numbers(ne_binary()) -> 'ok'.
free_numbers(AccountId) ->
    lager:debug("attempting to free all numbers assigned to account ~s", [AccountId]),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {'ok', JObj} ->
            _ = [release_number(Key, 'system')
                 || Key <- wh_json:get_keys(wh_json:public_fields(JObj))
                        ,wnm_util:is_reconcilable(Key)
                ],
            'ok';
        {_R, _} ->
            lager:debug("failed to open account ~s ~s document: ~p", [AccountId, ?WNM_PHONE_NUMBER_DOC, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add and reserve a number for an account
%% @end
%%--------------------------------------------------------------------
-spec reserve_number(ne_binary(), ne_binary(), ne_binary()) ->
                            operation_return().
-spec reserve_number(ne_binary(), ne_binary(), ne_binary(), api_object()) ->
                            operation_return().
-spec reserve_number(ne_binary(), ne_binary(), ne_binary(), api_object(), boolean()) ->
                            operation_return().

reserve_number(Number, AssignTo, AuthBy) ->
    reserve_number(Number, AssignTo, AuthBy, 'undefined', 'false').

reserve_number(Number, AssignTo, AuthBy, PublicFields) ->
    reserve_number(Number, AssignTo, AuthBy, PublicFields, 'false').

reserve_number(Number, AssignTo, AuthBy, PublicFields, DryRun) ->
    lager:debug("attempting to reserve ~s for account ~s", [Number, AssignTo]),
    Routines = [fun({_, #number{}}=E) -> E;
                   (#number{}=N) ->
                        wnm_number:reserved(N#number{assign_to=AssignTo
                                                     ,auth_by=AuthBy
                                                     ,dry_run=DryRun
                                                    })
                end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{dry_run='true'
                             ,services=Services
                             ,activations=ActivationCharges
                            }) ->
                         {'dry_run', [{'services', Services}
                                      ,{'activation_charges', ActivationCharges}
                                     ]};
                    (#number{number_doc=JObj}) ->
                         lager:debug("reserve successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end
                ,catch wnm_number:get(Number, PublicFields)
                ,Routines
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, aquiring the number from the provider
%% if necessary
%% @end

-spec assign_number_to_account(ne_binary(), ne_binary(), ne_binary()) ->
                                      operation_return().
-spec assign_number_to_account(ne_binary(), ne_binary(), ne_binary(), api_object()) ->
                                      operation_return().
-spec assign_number_to_account(ne_binary(), ne_binary(), ne_binary(), api_object(), boolean()) ->
                                      operation_return().

assign_number_to_account(Number, AssignTo, AuthBy) ->
    assign_number_to_account(Number, AssignTo, AuthBy, 'undefined', 'false').

assign_number_to_account(Number, AssignTo, AuthBy, PublicFields) ->
    assign_number_to_account(Number, AssignTo, AuthBy, PublicFields, 'false').

assign_number_to_account(Number, AssignTo, AuthBy, PublicFields, DryRun) ->
    lager:debug("attempting to assign ~s to account ~s: dry run : ~p", [Number, AssignTo, DryRun]),
    Routines = [fun(_) -> wnm_number:get(Number, PublicFields) end
                ,fun({'not_found', _}) ->
                         NewNumber = #number{number=Number
                                             ,module_name='wnm_other'
                                             ,module_data=wh_json:new()
                                             ,number_doc=wh_json:public_fields(PublicFields)
                                            },
                         wnm_number:create_discovery(NewNumber);
                    ({_, #number{}}=E) -> E;
                    (#number{}=N) -> N
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) ->
                         N#number{dry_run=DryRun}
                 end
                ,fun ({_, #number{}}=E) -> E;
                     (#number{}=N) ->
                         wnm_number:in_service(N#number{assign_to=AssignTo
                                                        ,auth_by=AuthBy
                                                       })
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{dry_run='true'
                             ,services=Services
                             ,activations=ActivationCharges
                            }) ->
                         {'dry_run', [{'services', Services}
                                      ,{'activation_charges', ActivationCharges}
                                     ]};
                    (#number{number_doc=JObj}) ->
                         lager:debug("assign number to account successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% move an in service number to the released state were it will be
%% recycled or cancled after a buffer period
%% @end
%%--------------------------------------------------------------------
-spec release_number(ne_binary(), 'system' | ne_binary()) -> operation_return().
release_number(Number, AuthBy) ->
    lager:debug("attempting to release ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:released(N#number{auth_by=AuthBy})
                 end
                ,fun({'no_change_required', #number{}=N}) ->
                         wnm_number:save_phone_number_docs(N);
                    ({_, #number{}}=E) -> E;
                    (#number{hard_delete='false'}=N) -> wnm_number:save(N);
                    (#number{hard_delete='true'}=N) -> wnm_number:delete(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number_doc=JObj}) ->
                         lager:debug("release successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lists attachments on a number
%% @end
%%--------------------------------------------------------------------
-spec list_attachments(ne_binary(), ne_binary()) ->
                              operation_return().
list_attachments(Number, AuthBy) ->
    lager:debug("attempting to list attachements on ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{state = ?NUMBER_STATE_PORT_IN}=N) -> N;
                    (#number{}=N) -> wnm_number:error_unauthorized(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{assigned_to=AssignedTo}=N) ->
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
                             'false' -> wnm_number:error_unauthorized(N);
                             'true' -> N
                         end
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number_doc=JObj}) ->
                         lager:debug("list attachements successfully completed"),
                         {'ok', wh_doc:attachments(JObj, wh_json:new())}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch an attachment on a number
%% @end
%%--------------------------------------------------------------------
-spec fetch_attachment(ne_binary(), ne_binary(), ne_binary()) -> operation_return().
fetch_attachment(Number, Name, AuthBy) ->
    lager:debug("fetch attachement on ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{state = ?NUMBER_STATE_PORT_IN}=N) -> N;
                    (#number{}=N) -> wnm_number:error_unauthorized(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{assigned_to=AssignedTo}=N) ->
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
                             'false' -> wnm_number:error_unauthorized(N);
                             'true' -> N
                         end
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number=Num}) ->
                         Db = wnm_util:number_to_db_name(Num),
                         lager:debug("attempting to fetch attachement ~s", [Name]),
                         couch_mgr:fetch_attachment(Db, Num, Name)
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec put_attachment(ne_binary(), ne_binary(), ne_binary(), wh_proplist(), ne_binary()) ->
                            operation_return().
put_attachment(Number, Name, Content, Options, AuthBy) ->
    lager:debug("add attachement to ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{state = ?NUMBER_STATE_PORT_IN}=N) -> N;
                    (#number{}=N) -> wnm_number:error_unauthorized(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{assigned_to=AssignedTo}=N) ->
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
                             'false' -> wnm_number:error_unauthorized(N);
                             'true' -> N
                         end
                 end
                ,fun({E, #number{error_jobj=JObj}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, JObj};
                    (#number{number=Num, number_doc=JObj}) ->
                         lager:debug("attempting to put attachement ~s", [Name]),
                         Db = wnm_util:number_to_db_name(Num),
                         Rev = wh_doc:revision(JObj),
                         couch_mgr:put_attachment(Db, Num, Name, Content, [{'rev', Rev}|Options])
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec delete_attachment(ne_binary(), ne_binary(), ne_binary()) ->
                               operation_return().
delete_attachment(Number, Name, AuthBy) ->
    lager:debug("delete attachement from ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{state = ?NUMBER_STATE_PORT_IN}=N) -> N;
                    (#number{}=N) -> wnm_number:error_unauthorized(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{assigned_to=AssignedTo}=N) ->
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
                             'false' -> wnm_number:error_unauthorized(N);
                             'true' -> N
                         end
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number=Num}) ->
                         lager:debug("attempting to delete attachement ~s", [Name]),
                         Db = wnm_util:number_to_db_name(Num),
                         couch_mgr:delete_attachment(Db, Num, Name)
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec get_public_fields(ne_binary(), ne_binary()) ->
                               operation_return().
get_public_fields(Number, AuthBy) ->
    lager:debug("attempting to get public fields for number ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) when AuthBy =:= 'system' -> N;
                    (#number{assigned_to=AssignedTo}=N) ->
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
                             'false' -> wnm_number:error_unauthorized(N);
                             'true' -> N
                         end
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{number_doc=JObj}) ->
                         lager:debug("fetch public fields successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, 'ok', Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec set_public_fields(ne_binary(), wh_json:object(), ne_binary()) ->
                               operation_return().
-spec set_public_fields(ne_binary(), wh_json:object(), ne_binary(), boolean()) ->
                               operation_return().
set_public_fields(Number, PublicFields, AuthBy) ->
    set_public_fields(Number, PublicFields, AuthBy, 'false').

set_public_fields(Number, PublicFields, AuthBy, DryRun) ->
    Routines = [fun({_, #number{}}=E) -> E;
                   (#number{assigned_to=AssignedTo}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
                            'false' -> wnm_number:error_unauthorized(N);
                            'true' -> N#number{dry_run=DryRun}
                        end
                end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> wnm_number:save(N)
                 end
                ,fun({E, #number{error_jobj=Reason}}) ->
                         lager:debug("create number prematurely ended: ~p", [E]),
                         {E, Reason};
                    (#number{dry_run='true'
                             ,services=Services
                             ,activations=ActivationCharges
                            }) ->
                         {'dry_run', [{'services', Services}
                                      ,{'activation_charges', ActivationCharges}
                                     ]};
                    (#number{number_doc=JObj}) ->
                         lager:debug("set public fields successfully completed"),
                         {'ok', wh_json:public_fields(JObj)}
                 end
               ],
    lists:foldl(fun(F, J) -> catch F(J) end, wnm_number:get(Number, PublicFields), Routines).


-spec track_assignment(ne_binary(), wh_proplist()) -> 'ok'.
track_assignment(Account, Props) ->
    lager:debug("tracking number assignment for account ~s", [Account]),
    lists:foreach(
      fun({Number, Assignment}) ->
              wnm_number:used_by(Number, Assignment)
      end
      ,Props
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over all the discovered numbers during a find operation and
%% ensure the modules data is stored for later acquisition.
%% @end
%%--------------------------------------------------------------------
-spec prepare_find_results(wh_proplist(), [], wh_proplist() | wh_json:keys()) -> wh_json:keys().
-spec prepare_find_results(wh_json:keys(), atom(), wh_json:object(), wh_json:keys(), wh_proplist()) ->
                                  wh_json:keys().
prepare_find_results([], Found, _) -> Found;
prepare_find_results([{'wnm_other', {'bulk', ModuleResults}}|T], Found, Opts) ->
    prepare_find_results(T, ModuleResults++Found, Opts);
prepare_find_results([{Module, {'ok', ModuleResults}}|T], Found, Opts) ->
    case wh_json:get_keys(ModuleResults) of
        [] -> prepare_find_results(T, Found, Opts);
        Numbers ->
            Results = prepare_find_results(Numbers, Module, ModuleResults, Found, Opts),
            prepare_find_results(T, Results, Opts)
    end;
prepare_find_results([_|T], Found, Opts) ->
    prepare_find_results(T, Found, Opts).

prepare_find_results([], _, _, Found,_) -> Found;
prepare_find_results([Number|Numbers], 'wnm_other' = ModuleName, ModuleResults, Found, Opts) ->
    JObj = wh_json:get_value(Number, ModuleResults),
    Result = case maybe_get_activation_charge(Opts) of
                 'undefined' ->
                     [wh_json:set_value(<<"number">>, Number, JObj) | Found];
                 Value ->
                     [wh_json:set_values([{<<"activation_charge">>, Value}
                                          ,{<<"number">>, Number}
                                         ], JObj)
                      | Found]
             end,
    prepare_find_results(Numbers, ModuleName, ModuleResults, Result, Opts);
prepare_find_results([Number|Numbers], ModuleName, ModuleResults, Found, Opts) ->
    JObj = wh_json:get_value(Number, ModuleResults),
    Result = case maybe_get_activation_charge(Opts) of
                 'undefined' ->
                     [wh_json:set_value(<<"number">>, Number, JObj) | Found];
                 Value ->
                     [wh_json:set_values([{<<"activation_charge">>, Value}
                                          ,{<<"number">>, Number}
                                         ], JObj)
                      | Found]
             end,
    case catch wnm_number:get(Number) of
        #number{state=State} ->
            case lists:member(State, ?WNM_AVALIABLE_STATES) of
                'true' ->
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Result, Opts);
                'false' ->
                    lager:debug("the discovery '~s' is not available: ~s", [Number, State]),
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Found, Opts)
            end;
        {'not_found', #number{}=N} ->
            NewNumber = N#number{number=Number
                                 ,module_name = ModuleName
                                 ,module_data=JObj
                                },
            case catch wnm_number:save(wnm_number:create_discovery(NewNumber)) of
                #number{} ->
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Result, Opts);
                {_R, #number{}} ->
                    lager:debug("failed to store discovery ~s: ~p", [Number, _R]),
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Found, Opts)
            end;
        {_R, #number{}} ->
            lager:debug("failed to determine state of discovery ~s: ~p", [Number, _R]),
            prepare_find_results(Numbers, ModuleName, ModuleResults, Found, Opts)
    end.

-spec maybe_get_activation_charge(wh_proplist()) -> 'undefined' | non_neg_integer().
maybe_get_activation_charge(Opts) ->
    case props:get_value(<<"services">>, Opts) of
        'undefined' -> 'undefined';
        Services ->
            wh_services:activation_charges(<<"phone_numbers">>
                                           ,props:get_value(<<"classification">>, Opts)
                                           ,Services
                                          )
    end.
