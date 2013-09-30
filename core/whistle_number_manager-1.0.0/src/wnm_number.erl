%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Handle state transitions enforcing business logic
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_number).

-export([create_discovery/1]).
-export([create_available/1]).
-export([create_port_in/1]).
-export([get/1, get/2]).
-export([save/1]).
-export([save_phone_number_docs/1]).
-export([delete/1]).
-export([activate_feature/2]).

-export([discovery/1]).
-export([port_in/1]).
-export([available/1]).
-export([reserved/1]).
-export([in_service/1]).
-export([released/1]).
-export([port_out/1]).
-export([disconnected/1]).

-export([error_invalid_state_transition/2]).
-export([error_unauthorized/1]).
-export([error_number_exists/1]).
-export([error_no_change_required/2]).
-export([error_not_reconcilable/1]).
-export([error_number_database/2]).
-export([error_carrier_not_specified/1]).
-export([error_number_not_found/1]).
-export([error_service_restriction/2]).
-export([error_provider_fault/2]).
-export([error_carrier_fault/2]).

-include("wnm.hrl").

-export_type([wnm_number/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new number record with the intial state of discovery
%% @end
%%--------------------------------------------------------------------
-spec create_discovery(wnm_number()) -> wnm_number().
create_discovery(#number{number=Number
                         ,number_doc=Doc
                         ,module_name=ModuleName
                         ,module_data=ModuleData
                        }) ->
    Num = wnm_util:normalize_number(Number),
    Updates = [{<<"_id">>, Num}
               ,{<<"pvt_module_name">>, ModuleName}
               ,{<<"pvt_module_data">>, ModuleData}
               ,{<<"pvt_number_state">>, <<"discovery">>}
               ,{<<"pvt_ported_in">>, 'false'}
               ,{<<"pvt_db_name">>, wnm_util:number_to_db_name(Num)}
               ,{<<"pvt_created">>, wh_util:current_tstamp()}
              ],
    JObj = wh_json:set_values(Updates, wh_json:public_fields(Doc)),
    json_to_record(JObj, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new wnm_local record with the intial state of available
%% @end
%%--------------------------------------------------------------------
-spec create_available(wnm_number()) -> wnm_number().
create_available(#number{auth_by='undefined'}=N) ->
    error_unauthorized(N);
create_available(#number{number=Number
                         ,auth_by=AuthBy
                         ,number_doc=Doc
                        }=N) ->
    Num = wnm_util:normalize_number(Number),
    Updates = [{<<"_id">>, Num}
               ,{<<"pvt_module_name">>, <<"wnm_local">>}
               ,{<<"pvt_module_data">>, wh_json:new()}
               ,{<<"pvt_number_state">>, <<"available">>}
               ,{<<"pvt_db_name">>, wnm_util:number_to_db_name(Num)}
               ,{<<"pvt_created">>, wh_util:current_tstamp()}
               ,{<<"pvt_authorizing_account">>, AuthBy}
              ],
    JObj = wh_json:set_values(Updates, wh_json:public_fields(Doc)),
    json_to_record(JObj, 'true', N).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new wnm_local record with the intial state of port_in
%% @end
%%--------------------------------------------------------------------
-spec create_port_in(wnm_number()) -> wnm_number().
create_port_in(#number{auth_by='undefined'}=N) ->
    error_unauthorized(N);
create_port_in(#number{number=Number
                       ,assign_to=AssignTo
                       ,auth_by=AuthBy
                       ,number_doc=Doc
                      }=N) ->
    Num = wnm_util:normalize_number(Number),
    ModuleName = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"porting_module_name">>, <<"wnm_local">>),
    Updates = [{<<"_id">>, Num}
               ,{<<"pvt_module_name">>, ModuleName}
               ,{<<"pvt_module_data">>, wh_json:new()}
               ,{<<"pvt_number_state">>, <<"port_in">>}
               ,{<<"pvt_ported_in">>, 'true'}
               ,{<<"pvt_db_name">>, wnm_util:number_to_db_name(Num)}
               ,{<<"pvt_created">>, wh_util:current_tstamp()}
               ,{<<"pvt_authorizing_account">>, AuthBy}
               ,{<<"pvt_assigned_to">>, AssignTo}
              ],
    JObj = wh_json:set_values(Updates, wh_json:public_fields(Doc)),
    json_to_record(JObj, 'true', N).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets a number record or throws
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> wnm_number().
-spec get(ne_binary(), api_object()) -> wnm_number().

get(Number) ->
    get(Number, 'undefined').

get(Number, PublicFields) ->
    Num = wnm_util:normalize_number(Number),
    Routines = [fun(#number{}=N) ->
                        case wnm_util:is_reconcilable(Num) of
                            'false' -> error_not_reconcilable(N);
                            'true' -> N
                        end
                end
                ,fun(#number{number_db=Db}=N) ->
                         case couch_mgr:open_cache_doc(Db, Num) of
                             {'ok', JObj} -> merge_public_fields(PublicFields, json_to_record(JObj, N));
                             {'error', 'not_found'} -> error_number_not_found(N);
                             {'error', Reason} -> error_number_database(Reason, N)
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end
                ,#number{number=Num, number_db=wnm_util:number_to_db_name(Num)}
                ,Routines
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the number doc and update the accounts phone_numbers
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{}=Number) ->
    Routines = [fun(#number{}=N) -> exec_providers(N, 'save') end
                ,fun(#number{}=N) -> N#number{number_doc=record_to_json(N)} end
                ,fun(#number{}=N) -> get_updated_phone_number_docs(N) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> save_number_doc(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> save_phone_number_docs(N)
                 end
                ,fun(#number{}=N) -> update_service_plans(N) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the account phone_numbers doc in the number record
%% @end
%%--------------------------------------------------------------------
-spec save_phone_number_docs(wnm_number()) -> wnm_number().
save_phone_number_docs(#number{phone_number_docs='undefined'}=Number) ->
    save_phone_number_docs(get_updated_phone_number_docs(Number));
save_phone_number_docs(#number{phone_number_docs=PhoneNumberDocs}=Number) ->
    case dict:size(PhoneNumberDocs) > 0 of
        'false' -> Number;
        'true' -> save_phone_number_docs(dict:to_list(PhoneNumberDocs), Number)
    end.

save_phone_number_docs([], Number) -> Number;
save_phone_number_docs([{Account, JObj}|Props], #number{number=Num}=Number) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {'ok', _} ->
            lager:debug("saved updated phone_numbers doc in account db ~s", [AccountDb]),
            save_phone_number_docs(Props, Number);
        {'error', 'conflict'} ->
            case resolve_account_phone_numbers_conflict(JObj, Num, AccountDb) of
                {'error', _} -> save_phone_number_docs(Props, Number);
                {'ok', J} -> save_phone_number_docs([{Account, J}|Props], Number)
            end;
        {'error', _R} ->
            lager:info("failed to update phone numbers doc in account ~s: ~p", [AccountDb, _R]),
            save_phone_number_docs(Props, Number)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete the number doc and remove it from the accounts
%% phone_numbers.
%% @end
%%--------------------------------------------------------------------
-spec delete(wnm_number()) -> wnm_number().
delete(Number) ->
    Routines = [fun(#number{}=N) -> exec_providers(N, 'delete') end
                ,fun(#number{}=N) -> N#number{number_doc=record_to_json(N)} end
                ,fun(#number{}=N) -> get_updated_phone_number_docs(N) end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> delete_number_doc(N)
                 end
                ,fun({_, #number{}}=E) -> E;
                    (#number{}=N) -> save_phone_number_docs(N)
                 end
                ,fun(#number{}=N) -> update_service_plans(N) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec discovery(wnm_number()) -> no_return().
discovery(#number{state = <<"discovery">>}=Number) ->
    error_no_change_required(<<"discovery">>, Number);
discovery(Number) ->
    error_invalid_state_transition(<<"discovery">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec port_in(wnm_number()) -> no_return().
port_in(#number{state = <<"port_in">>
                ,assigned_to=AssignTo
                ,assign_to=AssignTo
               }=Number) ->
    error_no_change_required(<<"port_in">>, Number);
port_in(#number{state = <<"port_in">>}=Number) ->
    error_unauthorized(Number);
port_in(Number) ->
    error_invalid_state_transition(<<"port_in">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available(wnm_number()) -> wnm_number().
available(#number{state = <<"released">>}=Number) ->
    Routines = [fun(#number{auth_by=AuthBy}=N) ->
                        %% Only the 'system' maintenance routines or a system admin can
                        %% move a released (aging) number back to available.
                        case AuthBy =:= 'system'
                            orelse wh_util:is_system_admin(AuthBy)
                        of
                            'true' -> N#number{state = <<"available">>};
                            'false' -> error_unauthorized(N)
                        end
                end
               ],
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines);
available(#number{state = <<"available">>}=Number) ->
    error_no_change_required(<<"available">>, Number);
available(Number) ->
    error_invalid_state_transition(<<"available">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reserved(wnm_number()) -> wnm_number().
reserved(#number{state = <<"discovery">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo
                            ,auth_by=AuthBy
                           }=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, 'true') of
                            'false' -> error_unauthorized(N);
                            'true' -> N
                        end
                end
                ,fun(#number{reserve_history=ReserveHistory
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{reserve_history=ordsets:add_element(AssignTo, ReserveHistory)}
                 end
                ,fun(#number{assigned_to='undefined'
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{state = <<"reserved">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo
                             ,prev_assigned_to=AssignedTo
                            }=N) ->
                         N#number{state = <<"reserved">>};
                    (#number{assigned_to=AssignedTo
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{state = <<"reserved">>
                                  ,assigned_to=AssignTo
                                  ,prev_assigned_to=AssignedTo
                                 }
                 end
                ,fun(#number{}=N) -> activate_phone_number(N) end
                ,fun(#number{module_name='undefined'}=N) ->
                         error_carrier_not_specified(N);
                    (#number{module_name=Module}=N) ->
                         Module:acquire_number(N)
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
reserved(#number{state = <<"available">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo
                            ,auth_by=AuthBy
                           }=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, 'true') of
                            'false' -> error_unauthorized(N);
                            'true' -> N
                        end
                end
                ,fun(#number{reserve_history=ReserveHistory
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{reserve_history=ordsets:add_element(AssignTo, ReserveHistory)}
                 end
                ,fun(#number{assigned_to='undefined'
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{state = <<"reserved">>
                                  ,assigned_to=AssignTo
                                 };
                    (#number{assigned_to=AssignedTo
                             ,prev_assigned_to=AssignedTo
                            }=N) ->
                         N#number{state = <<"reserved">>};
                    (#number{assigned_to=AssignedTo
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{state = <<"reserved">>
                                  ,assigned_to=AssignTo
                                  ,prev_assigned_to=AssignedTo
                                 }
                 end
                ,fun(#number{}=N) -> activate_phone_number(N) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
reserved(#number{state = <<"reserved">>
                 ,assigned_to=AssignedTo
                 ,assign_to=AssignedTo
                }=Number) ->
    error_no_change_required(<<"reserved">>, Number);
reserved(#number{state = <<"reserved">>}=Number) ->
    Routines = [fun(#number{auth_by=AuthBy
                            ,assigned_to=AssignedTo
                            ,assign_to=AssignTo
                           }=N) ->
                        case (not wh_util:is_empty(AssignTo))
                            andalso (wh_util:is_in_account_hierarchy(AssignedTo, AuthBy)
                                     orelse wh_util:is_in_account_hierarchy(AuthBy, AssignedTo))
                        of
                            'false' -> error_unauthorized(N);
                            'true' -> N
                        end
                end
                ,fun(#number{reserve_history=ReserveHistory
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{reserve_history=ordsets:add_element(AssignTo, ReserveHistory)}
                 end
                ,fun(#number{assigned_to='undefined'
                             ,assign_to=AssignTo
                            }=N) ->
                         N#number{state = <<"reserved">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) ->
                         N#number{state = <<"reserved">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"reserved">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{}=N) -> activate_phone_number(N) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
reserved(#number{state = <<"in_service">>}=Number) ->
    Routines = [fun(#number{auth_by=AuthBy, assigned_to=AssignedTo}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{state = <<"reserved">>}
                        end
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
reserved(Number) ->
    error_invalid_state_transition(<<"reserved">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec in_service(wnm_number()) -> wnm_number().
in_service(#number{state = <<"discovery">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case AuthBy =:= system orelse wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, assign_to=AssignedTo}=N) ->
                         N#number{state = <<"in_service">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{}=N) -> activate_phone_number(N) end
                ,fun(#number{module_name=undefined}=N) ->
                         error_carrier_not_specified(N);
                    (#number{module_name=Module}=N) ->
                         Module:acquire_number(N)
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"port_in">>}=Number) ->
    Routines = [fun(#number{assigned_to=AssignedTo, auth_by=AuthBy}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{state = <<"in_service">>}
                        end
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"available">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case AuthBy =:= system orelse wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, assign_to=AssignedTo}=N) ->
                         N#number{state = <<"in_service">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{}=N) -> activate_phone_number(N) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"reserved">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy, assigned_to=AssignedTo}=N) ->
                         case (wh_util:is_in_account_hierarchy(AssignedTo, AuthBy, true)
                               orelse wh_util:is_in_account_hierarchy(AuthBy, AssignedTo))
                             andalso wh_util:is_in_account_hierarchy(AssignedTo, AssignTo, true)
                         of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, assign_to=AssignedTo}=N) ->
                         N#number{state = <<"in_service">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{}=N) -> activate_phone_number(N) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"in_service">>, assigned_to=AssignedTo, assign_to=AssignedTo}=Number) ->
    error_no_change_required(<<"in_service">>, Number);
in_service(#number{state = <<"in_service">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                    case (wh_util:is_in_account_hierarchy(AssignTo, AuthBy, true)
                        orelse wh_util:is_in_account_hierarchy(AuthBy, AssignTo))
                    of
                        false -> error_unauthorized(N);
                        true -> N
                    end
                end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(Number) ->
    error_invalid_state_transition(<<"in_service">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec released(wnm_number()) -> wnm_number().
released(#number{state = <<"reserved">>}=Number) ->
    NewState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
    Routines = [fun(#number{assigned_to=AssignedTo, auth_by=AuthBy, number_doc=JObj}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{features=sets:new()
                                             ,number_doc=wh_json:private_fields(JObj)
                                            }
                        end
                end
                ,fun(#number{assigned_to=AssignedTo}=N) ->
                         N#number{state=NewState, assigned_to=undefined, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{module_name=ModuleName, prev_assigned_to=AssignedTo, reserve_history=ReserveHistory}=N) ->
                    History = ordsets:del_element(AssignedTo, ReserveHistory),
                    case ordsets:to_list(History) of
                        [] when ModuleName =:= wnm_local ->
                            lager:debug("flagging released local number for hard delete", []),
                            N#number{state = <<"released">>, reserve_history=ordsets:new(), hard_delete=true};
                        [] ->
                            lager:debug("moving ~p number to number_manager.released_state '~s'", [ModuleName, NewState]),
                            N#number{state = NewState, reserve_history=ordsets:new()};
                        [PrevReservation|_] ->
                            lager:debug("unwinding reservation history, reserving on account ~s", [PrevReservation]),
                            N#number{reserve_history=History
                                     ,assigned_to=PrevReservation
                                     ,state = <<"reserved">>
                                    }
                    end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
released(#number{state = <<"in_service">>}=Number) ->
    NewState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
    Routines = [fun(#number{assigned_to=AssignedTo, auth_by=AuthBy, number_doc=JObj}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{features=sets:new()
                                             ,number_doc=wh_json:private_fields(JObj)
                                            }
                        end
                end
                ,fun(#number{assigned_to=AssignedTo}=N) ->
                         N#number{state=NewState, assigned_to=undefined, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{module_name=ModuleName, prev_assigned_to=AssignedTo, reserve_history=ReserveHistory}=N) ->
                    History = ordsets:del_element(AssignedTo, ReserveHistory),
                    case ordsets:to_list(History) of
                        [] when ModuleName =:= wnm_local ->
                            lager:debug("flagging local number for hard delete", []),
                            N#number{state = <<"released">>, reserve_history=ordsets:new(), hard_delete=true};
                        [] ->
                            lager:debug("moving ~p number to number_manager.released_state '~s'", [ModuleName, NewState]),
                            N#number{state = NewState, reserve_history=ordsets:new()};
                        [PrevReservation|_] ->
                            lager:debug("unwinding reservation history, reserving on account ~s", [PrevReservation]),
                            N#number{reserve_history=History
                                     ,assigned_to=PrevReservation
                                     ,state = <<"reserved">>
                                    }
                    end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
released(#number{state = <<"released">>}=Number) ->
    error_no_change_required(<<"released">>, Number);
released(Number) ->
    error_invalid_state_transition(<<"released">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec port_out(wnm_number()) -> no_return().
port_out(Number) ->
    error_invalid_state_transition(<<"port_out">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disconnected(wnm_number()) -> no_return().
disconnected(Number) ->
    error_invalid_state_transition(<<"disconnected">>, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% convert a json object to the number record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record(wh_json:object(), boolean() | wnm_number()) -> wnm_number().
-spec json_to_record(wh_json:object(), boolean(), wnm_number()) -> wnm_number().
json_to_record(JObj, #number{}=Number) ->
    json_to_record(JObj, false, Number);
json_to_record(JObj, IsNew) when is_boolean(IsNew) ->
    json_to_record(JObj, IsNew, #number{}).

json_to_record(JObj, IsNew, #number{number=Num, number_db=Db}=Number) ->
    Number#number{
      number=wh_json:get_value(<<"_id">>, JObj, Num)
      ,number_db=wh_json:get_value(<<"pvt_db_name">>, JObj, Db)
      ,state=wh_json:get_ne_value(<<"pvt_number_state">>, JObj)
      ,current_state=wh_json:get_ne_value(<<"pvt_number_state">>, JObj)
      ,reserve_history=ordsets:from_list(wh_json:get_ne_value(<<"pvt_reserve_history">>, JObj, []))
      ,assigned_to=wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj)
      ,prev_assigned_to=wh_json:get_ne_value(<<"pvt_previously_assigned_to">>, JObj)
      ,module_name=wnm_util:get_carrier_module(JObj)
      ,module_data=wh_json:get_ne_value(<<"pvt_module_data">>, JObj)
      ,features=sets:from_list(wh_json:get_ne_value(<<"pvt_features">>, JObj, []))
      ,current_features=sets:from_list(wh_json:get_ne_value(<<"pvt_features">>, JObj, []))
      ,number_doc=JObj
      ,current_number_doc=case IsNew of true -> wh_json:new(); false -> JObj end
      ,used_by=wh_json:get_value(<<"used_by">>, JObj, <<>>)
     }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% convert a json object to the number record
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(wnm_number()) -> wh_json:object().
record_to_json(#number{number_doc=JObj}=N) ->
    Updates = [{<<"_id">>, N#number.number}
               ,{<<"pvt_number_state">>, N#number.state}
               ,{<<"pvt_reserve_history">>, ordsets:to_list(N#number.reserve_history)}
               ,{<<"pvt_assigned_to">>, N#number.assigned_to}
               ,{<<"pvt_previously_assigned_to">>, N#number.prev_assigned_to}
               ,{<<"pvt_module_name">>, wh_util:to_binary(N#number.module_name)}
               ,{<<"pvt_module_data">>, N#number.module_data}
               ,{<<"pvt_features">>, [wh_util:to_binary(F) || F <- sets:to_list(N#number.features)]}
               ,{<<"pvt_db_name">>, N#number.number_db}
               ,{<<"pvt_modified">>, wh_util:current_tstamp()}
               ,{<<"pvt_created">>, wh_json:get_value(<<"pvt_created">>, JObj, wh_util:current_tstamp())}
               ,{<<"used_by">>, N#number.used_by}
              ],
    lists:foldl(fun({K, undefined}, J) -> wh_json:delete_key(K, J);
                   ({K, V}, J) -> wh_json:set_value(K, V, J)
                end, JObj, Updates).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% conditionally merge the public fields with a number record
%% @end
%%--------------------------------------------------------------------
-spec merge_public_fields(_, wnm_number()) -> wnm_number().
merge_public_fields(PublicFields, #number{number_doc=JObj}=N) ->
    case wh_json:is_json_object(PublicFields) of
        'false' -> N;
        'true' ->
            N#number{number_doc=wh_json:merge_jobjs(wh_json:private_fields(JObj)
                                                    ,wh_json:public_fields(PublicFields)
                                                   )}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save the number doc in the numbers db
%% @end
%%--------------------------------------------------------------------
-spec save_number_doc(wnm_number()) -> wnm_number().
save_number_doc(#number{number_db=Db, number=Num, number_doc=JObj}=Number) ->
    case couch_mgr:save_doc(Db, JObj) of
        {error, not_found} ->
            lager:debug("attempting to creating new database '~s' for number '~s'", [Db, Num]),
            true = couch_mgr:db_create(Db),
            couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
            save_number_doc(Number);
        {error, Reason} ->
            lager:debug("failed to save '~s' in '~s': ~p", [Num, Db, Reason]),
            error_number_database(Reason, Number);
        {ok, J} ->
            lager:debug("saved '~s' in '~s'", [Num, Db]),
            Number#number{number_doc=J}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Delete the number doc in the numbers db
%% @end
%%--------------------------------------------------------------------
-spec delete_number_doc(wnm_number()) -> wnm_number().
delete_number_doc(#number{number_db=Db, number=Num, number_doc=JObj}=Number) ->
    case couch_mgr:del_doc(Db, JObj) of
        {ok, _} ->
            lager:debug("deleted '~s' from '~s'", [Num, Db]),
            Number#number{number_doc=wh_json:new()};
        {error, Reason} ->
            lager:debug("failed to delete '~s' in '~s': ~p", [Num, Db, Reason]),
            error_number_database(Reason, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Merge the changes to this number in the account phone_numbers doc
%% into the current phone_numbers doc.
%% @end
%%--------------------------------------------------------------------
-spec resolve_account_phone_numbers_conflict(wh_json:object(), ne_binary(), ne_binary()) -> {'ok', wh_json:object()} |
                                                                                                       {'error', _}.
resolve_account_phone_numbers_conflict(JObj, Num, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {error, _R}=E ->
            lager:info("failed to resolve phone numbers conflict in account ~s: ~p", [AccountDb, _R]),
            E;
        {ok, J} ->
            case wh_json:get_value(Num, JObj) of
                undefined -> {ok, wh_json:delete_key(Num, J)};
                Update -> {ok, wh_json:set_value(Num, Update, J)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% execute the save function of all providers, folding the jobj through
%% them and collecting any errors...
%% @end
%%--------------------------------------------------------------------
-spec exec_providers(wnm_number(), atom()) -> wnm_number().
-spec exec_providers([ne_binary(),...] | [], atom(), wnm_number()) -> wnm_number().

exec_providers(Number, Action) ->
    Providers = whapps_config:get(?WNM_CONFIG_CAT, <<"providers">>, ?WNM_DEAFULT_PROVIDER_MODULES),
    exec_providers(Providers, Action, Number).

exec_providers([], _, Number) -> Number;
exec_providers([Provider|Providers], Action, Number) ->
    case wh_util:try_load_module(<<"wnm_", Provider/binary>>) of
        false ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            exec_providers(Providers, Action, Number);
        Mod ->
            case apply(Mod, Action, [Number]) of
                #number{}=N -> exec_providers(Providers, Action, N);
                {error, Reason} ->
                    Errors = wh_json:from_list([{Provider, Reason}]),
                    wnm_number:error_provider_fault(Errors, Number)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% create invalid state error
%% @end
%%--------------------------------------------------------------------
-spec error_invalid_state_transition(ne_binary(), wnm_number()) -> no_return().
error_invalid_state_transition(Transition, #number{state = State}=N) ->
    Error = list_to_binary(["Invalid state transition from ", State, " to ", Transition]),
    lager:debug("~s", [Error]),
    throw({invalid_state_transition, N#number{error_jobj=wh_json:from_list([{<<"state_transition">>, Error}])}}).

-spec error_unauthorized(wnm_number()) -> no_return().
error_unauthorized(N) ->
    Error = <<"Not authorized to perform requested number operation">>,
    lager:debug("~s", [Error]),
    throw({unauthorized, N#number{error_jobj=wh_json:from_list([{<<"unauthorized">>, Error}])}}).

-spec error_no_change_required(ne_binary(), wnm_number()) -> no_return().
error_no_change_required(State, N) ->
    Error = <<"Number is already in state ", State/binary>>,
    lager:debug("~s", [Error]),
    throw({no_change_required, N#number{error_jobj=wh_json:from_list([{<<"no_change">>, Error}])}}).

-spec error_not_reconcilable(wnm_number()) -> no_return().
error_not_reconcilable(N) ->
    Error = <<"The number does not meet the minium requirements for reconciliation">>,
    lager:debug("~s", [Error]),
    throw({not_reconcilable, N#number{error_jobj=wh_json:from_list([{<<"not_routable">>, Error}])}}).

-spec error_number_database(atom(), wnm_number()) -> no_return().
error_number_database(Reason, N) ->
    Error = <<"The number database returned an error ", (wh_util:to_binary(Reason))/binary>>,
    lager:debug("~s", [Error]),
    throw({database_error, N#number{error_jobj=wh_json:from_list([{<<"number_database">>, Error}])}}).

-spec error_carrier_not_specified(wnm_number()) -> no_return().
error_carrier_not_specified(N) ->
    Error = <<"The number does not have a known/valid carrier associated with it">>,
    lager:debug("~s", [Error]),
    throw({unknown_carrier, N#number{error_jobj=wh_json:from_list([{<<"unknown_carrier">>, Error}])}}).

-spec error_number_not_found(wnm_number()) -> no_return().
error_number_not_found(N) ->
    Error = <<"The number could not be found">>,
    lager:debug("~s", [Error]),
    throw({not_found, N#number{error_jobj=wh_json:from_list([{<<"not_found">>, Error}])}}).

-spec error_number_exists(wnm_number()) -> no_return().
error_number_exists(N) ->
    Error = <<"The number already exists">>,
    lager:debug("~s", [Error]),
    throw({number_exists, N#number{error_jobj=wh_json:from_list([{<<"number_exists">>, Error}])}}).

-spec error_service_restriction(iolist() | ne_binary(), wnm_number()) -> no_return().
error_service_restriction(Reason, N) ->
    lager:debug("number billing restriction: ~s", [Reason]),
    throw({service_restriction, N#number{error_jobj=wh_json:from_list([{<<"credit">>, wh_util:to_binary(Reason)}])}}).

-spec error_provider_fault(wh_json:object(), wnm_number()) -> no_return().
error_provider_fault(Reason, N) ->
    lager:debug("feature provider(s) fault: ~p", [wh_json:encode(Reason)]),
    throw({provider_fault, N#number{error_jobj=wh_json:from_list([{<<"provider_fault">>, Reason}])}}).

-spec error_carrier_fault(wh_json:object() | ne_binary(), wnm_number()) -> no_return().
error_carrier_fault(Reason, N) ->
    lager:debug("carrier provider fault: ~p", [wh_json:encode(Reason)]),
    throw({carrier_fault, N#number{error_jobj=wh_json:from_list([{<<"carrier_fault">>, Reason}])}}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_updated_phone_number_docs(wnm_number()) -> wnm_number().
get_updated_phone_number_docs(#number{phone_number_docs=undefined}=Number) ->
    get_updated_phone_number_docs(Number#number{phone_number_docs=dict:new()});
get_updated_phone_number_docs(#number{state=State}=Number) ->
    Unavailable = lists:member(State, ?WNM_UNAVAILABLE_STATES),
    Routines = [fun(#number{prev_assigned_to=undefined}=N) -> N;
                   (#number{prev_assigned_to=PrevAssigned
                            ,assigned_to=PrevAssigned}=N) -> N;
                   (#number{prev_assigned_to=PrevAssigned}=N) ->
                        remove_from_phone_number_doc(PrevAssigned, N)
                end
                ,fun(#number{reserve_history=History}=N) ->
                         lists:foldl(fun update_phone_number_doc/2, N, ordsets:to_list(History))
                 end
                ,fun(#number{assigned_to=undefined}=N) -> N;
                    (#number{assigned_to=AssignedTo}=N) when Unavailable ->
                         update_phone_number_doc(AssignedTo, N);
                    (#number{assigned_to=AssignedTo}=N) ->
                        remove_from_phone_number_doc(AssignedTo, N)
                 end
               ],
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_from_phone_number_doc(ne_binary(), wnm_number()) -> wnm_number().
remove_from_phone_number_doc(Account, #number{number=Num, phone_number_docs=PhoneNumberDocs}=Number) ->
    case get_phone_number_doc(Account, Number) of
        {error, _} -> Number;
        {ok, JObj} ->
            case wh_json:delete_key(Num, JObj) of
                JObj ->
                    lager:debug("no need to remove ~s on phone_numbers for account ~s", [Num, Account]),
                    Number;
                UpdatedJObj ->
                    lager:debug("removed ~s on phone_numbers for account ~s", [Num, Account]),
                    Number#number{phone_number_docs=dict:store(Account, UpdatedJObj, PhoneNumberDocs)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_phone_number_doc(ne_binary(), wnm_number()) -> wnm_number().
update_phone_number_doc(Account, #number{number=Num, phone_number_docs=PhoneNumberDocs}=Number) ->
    case get_phone_number_doc(Account, Number) of
        {error, _} -> Number;
        {ok, JObj} ->
            Features = create_number_summary(Account, Number),
            case wh_json:set_value(Num, Features, JObj) of
                JObj ->
                    lager:debug("no need to update ~s on phone_numbers for account ~s", [Num, Account]),
                    Number;
                UpdatedJObj ->
                    lager:debug("updated ~s on phone_numbers for account ~s", [Num, Account]),
                    Number#number{phone_number_docs=dict:store(Account, UpdatedJObj, PhoneNumberDocs)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_phone_number_doc(ne_binary(), wnm_number()) -> {'ok', wh_json:object()} |
                                                               {'error', _}.
get_phone_number_doc(Account, #number{phone_number_docs=Docs}) ->
    case dict:find(Account, Docs) of
        error -> load_phone_number_doc(Account);
        {ok, _}=Ok -> Ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_number_summary(ne_binary(), wnm_number()) -> wh_json:object().
create_number_summary(_Account, #number{state=State, features=Features, module_name=ModuleName
                                ,assigned_to=AssignedTo, number_doc=Doc, used_by=UsedBy}) ->
    MaybeOwned = case (ModuleName == 'wnm_local') of
        'true' -> ['local'];
        'false' -> []
    end,
    NFeatures =  lists:merge([wh_util:to_binary(F) || F <- sets:to_list(Features)], MaybeOwned),
    wh_json:from_list([{<<"state">>, State}
                       ,{<<"features">>, NFeatures}
                       ,{<<"assigned_to">>, AssignedTo}
                       ,{<<"used_by">>, UsedBy}
                       ,{<<"created">>, wh_json:get_value(<<"pvt_created">>, Doc, 0)}
                       ,{<<"updated">>, wh_json:get_value(<<"pvt_modified">>, Doc, 0)}
                      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_phone_number_doc(ne_binary()) -> {'ok', wh_json:object()} |
                                                  {'error', _}.
load_phone_number_doc(Account) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    AccountId = wh_util:format_account_id(Account, raw),
    PVTs = [{<<"_id">>, ?WNM_PHONE_NUMBER_DOC}
            ,{<<"pvt_account_db">>, AccountDb}
            ,{<<"pvt_account_id">>, AccountId}
            ,{<<"pvt_vsn">>, <<"1">>}
            ,{<<"pvt_type">>, ?WNM_PHONE_NUMBER_DOC}
            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
           ],
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {ok, J} ->
            lager:debug("loaded phone_numbers from ~s", [AccountId]),
            {ok, wh_json:set_values(PVTs, J)};
        {error, not_found} ->
            lager:debug("creating phone_numbers in ~s", [AccountId]),
            {ok, wh_json:from_list([{<<"pvt_created">>, wh_util:current_tstamp()} | PVTs])};
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_service_plans(wnm_number()) -> wnm_number().
update_service_plans(#number{assigned_to=AssignedTo, prev_assigned_to=PrevAssignedTo}=N) ->
    _ = wh_services:reconcile(AssignedTo, <<"phone_numbers">>),
    _ = wh_services:reconcile(PrevAssignedTo, <<"phone_numbers">>),
    _ = commit_activations(N),
    N.

commit_activations(#number{billing_id=undefined}=Number) ->
    Number;
commit_activations(#number{activations=Activations}=N) ->
    _ = wh_transactions:save(Activations),
    N.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activate_feature(ne_binary(), wnm_number()) -> wnm_number().
-spec activate_feature(ne_binary(), integer(), wnm_number()) -> wnm_number().

activate_feature(Feature, #number{billing_id='undefined', assigned_to=Account}=N) ->
    activate_feature(Feature, N#number{billing_id=wh_services:get_billing_id(Account)});
activate_feature(Feature, #number{services='undefined', billing_id=Account}=N) ->
    activate_feature(Feature, N#number{services=wh_services:fetch(Account)});
activate_feature(Feature, #number{services=Services}=N) ->
    Units = wh_service_phone_numbers:feature_activation_charge(Feature, Services),
    activate_feature(Feature, Units, N).
activate_feature(Feature, 0, #number{features=Features}=N) ->
    lager:debug("no activation charge for ~s", [Feature]),
    N#number{features=sets:add_element(Feature, Features)};
activate_feature(Feature, Units, #number{current_balance='undefined', billing_id=Account}=N) ->
    activate_feature(Feature, Units, N#number{current_balance=wht_util:current_balance(Account)});
activate_feature(Feature, Units, #number{current_balance=Balance}=N) when Balance - Units < 0 ->
    Reason = io_lib:format("not enough credit to activate feature '~s' for $~p", [Feature, wht_util:units_to_dollars(Units)]),
    lager:debug("~s", [Reason]),
    error_service_restriction(Reason, N);
activate_feature(Feature, Units, #number{current_balance=Balance, features=Features}=N) ->
    N#number{activations=append_feature_debit(Feature, Units, N)
             ,features=sets:add_element(Feature, Features)
             ,current_balance=Balance - Units
            }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activate_phone_number(wnm_number()) -> wnm_number().

activate_phone_number(#number{billing_id='undefined', assigned_to=Account}=N) ->
    activate_phone_number(N#number{billing_id=wh_services:get_billing_id(Account)});
activate_phone_number(#number{services='undefined', billing_id=Account}=N) ->
    activate_phone_number(N#number{services=wh_services:fetch(Account)});
activate_phone_number(#number{services=Services, number=Number}=N) ->
    Units = wh_service_phone_numbers:phone_number_activation_charge(Number, Services),
    activate_phone_number(Units, N).

activate_phone_number(0, #number{number=Number}=N) ->
    lager:debug("no activation charge for ~s", [Number]),
    N;
activate_phone_number(Units, #number{current_balance='undefined', billing_id=Account}=N) ->
    activate_phone_number(Units, N#number{current_balance=wht_util:current_balance(Account)});
activate_phone_number(Units, #number{current_balance=Balance}=N) when Balance - Units < 0 ->
    Reason = io_lib:format("not enough credit to activate number for $~p", [wht_util:units_to_dollars(Units)]),
    lager:debug("~s", [Reason]),
    error_service_restriction(Reason, N);
activate_phone_number(Units, #number{current_balance=Balance}=N) ->
    N#number{activations=append_phone_number_debit(Units, N)
             ,current_balance=Balance - Units}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec append_feature_debit(wh_json:json_string(), integer(), wnm_number()) -> wh_json:objects().
append_feature_debit(Feature, Units, #number{billing_id=Ledger
                                             ,assigned_to=AccountId
                                             ,activations=Activations
                                             ,number=Number
                                            }) ->
    LedgerId =  wh_util:format_account_id(Ledger, 'raw'),
    Routines = [fun(T) ->
                        case LedgerId =/= AccountId of
                            'false' ->
                                wh_transaction:set_reason(<<"feature_activation">>, T);
                            'true' ->
                                T1 = wh_transaction:set_sub_account_id(AccountId, T),
                                wh_transaction:set_reason(<<"sub_account_feature_activation">>, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_feature(Feature, T) end
                ,fun(T) -> wh_transaction:set_number(Number, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"number feature activation for "
                                                          ,(wh_util:to_binary(Feature))/binary>>, T)
                 end
               ],
    lager:debug("staging feature '~s' activation charge $~p for ~s via billing account ~s"
                ,[Feature, wht_util:units_to_dollars(Units), AccountId, LedgerId]),
    [lists:foldl(fun(F, T) -> F(T) end, wh_transaction:debit(Ledger, Units), Routines)|Activations].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec append_phone_number_debit(integer(), wnm_number()) -> wh_json:objects().
append_phone_number_debit(Units, #number{billing_id=Ledger, assigned_to=AccountId
                                         ,activations=Activations, number=Number}) ->
    LedgerId =  wh_util:format_account_id(Ledger, 'raw'),
    Routines = [fun(T) ->
                        case LedgerId =/= AccountId of
                            'false' ->
                                wh_transaction:set_reason(<<"number_activation">>, T);
                            'true' ->
                                T1 = wh_transaction:set_sub_account_id(AccountId, T),
                                wh_transaction:set_reason(<<"sub_account_number_activation">>, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_number(Number, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"number activation for "
                                                          ,(wh_util:to_binary(Number))/binary>>, T)
                 end
               ],
    lager:debug("staging number activation charge $~p for ~s via billing account ~s"
                ,[wht_util:units_to_dollars(Units), AccountId, LedgerId]),
    [lists:foldl(fun(F, T) -> F(T) end, wh_transaction:debit(Ledger, Units), Routines)|Activations].
