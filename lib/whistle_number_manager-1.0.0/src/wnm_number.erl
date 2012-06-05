%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% Handle state transitions enforcing business logic
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_number).

-export([get/1, get/2]).
-export([create_available/2]).
-export([create_discovery/3]).
-export([create_port_in/3]).
-export([save/1, save/2]).
-export([delete/1, delete/2]).
-export([remove_account_phone_numbers/2]).
-export([update_account_phone_numbers/2]).

-export([discovery/1]).
-export([port_in/1]).
-export([available/1]).
-export([reserved/1]).
-export([in_service/1]).
-export([released/1]).
-export([port_out/1]).
-export([disconnected/1]).

-include("wh_number_manager.hrl").

-export_type([wnm_number/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets a number record or throws
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (ne_binary()) -> wnm_number().
-spec get/2 :: (ne_binary(), 'undefined' | wh_json:json_object()) -> wnm_number().

get(Number) ->
    get(Number, undefined).

get(Number, PublicFields) ->
    Routines = [fun(#number{number=Num}=N) -> 
                        case wnm_util:is_reconcilable(Num) of
                            false -> error_not_reconcilable(N);
                            true -> N
                        end
                end
                ,fun(#number{number=Num, number_db=Db}=N) ->
                         case couch_mgr:open_doc(Db, Num) of
                             {ok, JObj} -> merge_public_fields(PublicFields, json_to_record(JObj, N));
                             {error, Reason} -> error_number_database(Reason, N)
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, #number{number=wnm_util:normalize_number(Number)
                                               ,number_db=wnm_util:number_to_db_name(Number)}, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new number record with the intial state of discovery
%% @end
%%--------------------------------------------------------------------
-spec create_discovery/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> wnm_number().
create_discovery(Number, ModuleName, ModuleData) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Number),
    Routines = [fun(J) -> wh_json:set_value(<<"_id">>, Num, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_name">>, ModuleName, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, ModuleData, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_number_state">>, <<"discovery">>, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_db_name">>, Db, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end
               ],
    JObj = lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines),
    json_to_record(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new wnm_local record with the intial state of available
%% @end
%%--------------------------------------------------------------------
-spec create_available/2 :: (ne_binary(), 'undefined' | ne_binary()) -> wnm_number().
create_available(_, undefined) ->
    Error = <<"Can not create an available number without an authorizing id">>,
    throw(#number{error=unauthorized
                  ,error_jobj=wh_json:from_list([{<<"unauthorized">>, Error}])
                 });
create_available(Number, AuthBy) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Number),
    Routines = [fun(J) -> wh_json:set_value(<<"_id">>, Num, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_name">>, <<"wnm_local">>, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, wh_json:new(), J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_number_state">>, <<"available">>, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_db_name">>, Db, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_authorizing_account">>, AuthBy, J) end
               ],
    JObj = lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines),
    json_to_record(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new wnm_local record with the intial state of port_in
%% @end
%%--------------------------------------------------------------------
-spec create_port_in/3 :: (ne_binary(), ne_binary(), ne_binary()) -> wnm_number().
create_port_in(Number, AssignTo, AuthBy) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Number),
    Routines = [fun(J) -> wh_json:set_value(<<"_id">>, Num, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_name">>, <<"wnm_local">>, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, wh_json:new(), J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_number_state">>, <<"port_in">>, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_ported_in">>, true, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_db_name">>, Db, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_authorizing_account">>, AuthBy, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J) end
               ],
    JObj = lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines),
    json_to_record(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec discovery/1 :: (wnm_number()) -> wnm_number().
discovery(#number{state = <<"discovery">>}=Number) ->
    error_no_change_required(Number);
discovery(Number) ->
    error_invalid_state_transition(<<"discovery">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec port_in/1 :: (wnm_number()) -> wnm_number().
port_in(#number{state = <<"port_in">>, assigned_to=AssignTo, assign_to=AssignTo}=Number) ->
    error_no_change_required(Number);
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
-spec available/1 :: (wnm_number()) -> wnm_number().
available(#number{state = <<"released">>}=Number) ->
    Routines = [fun(#number{auth_by=AuthBy}=N) ->
                        %% Only the 'system' maintenance routines or a system admin can
                        %% move a released (aging) number back to available.
                        case AuthBy =:= system 
                            orelse wh_util:is_system_admin(AuthBy) 
                        of
                            true -> N#number{state = <<"available">>};
                            false -> error_unauthorized(N)
                        end
                end
               ],
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines);    
available(#number{state = <<"available">>}=Number) ->
    error_no_change_required(Number);
available(Number) ->
    error_invalid_state_transition(<<"available">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reserved/1 :: (wnm_number()) -> wnm_number().
reserved(#number{state = <<"discovery">>}=Number) -> %%, JObj, AssignTo, AuthBy) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{module_name=undefined}=N) ->
                         error_carrier_not_specified(N);
                    (#number{module_name=Module}=N) ->
                         Module:acquire_number(N)
                 end
                ,fun(#number{reserve_history=ReserveHistory, assign_to=AssignTo}=N) ->
                         N#number{reserve_history=ordsets:add_element(AssignTo, ReserveHistory)}
                 end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) -> 
                         N#number{state = <<"reserved">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) -> 
                         N#number{state = <<"reserved">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) -> 
                         %% _ = update_account_phone_numbers(wh_json:set_value(<<"pvt_number_state">>, <<"released">>, J), AssignedTo),
                         N#number{state = <<"reserved">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end

               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
reserved(#number{state = <<"available">>}=Number) -> %%, JObj, AssignTo, AuthBy) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{reserve_history=ReserveHistory, assign_to=AssignTo}=N) ->
                         N#number{reserve_history=ordsets:add_element(AssignTo, ReserveHistory)}
                 end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) -> 
                         N#number{state = <<"reserved">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) -> 
                         N#number{state = <<"reserved">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) -> 
                         %% _ = update_account_phone_numbers(wh_json:set_value(<<"pvt_number_state">>, <<"released">>, J), AssignedTo),
                         N#number{state = <<"reserved">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
reserved(#number{state = <<"reserved">>, assigned_to=AssignedTo, assign_to=AssignedTo}=Number) ->
    error_no_change_required(Number);
reserved(#number{state = <<"reserved">>}=Number) ->
    Routines = [fun(#number{auth_by=AuthBy, assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                        case (not wh_util:is_empty(AssignTo))
                            andalso (wh_util:is_in_account_hierarchy(AssignedTo, AuthBy)
                                     orelse wh_util:is_in_account_hierarchy(AuthBy, AssignedTo))
                        of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end           
                ,fun(#number{reserve_history=ReserveHistory, assign_to=AssignTo}=N) ->
                         N#number{reserve_history=ordsets:add_element(AssignTo, ReserveHistory)}
                 end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) -> 
                         N#number{state = <<"reserved">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) -> 
                         N#number{state = <<"reserved">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) -> 
                         %% _ = update_account_phone_numbers(wh_json:set_value(<<"pvt_number_state">>, <<"released">>, J), AssignedTo),
                         N#number{state = <<"reserved">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
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
-spec in_service/1 :: (wnm_number()) -> wnm_number().
in_service(#number{state = <<"discovery">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{module_name=undefined}=N) ->
                         error_carrier_not_specified(N);
                    (#number{module_name=Module}=N) ->
                         Module:acquire_number(N)
                 end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) -> 
                         N#number{state = <<"in_service">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) -> 
                         N#number{state = <<"in_service">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"port_in">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{state = <<"in_service">>}
                        end
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"available">>}=Number) ->
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N
                        end
                end
                ,fun(#number{assigned_to=undefined, assign_to=AssignTo}=N) -> 
                         N#number{state = <<"in_service">>, assigned_to=AssignTo};
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) -> 
                         N#number{state = <<"in_service">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
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
                    (#number{assigned_to=AssignedTo, prev_assigned_to=AssignedTo}=N) -> 
                         N#number{state = <<"in_service">>};
                    (#number{assigned_to=AssignedTo, assign_to=AssignTo}=N) ->
                         N#number{state = <<"in_service">>, assigned_to=AssignTo, prev_assigned_to=AssignedTo}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
in_service(#number{state = <<"in_service">>, assigned_to=AssignedTo, assign_to=AssignedTo}=Number) ->
    error_no_change_required(Number);
in_service(#number{state = <<"in_service">>}=Number) ->
    error_unauthorized(Number);
in_service(Number) ->
    error_invalid_state_transition(<<"in_service">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec released/1 :: (wnm_number()) -> wnm_number().
released(#number{state = <<"reserved">>}=Number) ->
    NewState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy, number_doc=JObj}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{features=sets:new()
                                             ,number_doc=wh_json:private_fields(JObj)
                                            }
                        end
                end
                ,fun(#number{assigned_to=AssignedTo}=N) ->
                         %% _ = update_account_phone_numbers(wh_json:set_value(<<"pvt_number_state">>, <<"released">>, J), AssignedTo),
                         N#number{state=NewState, assigned_to=undefined, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{module_name=ModuleName, prev_assigned_to=AssignedTo, reserve_history=ReserveHistory}=N) ->
                    History = ordsets:del_element(AssignedTo, ReserveHistory),
                    case ordsets:to_list(History) of
                        [] when ModuleName =:= <<"wnm_local">> -> 
                            lager:debug("flagging released local number for hard delete", []),
                            N#number{state = <<"released">>, reserve_history=[], hard_delete=true};
                        [] -> 
                            N#number{state = <<"released">>, reserve_history=[]};
                        [PrevReservation|_] ->
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
    Routines = [fun(#number{assign_to=AssignTo, auth_by=AuthBy, number_doc=JObj}=N) ->
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignTo, true) of
                            false -> error_unauthorized(N);
                            true -> N#number{features=sets:new()
                                             ,number_doc=wh_json:private_fields(JObj)
                                            }
                        end
                end
                ,fun(#number{assigned_to=AssignedTo}=N) ->
                         %% _ = update_account_phone_numbers(wh_json:set_value(<<"pvt_number_state">>, <<"released">>, J), AssignedTo),
                         N#number{state=NewState, assigned_to=undefined, prev_assigned_to=AssignedTo}
                 end
                ,fun(#number{module_name=ModuleName, prev_assigned_to=AssignedTo, reserve_history=ReserveHistory}=N) ->
                    History = ordsets:del_element(AssignedTo, ReserveHistory),
                    case ordsets:to_list(History) of
                        [] when ModuleName =:= <<"wnm_local">> -> 
                            lager:debug("flagging released local number for hard delete", []),
                            N#number{state = <<"released">>, reserve_history=[], hard_delete=true};
                        [] -> 
                            N#number{state = <<"released">>, reserve_history=[]};
                        [PrevReservation|_] ->
                            N#number{reserve_history=History
                                     ,assigned_to=PrevReservation
                                     ,state = <<"reserved">>
                                    }
                    end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, Number, Routines);
released(#number{state = <<"released">>}=Number) ->
    error_no_change_required(Number);
released(Number) ->
    error_invalid_state_transition(<<"released">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec port_out/1 :: (wnm_number()) -> wnm_number().
port_out(Number) ->
    error_invalid_state_transition(<<"port_out">>, Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disconnected/1 :: (wnm_number()) -> wnm_number().
disconnected(Number) ->
    error_invalid_state_transition(<<"disconnected">>, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetch and update the accounts phone_numbers document with the new
%% number
%% @end
%%--------------------------------------------------------------------
-spec get_phone_numbers_doc/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                       {error, _}.
get_phone_numbers_doc(_, undefined) ->
    {error, no_account_id};
get_phone_numbers_doc(JObj, Account) ->
    State = wh_json:get_value(<<"pvt_number_state">>, JObj),
    Available = lists:member(State, ?WNM_UNAVAILABLE_STATES),
    get_phone_numbers_doc(JObj, Account, State, Available).

get_phone_numbers_doc(JObj, Account, State, true) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    Features = wh_json:get_value(<<"pvt_features">>, JObj, []),
    Number = wh_json:get_value(<<"_id">>, JObj),
    Summary = wh_json:from_list([{<<"state">>, wh_json:get_value(<<"pvt_number_state">>, JObj)}
                                 ,{<<"e911">>, lists:member(<<"dash_e911">>, Features)}
                                 ,{<<"cnam">>, lists:member(<<"cnam">>, Features)}
                                 ,{<<"failover">>, lists:member(<<"failover">>, Features)}
                                 ,{<<"local_number">>, wh_json:get_value(<<"pvt_module_name">>, JObj) =:= <<"wnm_local">>}
                                 ,{<<"force_outbound">>, wh_json:is_true(<<"force_outbound">>, JObj, false)
                                   orelse State =:= <<"port_in">>
                                       orelse State =:= <<"port_out">>
                                  }
                                ]),
    Updates = [{<<"_id">>, <<"phone_numbers">>}
               ,{Number, Summary}
               ,{<<"pvt_type">>, <<"phone_numbers">>}
               ,{<<"pvt_modified">>, wh_util:current_tstamp()}
               ,{<<"pvt_vsn">>, <<"1">>}
               ,{<<"pvt_account_db">>, AccountDb}
               ,{<<"pvt_account_id">>, AccountId}
              ],
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {ok, PhoneNumbers} ->
            {ok, wh_json:set_values(Updates, PhoneNumbers)};
        {error, not_found} ->
            {ok, wh_json:from_list(Updates)};
        {error, _R}=E ->
            lager:debug("failed to open ~s/~s: ~p", [AccountDb, ?WNM_PHONE_NUMBER_DOC, _R]),
            E
    end;
get_phone_numbers_doc(JObj, Account, _, false) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    Number = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {ok, PhoneNumbers} ->
            {ok, wh_json:delete_key(Number, PhoneNumbers)};
        {error, not_found} ->
            {ok, wh_json:from_list([{<<"_id">>, <<"phone_numbers">>}
                                    ,{<<"pvt_type">>, <<"phone_numbers">>}
                                    ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                    ,{<<"pvt_vsn">>, <<"1">>}
                                    ,{<<"pvt_account_db">>, AccountDb}
                                    ,{<<"pvt_account_id">>, AccountId}
                                   ])};
        {error, _R}=E ->
            lager:debug("failed to open ~s/~s: ~p", [AccountDb, ?WNM_PHONE_NUMBER_DOC, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save the accounts phone_numbers document, merging changes if it
%% conflicts
%% @end
%%-------------------------------------------------------------------- 
-spec save_phone_numbers_doc/2 :: (wh_json:json_object(), wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                                                    {'error', _}.
save_phone_numbers_doc(JObj, PhoneNumbersDoc) ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, PhoneNumbersDoc),
    case wh_service_numbers:update(PhoneNumbersDoc) of
        {error, _}=E -> E;
        ok ->
            case couch_mgr:save_doc(AccountDb, PhoneNumbersDoc) of
                {ok, _}=Ok -> 
                    lager:debug("saved updated phone_numbers doc in account db ~s", [AccountDb]),
                    Ok;
                {error, conflict} ->
                    save_phone_numbers_doc(JObj, get_phone_numbers_doc(JObj, AccountDb));
                {error, _R}=E ->
                    lager:info("failed to update phone numbers doc in account ~s: ~p", [AccountDb, _R]),
                    E
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the phone number document creating the database if it doesnt
%% already exist
%% @end
%%--------------------------------------------------------------------
-spec save_phone_number/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                        {'error', _}.
save_phone_number(JObj) ->
    Num = wh_json:get_value(<<"_id">>, JObj),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to save '~s' in '~s'", [Num, Db]),
    case couch_mgr:save_doc(Db, JObj) of
        {ok, _}=Ok -> Ok;
        {error, not_found} ->
            lager:debug("storing number '~s' in a new database '~s'", [Num, Db]),
            couch_mgr:db_create(Db),
            couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
            save_phone_number(JObj);
        {error, _R}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Helper function to save the number, add to the account doc if required,
%% and run any providers
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wh_json:json_object()) -> {ok, wh_json:json_object()} |
                                           {error, term()}.
-spec save/2 :: (wh_json:json_object(), {'error', _} | {'ok', wh_json:json_object()} | wh_json:json_object()) -> {ok, wh_json:json_object()} |
                                                                                                                 {error, term()}.
save(JObj) ->
    Num = wh_json:get_value(<<"_id">>, JObj),
    Db = wnm_util:number_to_db_name(Num),
    save(JObj, couch_mgr:open_doc(Db, Num)).

save(JObj, {error, _}) ->
    save(JObj, wh_json:new());
save(JObj, {ok, PriorJObj}) ->
    save(JObj, PriorJObj);
save(JObj, PriorJObj) ->
    Num = wh_json:get_value(<<"_id">>, JObj),
    Routines = [fun(J) -> {ok, wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J)} end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         State = wh_json:get_value(<<"pvt_number_state">>, J),
                         case wnm_util:exec_providers_save(J, PriorJObj, Num, State) of
                             {ok, _}=Ok -> Ok;
                             {error, _R} ->
                                 lager:debug("providers failed: ~p", [_R]),
                                 {ok, J}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AccountId = wnm_util:find_account_id(J),
                         case update_account_phone_numbers(J, AccountId) of
                             {error, _}=E -> E;
                             {ok, _} -> save_phone_number(J)
                         end
                 end
                ,fun({error, _R}=E) ->
                         lager:debug("failed to save number: ~p", [_R]),
                         E;
                    ({ok, _}=Ok) ->
                         lager:debug("updated phone number ~s", [Num]),
                         Ok
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                             {'error', _}.
-spec delete/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                        {'error', _}.
delete(JObj) ->
    delete(JObj, wh_json:get_value(<<"pvt_assigned_to">>, JObj)).

delete(JObj, AccountId) ->
    Num = wh_json:get_value(<<"_id">>, JObj),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("executing hard delete of number ~s", [Num]),
    case couch_mgr:del_doc(Db, JObj) of
        {ok, _}=Ok ->
            _ = remove_account_phone_numbers(Num, AccountId),
            Ok;
        {error, _R}=E ->
            lager:debug("failed to delete number ~s: ~p", [Num, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_account_phone_numbers/2 :: (ne_binary(), 'undefined' | ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                                    {error, _}.
remove_account_phone_numbers(_, undefined) ->
    {error, no_account_id};
remove_account_phone_numbers(Number, Account) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {error, _R} ->
            lager:debug("failed to open account ~s document for removal of ~s: ~p", [?WNM_PHONE_NUMBER_DOC, Number, _R]),
            ok;
        {ok, PhoneNumbers} ->
            UpdatedPhoneNumbers = wh_json:delete_key(Number, PhoneNumbers),
            _ = wh_service_numbers:update(UpdatedPhoneNumbers),
            couch_mgr:save_doc(AccountDb, UpdatedPhoneNumbers)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates the account phone_numbers document to be saved later.
%% @end
%%--------------------------------------------------------------------
-spec update_account_phone_numbers/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                              {error, _}.
update_account_phone_numbers(_, undefined) ->
    {error, no_account_id};
update_account_phone_numbers(JObj, Account) ->
    case get_phone_numbers_doc(JObj, Account) of
        {error, _}=E -> E;
        {ok, PhoneNumbersDoc} -> 
            save_phone_numbers_doc(JObj, PhoneNumbersDoc)
    end.


































































%%--------------------------------------------------------------------
%% Start of new routines
%%--------------------------------------------------------------------




%%--------------------------------------------------------------------
%% @private
%% @doc
%% convert a json object to the number record
%% @end
%%--------------------------------------------------------------------
json_to_record(JObj) ->
    json_to_record(JObj, #number{}).

json_to_record(JObj, #number{number=Num, number_db=Db}=Number) ->
    Number#number{number=wh_json:get_value(<<"_id">>, JObj, Num)
                  ,number_db=wh_json:get_vaue(<<"pvt_db_name">>, JObj, Db)
                  ,state=wh_json:get_ne_value(<<"pvt_number_state">>, JObj)
                  ,reserve_history=ordsets:from_list(wh_json:get_ne_value(<<"pvt_reserve_history">>, JObj, []))
                  ,assigned_to=wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj)
                  ,prev_assigned_to=wh_json:get_ne_value(<<"pvt_previously_assigned_to">>, JObj)
                  ,module_name=wnm_util:get_carrier_module(JObj)
                  ,module_data=wh_json:get_ne_value(<<"pvt_module_data">>, JObj) 
                  ,features=sets:from_list(wh_json:get_ne_value(<<"pvt_features">>, JObj, [])) 
                  ,number_doc=JObj
                  ,current_number_doc=JObj
                 }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% conditionally merge the public fields with a number record
%% @end
%%--------------------------------------------------------------------
merge_public_fields(PublicFields, #number{number=JObj}=N) ->
    case wh_json:is_json_object(PublicFields) of
        false -> N;
        true ->
            N#number{number=wh_json:merge_jobjs(wh_json:private_fields(JObj), PublicFields)}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% create invalid state error
%% @end
%%--------------------------------------------------------------------
error_invalid_state_transition(Transition, #number{state = State}=N) ->
    Error = list_to_binary(["Invalid state transition from ", Transition, " to ", State]),
    throw(N#number{error=invalid_state_transition
                   ,error_jobj=wh_json:from_list([{<<"state_transition">>, Error}])
                  }).
    
error_unauthorized(N) ->
    Error = <<"Not authorized to manage number">>,
    throw(N#number{error=unauthorized
                   ,error_jobj=wh_json:from_list([{<<"unauthorized">>, Error}])
                  }).

error_no_change_required(N) ->
    Error = <<"Number is already in state port_in">>,
    throw(N#number{error=no_change_required
                   ,error_jobj=wh_json:from_list([{<<"no_change">>, Error}])
                  }).

error_not_reconcilable(N) ->
    Error = <<"The number does not met the minium requirements for reconciliation">>,
    throw(N#number{error=not_reconcilable
                   ,error_jobj=wh_json:from_list([{<<"not_routable">>, Error}])
                  }).

error_number_database(Reason, N) ->
    Error = <<"The number database returned an error ", (wh_util:to_binary(Reason))/binary>>,
    throw(N#number{error=database_error
                   ,error_jobj=wh_json:from_list([{<<"number_database">>, Error}])
                  }).

error_carrier_not_specified(N) ->
    Error = <<"The number does not have a known/valid carrier associated with it">>,
    throw(N#number{error=unknown_carrier
                   ,error_jobj=wh_json:from_list([{<<"unknown_carrier">>, Error}])
                  }).    

error_carrier_fault(N) ->
    Error = <<"Carrier failed to preform a required operation">>,
    throw(N#number{error=carrier_fault
                   ,error_jobj=wh_json:from_list([{<<"carrier_fault">>, Error}])
                  }).
