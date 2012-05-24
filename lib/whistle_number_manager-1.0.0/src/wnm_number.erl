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
-export([delete/2]).
-export([remove_account_phone_numbers/2]).
-export([update_account_phone_numbers/2]).
-export([discovery/3, discovery/4]).
-export([port_in/3, port_in/4]).
-export([available/3, available/4]).
-export([reserved/3, reserved/4]).
-export([in_service/3, in_service/4]).
-export([released/2, released/3]).
-export([port_out/2, port_out/3]).
-export([disconnected/2, disconnected/3]).

-include("wh_number_manager.hrl").


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets a number document
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} |
                                {'error', term()}.
-spec get/2 :: (ne_binary(), 'undefined' | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                                     {'error', term()}.

get(Number) ->
    get(Number, undefined).

get(Number, PublicFields) ->
    Num = wnm_util:normalize_number(Number),
    Routines = [fun(J) ->    
                        case wnm_util:is_reconcilable(Num) of
                            false -> {error, not_reconcilable};
                            true -> {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, _}) -> 
                         Db = wnm_util:number_to_db_name(Num),
                         couch_mgr:open_doc(Db, Num) 
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         case wh_json:is_json_object(PublicFields) of
                             false -> {ok, J};
                             true ->
                                 {ok, wh_json:merge_jobjs(wh_json:private_fields(J), PublicFields)}
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new number with the intial state of discovery
%% @end
%%--------------------------------------------------------------------
-spec create_discovery/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> wh_json:json_object().
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
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new wnm_local with the intial state of available
%% @end
%%--------------------------------------------------------------------
-spec create_available/2 :: (ne_binary(), 'undefined' | ne_binary()) -> {'ok', wh_json:json_object()} | {'error', 'unauthorized'}.
create_available(_, undefined) ->
    {error, unauthorized};
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
    {ok, lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new wnm_local with the intial state of available
%% @end
%%--------------------------------------------------------------------
-spec create_port_in/3 :: (ne_binary(), ne_binary(), ne_binary()) -> wh_json:json_object().
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
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

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
    Db = wnm_util:number_to_db_name(Num), 
    Routines = [fun(J) -> {ok, wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J)} end
                ,fun({ok, J}) ->
                         lager:debug("attempting to save '~s' in '~s'", [Num, Db]),
                         case couch_mgr:save_doc(Db, J) of
                             {error, not_found} ->
                                 lager:debug("storing number '~s' in a new database '~s'", [Num, Db]),
                                 couch_mgr:db_create(Db),
                                 couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
                                 couch_mgr:save_doc(Db, J);
                             Else -> Else
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         State = wh_json:get_value(<<"pvt_number_state">>, J),
                         case wnm_util:exec_providers_save(J, PriorJObj, Num, State) of
                             {ok, J} -> 
                                 lager:debug("service providers did not modify number document skipping second save operation"),
                                 {ok, J};
                             {ok, JObj2} ->
                                 couch_mgr:save_doc(Db, JObj2);
                             {error, _R} -> 
                                 lager:debug("providers failed: ~p", [_R]),
                                 {ok, J}
                         end                                                   
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("failed to save number: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         AccountId = wnm_util:find_account_id(JObj),
                         update_account_phone_numbers(J, AccountId)
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (wh_json:json_object(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                          {'error', _}.
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
-spec remove_account_phone_numbers/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                      {'error', _}.
remove_account_phone_numbers(Number, Account) ->
    AccountDb = wh_util:format_account_id(Account, encoded),    
    case couch_mgr:open_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {error, _R} -> 
            lager:debug("failed to open account ~s document for removal of ~s: ~p", [?WNM_PHONE_NUMBER_DOC, Number, _R]),
            ok;
        {ok, PhoneNumbers} ->
            couch_mgr:save_doc(AccountDb, wh_json:delete_key(Number, PhoneNumbers))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Adds a number to the list kept on the account defintion doc, then
%% aggregates the new document to the accounts db.
%% @end
%%--------------------------------------------------------------------
-spec update_account_phone_numbers/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                           {error, _}.
update_account_phone_numbers(_, undefined) ->
    {error, no_account_id};
update_account_phone_numbers(JObj, Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    Features = wh_json:get_value(<<"pvt_features">>, JObj, []),
    Number = wh_json:get_value(<<"_id">>, JObj),
    State = wh_json:get_value(<<"pvt_number_state">>, JObj),
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
    OnCreate = [{<<"pvt_type">>, <<"phone_numbers">>}
                ,{<<"pvt_created">>, wh_util:current_tstamp()}
                ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                ,{<<"pvt_vsn">>, <<"1">>}
                ,{<<"pvt_account_db">>, AccountDb}
                ,{<<"pvt_account_id">>, AccountId}
               ],
    case couch_mgr:update_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC, [{Number, Summary}], OnCreate) of
        {ok, _} ->
            lager:debug("updated the account ~s document", [?WNM_PHONE_NUMBER_DOC]);
        {error, _R}=E ->
            lager:debug("failed to save account ~s document for ~s: ~p", [?WNM_PHONE_NUMBER_DOC, Number, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec discovery/3 :: (wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> {'error', 'invalid_state_transition'}.
-spec discovery/4 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> {'error', 'invalid_state_transition'}.

discovery(JObj, AssignTo, AuthBy) ->
    discovery(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AssignTo, AuthBy).

discovery(<<"discovery">>, _, _, _) ->
    {error, invalid_state_transition};
discovery(<<"port_in">>, _, _, _) ->
    {error, invalid_state_transition};
discovery(<<"available">>, _, _, _) ->
    {error, invalid_state_transition};    
discovery(<<"reserved">>, _, _, _) ->
    {error, invalid_state_transition};
discovery(<<"in_service">>, _, _, _) ->
    {error, invalid_state_transition};
discovery(<<"released">>, _, _, _) ->
    {error, invalid_state_transition};
discovery(<<"port_out">>, _, _, _) ->
    {error, invalid_state_transition};
discovery(<<"disconnected">>, _, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec port_in/3 :: (wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> {'error','invalid_state_transition' | 'no_change_required' | 'unauthorized'}.
-spec port_in/4 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> {'error','invalid_state_transition' | 'no_change_required' | 'unauthorized'}.

port_in(JObj, AssignTo, AuthBy) ->
    port_in(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AssignTo, AuthBy).

port_in(<<"discovery">>, _, _, _) ->
    {error, invalid_state_transition};    
port_in(<<"port_in">>, JObj, AssignTo, _) ->
    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
        AssignTo -> {error, no_change_required};
        _Else -> {error, unauthorized}
    end;
port_in(<<"available">>, _, _, _) ->
    {error, invalid_state_transition};
port_in(<<"reserved">>, _, _, _) ->
    {error, invalid_state_transition};
port_in(<<"in_service">>, _, _, _) ->
    {error, invalid_state_transition};
port_in(<<"released">>, _, _, _) ->
    {error, invalid_state_transition};
port_in(<<"port_out">>, _, _, _) ->
    {error, invalid_state_transition};
port_in(<<"disconnected">>, _, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec available/3 :: (wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> transition_return().
-spec available/4 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> transition_return().

available(JObj, AssignTo, AuthBy) ->
    available(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AssignTo, AuthBy).

available(<<"discovery">>, _, _, _) ->
    {error, invalid_state_transition};    
available(<<"port_in">>, _, _, _) ->
    {error, invalid_state_transition};
available(<<"available">>, _, _, _) ->
    {error, no_change_required};
available(<<"reserved">>, _, _, _) ->
    {error, invalid_state_transition};
available(<<"in_service">>, _, _, _) ->
    {error, invalid_state_transition};
available(<<"released">>, JObj, _, AuthBy) ->
    Routines = [fun(J) ->
                        %% Only the 'system' maintenance routines or a system admin can
                        %% move a released (aging) number back to available.
                        case AuthBy =:= system orelse wh_util:is_system_admin(AuthBy) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"available">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
available(<<"port_out">>, _, _, _) ->
    {error, invalid_state_transition};
available(<<"disconnected">>, _, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec reserved/3 :: (wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> transition_return().
-spec reserved/4 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> transition_return().

reserved(JObj, AssignTo, AuthBy) ->
    reserved(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AssignTo, AuthBy).

reserved(<<"discovery">>, JObj, AssignTo, AuthBy) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AssignTo) orelse wh_util:is_empty(AuthBy) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         History = wh_json:get_value(<<"pvt_wnm_prev_assignments">>, J, []),
                         {ok, wh_json:set_value(<<"pvt_wnm_prev_assignments">>, [AssignTo|History], J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         Number = wh_json:get_value(<<"_id">>, J),
                         case wnm_util:get_carrier_module(J) of
                             {ok, Module, Data} ->
                                 D = wh_json:set_value(<<"authorizing_account">>, AuthBy, Data),
                                 case Module:acquire_number(Number, D) of
                                     {error, _R} ->
                                         lager:debug("carrier module '~s' failed to acquire number: ~p", [Module, _R]),
                                         {error, carrier_fault};
                                     {ok, NewData} -> {ok, wh_json:set_value(<<"pvt_module_data">>, NewData, J)}
                                 end;
                             {error, not_specified} ->
                                 lager:debug("carrier module not specified on number document"),
                                 {error, unknown_carrier};
                             {error, unknown_module} ->
                                 lager:debug("carrier module specified on number document does not exist"),
                                 {error, unknown_carrier}
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
reserved(<<"port_in">>, _, _, _) ->
    {error, invalid_state_transition};
reserved(<<"available">>, JObj, AssignTo, _) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AssignTo) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         History = wh_json:get_value(<<"pvt_wnm_prev_assignments">>, J, []),
                         {ok, wh_json:set_value(<<"pvt_wnm_prev_assignments">>, [AssignTo|History], J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
reserved(<<"reserved">>, _, _, undefined) ->
    {error, unauthorized};
reserved(<<"reserved">>, JObj, AssignTo, AuthBy) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AssignTo) orelse wh_util:is_empty(AuthBy) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun(J) -> 
                        case wh_json:get_value(<<"pvt_assigned_to">>, J) of
                            AssignTo -> {error, no_change_required};
                            _Else -> {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_util:is_in_account_hierarchy(AssignedTo, AuthBy) 
                             orelse wh_util:is_in_account_hierarchy(AuthBy, AssignedTo) 
                         of
                             false -> {error, unauthorized};
                             true ->  {ok, J}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         History = wh_json:get_value(<<"pvt_wnm_prev_assignments">>, J, []),
                         {ok, wh_json:set_value(<<"pvt_wnm_prev_assignments">>, [AssignTo|History], J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end    
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
reserved(<<"in_service">>, JObj, _, AuthBy) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AuthBy) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun(J) ->
                        %% Only the account that the number belongs to or an ancestor of that
                        %% account is authorized to move a in_service number back to reserved
                        %% for the current account.
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true ->  {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
reserved(<<"released">>, _, _, _) ->
    {error, invalid_state_transition};
reserved(<<"port_out">>, _, _, _) ->
    {error, invalid_state_transition};
reserved(<<"disconnected">>, _, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec in_service/3 :: (wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> transition_return().
-spec in_service/4 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> transition_return().

in_service(JObj, AssignTo, AuthBy) ->
    in_service(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AssignTo, AuthBy).

in_service(<<"discovery">>, JObj, AssignTo, AuthBy) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AssignTo) orelse wh_util:is_empty(AuthBy) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"in_service">>, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         Number = wh_json:get_value(<<"_id">>, J),
                         case wnm_util:get_carrier_module(J) of
                             {ok, Module, Data} ->
                                 D = wh_json:set_value(<<"authorizing_account">>, AuthBy, Data),
                                 case Module:acquire_number(Number, D) of
                                     {error, _R} ->
                                         lager:debug("carrier module '~s' failed to acquire number: ~p", [Module, _R]),
                                         {error, carrier_fault};
                                     {ok, NewData} -> {ok, wh_json:set_value(<<"pvt_module_data">>, NewData, J)}
                                 end;
                             {error, not_specified} ->
                                 lager:debug("carrier module not specified on number document"),
                                 {error, unknown_carrier};
                             {error, unknown_module} ->
                                 lager:debug("carrier module specified on number document does not exist"),
                                 {error, unknown_carrier}
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
in_service(<<"port_in">>, JObj, _, _) ->
    Routines = [fun(J) -> {ok, J} end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"in_service">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
in_service(<<"available">>, JObj, AssignTo, _) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AssignTo) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"in_service">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
in_service(<<"reserved">>, JObj, AssignTo, AuthBy) ->
    Routines = [fun(J) -> 
                        case wh_util:is_empty(AssignTo) orelse wh_util:is_empty(AuthBy) of 
                            true -> {error, unauthorized};
                            false -> {ok, J} 
                        end
                end
                ,fun(J) ->
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case (wh_util:is_in_account_hierarchy(AssignedTo, AuthBy, true) 
                              orelse wh_util:is_in_account_hierarchy(AuthBy, AssignedTo)) 
                            andalso wh_util:is_in_account_hierarchy(AssignedTo, AssignTo, true)
                        of
                            false -> {error, unauthorized};
                            true ->  {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end    
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_assigned_to">>, AssignTo, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         {ok, wh_json:set_value(<<"pvt_number_state">>, <<"in_service">>, J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
in_service(<<"in_service">>, JObj, AssignTo, _) ->
    case wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj) of
        AssignTo -> {error, no_change_required};
        _Else -> {error, unauthorized}
    end;
in_service(<<"released">>, _, _, _) ->
    {error, invalid_state_transition};
in_service(<<"port_out">>, _, _, _) ->
    {error, invalid_state_transition};
in_service(<<"disconnected">>, _, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec released/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> transition_return().
-spec released/3 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary()) -> transition_return().

released(JObj, AuthBy) ->
    released(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AuthBy).

released(<<"discovery">>, _, _) ->
    {error, invalid_state_transition};    
released(<<"port_in">>, _, _) ->
    {error, invalid_state_transition};
released(<<"available">>, _, _) ->
    {error, invalid_state_transition};
released(<<"reserved">>, JObj, AuthBy) ->
    Routines = [fun(J) ->
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         NewState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
                         {ok, wh_json:set_value(<<"pvt_number_state">>, NewState, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:private_fields(J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:set_value(<<"pvt_features">>, [], J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:delete_key(<<"pvt_assigned_to">>, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         Module = wh_json:get_value(<<"pvt_module_name">>, JObj),
                         PrevAssignedTo = wh_json:get_ne_value(<<"pvt_previously_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_wnm_prev_assignments">>, J, []) of
                             [] when Module =:= <<"wnm_local">> ->
                                 lager:debug("flagging number for hard delete", []),
                                 {ok, wh_json:set_value(<<"pvt_deleted">>, true, J)};
                             [PrevAssignedTo] when Module =:= <<"wnm_local">> ->
                                 lager:debug("flagging number for hard delete", []),
                                 {ok, wh_json:set_value(<<"pvt_deleted">>, true, J)};
                             [PrevAssignedTo] ->
                                 {ok, J};
                             [PrevAccount] -> 
                                 Prop = [{<<"pvt_assigned_to">>, PrevAccount}
                                         ,{<<"pvt_number_state">>, <<"reserved">>}
                                        ],
                                 {ok, wh_json:set_values(Prop, J)};
                             [PrevAccount|PrevAssignments] -> 
                                 Prop = [{<<"pvt_assigned_to">>, PrevAccount}
                                         ,{<<"pvt_number_state">>, <<"reserved">>}
                                         ,{<<"pvt_wnm_prev_assignments">>, PrevAssignments}
                                        ],
                                 {ok, wh_json:set_values(Prop, J)};
                             _Else -> {ok, J}    
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
released(<<"in_service">>, JObj, AuthBy) ->
    Routines = [fun(J) ->
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:private_fields(J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:set_value(<<"pvt_features">>, [], J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         NewState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
                         {ok, wh_json:set_value(<<"pvt_number_state">>, NewState, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_previously_assigned_to">>, J) of
                             AssignedTo -> {ok, J};
                             _ when AssignedTo =:= undefined -> {ok, J};
                             _Else -> {ok, wh_json:set_value(<<"pvt_previously_assigned_to">>, AssignedTo, J)}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:delete_key(<<"pvt_assigned_to">>, J)}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         Module = wh_json:get_value(<<"pvt_module_name">>, JObj),
                         PrevAssignedTo = wh_json:get_ne_value(<<"pvt_previously_assigned_to">>, J),
                         case wh_json:get_value(<<"pvt_wnm_prev_assignments">>, J, []) of
                             [] when Module =:= <<"wnm_local">> ->
                                 lager:debug("flagging number for hard delete", []),
                                 {ok, wh_json:set_value(<<"pvt_deleted">>, true, J)};
                             [PrevAssignedTo] when Module =:= <<"wnm_local">> ->
                                 lager:debug("flagging number for hard delete", []),
                                 {ok, wh_json:set_value(<<"pvt_deleted">>, true, J)};
                             [PrevAssignedTo] ->
                                 {ok, J};
                             [PrevAccount] -> 
                                 Prop = [{<<"pvt_assigned_to">>, PrevAccount}
                                         ,{<<"pvt_number_state">>, <<"reserved">>}
                                        ],
                                 {ok, wh_json:set_values(Prop, J)};
                             [PrevAccount|PrevAssignments] -> 
                                 Prop = [{<<"pvt_assigned_to">>, PrevAccount}
                                         ,{<<"pvt_number_state">>, <<"reserved">>}
                                         ,{<<"pvt_wnm_prev_assignments">>, PrevAssignments}
                                        ],
                                 {ok, wh_json:set_values(Prop, J)};
                             _Else -> {ok, J}
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines);
released(<<"released">>, _, _) ->
    {error, no_change_required};
released(<<"port_out">>, _, _) ->
    {error, invalid_state_transition};
released(<<"disconnected">>, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec port_out/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {'error','invalid_state_transition'}.
-spec port_out/3 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary()) -> {'error','invalid_state_transition'}.

port_out(JObj, AuthBy) ->
    port_out(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AuthBy).

port_out(<<"discovery">>, _, _) ->
    {error, invalid_state_transition};    
port_out(<<"port_in">>, _, _) ->
    {error, invalid_state_transition};
port_out(<<"available">>, _, _) ->
    {error, invalid_state_transition};
port_out(<<"reserved">>, _, _) ->
    {error, invalid_state_transition};
port_out(<<"in_service">>, _, _) ->
    {error, invalid_state_transition};
port_out(<<"released">>, _, _) ->
    {error, invalid_state_transition};
port_out(<<"port_out">>, _, _) ->
    {error, invalid_state_transition};
port_out(<<"disconnected">>, _, _) ->
    {error, invalid_state_transition}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec disconnected/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {'error','invalid_state_transition'}.
-spec disconnected/3 :: (ne_binary(), wh_json:json_object(), 'undefined' | ne_binary()) -> {'error','invalid_state_transition'}.

disconnected(JObj, AuthBy) ->
    disconnected(wh_json:get_value(<<"pvt_number_state">>, JObj), JObj, AuthBy).

disconnected(<<"discovery">>, _, _) ->
    {error, invalid_state_transition};    
disconnected(<<"port_in">>, _, _) ->
    {error, invalid_state_transition};
disconnected(<<"available">>, _, _) ->
    {error, invalid_state_transition};
disconnected(<<"reserved">>, _, _) ->
    {error, invalid_state_transition};
disconnected(<<"in_service">>, _, _) ->
    {error, invalid_state_transition};
disconnected(<<"released">>, _, _) ->
    {error, invalid_state_transition};
disconnected(<<"port_out">>, _, _) ->
    {error, invalid_state_transition};
disconnected(<<"disconnected">>, _, _) ->
    {error, invalid_state_transition}.
