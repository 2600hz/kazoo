%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_number_manager).

-export([find/1, find/2]).
-export([reconcile_number/2]).
-export([assign_number_to_account/2]).
-export([get_public_fields/2, set_public_fields/3]).
-export([lookup_account_by_number/1]).
-export([free_numbers/1]).

-include("../include/wh_number_manager.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for avaliable numbers.
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (ne_binary()) -> [] | [ne_binary(),...].
-spec find/2 :: (ne_binary(), ne_binary()) -> [] | [ne_binary(),...].

find(Number) ->
    find(Number, <<"1">>).

find(Number, Quanity) ->
    Num = wnm_util:normalize_number(Number),
    ?LOG("attempting to find ~s numbers with prefix '~s'", [Quanity, Number]),
    Results = [{Module, catch(Module:find_numbers(Num, Quanity))} 
               || Module <- wnm_util:list_carrier_modules()],
    prepare_find_results(Results, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, creating it as a local number if
%% missing
%% @end
%%--------------------------------------------------------------------
-spec reconcile_number/2 :: (ne_binary(), ne_binary()) -> {ok, json_object()} |
                                                          {error, term()}.
reconcile_number(Number, AccountId) ->
    Regex = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"reconcile_regex">>, <<"^\\+{0,1}1{0,1}(\\d{10})$">>),
    Num = wnm_util:normalize_number(Number),
    ?LOG("attempting to reconcile number '~s' with account '~s'", [Num, AccountId]),
    try 
        case re:run(Num, Regex) of
            nomatch ->
                ?LOG("number '~s' is not reconcilable", [Num]),
                throw(not_reconcilable);
            _ ->
                ?LOG("number '~s' can be reconciled, proceeding", [Num]),
                ok
        end,
        JObj = case store_discovery(Num, <<"wnm_local">>, wh_json:new(), {<<"reserved">>, AccountId}) of
                    {ok, J1} -> J1;
                    {error, {conflict, J2}} -> J2;
                    {error, R} -> throw(R)
                end,
        case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
            AccountId -> 
                ?LOG("number already exists and is routed to the account"),
                {ok, wh_json:public_fields(JObj)};
            _ ->
                assign_number_to_account(Num, AccountId)
        end
    catch
        throw:Reason -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, aquiring the number from the provider
%% if necessary
%% @end
%%--------------------------------------------------------------------
assign_number_to_account(Number, AccountId) ->
    ?LOG("attempting to assign ~s to account ~s", [Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    try
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, R1} -> 
                        ?LOG("failed to open number DB: ~p", [R1]),
                        throw(not_found);
                   {ok, J} -> J
               end,
        {Module, ModuleData} = case wnm_util:get_carrier_module(JObj1) of
                                   {error, not_specified} -> 
                                       ?LOG("carrier module not specified on number document"),
                                       throw(unknown_carrier);
                                   {error, unknown_module} -> 
                                       ?LOG("carrier module specified on number document does not exist"),
                                       throw(unknown_carrier);
                                   {ok, Mod, Data1} -> 
                                       {Mod, wh_json:set_value(<<"acquire_for">>, AccountId, Data1)}
                               end,
        NumberState = case wh_json:get_value(<<"pvt_number_state">>, JObj1, <<"unknown">>) of
                          <<"reserved">> -> 
                              case wh_json:get_value(<<"pvt_reserved_for">>, JObj1) of
                                  AccountId -> 
                                      ?LOG("allowing account to claim a reserved number"),
                                      <<"claim">>;
                                  _ ->
                                      ?LOG("number is reserved for another account"),
                                      throw(reserved)
                              end;
                          <<"in_service">> -> 
                              ?LOG("number is already in service"),
                              throw(unavailable);
                          Else -> 
                              ?LOG("allowing assignment of a number currently in state ~s", [Else]),
                              Else
                      end,
        {NewNumberState, NewModuleData} = case Module:acquire_number(Num, NumberState, ModuleData) of
                                              {error, Error} -> 
                                                  ?LOG("module failed to acquire number: ~p", [Error]),
                                                  throw(Error);
                                              {ok, State, Data2} -> 
                                                  {State, Data2}
                                          end,
        Updaters = [fun(J) -> wh_json:set_value(<<"pvt_number_state">>, NewNumberState, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, NewModuleData, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_assigned_to">>, AccountId, J) end
                   ],
        case couch_mgr:save_doc(Db, lists:foldr(fun(F, J) -> F(J) end, JObj1, Updaters)) of
            {ok, JObj2} -> 
                add_number_to_account(Num, AccountId),
                {ok, wh_json:public_fields(JObj2)};
            {error, R2}=E -> 
                ?LOG("failed to save number document with new assignment: ~p", [R2]),
                E
        end
    catch
        throw:Reason -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to find the number, and if sucessful return the account
%% assignment
%% @end
%%--------------------------------------------------------------------
-spec lookup_account_by_number/1 :: (ne_binary()) -> {ok, ne_binary(), boolean()} |
                                                     {error, term()}.
lookup_account_by_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    ?LOG("attempting to lookup '~s' in '~s'", [Num, Db]),
    DefaultAccount = whapps_config:get_non_empty(?WNM_CONFIG_CAT, <<"default_account">>, <<>>),
    case couch_mgr:open_doc(Db, Num) of
        {ok, JObj} ->
            ?LOG("found number"),
            {ok, wh_json:get_value(<<"pvt_assigned_to">>, JObj, DefaultAccount)
             ,wh_json:is_true(<<"force_outbound">>, JObj, false)};
        {error, R1} when DefaultAccount =/= undefined -> 
            ?LOG("failed to lookup number, using default account ~s: ~p", [DefaultAccount, R1]),
            {ok, DefaultAccount, false};
        {error, R2}=E -> 
            ?LOG("failed to lookup number: ~p", [R2]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
get_public_fields(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    ?LOG("attempting to lookup '~s' in '~s'", [Num, Db]),
    case couch_mgr:open_doc(Db, Num) of
        {ok, JObj} -> 
            case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                AccountId ->
                    ?LOG("found number assigned to ~s", [AccountId]),
                    {ok, wh_json:public_fields(JObj)};
                _Else ->
                    ?LOG("found number was not assigned to ~s, returning unathorized", [AccountId]),
                    {error, unathorized}
            end;
        {error, R}=E -> 
            ?LOG("failed to lookup number: ~p", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
set_public_fields(Number, AccountId, PublicJObj) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    ?LOG("attempting to lookup '~s' in '~s'", [Num, Db]),
    try
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, R1} -> 
                        ?LOG("failed to lookup number: ~p", [R1]),
                        throw(not_found);
                   {ok, J} -> 
                       case wh_json:get_value(<<"pvt_assigned_to">>, J) of
                           AccountId -> 
                               ?LOG("found number assigned to ~s", [AccountId]),
                               J;
                           _Else -> 
                               ?LOG("found number was not assigned to ~s, returning unathorized", [AccountId]),
                               throw(unathorized)
                       end
               end,
        case couch_mgr:save_doc(Db, wh_json:merge_jobjs(wh_json:private_fields(JObj1), PublicJObj)) of
            {ok, JObj2} -> 
                ?LOG("updated public fields on number"),
                add_number_to_account(Num, AccountId),
                {ok, wh_json:public_fields(JObj2)};
            {error, R2}=E -> 
                ?LOG("failed to save public fields on number: ~p", [R2]),
                E
        end
    catch
        throw:Reason -> {error, Reason}
    end.

release_number(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    try
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, R1} -> 
                        ?LOG("failed to open number DB: ~p", [R1]),
                        throw(not_found);
                   {ok, J} -> J
               end,
        {Module, ModuleData} = case wnm_util:get_carrier_module(JObj1) of
                                   {error, not_specified} -> 
                                       ?LOG("carrier module not specified on number document"),
                                       throw(unknown_carrier);
                                   {error, unknown_module} -> 
                                       ?LOG("carrier module specified on number document does not exist"),
                                       throw(unknown_carrier);
                                   {ok, Mod, Data1} -> 
                                       {Mod, Data1}
                               end,
        case wh_json:get_value(<<"pvt_number_state">>, JObj1, <<"unknown">>) of
            <<"in_service">> -> 
                case wh_json:get_value(<<"pvt_assigned_to">>, JObj1) of
                    AccountId -> 
                        ?LOG("allowing account to free their in service numbers"),
                        ok;
                    _ ->
                        ?LOG("number is in service for another account"),
                        throw(unathorized)
                end;
            _Else -> 
                ?LOG("number is not in service"),
                throw(unavailable)
        end,
        NewModuleData = case Module:release_number(Num, ModuleData) of
                            {error, Error} -> 
                                ?LOG("module failed to release number: ~p", [Error]),
                                throw(Error);
                            {ok, Data2} -> 
                                Data2
                        end,
        Updaters = [fun(J) -> wh_json:set_value(<<"pvt_number_state">>, <<"released">>, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, NewModuleData, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                    ,fun(J) -> wh_json:delete_key(<<"pvt_assigned_to">>, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_previously_assigned_to">>, AccountId, J) end
                   ],
        case couch_mgr:save_doc(Db, lists:foldr(fun(F, J) -> F(J) end, JObj1, Updaters)) of
            {ok, _} -> 
                remove_number_from_account(Num, AccountId),
                ok;
            {error, R2}=E -> 
                ?LOG("failed to save number document with new assignment: ~p", [R2]),
                E
        end
    catch
        throw:not_found ->
            remove_number_from_account(Num, AccountId),
            ok;
        _:_ ->
            {error, fault}
    end.

free_numbers(AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of        
        {ok, JObj} ->
                 [ok = release_number(Number, AccountId) 
                  || Number <- wh_json:get_value(<<"pvt_wnm_numbers">>, JObj, [])],
                 ok;
        _ -> ok
    end.
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over all the discovered numbers during a find operation and
%% ensure the modules data is stored for later acquisition.
%% @end
%%--------------------------------------------------------------------
-spec prepare_find_results/2 :: (proplist(), [] | [ne_binary(),...]) -> [] | [ne_binary(),...].
-spec prepare_find_results/4 :: ([] | [ne_binary(),...], ne_binary(), json_object(), [] | [ne_binary(),...]) 
                                -> [] | [ne_binary(),...].

prepare_find_results([], Found) ->
    Results = lists:flatten(Found),
    ?LOG("discovered ~p avaliable numbers", [length(Results)]),
    Results;
prepare_find_results([{Module, {ok, ModuleResults}}|T], Found) ->
    case wh_json:get_keys(ModuleResults) of
        [] -> prepare_find_results(T, Found);
        Numbers ->
            Results = prepare_find_results(Numbers, wh_util:to_binary(Module)
                                           ,ModuleResults, Found),
            prepare_find_results(T, [Results|Found])
    end;
prepare_find_results([_|T], Found) ->
    prepare_find_results(T, Found).

prepare_find_results([], _, _, Found) ->
    Found;
prepare_find_results([Number|Numbers], ModuleName, ModuleResults, Found) ->
    ModuleData = wh_json:get_value(Number, ModuleResults),
    case store_discovery(Number, ModuleName, ModuleData) of
        {error, {conflict, JObj}} ->
            State = wh_json:get_value(<<"pvt_number_state">>, JObj),
            case lists:member(State, ?WNM_AVALIABLE_STATES) of
                true -> 
                    prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
                false -> 
                    ?LOG("the discovery '~s' is not avaliable: ~s", [Number, State]),
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
            end;
        {ok, _} ->
            prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
        _Else ->
            ?LOG("the discovery '~s' could not be stored, ignoring", [Number, _Else]),
            prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Store a newly discovered number (first time)
%% @end
%%--------------------------------------------------------------------
-spec store_discovery/3 :: (ne_binary(), ne_binary(), json_object()) -> {ok, json_object()} | {error, term()}.
-spec store_discovery/4 :: (ne_binary(), ne_binary(), json_object(), ne_binary()) -> {ok, json_object()} | {error, term()}.

store_discovery(Number, ModuleName, ModuleData) ->
    store_discovery(Number, ModuleName, ModuleData, <<"discovery">>).

store_discovery(Number, ModuleName, ModuleData, State) ->
    Db = wnm_util:number_to_db_name(Number),
    Generators = [fun(J) -> wh_json:set_value(<<"_id">>, Number, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_module_name">>, ModuleName, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, ModuleData, J) end
                  ,fun(J) ->
                           case State of
                               {<<"reserved">>, AccountId} ->
                                   wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>
                                                         ,wh_json:set_value(<<"pvt_reserved_for">>, AccountId, J));
                               Else ->
                                   wh_json:set_value(<<"pvt_number_state">>, Else, J)
                           end
                   end
                  ,fun(J) -> wh_json:set_value(<<"pvt_db_name">>, Db, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end 
                  ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                 ],
    JObj = lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Generators),
    case couch_mgr:save_doc(Db, JObj) of
        {ok, _}=Ok ->
            ?LOG("stored newly discovered number '~s'", [Number]),
            Ok;
        {error, not_found} ->
            ?LOG("storing discovered number '~s' in a new database '~s'", [Number, Db]),
            couch_mgr:db_create(Db),
            couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
            couch_mgr:save_doc(Db, JObj);
        {error, conflict} ->
            case couch_mgr:open_doc(Db, Number) of
                {ok, Conflict} ->
                    ?LOG("discovered number '~s' exists in database '~s'", [Number, Db]),
                    {error, {conflict, Conflict}};
                _Else ->
                    ?LOG("discovered number '~s' was in conflict but opening the existing failed: ~p", [Number, _Else]),
                    {error, conflict}
            end;
        Else ->
            ?LOG("storing the discovered number '~s' failed: ~p", [Number, Else]),
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a number to the list kept on the account defintion doc, then
%% aggregates the new document to the accounts db.
%% @end
%%--------------------------------------------------------------------
-spec add_number_to_account/2 :: (ne_binary(), ne_binary()) -> {ok, json_object()} |
                                                               {error, term()}.
add_number_to_account(Number, AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {ok, JObj} ->
            Numbers = wh_json:get_value(<<"pvt_wnm_numbers">>, JObj, []),
            case couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_wnm_numbers">>
                                                              ,[Number|lists:delete(Number,Numbers)]
                                                          ,JObj)) of
                {ok, AccountDef} ->
                    ?LOG("added '~s' to account definition ~s", [Number, AccountId]),
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, AccountDef);
                Else ->
                    ?LOG("failed to save account definition when adding '~s': ~p", [Number, Else]),
                    Else
            end;
        Else ->
            ?LOG("failed to load account definition when adding '~s': ~p", [Number, Else]),
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes a number from the list kept on the account defintion doc, then
%% aggregates the new document to the accounts db.
%% @end
%%--------------------------------------------------------------------
-spec remove_number_from_account/2 :: (ne_binary(), ne_binary()) -> {ok, json_object()} |
                                                                    {error, term()}.
remove_number_from_account(Number, AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {ok, JObj} ->
            Numbers = wh_json:get_value(<<"pvt_wnm_numbers">>, JObj, []),
            case couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_wnm_numbers">>
                                                              ,lists:delete(Number,Numbers), JObj)) of
                {ok, AccountDef} ->
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, AccountDef);
                Else ->
                    Else
            end;
        Else ->
            Else
    end.
