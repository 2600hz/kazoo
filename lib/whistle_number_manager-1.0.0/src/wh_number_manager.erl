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
-export([get_account/1, set_account/2]).
-export([get_number/2, save_number/3]).

-include("../include/wh_number_manager.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for avaliable numbers.
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (ne_binary()) -> [] | [ne_binary(),...].
-spec find/2 :: (ne_binary(), pos_integer()) -> [] | [ne_binary(),...].

find(Number) ->
    find(Number, 1).

find(Number, Quanity) ->
    Num = wnm_util:normalize_number(Number),
    Results = [{Module, catch(Module:find_numbers(Num, Quanity))} 
               || Module <- wnm_util:list_carrier_modules()],
    prepare_find_results(Results, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, aquiring the number from the provider
%% if necessary
%% @end
%%--------------------------------------------------------------------
set_account(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    try
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, _} -> throw(not_found);
                   {ok, J} -> J
               end,
        {Module, ModuleData} = case wnm_util:get_carrier_module(JObj1) of
                                   {error, not_specified} -> 
                                       throw(unknown_carrier);
                                   {error, unknown_module} -> 
                                       throw(unknown_carrier);
                                   {ok, Mod, Data1} -> {Mod, Data1}
                               end,
        NumberState = case wh_json:get_value(<<"pvt_number_state">>, JObj1, <<"unknown">>) of
                          <<"reserved">> -> 
                              case wh_json:get_value(<<"pvt_reserved_for">>, JObj1) of
                                  AccountId -> <<"claim">>;
                                  _ -> throw(reserved)
                              end;
                          <<"in_service">> -> throw(unavailable);
                          Else -> Else
                      end,
        {NewNumberState, NewModuleData} = case Module:acquire_number(Num, NumberState, ModuleData) of
                                              {error, Error} -> throw(Error);
                                              {ok, State, Data2} -> {State, Data2}
                                          end,
        Updaters = [fun(J) -> wh_json:set_value(<<"pvt_number_state">>, NewNumberState, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, NewModuleData, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_assigned_to">>, AccountId, J) end
                   ],
        case couch_mgr:save_doc(Db, lists:foldr(fun(F, J) -> F(J) end, JObj1, Updaters)) of
            {ok, JObj2} -> 
                add_number_to_account(JObj2, AccountId),
                {ok, wh_json:public_fields(JObj2)};
            {error, _}=E -> 
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
get_account(Number) ->
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
get_number(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {ok, JObj} -> 
            case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                AccountId ->
                    {ok, wh_json:public_fields(JObj)};
                _Else ->
                    {error, unathorized}
            end;
        {error, _}=E -> 
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
save_number(Number, AccountId, PublicJObj) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    try
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, _} -> throw(not_found);
                   {ok, J} -> 
                       case wh_json:get_value(<<"pvt_assigned_to">>, J) of
                           AccountId -> J;
                           _Else -> throw(unathorized)
                       end
               end,
        case couch_mgr:save_doc(Db, wh_json:merge_jobjs(wh_json:private_fields(JObj1), PublicJObj)) of
            {ok, JObj2} -> 
                add_number_to_account(JObj2, AccountId),
                {ok, wh_json:public_fields(JObj2)};
            {error, _}=E -> 
                E
        end
    catch
        throw:Reason -> {error, Reason}
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
    lists:flatten(Found);
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
            case lists:member(wh_json:get_value(<<"pvt_number_state">>, JObj), ?WNM_AVALIABLE_STATES) of
                true -> prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
                false -> prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
            end;
        {ok, _} ->
            prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
        _Else ->
            prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
    end.    


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Store a newly discovered number (first time)
%% @end
%%--------------------------------------------------------------------
-spec store_discovery/3 :: (ne_binary(), ne_binary(), json_object()) -> {ok, json_object()} | {error, term()}.
store_discovery(Number, ModuleName, ModuleData) ->
    Db = wnm_util:number_to_db_name(Number),
    Generators = [fun(J) -> wh_json:set_value(<<"_id">>, Number, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_module_name">>, ModuleName, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, ModuleData, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_number_state">>, <<"discovery">>, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_db_name">>, Db, J) end
                  ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end 
                  ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                 ],
    JObj = lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Generators),
    case couch_mgr:save_doc(Db, JObj) of
        {error, not_found} ->
            couch_mgr:db_create(Db),
            couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
            couch_mgr:save_doc(Db, JObj);
        {error, conflict} ->
            case couch_mgr:open_doc(Db, Number) of
                {ok, Conflict} ->
                    {error, {conflict, Conflict}};
                _Else ->
                    {error, conflict}
            end;
        Else ->
            Else
    end.

add_number_to_account(JObj, AccountId) ->
    ok.
