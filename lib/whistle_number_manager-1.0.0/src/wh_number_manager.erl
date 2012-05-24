%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_number_manager).

-export([find/1, find/2]).
-export([lookup_account_by_number/1]).
-export([create_number/3, create_number/4]).
-export([port_in/3, port_in/4]).
-export([reconcile_number/3]).
-export([free_numbers/1]).
-export([reserve_number/3, reserve_number/4]).
-export([assign_number_to_account/3, assign_number_to_account/4]).
-export([release_number/2]).
-export([list_attachments/2]).
-export([fetch_attachment/3]).
-export([put_attachment/5]).
-export([delete_attachment/3]).
-export([get_public_fields/2, set_public_fields/3]).

-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the various providers for available numbers.
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (ne_binary()) -> [] | [ne_binary(),...].
-spec find/2 :: (ne_binary(), ne_binary()) -> [] | [ne_binary(),...].

find(Number) ->
    find(Number, <<"1">>).

find(Number, Quanity) ->
    Num = wnm_util:normalize_number(Number),
    lager:debug("attempting to find ~p numbers with prefix '~s'", [Quanity, Number]),
    Results = [{Module, catch(Module:find_numbers(Num, Quanity))}
               || Module <- wnm_util:list_carrier_modules()],
    prepare_find_results(Results, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to find the number, and if sucessful return the account
%% assignment
%% @end
%%--------------------------------------------------------------------
-spec lookup_account_by_number/1 :: (ne_binary()) -> {ok, ne_binary(), boolean()} |
                                                     {error, term()}.
lookup_account_by_number(undefined) ->
    {error, number_undefined};
lookup_account_by_number(Number) ->
    case wnm_number:get(Number) of
        {error, _}=E -> E;
        {ok, JObj} ->
            lager:debug("found number in db"),
            AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj),
            NumberState = wh_json:get_value(<<"pvt_number_state">>, JObj),
            AccountEnabled = wh_util:is_account_enabled(AssignedTo),
            if
                AssignedTo =:= undefined ->
                    lager:debug("number not assigned to an account"),
                    {error, unassigned};
                NumberState =/= <<"in_service">> ->
                    lager:debug("number assigned to acccount id '~s' but not in service (~s)", [AssignedTo, NumberState]),
                    {error, {not_in_service, AssignedTo}};
                AccountEnabled =:= false ->
                    lager:debug("number assigned to acccount id '~s' but account is disabled", [AssignedTo]),
                    {error, {account_disabled, AssignedTo}};
                NumberState =:= <<"port_in">> ->
                    {ok, AssignedTo, true};
                NumberState =:= <<"port_out">> ->
                    {ok, AssignedTo, true};
                true ->
                    {ok, AssignedTo, wh_json:is_true(<<"force_outbound">>, JObj, false)}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add and reserve a number for an account
%% @end
%%--------------------------------------------------------------------
-spec create_number/3 :: (ne_binary(), ne_binary(), ne_binary()) -> transition_return().
-spec create_number/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> transition_return().

create_number(Number, AssignTo, AuthBy) ->
    create_number(Number, AssignTo, AuthBy, undefined).

create_number(Number, AssignTo, AuthBy, PublicFields) ->
    lager:debug("attempting to create number ~s for account ~s", [Number, AssignTo]),    
    Routines = [fun({error, not_found}) -> 
                         AccountId = wh_util:format_account_id(AuthBy, raw),
                         AccountDb = wh_util:format_account_id(AuthBy, encoded),
                         case couch_mgr:open_doc(AccountDb, AccountId) of
                             {error, _} -> {error, unauthorized};
                             {ok, JObj} ->
                                 case wh_json:is_true(<<"pvt_wnm_allow_additions">>, JObj) of
                                     true -> 
                                         lager:debug("number doesnt exist but account ~s is authorized to create it", [AuthBy]),
                                         {ok, wnm_number:create_available(Number, AuthBy)};
                                     false -> {error, unauthorized}
                                 end
                         end;
                    ({error, _}=E) -> E;
                    ({ok, _}) -> {error, unauthorized}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:reserved(J, AssignTo, AuthBy)
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:save(J)
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("create number prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         lager:debug("create number successfully completed", []),
                         {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number, PublicFields), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a port in number for an account
%% @end
%%--------------------------------------------------------------------
-spec port_in/3 :: (ne_binary(), ne_binary(), ne_binary()) -> transition_return().
-spec port_in/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> transition_return().

port_in(Number, AssignTo, AuthBy) ->
    port_in(Number, AssignTo, AuthBy, undefined).

port_in(Number, AssignTo, AuthBy, PublicFields) ->
    lager:debug("attempting to port_in number ~s for account ~s", [Number, AssignTo]),    
    Routines = [fun({error, not_found}) -> {ok, wnm_number:create_port_in(Number, AssignTo, AuthBy)};
                   ({error, _}) -> {error, unauthorized};
                   ({ok, _}) -> {error, unauthorized}
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:save(J)
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("port in number prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         lager:debug("port in number successfully completed", []),
                         {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number, PublicFields), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, creating it as a local number if
%% missing
%% @end
%%--------------------------------------------------------------------
-spec reconcile_number/3 :: (ne_binary(), ne_binary(), ne_binary()) -> transition_return().
reconcile_number(Number, AssignTo, AuthBy) ->
    lager:debug("attempting to reconcile number ~s with account ~s", [Number, AssignTo]),
    Routines = [fun({error, not_found}) -> {ok, wnm_number:create_available(Number, AuthBy)};
                    ({error, _}=E) -> E;
                    ({ok, _}=Ok) -> Ok
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:in_service(J, AssignTo, AuthBy)
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:save(J)
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("reconcile prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         lager:debug("reconcile successfully completed", []),
                         {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release all numbers currently assigned to an account
%% @end
%%--------------------------------------------------------------------
-spec free_numbers/1 :: (ne_binary()) -> ok.
free_numbers(AccountId) ->
    lager:debug("attempting to free all numbers assigned to account ~s", [AccountId]),
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {ok, JObj} ->
            _ = [case release_number(Number, AccountId) of
                     {ok, _} -> ok;
                     {error, _} -> wnm_util:update_numbers_on_account(Number, <<"released">>, AccountId)
                 end
                 || Number <- wh_json:get_value(<<"pvt_wnm_in_service">>, JObj, [])
                        ++ wh_json:get_value(<<"pvt_wnm_reserved">>, JObj, [])
                ],
            ok;
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add and reserve a number for an account
%% @end
%%--------------------------------------------------------------------
-spec reserve_number/3 :: (ne_binary(), ne_binary(), ne_binary()) -> transition_return().
-spec reserve_number/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> transition_return().

reserve_number(Number, AssignTo, AuthBy) ->
    reserve_number(Number, AssignTo, AuthBy, undefined).

reserve_number(Number, AssignTo, AuthBy, PublicFields) ->
    lager:debug("attempting to reserve ~s for account ~s", [Number, AssignTo]),    
    Routines = [fun({error, _}=E) -> E;
                    ({ok, _}=Ok) -> Ok
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:reserved(J, AssignTo, AuthBy)
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->  wnm_number:save(J)
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("reserve prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         lager:debug("reserve successfully completed", []),
                         {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number, PublicFields), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, aquiring the number from the provider
%% if necessary
%% @end
%%--------------------------------------------------------------------
-spec assign_number_to_account/3 :: (ne_binary(), ne_binary(), ne_binary()) -> transition_return().
-spec assign_number_to_account/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:json_object()) -> transition_return().

assign_number_to_account(Number, AssignTo, AuthBy) ->
    assign_number_to_account(Number, AssignTo, AuthBy, undefined).

assign_number_to_account(Number, AssignTo, AuthBy, PublicFields) ->
    lager:debug("attempting to assign ~s to account ~s", [Number, AssignTo]),
    Routines = [fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:in_service(J, AssignTo, AuthBy)
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:save(J)
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("assign number to account prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         lager:debug("assign number to account successfully completed", []),
                         {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number, PublicFields), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% move an in service number to the released state were it will be
%% recycled or cancled after a buffer period
%% @end
%%--------------------------------------------------------------------
-spec release_number/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} | {error, _}.
release_number(Number, AuthBy) ->    
    lager:debug("attempting to release ~s", [Number]),
    Routines = [fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:released(J, AuthBy)
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         case wh_json:is_true(<<"pvt_deleted">>, J) of
                             false -> wnm_number:save(J);
                             true -> 
                                 Num = wh_json:get_value(<<"_id">>, J),
                                 Db = wnm_util:number_to_db_name(Num),
                                 lager:debug("executing hard delete of number ~s", [Num]),
                                 couch_mgr:del_doc(Db, J)
                         end
                 end
                ,fun({error, _R}=E) ->
                         _ = wnm_util:update_numbers_on_account(Number, <<"released">>),
                         lager:debug("release prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         lager:debug("release successfully completed", []),
                         {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lists attachments on a number
%% @end
%%--------------------------------------------------------------------
-spec list_attachments/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
list_attachments(Number, AuthBy) ->
    lager:debug("attempting to list attachements on ~s", [Number]),
    Routines = [fun(_) -> wnm_number:get(Number) end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         case wh_json:get_value(<<"pvt_number_state">>, J) of
                             <<"port_in">> -> {ok, J};
                             _Else -> {error, unauthorized}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("list attachments prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         {ok, wh_json:get_value(<<"_attachments">>, J, wh_json:new())}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, undefined, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch an attachment on a number
%% @end
%%--------------------------------------------------------------------
-spec fetch_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
fetch_attachment(Number, Name, AuthBy) ->
    lager:debug("attempting to fetch attachement ~s on ~s for account ~s", [Name, Number]),
    Routines = [fun({error, _}=E) -> E;
                   ({ok, J}) -> 
                        case wh_json:get_value(<<"pvt_number_state">>, J) of
                            <<"port_in">> -> {ok, J};
                            _Else -> {error, unauthorized}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                             false -> {error, unauthorized};
                             true -> {ok, J}
                         end
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("fetch attachments prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         Num = wh_json:get_value(<<"_id">>, J),
                         Db = wnm_util:number_to_db_name(Num),
                         couch_mgr:fetch_attachment(Db, Num, Name)
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec put_attachment/5 :: (ne_binary(), ne_binary(), ne_binary(), proplist(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom}.
put_attachment(Number, Name, Content, Options, AuthBy) ->
    lager:debug("attempting to add an attachement to ~s", [Number]),
    Routines = [fun({error, _}=E) -> E;
                   ({ok, J}) -> 
                        case wh_json:get_value(<<"pvt_number_state">>, J) of
                            <<"port_in">> -> {ok, J};
                            _Else -> {error, unauthorized}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                         AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                         case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                             false -> {error, unauthorized};
                             true -> {ok, J}
                         end
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("put attachments prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> 
                         Rev = wh_json:get_value(<<"_rev">>, J),
                         Num = wh_json:get_value(<<"_id">>, J),
                         Db = wnm_util:number_to_db_name(Num),
                         couch_mgr:put_attachment(Db, Num, Name, Content, [{rev, Rev}|Options])
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec delete_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
delete_attachment(Number, Name, AuthBy) ->
    lager:debug("attempting to delete attachement ~s from ~s", [Name, Number]),
    Routines = [fun({error, _}=E) -> E;
                   ({ok, J}) -> 
                        case wh_json:get_value(<<"pvt_number_state">>, J) of
                            <<"port_in">> -> {ok, J};
                            _Else -> {error, unauthorized}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) ->
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("delete attachment prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) ->
                         Num = wh_json:get_value(<<"_id">>, J),
                         Db = wnm_util:number_to_db_name(Num),
                         io:format("~p ~p ~p~n", [Db, Num, Name]),
                         couch_mgr:delete_attachment(Db, Num, Name)
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec get_public_fields/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                           {error, atom()}.
get_public_fields(Number, AuthBy) ->
    lager:debug("attempting to get public fields for number ~s", [Number]),
    Routines = [fun({error, _}=E) -> E;
                   ({ok, J}) -> 
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("fetch public fields prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number), Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec set_public_fields/3 :: (ne_binary(), wh_json:json_object(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                  {error, atom()}.
set_public_fields(Number, PublicFields, AuthBy) ->
    lager:debug("attempting to set public fields for number ~s", [Number]),
    Routines = [fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                        AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, J),
                        case wh_util:is_in_account_hierarchy(AuthBy, AssignedTo, true) of
                            false -> {error, unauthorized};
                            true -> {ok, J}
                        end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> wnm_number:save(J)
                 end
                ,fun({error, _R}=E) -> 
                         lager:debug("set public fields prematurely ended: ~p", [_R]),
                         E;
                    ({ok, J}) -> {ok, wh_json:public_fields(J)}
                 end
               ], 
    lists:foldl(fun(F, J) -> F(J) end, wnm_number:get(Number, PublicFields), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over all the discovered numbers during a find operation and
%% ensure the modules data is stored for later acquisition.
%% @end
%%--------------------------------------------------------------------
-spec prepare_find_results/2 :: (proplist(), [] | [ne_binary(),...]) -> [] | [ne_binary(),...].
-spec prepare_find_results/4 :: ([] | [ne_binary(),...], ne_binary(), wh_json:json_object(), [] | [ne_binary(),...])
                                -> [] | [ne_binary(),...].

prepare_find_results([], Found) ->
    Results = lists:flatten(Found),
    lager:debug("discovered ~p available numbers", [length(Results)]),
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
    case wnm_number:get(Number) of
        {error, not_found} ->
            ModuleData = wh_json:get_value(Number, ModuleResults),
            JObj = wnm_number:create_discovery(Number, ModuleName, ModuleData),
            case wnm_number:save(JObj, wh_json:new()) of
                {ok, _} -> prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
                {error, _R} ->
                    lager:debug("failed to store discovery ~s: ~p", [Number, _R]),
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
            end;
        {ok, JObj} ->
            State = wh_json:get_value(<<"pvt_number_state">>, JObj),
            case lists:member(State, ?WNM_AVALIABLE_STATES) of
                true ->
                    prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
                false ->
                    lager:debug("the discovery '~s' is not available: ~s", [Number, State]),
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
            end;
        {error, _R} ->
            lager:debug("failed to determine state of discovery ~s: ~p", [Number, _R]),
            prepare_find_results(Numbers, ModuleName, ModuleResults, Found)    
    end.
