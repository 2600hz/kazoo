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
-export([reconcile_number/2]).
-export([assign_number_to_account/2, assign_number_to_account/3]).
-export([get_public_fields/1, set_public_fields/2]).
-export([list_attachments/1]).
-export([fetch_attachment/2]).
-export([delete_attachment/2]).
-export([put_attachment/3, put_attachment/4]).
-export([reserve_number/2, reserve_number/3]).
-export([lookup_account_by_number/1]).
-export([release_number/1]).
-export([free_numbers/1]).

-include("../include/wh_number_manager.hrl").
-include_lib("whistle/include/wh_databases.hrl").

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
    lager:debug("attempting to find ~s numbers with prefix '~s'", [Quanity, Number]),
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
lookup_account_by_number(Number) when size(Number) < 5 ->
    {error, number_too_short};
lookup_account_by_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to lookup '~s' in '~s'", [Num, Db]),
    case couch_mgr:open_doc(Db, Num) of
        {ok, JObj} ->
            lager:debug("found number in db"),
            AssignedTo = wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj),
            NumberState = wh_json:get_value(<<"pvt_number_state">>, JObj),
            AccountEnabled = wh_util:is_account_enabled(AssignedTo),
            if
                AssignedTo =:= undefined ->
                    lager:debug("number assigned to account id is unknown"),
                    {error, unassigned};
                NumberState =/= <<"in_service">> ->
                    lager:debug("number assigned to acccount id '~s' but not in service (~s)", [AssignedTo, NumberState]),
                    {error, {not_in_service, AssignedTo}};
                AccountEnabled =:= false ->
                    lager:debug("number assigned to acccount id '~s' but account is disabled", [AssignedTo]),
                    {error, {account_disabled, AssignedTo}};
                true ->
                    {ok, AssignedTo, wh_json:is_true(<<"force_outbound">>, JObj, false)}
            end;
        {error, R2}=E ->
            lager:debug("failed to find number: ~p", [R2]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, creating it as a local number if
%% missing
%% @end
%%--------------------------------------------------------------------
-spec reconcile_number/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                          {error, term()}.
-spec reconcile_number/3 :: (ne_binary(), ne_binary(), boolean()) -> {ok, wh_json:json_object()} |
                                                                     {error, term()}.

reconcile_number(Number, AccountId) ->
    reconcile_number(Number, AccountId, wnm_util:is_reconcilable(Number)).

reconcile_number(_, _, false) ->
    {error, not_reconcilable};
reconcile_number(Number, AccountId, true) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to reconcile number '~s' with account '~s'", [Num, AccountId]),
    case couch_mgr:lookup_doc_rev(Db, Num) of
        {ok, _} -> assign_number_to_account(Num, AccountId);
        {error, _} ->
            lager:debug("failed to fine number doc for reconcile, creating as reserved", []),
            case reserve_number(Number, AccountId) of
                {ok, _} -> assign_number_to_account(Num, AccountId);
                {error, _} -> 
                    lager:debug("unable to reserve number during reconcile", []),
                    {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add and reserve a number for an account
%% @end
%%--------------------------------------------------------------------
-spec reserve_number/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} | {error, term()}.
-spec reserve_number/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.
-spec reserve_number/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), boolean()) -> {ok, wh_json:json_object()} | {error, term()}.

reserve_number(Number, AccountId) ->
    reserve_number(Number, AccountId, wh_json:new()).

reserve_number(Number, AccountId, PublicFields) ->
    reserve_number(Number, AccountId, PublicFields, wnm_util:is_reconcilable(Number)).

reserve_number(_, _, _, false) ->
    {error, not_reconcilable};
reserve_number(Number, AccountId, PublicFields, true) ->
    lager:debug("attempting to reserve ~s for account ~s", [Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _} ->
            lager:debug("attempted to reserve a new number, creating", []),
            case store_discovery(Num, <<"wnm_local">>, wh_json:new(), {<<"reserved">>, AccountId}) of
                {ok, JObj} -> {ok, wh_json:public_fields(JObj)};
                {error, _R} ->
                    lager:debug("unable to store number during reserve: ~p", [_R]),
                    {error, not_found}
            end;
        {ok, JObj} ->
            State = wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>),
            case lists:member(State, ?WNM_AVALIABLE_STATES) of
                false ->
                    lager:debug("disallowing reservation of a number in state ~s", [State]),
                    {error, unauthorized};
                true ->
                    Updaters = [fun(J) -> wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>, J) end
                                ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                                ,fun(J) -> wh_json:set_value(<<"pvt_assigned_to">>, AccountId, J) end
                                ,fun(J) -> wh_json:merge_jobjs(wh_json:private_fields(J), PublicFields) end
                               ],
                    save_number(Db, Num, lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters), JObj)
            end            
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, aquiring the number from the provider
%% if necessary
%% @end
%%--------------------------------------------------------------------
-spec assign_number_to_account/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} | {error, atom()}.
-spec assign_number_to_account/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, atom()}.
-spec assign_number_to_account/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), boolean()) -> {ok, wh_json:json_object()} | {error, atom()}.

assign_number_to_account(Number, AccountId) ->
    assign_number_to_account(Number, AccountId, wh_json:new()).

assign_number_to_account(Number, AccountId, PublicFields) ->
    assign_number_to_account(Number, AccountId, PublicFields, wnm_util:is_reconcilable(Number)).

assign_number_to_account(_, _, _, false) ->
    {error, not_reconcilable};
assign_number_to_account(Number, AccountId, PublicFields, true) ->
    lager:debug("attempting to assign ~s to account ~s", [Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("failed to open number doc for assignment to an account: ~p", [_R]),
            {error, not_found};
        {ok, JObj} ->
            Routines = [fun(J) ->     
                                case wh_json:get_value(<<"pvt_number_state">>, J, <<"unknown">>) of
                                    <<"in_service">> ->
                                        lager:debug("number is already in service"),
                                        {error, unavailable};
                                    _Else ->
                                        lager:debug("allowing assignment of a number currently in state ~s", [_Else]),
                                        {ok, J}
                                end
                        end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) ->
                                 State = wh_json:get_value(<<"pvt_number_state">>, J, <<"unknown">>),
                                 case wnm_util:get_carrier_module(J) of
                                     {ok, Module, Data} ->
                                         case Module:acquire_number(Num, State, wh_json:set_value(<<"acquire_for">>, AccountId, Data)) of
                                             {error, Error} ->
                                                 lager:debug("module failed to acquire number: ~p", [Error]),
                                                 {error, Error};
                                             {ok, NewState, NewData} ->
                                                 {ok, wh_json:set_value(<<"pvt_number_state">>, NewState,
                                                                        wh_json:set_value(<<"pvt_module_data">>, NewData, J))}
                                         end;
                                     {error, not_specified} ->
                                         lager:debug("carrier module not specified on number document"),
                                         {error, unknown_carrier};
                                     {error, unknown_module} ->
                                         lager:debug("carrier module specified on number document does not exist"),
                                         {error, unknown_carrier}
                                 end
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> {ok, wh_json:set_value(<<"pvt_assigned_to">>, AccountId, J)}
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> {ok, wh_json:merge_jobjs(wh_json:private_fields(J), PublicFields)}
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> save_number(Db, Num, J, JObj)
                         end
                       ],
            lists:foldr(fun(F, J) -> F(J) end, JObj, Routines)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% move an in service number to the released state were it will be
%% recycled or cancled after a buffer period
%% @end
%%--------------------------------------------------------------------
-spec release_number/1 :: (ne_binary()) -> ok | {error, atom()}.
release_number(Number) ->
    release_number(Number, wnm_util:is_reconcilable(Number)).

release_number(_, false) ->
    {error, not_reconcilable};
release_number(Number, true) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("failed to open number doc for assignment to an account: ~p", [_R]),
            {error, not_found};
        {ok, JObj} ->
            Routines = [fun(J) ->
                                case wh_json:get_value(<<"pvt_number_state">>, J, <<"unknown">>) of
                                    <<"in_service">> -> {ok, J};
                                    <<"reserved">> -> {ok, J};
                                    <<"porting">> -> {ok, J};
                                    _Else ->
                                        lager:debug("unable to relase a number currently in state ~s, updating account def", [_Else]),
                                        update_numbers_on_account(Num, <<"released">>),
                                        {error, unauthorized}
                                end
                        end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) ->
                                 case wnm_util:get_carrier_module(J) of
                                     {ok, Module, Data} ->
                                         case Module:release_number(Num, Data) of
                                             {error, Error} ->
                                                 lager:debug("module failed to release number: ~p", [Error]),
                                                 {error, Error};
                                             {ok, NewData} ->
                                                 NewState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
                                                 {ok, wh_json:set_value(<<"pvt_number_state">>, NewState,
                                                                        wh_json:set_value(<<"pvt_module_data">>, NewData, J))}
                                         end;
                                     {error, not_specified} ->
                                         lager:debug("carrier module not specified on number document"),
                                         {error, unknown_carrier};
                                     {error, unknown_module} ->
                                         lager:debug("carrier module specified on number document does not exist"),
                                         {error, unknown_carrier}
                                 end
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) ->
                                 AccountId = wh_json:get_value(<<"pvt_assigned_to">>, J),
                                 wh_json:set_value(<<"pvt_previously_assigned_to">>, AccountId, J)
                         end    
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> {ok, wh_json:delete_key(<<"pvt_assigned_to">>, J)}
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> {ok, wh_json:delete_key(<<"_attachments">>, J)}
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> {ok, wh_json:private_fields(J)}
                         end
                        ,fun({error, _}=E) -> E;
                            ({ok, J}) -> save_number(Db, Num, J, JObj)
                         end
                       ],
            lists:foldr(fun(F, J) -> F(J) end, JObj, Routines)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release all numbers currently assigned to an account
%% @end
%%--------------------------------------------------------------------
-spec free_numbers/1 :: (ne_binary()) -> ok.
free_numbers(AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {ok, JObj} ->
            [ok = release_number(Number)
             || Number <- wh_json:get_value(<<"pvt_wnm_in_service">>, JObj, [])
                    ++ wh_json:get_value(<<"pvt_wnm_in_reserved">>, JObj, [])
            ],
            ok;
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lists attachments on a number
%% @end
%%--------------------------------------------------------------------
-spec list_attachments/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
list_attachments(Number) ->
    lager:debug("attempting to list attachements on ~s", [Number]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("failed to open number doc for listing attachments: ~p", [_R]),
            {error, not_found};
        {ok, JObj} ->
            {ok, wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch an attachment on a number
%% @end
%%--------------------------------------------------------------------
-spec fetch_attachment/2 :: ( ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
fetch_attachment(Number, Name) ->
    lager:debug("attempting to fetch attachement ~s on ~s for account ~s", [Name, Number]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    couch_mgr:fetch_attachment(Db, Num, Name).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec put_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
-spec put_attachment/4 :: (ne_binary(), ne_binary(), ne_binary(), proplist()) -> {'ok', wh_json:json_object()} | {'error', atom}.

put_attachment(Number, Name, Content) ->
    put_attachment(Number, Name, Content, []).

put_attachment(Number, Name, Content, Options) ->
    lager:debug("attempting to add an attachement to ~s", [Number]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("failed to open number doc for put attachment: ~p", [_R]),
            {error, not_found};
        {ok, JObj} ->
            Rev = wh_json:get_value(<<"_rev">>, JObj),
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> ->
                    couch_mgr:put_attachment(Db, Num, Name, Content, [{rev, Rev}|Options]);
                <<"porting">> ->
                    couch_mgr:put_attachment(Db, Num, Name, Content, [{rev, Rev}|Options]);
                <<"in_service">> ->
                    couch_mgr:put_attachment(Db, Num, Name, Content, [{rev, Rev}|Options]);
                _Else ->
                    lager:debug("disallowing attachment to a number in state ~s", [_Else]),
                    {error, unauthorized}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec delete_attachment/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.

delete_attachment(Number, Name) ->
    lager:debug("attempting to delete attachement ~s from ~s", [Name, Number]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("failed to open number doc for delete attachment: ~p", [_R]),
            {error, not_found};
        {ok, JObj} ->
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> ->
                    couch_mgr:delete_attachment(Db, Num, Name);
                <<"porting">> ->
                    couch_mgr:delete_attachment(Db, Num, Name);
                <<"in_service">> ->
                    couch_mgr:delete_attachment(Db, Num, Name);
                _Else ->
                    lager:debug("disallowing delete attachment from a number in state ~s", [_Else]),
                    {error, unauthorized}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec get_public_fields/1 :: (ne_binary()) -> {ok, wh_json:json_object()} |
                                              {error, atom()}.
get_public_fields(Number) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to lookup '~s' in '~s'", [Num, Db]),
    case couch_mgr:open_doc(Db, Num) of
        {ok, JObj} -> {ok, wh_json:public_fields(JObj)};
        {error, R}=E ->
            lager:debug("failed to lookup number: ~p", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec set_public_fields/2 :: (ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} |
                                                                     {error, atom()}.
set_public_fields(Number, PublicFields) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to set public fields for '~s' in '~s'", [Num, Db]),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("failed to lookup number: ~p", [_R]),
            {error, not_found};
        {ok, JObj} ->
            Routines = [fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                        ,fun(J) -> wh_json:merge_jobjs(wh_json:private_fields(J), PublicFields) end
                       ],
            save_number(Db, Num, lists:foldr(fun(F, J) -> F(J) end, JObj, Routines), JObj)
    end.

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
    ModuleData = wh_json:get_value(Number, ModuleResults),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _} ->
            case store_discovery(Number, ModuleName, ModuleData) of
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
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Store a newly discovered number (first time)
%% @end
%%--------------------------------------------------------------------
-spec store_discovery/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.
-spec store_discovery/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), ne_binary() | {ne_binary(), ne_binary()}) -> {ok, wh_json:json_object()} | {error, term()}.
-spec store_discovery/5 :: (ne_binary(), ne_binary(), wh_json:json_object(), ne_binary() | {ne_binary(), ne_binary()}, wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.

store_discovery(Number, ModuleName, ModuleData) ->
    store_discovery(Number, ModuleName, ModuleData, <<"discovery">>).

store_discovery(Number, ModuleName, ModuleData, State) ->
    store_discovery(Number, ModuleName, ModuleData, State, wh_json:new()).

store_discovery(Number, ModuleName, ModuleData, State, PublicFields) ->
    Db = wnm_util:number_to_db_name(Number),
    Routines = [fun(J) -> wh_json:set_value(<<"_id">>, Number, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_name">>, ModuleName, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, ModuleData, J) end
                ,fun(J) ->
                         case State of
                             {<<"reserved">>, AccountId} ->
                                 wh_json:set_value(<<"pvt_number_state">>, <<"reserved">>
                                                       ,wh_json:set_value(<<"pvt_assigned_to">>, AccountId, J));
                             Else ->
                                 wh_json:set_value(<<"pvt_number_state">>, Else, J)
                         end
                 end
                ,fun(J) -> wh_json:set_value(<<"pvt_db_name">>, Db, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end
               ],
    JObj = lists:foldr(fun(F, J) -> F(J) end, PublicFields, Routines),
    case save_number(Db, Number, JObj, wh_json:new()) of
        {ok, _}=Ok ->
            lager:debug("stored newly discovered number '~s'", [Number]),
            Ok;
        {error, not_found} ->
            lager:debug("storing discovered number '~s' in a new database '~s'", [Number, Db]),
            couch_mgr:db_create(Db),
            couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
            save_number(Db, Number, JObj, wh_json:new());
        {error, _R}=E -> E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a number to the list kept on the account defintion doc, then
%% aggregates the new document to the accounts db.
%% @end
%%--------------------------------------------------------------------
-spec update_numbers_on_account/1 :: (wh_json:json_object()) -> {ok, wh_json:json_object()} |
                                                                {error, term()}.
-spec update_numbers_on_account/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                                   {error, term()}.
-spec update_numbers_on_account/3 :: (ne_binary(), ne_binary(), undefined | ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                            {error, term()}.

update_numbers_on_account(JObj) ->
    State = wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>),
    Number = wh_json:get_value(<<"_id">>, JObj),
    update_numbers_on_account(Number, State, find_account_id(JObj)).

update_numbers_on_account(Number, State) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, _R} ->
            lager:debug("unable to load ~s for update on account to ~s: ~p", [Number, State, _R]),
            {error, not_found};
        {ok, JObj} ->
            update_numbers_on_account(Number, State, find_account_id(JObj))
    end.

update_numbers_on_account(_, _, undefined) ->
    {error, not_account_id};
update_numbers_on_account(Number, State, AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {error, _R}=E ->
            lager:debug("failed to load account definition when adding '~s': ~p", [Number, _R]),
            E;
        {ok, JObj} ->
            lager:debug("setting number ~s to state ~s on the account ~s", [Number, State, AccountId]),
            Migrate = wh_json:get_value(<<"pvt_wnm_numbers">>, JObj, []),
            Updated = lists:foldr(fun(<<"numbers">>, J) when Migrate =/= [] ->
                                          N = wh_json:get_value(<<"pvt_wnm_in_service">>, J, []),
                                          wh_json:set_value(<<"pvt_wnm_in_service">>, N ++ Migrate,
                                                            wh_json:delete_key(<<"pvt_wnm_numbers">>, J));
                                     (<<"numbers">>, J) when Migrate =:= [] ->
                                          wh_json:delete_key(<<"pvt_wnm_numbers">>, J);
                                     (S, J) when S =:= State ->
                                          lager:debug("adding number ~s to state ~s", [Number, S]),
                                          N = wh_json:get_value(<<"pvt_wnm_", S/binary>>, JObj, []),
                                          wh_json:set_value(<<"pvt_wnm_", S/binary>>, [Number|lists:delete(Number,N)], J);
                                     (S, J) ->
                                          lager:debug("removing number ~s from state ~s", [Number, S]),
                                          N = wh_json:get_value(<<"pvt_wnm_", S/binary>>, JObj, []),
                                          wh_json:set_value(<<"pvt_wnm_", S/binary>>, lists:delete(Number,N), J)
                                  end, JObj, [<<"numbers">>|?WNM_NUMBER_STATUS]),
            Cleaned = lists:foldr(fun(S, J) ->
                                          Nums = wh_json:get_value(<<"pvt_wnm_", S/binary>>, J, []),
                                          Clean = ordsets:to_list(ordsets:from_list(Nums)),
                                          wh_json:set_value(<<"pvt_wnm_", S/binary>>, Clean, J)
                                  end, Updated, ?WNM_NUMBER_STATUS),
            case couch_mgr:save_doc(Db, Cleaned) of
                {ok, AccountDef} ->
                    lager:debug("updated the account definition"),
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, AccountDef);
                {error, _R}=E ->
                    lager:debug("failed to save account definition when adding '~s': ~p", [Number, _R]),
                    E
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to save the number, add to the account doc if required,
%% and run any providers
%% @end
%%--------------------------------------------------------------------
-spec save_number/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.
save_number(Db, Number, JObj, PriorJObj) ->
    lager:debug("attempting to save '~s' in '~s'", [Number, Db]),
    CallId = get(callid),
    Routines = [fun(J) -> {ok, wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J)} end
                ,fun({ok, J}) -> couch_mgr:save_doc(Db, J) end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> 
                         State = wh_json:get_value(<<"pvt_number_state">>, J),
                         spawn(fun() ->
                                       put(callid, CallId),
                                       update_numbers_on_account(J)
                               end),
                         exec_providers_save(J, PriorJObj, Number, State)
                 end
                ,fun({error, _}=E) -> E;
                     ({ok, J}) -> 
                         case wh_json:delete_key(<<"_rev">>, J) =:= wh_json:delete_key(<<"_rev">>, JObj) of
                             false -> couch_mgr:save_doc(Db, J);
                             true -> 
                                 lager:debug("provides did not modify number document skipping second save operation"),
                                 {ok, J}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, J}) -> {ok, wh_json:public_fields(J)}
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% execute the save function of all providers, folding the jobj through
%% them and collecting any errors...
%% @end
%%--------------------------------------------------------------------
-spec exec_providers_save/4 :: (wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary()) -> {ok | error, wh_json:json_object()}.
-spec exec_providers_save/6 :: (list(), wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary(), list()) -> {ok | error, wh_json:json_object()}.

exec_providers_save(JObj, PriorJObj, Number, State) ->
    Providers = whapps_config:get(?WNM_CONFIG_CAT, <<"providers">>, []),
    exec_providers_save(Providers, JObj, PriorJObj, Number, State, []).

exec_providers_save([], JObj, _, _, _, []) ->
    {ok, JObj};
exec_providers_save([], _, _, _, _, Result) ->
    {error, wh_json:from_list(Result)};
exec_providers_save([Provider|Providers], JObj, PriorJObj, Number, State, Result) ->
    try
        lager:debug("executing provider ~s", [Provider]),
        case wnm_util:try_load_module(<<"wnm_", Provider/binary>>) of
            false ->
                lager:debug("provider ~s is unknown, skipping", [Provider]),
                exec_providers_save(Providers, JObj, PriorJObj, Number, State, Result);
            Mod ->
                case Mod:save(JObj, PriorJObj, Number, State) of
                    {ok, J} ->
                        exec_providers_save(Providers, J, PriorJObj, Number, State, Result);
                    {error, Error} ->
                        lager:debug("provider ~s created error: ~p", [Provider, Error]),
                        exec_providers_save(Providers, JObj, PriorJObj, Number, State, [{Provider, Error}|Result])
                end
        end
    catch
        _:R ->
            lager:debug("executing provider ~s threw exception: ~p", [Provider, R]),
            exec_providers_save(Providers, JObj, PriorJObj, Number, State, [{Provider, <<"threw exception">>}|Result])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check all the fields that might have an account id in hierarchical
%% order
%% @end
%%--------------------------------------------------------------------
-spec find_account_id/1 :: (wh_json:json_object()) -> 'undefined' | ne_binary().
find_account_id(JObj) ->
    SearchFuns = [fun(_) -> wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj) end
                  ,fun(undefined) -> wh_json:get_ne_value(<<"pvt_reserved_for">>, JObj);
                      (Else) -> Else
                   end
                  ,fun(undefined) -> wh_json:get_ne_value(<<"pvt_previously_assigned_to">>, JObj);
                      (Else) -> Else
                   end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, undefined, SearchFuns).
