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
-export([assign_number_to_account/2, assign_number_to_account/3]).
-export([get_public_fields/2, set_public_fields/3]).
-export([list_attachments/2]).
-export([fetch_attachment/3]).
-export([delete_attachment/3]).
-export([put_attachment/4, put_attachment/5]).
-export([reserve_number/2, reserve_number/3]).
-export([lookup_account_by_number/1]).
-export([release_number/2]).
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
%% Lists attachments on a number
%% @end
%%--------------------------------------------------------------------
-spec list_attachments/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
list_attachments(Number, AccountId) ->
    lager:debug("attempting to list attachements on ~s for account ~s", [Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, R1} -> 
            lager:debug("failed to open number DB: ~p", [R1]),
            {error, not_found};
        {ok, JObj} -> 
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> -> 
                    case wh_json:get_value(<<"pvt_reserved_for">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to list attachments on a reserved number"),
                            {ok, wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())};
                        _ ->
                            lager:debug("number is reserved for another account"),
                            {error, reserved}
                    end;
                <<"in_service">> -> 
                    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to list attachments"),
                            {ok, wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())};
                        _ ->
                            lager:debug("number belongs to another account"),
                            {error, unauthorized}
                    end;
                Else -> 
                    lager:debug("disallowing listing attachments for a number in state ~s", [Else]),
                    {error, unauthorized}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch an attachment on a number
%% @end
%%--------------------------------------------------------------------
-spec fetch_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.

fetch_attachment(Number, AccountId, Name) ->
    lager:debug("attempting to fetch attachement ~s on ~s for account ~s", [Name, Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, R1} -> 
            lager:debug("failed to open number DB: ~p", [R1]),
            {error, not_found};
        {ok, JObj} -> 
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> -> 
                    case wh_json:get_value(<<"pvt_reserved_for">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to fetch attachment on a reserved number"),
                            couch_mgr:fetch_attachment(Db, Num, Name);
                        _ ->
                            lager:debug("number is reserved for another account"),
                            {error, reserved}
                    end;
                <<"in_service">> -> 
                    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to fetch an attachment"),
                            couch_mgr:fetch_attachment(Db, Num, Name);
                        _ ->
                            lager:debug("number belongs to another account"),
                            {error, unauthorized}
                    end;
                Else -> 
                    lager:debug("disallowing attachment to a number in state ~s", [Else]),
                    {error, unauthorized}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec put_attachment/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
-spec put_attachment/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), proplist()) 
                          -> {'ok', wh_json:json_object()} | {'error', atom}.

put_attachment(Number, AccountId, Name, Content) ->
    put_attachment(Number, AccountId, Name, Content, []).    

put_attachment(Number, AccountId, Name, Content, Options) ->
    lager:debug("attempting to add an attachement to ~s for account ~s", [Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, R1} -> 
            lager:debug("failed to open number DB: ~p", [R1]),
            {error, not_found};
        {ok, JObj} -> 
            Rev = wh_json:get_value(<<"_rev">>, JObj),
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> -> 
                    case wh_json:get_value(<<"pvt_reserved_for">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to add an attachment to a reserved number"),
                            case couch_mgr:put_attachment(Db, Num, Name, Content, [{rev, Rev}|Options]) of
                                {ok, _} ->
                                    Attachments = wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())),
                                    {ok, wh_json:public_fields(wh_json:set_value(<<"attachments">>
                                                                                     ,[Name|lists:delete(Name, Attachments)]
                                                                                 ,JObj))};
                                Else ->
                                    Else
                            end;
                        _ ->
                            lager:debug("number is reserved for another account"),
                            {error, reserved}
                    end;
                <<"in_service">> -> 
                    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to add an attachment"),
                            case couch_mgr:put_attachment(Db, Num, Name, Content, [{rev, Rev}|Options]) of
                                {ok, _} -> 
                                    Attachments = wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())),
                                    {ok, wh_json:public_fields(wh_json:set_value(<<"attachments">>
                                                                                     ,[Name|lists:delete(Name, Attachments)]
                                                                                 ,JObj))};
                                Else ->
                                    Else
                            end;
                        _ ->
                            lager:debug("number belongs to another account"),
                            {error, unauthorized}
                    end;
                Else -> 
                    lager:debug("disallowing attachment to a number in state ~s", [Else]),
                    {error, unauthorized}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add an attachment to a number
%% @end
%%--------------------------------------------------------------------
-spec delete_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.

delete_attachment(Number, AccountId, Name) ->
    lager:debug("attempting to delete attachement ~s from ~s for account ~s", [Name, Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    case couch_mgr:open_doc(Db, Num) of
        {error, R1} -> 
            lager:debug("failed to open number DB: ~p", [R1]),
            {error, not_found};
        {ok, JObj} -> 
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> -> 
                    case wh_json:get_value(<<"pvt_reserved_for">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to delete attachment from a reserved number"),
                            case couch_mgr:delete_attachment(Db, Num, Name) of
                                {ok, _} ->
                                    Attachments = wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())),
                                    {ok, wh_json:public_fields(wh_json:set_value(<<"attachments">>
                                                                                     ,lists:delete(Name, Attachments)
                                                                                 ,JObj))};
                                Else ->
                                    Else
                            end;
                        _ ->
                            lager:debug("number is reserved for another account"),
                            {error, reserved}
                    end;
                <<"in_service">> -> 
                    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                        AccountId -> 
                            lager:debug("allowing account to delete an attachment"),
                            case couch_mgr:delete_attachment(Db, Num, Name) of
                                {ok, _} -> 
                                    Attachments = wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())),
                                    {ok, wh_json:public_fields(wh_json:set_value(<<"attachments">>
                                                                                     ,lists:delete(Name, Attachments)
                                                                                 ,JObj))};
                                Else ->
                                    Else
                            end;
                        _ ->
                            lager:debug("number belongs to another account"),
                            {error, unauthorized}
                    end;
                Else -> 
                    lager:debug("disallowing delete attachment from a number in state ~s", [Else]),
                    {error, unauthorized}
            end
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
reconcile_number(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    lager:debug("attempting to reconcile number '~s' with account '~s'", [Num, AccountId]),
    try 
        case wnm_util:is_reconcilable(Num) of 
            false -> throw(not_reconcilable);
            true -> ok
        end,
        JObj = case store_discovery(Num, <<"wnm_local">>, wh_json:new(), {<<"reserved">>, AccountId}) of
                    {ok, J1} -> J1;
                    {error, {conflict, J2}} -> J2;
                    {error, R} -> throw(R)
                end,
        case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
            AccountId -> 
                lager:debug("number already exists and is routed to the account"),
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
%% Add and reserve a number for an account
%% @end
%%--------------------------------------------------------------------
-spec reserve_number/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} | {error, term()}.
-spec reserve_number/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.

reserve_number(Number, AccountId) ->
    reserve_number(Number, AccountId, wh_json:new()).

reserve_number(Number, AccountId, PublicFields) ->
    Num = wnm_util:normalize_number(Number),
    case wnm_util:is_reconcilable(Num) of 
        false -> {error, not_reconcilable};
        true -> 
            case store_discovery(Num, <<"wnm_local">>, wh_json:new(), {<<"reserved">>, AccountId}, PublicFields) of
                {error, {conflict, _}} -> {error, conflict};
                {ok, JObj} -> {ok, wh_json:public_fields(JObj)};
                Else -> Else
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign a number to an account, aquiring the number from the provider
%% if necessary
%% @end
%%--------------------------------------------------------------------
-spec assign_number_to_account/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                                  {error, atom()}.
assign_number_to_account(Number, AccountId) ->
    assign_number_to_account(Number, AccountId, undefined).

assign_number_to_account(Number, AccountId, PublicFields) ->
    lager:debug("attempting to assign ~s to account ~s", [Number, AccountId]),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    try
        case wnm_util:is_reconcilable(Num) of 
            false -> throw(not_reconcilable);
            true -> ok
        end,
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, R1} -> 
                        lager:debug("failed to open number DB: ~p", [R1]),
                        throw(not_found);
                   {ok, J} -> J
               end,
        {Module, ModuleData} = case wnm_util:get_carrier_module(JObj1) of
                                   {error, not_specified} -> 
                                       lager:debug("carrier module not specified on number document"),
                                       throw(unknown_carrier);
                                   {error, unknown_module} -> 
                                       lager:debug("carrier module specified on number document does not exist"),
                                       throw(unknown_carrier);
                                   {ok, Mod, Data1} -> 
                                       {Mod, wh_json:set_value(<<"acquire_for">>, AccountId, Data1)}
                               end,
        NumberState = case wh_json:get_value(<<"pvt_number_state">>, JObj1, <<"unknown">>) of
                          <<"reserved">> -> 
                              case wh_json:get_value(<<"pvt_reserved_for">>, JObj1) of
                                  AccountId -> 
                                      lager:debug("allowing account to claim a reserved number"),
                                      <<"claim">>;
                                  _ ->
                                      lager:debug("number is reserved for another account"),
                                      throw(reserved)
                              end;
                          <<"in_service">> -> 
                              lager:debug("number is already in service"),
                              throw(unavailable);
                          Else -> 
                              lager:debug("allowing assignment of a number currently in state ~s", [Else]),
                              Else
                      end,
        {NewNumberState, NewModuleData} = case Module:acquire_number(Num, NumberState, ModuleData) of
                                              {error, Error} -> 
                                                  lager:debug("module failed to acquire number: ~p", [Error]),
                                                  throw(Error);
                                              {ok, State, Data2} -> 
                                                  {State, Data2}
                                          end,
        Updaters = [fun(J) -> wh_json:set_value(<<"pvt_number_state">>, NewNumberState, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, NewModuleData, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_assigned_to">>, AccountId, J) end
                    ,fun(J) when is_atom(PublicFields) -> J;
                        (J) -> wh_json:merge_jobjs(wh_json:private_fields(J), PublicFields)
                     end
                   ],
        save_number(Db, Num, lists:foldr(fun(F, J) -> F(J) end, JObj1, Updaters), JObj1)
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
            lager:debug("failed to lookup number: ~p", [R2]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the user configurable fields
%% @end
%%--------------------------------------------------------------------
-spec get_public_fields/2 :: (ne_binary(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                           {error, atom()}.
get_public_fields(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to lookup '~s' in '~s'", [Num, Db]),
    case couch_mgr:open_doc(Db, Num) of
        {ok, JObj} -> 
            case wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>) of
                <<"reserved">> -> 
                    case wh_json:get_value(<<"pvt_reserved_for">>, JObj) of
                        AccountId ->
                            lager:debug("found number assigned to ~s", [AccountId]),
                            Attachments = wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())),
                            {ok, wh_json:public_fields(wh_json:set_value(<<"attachments">>, Attachments, JObj))};
                        _Else ->
                            lager:debug("found number was not assigned to ~s, returning unauthorized", [AccountId]),
                            {error, unauthorized}
                    end;
                <<"in_service">> -> 
                    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
                        AccountId ->
                            lager:debug("found number assigned to ~s", [AccountId]),
                            Attachments = wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())),
                            {ok, wh_json:public_fields(wh_json:set_value(<<"attachments">>, Attachments, JObj))};
                        _Else ->
                            lager:debug("found number was not assigned to ~s, returning unauthorized", [AccountId]),
                            {error, unauthorized}
                    end;
                Else -> 
                    lager:debug("disallowing attachment to a number in state ~s", [Else]),
                    {error, unauthorized}
            end;
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
-spec set_public_fields/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} |
                                                                          {error, atom()}.
set_public_fields(Number, AccountId, Data) ->
    PublicJObj = wh_json:delete_key(<<"attachments">>, Data),
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    lager:debug("attempting to lookup '~s' in '~s'", [Num, Db]),
    try
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, R1} -> 
                        lager:debug("failed to lookup number: ~p", [R1]),
                        throw(not_found);
                   {ok, J} -> 
                        case wh_json:get_value(<<"pvt_number_state">>, J, <<"unknown">>) of
                            <<"reserved">> -> 
                                case wh_json:get_value(<<"pvt_reserved_for">>, J) of
                                    AccountId ->
                                        lager:debug("found number assigned to ~s", [AccountId]),
                                        J;
                                    _Else ->
                                        lager:debug("found number was not assigned to ~s, returning unauthorized", [AccountId]),
                                        throw(unauthorized)
                                end;
                            <<"in_service">> -> 
                                case wh_json:get_value(<<"pvt_assigned_to">>, J) of
                                    AccountId ->
                                        lager:debug("found number assigned to ~s", [AccountId]),
                                        J;
                                    _Else ->
                                        lager:debug("found number was not assigned to ~s, returning unauthorized", [AccountId]),
                                        throw(unauthorized)
                                end;
                            Else -> 
                                lager:debug("disallowing attachment to a number in state ~s", [Else]),
                                throw(unauthorized)
                        end
               end,
        save_number(Db, Num, wh_json:merge_jobjs(wh_json:private_fields(JObj1), PublicJObj), JObj1)
    catch
        throw:Reason -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% move an in service number to the released state were it will be 
%% recycled or cancled after a buffer period
%% @end
%%--------------------------------------------------------------------
-spec release_number/2 :: (ne_binary(), ne_binary()) -> ok | {error, atom()}.
release_number(Number, AccountId) ->
    Num = wnm_util:normalize_number(Number),
    Db = wnm_util:number_to_db_name(Num),
    try
        case wnm_util:is_reconcilable(Num) of 
            false -> throw(not_reconcilable);
            true -> ok
        end,
        ReleaseState = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"released_state">>, <<"available">>),
        JObj1 = case couch_mgr:open_doc(Db, Num) of
                   {error, R1} -> 
                        lager:debug("failed to open number DB: ~p", [R1]),
                        throw(not_found);
                   {ok, J} -> J
               end,
        {Module, ModuleData} = case wnm_util:get_carrier_module(JObj1) of
                                   {error, not_specified} -> 
                                       lager:debug("carrier module not specified on number document"),
                                       throw(unknown_carrier);
                                   {error, unknown_module} -> 
                                       lager:debug("carrier module specified on number document does not exist"),
                                       throw(unknown_carrier);
                                   {ok, Mod, Data1} -> 
                                       {Mod, Data1}
                               end,
        case wh_json:get_value(<<"pvt_number_state">>, JObj1, <<"unknown">>) of
            <<"in_service">> -> 
                case wh_json:get_value(<<"pvt_assigned_to">>, JObj1) of
                    AccountId -> 
                        lager:debug("allowing account to free their in service numbers"),
                        ok;
                    _ ->
                        lager:debug("number is in service for another account"),
                        throw(unauthorized)
                end;
            _Else -> 
                lager:debug("number is not in service"),
                throw(unavailable)
        end,
        NewModuleData = case Module:release_number(Num, ModuleData) of
                            {error, Error} -> 
                                lager:debug("module failed to release number: ~p", [Error]),
                                throw(Error);
                            {ok, Data2} -> 
                                Data2
                        end,
        Updaters = [fun(J) -> wh_json:set_value(<<"pvt_number_state">>, ReleaseState, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_module_data">>, NewModuleData, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                    ,fun(J) -> wh_json:delete_key(<<"pvt_assigned_to">>, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_previously_assigned_to">>, AccountId, J) end
                   ],
        save_number(Db, Num, lists:foldr(fun(F, J) -> F(J) end, wh_json:private_fields(JObj1), Updaters), JObj1),
        ok
    catch
        throw:not_found ->
            update_numbers_on_account(Num, <<"released">>, AccountId),
            ok;
        throw:unauthorized ->
            update_numbers_on_account(Num, <<"released">>, AccountId),
            ok;
        throw:unavailable ->
            update_numbers_on_account(Num, <<"released">>, AccountId),
            ok;
        _:_ ->
            {error, fault}
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
                 [ok = release_number(Number, AccountId) 
                  || Number <- wh_json:get_value(<<"pvt_wnm_in_service">>, JObj, [])
                         ++ wh_json:get_value(<<"pvt_wnm_in_reserved">>, JObj, [])
                 ],
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
    case store_discovery(Number, ModuleName, ModuleData) of
        {error, {conflict, JObj}} ->
            State = wh_json:get_value(<<"pvt_number_state">>, JObj),
            case lists:member(State, ?WNM_AVALIABLE_STATES) of
                true -> 
                    prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
                false -> 
                    lager:debug("the discovery '~s' is not available: ~s", [Number, State]),
                    prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
            end;
        {ok, _} ->
            prepare_find_results(Numbers, ModuleName, ModuleResults, [Number|Found]);
        _Else ->
            lager:debug("the discovery '~s' could not be stored, ignoring", [Number, _Else]),
            prepare_find_results(Numbers, ModuleName, ModuleResults, Found)
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Store a newly discovered number (first time)
%% @end
%%--------------------------------------------------------------------
-spec store_discovery/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.
-spec store_discovery/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), ne_binary() | {ne_binary(), ne_binary()}) -> {ok, wh_json:json_object()} | {error, term()}.
-spec store_discovery/5 :: (ne_binary(), ne_binary(), wh_json:json_object(), ne_binary() | {ne_binary(), ne_binary()}, wh_json:json_object()) 
                           -> {ok, wh_json:json_object()} | {error, term()}.

store_discovery(Number, ModuleName, ModuleData) ->
    store_discovery(Number, ModuleName, ModuleData, <<"discovery">>).

store_discovery(Number, ModuleName, ModuleData, State) ->
    store_discovery(Number, ModuleName, ModuleData, State, wh_json:new()).

store_discovery(Number, ModuleName, ModuleData, State, PublicFields) ->
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
    JObj = lists:foldr(fun(F, J) -> F(J) end, PublicFields, Generators),
    case save_number(Db, Number, JObj, wh_json:new()) of
        {ok, _}=Ok ->
            lager:debug("stored newly discovered number '~s'", [Number]),
            Ok;
        {error, not_found} ->
            lager:debug("storing discovered number '~s' in a new database '~s'", [Number, Db]),
            couch_mgr:db_create(Db),
            couch_mgr:revise_views_from_folder(Db, whistle_number_manager),
            save_number(Db, Number, JObj, wh_json:new());
        {error, conflict} ->
            case couch_mgr:open_doc(Db, Number) of
                {ok, Conflict} ->
                    lager:debug("discovered number '~s' exists in database '~s'", [Number, Db]),
                    {error, {conflict, Conflict}};
                _Else ->
                    lager:debug("discovered number '~s' was in conflict but opening the existing failed: ~p", [Number, _Else]),
                    {error, conflict}
            end;
        Else ->
            lager:debug("storing the discovered number '~s' failed: ~p", [Number, Else]),
            Else
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
-spec update_numbers_on_account/3 :: (ne_binary(), ne_binary(), undefined | ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                            {error, term()}.

update_numbers_on_account(JObj) ->
    State = wh_json:get_value(<<"pvt_number_state">>, JObj, <<"unknown">>),
    Number = wh_json:get_value(<<"_id">>, JObj),
    AccountId = wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj
                                     ,wh_json:get_ne_value(<<"pvt_reserved_for">>, JObj)),
    update_numbers_on_account(Number, State, AccountId).

update_numbers_on_account(_, _, undefined) ->
    {error, not_account_id};
update_numbers_on_account(Number, State, AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
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
                Else ->
                    lager:debug("failed to save account definition when adding '~s': ~p", [Number, Else]),
                    Else
            end;
        Else ->
            lager:debug("failed to load account definition when adding '~s': ~p", [Number, Else]),
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to save the number, add to the account doc if required,
%% and run any providers 
%% @end
%%--------------------------------------------------------------------
-spec save_number/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), wh_json:json_object()) -> {ok, wh_json:json_object()} | {error, term()}.
save_number(Db, Number, JObj1, PriorJObj) ->
    lager:debug("attempting to save '~s' in '~s'", [Number, Db]),
    CallId = get(callid),
    case couch_mgr:save_doc(Db, JObj1) of
        {ok, JObj2} -> 
            State = wh_json:get_value(<<"pvt_number_state">>, JObj2),
            spawn(fun() ->
                          put(callid, CallId),
                          update_numbers_on_account(JObj2)
                  end),
            case exec_providers_save(JObj2, PriorJObj, Number, State) of
                {JObj3, []} ->
                    case couch_mgr:save_doc(Db, JObj3) of
                        {ok, J} -> {ok, wh_json:public_fields(J)};
                        {error, R3}=E -> 
                            lager:debug("failed to re-save number ~s: ~p", [Number, R3]),
                            E
                    end;
                {_, Errors} ->
                  {error, wh_json:from_list(Errors)}
            end;        
        {error, R2}=E -> 
            lager:debug("failed to save number ~s: ~p", [Number, R2]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% execute the save function of all providers, folding the jobj through
%% them and collecting any errors...
%% @end
%%--------------------------------------------------------------------
-spec exec_providers_save/4 :: (wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary()) 
                               -> {wh_json:json_object(), proplist()}.
-spec exec_providers_save/6 :: (list(), wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary(), list()) 
                               -> {wh_json:json_object(), proplist()}.

exec_providers_save(JObj, PriorJObj, Number, State) ->
    Providers = whapps_config:get(?WNM_CONFIG_CAT, <<"providers">>, []),
    exec_providers_save(Providers, JObj, PriorJObj, Number, State, []).

exec_providers_save([], JObj, _, _, _, Result) ->
    {JObj, Result};
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
