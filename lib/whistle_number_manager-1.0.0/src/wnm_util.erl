%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_util).

-export([is_tollfree/1]).
-export([is_reconcilable/1]).
-export([list_carrier_modules/0]).
-export([get_carrier_module/1]).
-export([try_load_module/1]).
-export([number_to_db_name/1]).
-export([normalize_number/1]).
-export([to_e164/1, to_npan/1, to_1npan/1]).
-export([is_e164/1, is_npan/1, is_1npan/1]).
-export([exec_providers_save/4]).
-export([update_numbers_on_account/1
         ,update_numbers_on_account/2
         ,update_numbers_on_account/3
        ]).

-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determines if a given number is reconcilable
%% @end
%%--------------------------------------------------------------------
-spec is_reconcilable/1 :: (ne_binary()) -> boolean().
is_reconcilable(Number) ->
    Regex = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"reconcile_regex">>, <<"^\\+{0,1}1{0,1}(\\d{10})$">>),
    Num = wnm_util:normalize_number(Number),
    case re:run(Num, Regex) of
        nomatch ->
            lager:debug("number '~s' is not reconcilable", [Num]),
            false;
        _ ->
            lager:debug("number '~s' can be reconciled, proceeding", [Num]),
            true
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determines if a given number is tollfree
%% @end
%%--------------------------------------------------------------------
-spec is_tollfree/1 :: (ne_binary()) -> boolean().
is_tollfree(Number) ->
    Num = normalize_number(Number),
    Regex = whapps_config:get(?WNM_CONFIG_CAT, <<"is_tollfree_regex">>, ?WNM_DEAFULT_TOLLFREE_RE),
    case re:run(Num, Regex) of
        nomatch -> false;
        _ -> true
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a number doc determine if the carrier module is available
%% and if return the name as well as the data
%% @end
%%--------------------------------------------------------------------
-spec get_carrier_module/1 :: (wh_json:json_object()) -> {ok, atom(), wh_json:json_object()} 
                                                     | {error, not_specified | unknown_module}.
get_carrier_module(JObj) ->
    case wh_json:get_value(<<"pvt_module_name">>, JObj) of
        undefined -> {error, not_specified};
        Module ->
            Carriers = list_carrier_modules(),
            Carrier = try_load_module(Module),
            case lists:member(Carrier, Carriers) of
                true -> {ok, Carrier, wh_json:get_value(<<"pvt_module_data">>, JObj, wh_json:new())};
                false -> {error, unknown_module}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec list_carrier_modules/0 :: () -> [] | [atom(),...].
list_carrier_modules() ->
    CarrierModules = 
        whapps_config:get(?WNM_CONFIG_CAT, <<"carrier_modules">>, ?WNM_DEAFULT_CARRIER_MODULES),
    [Module || M <- CarrierModules, (Module = try_load_module(M)) =/= false].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a number determine the database name that it should belong to.
%% @end
%%--------------------------------------------------------------------
-spec number_to_db_name/1 :: (ne_binary()) -> ne_binary().
number_to_db_name(<<NumPrefix:5/binary, _/binary>>) ->
    wh_util:to_binary(
      http_uri:encode(
        wh_util:to_list(
          list_to_binary([?WNM_DB_PREFIX, NumPrefix])
         )
       )
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a provided term into a e164 binary string.
%% @end
%%--------------------------------------------------------------------
-spec normalize_number/1 :: (string() | binary()) -> binary().
normalize_number(Number) when is_binary(Number) ->
    to_e164(Number);
normalize_number(Number) ->
    normalize_number(wh_util:to_binary(Number)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a module name try to verify its existance, loading it into the
%% the vm if possible.
%% @end
%%--------------------------------------------------------------------
-spec try_load_module/1 :: (string() | binary()) -> atom() | false.
try_load_module(Name) ->
    try
        Module  = wh_util:to_atom(Name),
        case erlang:module_loaded(Module) of
            true -> 
                Module;
            false -> 
                {module, Module} = code:ensure_loaded(Module),
                Module
        end
    catch
        error:badarg ->
            lager:debug("carrier module ~s not found", [Name]),
            case code:where_is_file(wh_util:to_list(<<Name/binary, ".beam">>)) of
                non_existing ->
                    lager:debug("beam file not found for ~s", [Name]),
                    false;
                _Path ->
                    lager:debug("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Name, true), %% put atom into atom table
                    try_load_module(Name)
            end;
        _:_ ->
            false
    end.

-spec is_e164/1 :: (ne_binary()) -> boolean().
-spec is_npan/1 :: (ne_binary()) -> boolean().
-spec is_1npan/1 :: (ne_binary()) -> boolean().

is_e164(DID) ->
    re:run(DID, <<"^\\+1\\d{10}$">>) =/= nomatch.

is_npan(DID) ->
    re:run(DID, <<"^\\d{10}$">>) =/= nomatch.

is_1npan(DID) ->
    re:run(DID, <<"^1\\d{10}$">>) =/= nomatch.

%% +18001234567 -> +18001234567
-spec to_e164/1 :: (ne_binary()) -> ne_binary().
to_e164(<<$+, _/binary>> = N) ->
    N;
to_e164(<<"011", N/binary>>) ->
    <<$+, N/binary>>;
to_e164(<<"00", N/binary>>) ->
    <<$+, N/binary>>;
to_e164(<<"+1", _/binary>> = N) when erlang:byte_size(N) =:= 12 ->
    N;
%% 18001234567 -> +18001234567
to_e164(<<$1, _/binary>> = N) when erlang:byte_size(N) =:= 11 ->
    << $+, N/binary>>;
%% 8001234567 -> +18001234567
to_e164(N) when erlang:byte_size(N) =:= 10 ->
    <<$+, $1, N/binary>>;
to_e164(Other) ->
    Other.

%% end up with 8001234567 from 1NPAN and E.164
-spec to_npan/1 :: (ne_binary()) -> ne_binary().
to_npan(<<"011", N/binary>>) ->
    to_npan(N);
to_npan(<<$+, $1, N/binary>>) when erlang:byte_size(N) =:= 10 ->
    N;
to_npan(<<$1, N/binary>>) when erlang:byte_size(N) =:= 10 ->
    N;
to_npan(NPAN) when erlang:byte_size(NPAN) =:= 10 ->
    NPAN;
to_npan(Other) ->
    Other.

-spec to_1npan/1 :: (ne_binary()) -> ne_binary().
to_1npan(<<"011", N/binary>>) ->
    to_1npan(N);
to_1npan(<<$+, $1, N/binary>>) when erlang:byte_size(N) =:= 10 ->
    <<$1, N/binary>>;
to_1npan(<<$1, N/binary>>=NPAN1) when erlang:byte_size(N) =:= 10 ->
    NPAN1;
to_1npan(NPAN) when erlang:byte_size(NPAN) =:= 10 ->
    <<$1, NPAN/binary>>;
to_1npan(Other) ->
    Other.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% execute the save function of all providers, folding the jobj through
%% them and collecting any errors...
%% @end
%%--------------------------------------------------------------------
-spec exec_providers_save/4 :: (wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary()) -> {'ok' | 'error', wh_json:json_object()}.
-spec exec_providers_save/6 :: (list(), wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary(), list()) -> {'ok' | 'error', wh_json:json_object()}.

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
        case try_load_module(<<"wnm_", Provider/binary>>) of
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
%% @public
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
    {error, no_account_id};
update_numbers_on_account(Number, State, AccountId) ->
    Db = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {error, _R}=E ->
            lager:debug("failed to load account definition when adding '~s': ~p", [Number, _R]),
            E;
        {ok, JObj} ->
            lager:debug("updating number ~s pvt state to '~s' on account definition ~s", [Number, State, AccountId]),
            Migrate = wh_json:get_value(<<"pvt_wnm_numbers">>, JObj, []),
            Updated = lists:foldl(fun(<<"numbers">>, J) when Migrate =/= [] ->
                                          N = wh_json:get_value(<<"pvt_wnm_in_service">>, J, []),
                                          wh_json:set_value(<<"pvt_wnm_in_service">>, N ++ Migrate,
                                                            wh_json:delete_key(<<"pvt_wnm_numbers">>, J));
                                     (<<"numbers">>, J) when Migrate =:= [] ->
                                          wh_json:delete_key(<<"pvt_wnm_numbers">>, J);
                                     (S, J) when S =:= State ->
                                          N = wh_json:get_value(<<"pvt_wnm_", S/binary>>, JObj, []),
                                          wh_json:set_value(<<"pvt_wnm_", S/binary>>, [Number|lists:delete(Number,N)], J);
                                     (S, J) ->
                                          N = wh_json:get_value(<<"pvt_wnm_", S/binary>>, JObj, []),
                                          wh_json:set_value(<<"pvt_wnm_", S/binary>>, lists:delete(Number,N), J)
                                  end, JObj, [<<"numbers">>|?WNM_NUMBER_STATUS]),
            Cleaned = lists:foldl(fun(S, J) ->
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

%% PROPER TESTING
%%
%% (AAABBBCCCC, 1AAABBBCCCC) -> AAABBBCCCCCC.
prop_to_npan() ->
    ?FORALL(Number, range(1000000000,19999999999),
            begin
                BinNum = wh_util:to_binary(Number),
                NPAN = to_npan(BinNum),
                case byte_size(BinNum) of
                    11 -> BinNum =:= <<"1", NPAN/binary>>;
                    _ -> NPAN =:= BinNum
                end
            end).

%% (AAABBBCCCC, 1AAABBBCCCC) -> 1AAABBBCCCCCC.
prop_to_1npan() ->
    ?FORALL(Number, range(1000000000,19999999999),
            begin
                BinNum = wh_util:to_binary(Number),
                OneNPAN = to_1npan(BinNum),
                case byte_size(BinNum) of
                    11 -> OneNPAN =:= BinNum;
                    _ -> OneNPAN =:= <<"1", BinNum/binary>>
                end
            end).

%% (AAABBBCCCC, 1AAABBBCCCC) -> +1AAABBBCCCCCC.
prop_to_e164() ->
    ?FORALL(Number, range(1000000000,19999999999),
            begin
                BinNum = wh_util:to_binary(Number),
                E164 = to_e164(BinNum),
                case byte_size(BinNum) of
                    11 -> E164 =:= <<$+, BinNum/binary>>;
                    10 -> E164 =:= <<$+, $1, BinNum/binary>>;
                    _ -> E164 =:= BinNum
                end
            end).

%% EUNIT TESTING
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {timeout, 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
      ]}}.

to_e164_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"+11234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_e164(N), Ans) end, Ns).

to_npan_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"1234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_npan(N), Ans) end, Ns).

to_1npan_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"11234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_1npan(N), Ans) end, Ns).

-endif.
