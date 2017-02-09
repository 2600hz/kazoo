%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz INC
%%% @doc
%%%   Bulk operations on numbers.
%%%   Note: functions should not `throw`, instead return `ret()`.
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_numbers).

-export([todo/1
        ,options/1, options/2
        ,plan/1, plan/2
        ,services/1, services/2
        ,transactions/1, transactions/2, transaction/2
        ,charges/1, charges/2, charge/2, charge/3
        ]).
-export([ok/2, ko/3]).
-export([add_oks/2]).
-export([assigned_to/1
        ,prev_assigned_to/1
        ,to_json/1
        ]).

-export([get/1, get/2
        ,create/2
        ,move/2, move/3
        ,update/2, update/3
        ,release/1, release/2
        ,delete/2
        ,reconcile/2
        ,reserve/2

        ,assign_to_app/2, assign_to_app/3

        ,free/1
        ,emergency_enabled/1

        ,account_listing/1
        ]).

-export([pipe/2]).
-export([do/2]).
-export([merge_okkos/2, merge_okkos/1]).
-export([from_jobjs/1]).

-include("knm.hrl").

-type num() :: ne_binary().  %%TODO: support ranges?
-type nums() :: [num()].
-type ok() :: knm_number:knm_number().
-type oks() :: [ok()].
-type ko() :: knm_errors:error() | atom().
-type kos() :: #{num() => ko()}.
-type ret() :: #{ok => oks()
                ,ko => kos()
                ,services => services()
                ,charges => charges()
                ,options => options()
                }.

-export_type([ret/0
             ,ok/0, oks/0
             ,ko/0, kos/0
             ,num/0, nums/0
             ]).

-type t() :: t(oks()).
-type t(OKs) :: t(OKs, OKs).
-type t(OKs, TODOs) :: #{todo => nums() | TODOs
                        ,ok => OKs
                        ,ko => kos()

                        ,options => options()
                        ,plan => plan()
                        ,services => services()
                        ,transactions => transactions()
                        ,charges => charges()
                        }.

-type t_pn() :: t(knm_phone_number:knm_phone_number()).

-opaque collection() :: t().
-export_type([collection/0]).

-type options() :: knm_number_options:options().
-type plan() :: kz_service_plans:plan() | undefined.
-type services() :: kz_services:services() | undefined.
-type transaction() :: kz_transaction:transaction().
-type transactions() :: kz_transaction:transactions().
-type charges() :: [{ne_binary(), non_neg_integer()}].

-export_type([options/0
             ,plan/0
             ,services/0
             ,transaction/0, transactions/0
             ,charges/0
             ]).

-type applier() :: applier(t()).
-type applier(A) :: fun((A) -> A).
-type appliers() :: [applier()].
-type appliers(A) :: [applier(A)].

%% @private
-spec num(knm_number:knm_number()) -> num().
num(N) ->
    knm_phone_number:number(knm_number:phone_number(N)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Either `nums()' xor `oks()'.
%% @end
%%--------------------------------------------------------------------
-spec todo(t()) -> nums() | oks().
todo(#{todo := ToDo}) -> ToDo.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set of numbers' assigned_to fields.
%% @end
%%--------------------------------------------------------------------
-spec assigned_to(t()) -> api_ne_binary().
assigned_to(#{todo := Ns}) ->
    F = fun (N, S) ->
                case knm_phone_number:assigned_to(knm_number:phone_number(N)) of
                    undefined -> S;
                    ?MATCH_ACCOUNT_RAW(AccountId) -> sets:add_element(AccountId, S)
                end
        end,
    case sets:to_list(lists:foldl(F, sets:new(), Ns)) of
        [] -> undefined;
        [AccountId] -> AccountId
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set of numbers' prev_assigned_to fields.
%% @end
%%--------------------------------------------------------------------
-spec prev_assigned_to(t()) -> api_ne_binary().
prev_assigned_to(#{todo := Ns}) ->
    F = fun (N, S) ->
                case knm_phone_number:prev_assigned_to(knm_number:phone_number(N)) of
                    undefined -> S;
                    ?MATCH_ACCOUNT_RAW(AccountId) -> sets:add_element(AccountId, S)
                end
        end,
    case sets:to_list(lists:foldl(F, sets:new(), Ns)) of
        [] -> undefined;
        [AccountId] -> AccountId
    end.

%% @public
-spec options(t()) -> options().
-spec options(options(), t()) -> t().
options(#{options := V}) -> V.
options(V, T) -> T#{options => V}.

%% @public
-spec plan(t()) -> plan().
-spec plan(plan(), t()) -> t().
plan(#{plan := V}) -> V.
plan(V, T) -> T#{plan => V}.

%% @public
-spec services(t()) -> services().
-spec services(services(), t()) -> t().
services(#{services := V}) -> V.
services(V, T) -> T#{services => V}.

%% @public
-spec transactions(t()) -> transactions().
-spec transactions(transactions(), t()) -> t().
transactions(#{transactions := V}) -> V.
transactions(V, T) -> T#{transactions => V}.

%% @public
-spec transaction(kz_transaction:transaction(), t()) -> t().
transaction(V, T=#{transactions := Vs}) -> T#{transactions => [V | Vs]}.

%% @public
-spec charges(t()) -> charges().
-spec charges(charges(), t()) -> t().
charges(#{charges := V}) -> V.
charges(V, T) -> T#{charges => V}.

%% @public
-spec charge(ne_binary(), t()) -> non_neg_integer().
-spec charge(ne_binary(), non_neg_integer(), t()) -> t().
charge(K, #{charges := Vs}) -> props:get_value(K, Vs, 0).
charge(K, V, T=#{charges := Vs}) -> T#{charges => [{K, V} | Vs]}.

%% @public
-spec ok(ok() | oks(), t()) -> t().
ok(Numbers, T) when is_list(Numbers) -> T#{ok => Numbers};
ok(Number, T) when not is_list(Number) ->
    T#{ok => [Number | maps:get(ok, T)]}.

%% @public
-spec add_oks(oks(), t()) -> t().
%%FIXME: unify with ok/2.
add_oks(Numbers, T=#{ok := OKs}) when is_list(Numbers) ->
    T#{ok => Numbers ++ OKs}.

%% @public
-spec ko(num() | knm_number:knm_number() | nums(), ko(), t()) -> t().
ko(?NE_BINARY=Num, Reason, T) ->
    lager:debug("number ~s error: ~p", [Num, Reason]),
    KOs = maps:get(ko, T),
    T#{ko => KOs#{Num => Reason}};
ko(Ns=[_|_], Reason, T0) ->
    F = fun (N, T) -> ko(N, Reason, T) end,
    lists:foldl(F, T0, Ns);
ko(N, Reason, T) ->
    PN = knm_number:phone_number(N),
    Num = knm_phone_number:number(PN),
    lager:debug("number ~s state: ~s", [Num, knm_phone_number:state(PN)]),
    ko(Num, Reason, T).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to get numbers from DB.
%% Note: each number in `Nums' has to be normalized.
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binaries()) -> ret().
-spec get(ne_binaries(), knm_number_options:options()) -> ret().
get(Nums) -> get(Nums, knm_number_options:default()).
get(Nums, Options) -> ret(do_get(Nums, Options)).

-spec do_get(ne_binaries(), knm_number_options:options()) -> t().
do_get(Nums, Options) ->
    {Yes, No} = are_reconcilable(Nums),
    pipe(new(Options, Yes, No)
        ,[fun knm_phone_number:fetch/1  %% fetch/1 puts PNs in "ok"!
         ,fun knm_number:new/1
         ]).

-spec do_get_pn(ne_binaries(), knm_number_options:options()) -> t_pn().
-spec do_get_pn(ne_binaries(), knm_number_options:options(), reason_t()) -> t_pn().
do_get_pn(Nums, Options) ->
    {Yes, No} = are_reconcilable(Nums),
    do(fun knm_phone_number:fetch/1, new(Options, Yes, No)).
do_get_pn(Nums, Options, Error) ->
    {Yes, No} = are_reconcilable(Nums),
    do(fun knm_phone_number:fetch/1, new(Options, Yes, No, Error)).

%% @public (used by knm_number_crawler)
-spec from_jobjs(kz_json:objects()) -> t_pn().
from_jobjs(JObjs) ->
    Options = knm_number_options:default(),
    PNs = [knm_phone_number:from_json_with_options(Doc, Options)
           || JObj <- JObjs,
              Doc <- [kz_json:get_value(<<"doc">>, JObj)],
              kz_doc:type(Doc) =:= <<"number">>
          ],
    new(Options, PNs, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to create new numbers in DB or modify existing ones.
%% Note: `assign_to' number option MUST be set.
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binaries(), knm_number_options:options()) -> ret().
create(Nums, Options) ->
    ?MATCH_ACCOUNT_RAW(AccountId) = knm_number_options:assign_to(Options), %%FIXME: can crash
    T0 = do_get_pn(Nums, Options, knm_errors:to_json(not_reconcilable)),
    case take_not_founds(T0) of
        {#{ok := []}, []} -> T0;
        {T1, NotFounds} ->
            ToState = knm_number:state_for_create(AccountId, Options),
            lager:debug("picked state ~s for ~s for ~p", [ToState, AccountId, Nums]),
            NewOptions = [{'state', ToState} | Options],
            ret(pipe(maybe_create(NotFounds, options(NewOptions, T1))
                    ,[fun knm_number:new/1
                     ,fun knm_number_states:to_options_state/1
                     ,fun save_numbers/1
                     ]))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binaries(), ne_binary()) -> ret().
-spec move(ne_binaries(), ne_binary(), knm_number_options:options()) -> ret().
move(Nums, MoveTo) ->
    move(Nums, MoveTo, knm_number_options:default()).

move(Nums, ?MATCH_ACCOUNT_RAW(MoveTo), Options0) ->
    Options = [{'assign_to', MoveTo} | Options0],
    {TFound, NotFounds} = take_not_founds(do_get(Nums, Options)),
    Updates = knm_number_options:to_phone_number_setters(Options0),
    TUpdated = do_in_wrap(fun (T) -> knm_phone_number:setters(T, Updates) end, TFound),
    {TDiscovered, NotExisting} = take_not_founds(do(fun discover/1, new(Options, NotFounds))),
    TNew = do_move_not_founds(NotExisting, Options),
    T = merge_okkos([TUpdated, TDiscovered, TNew]),
    ret(do(fun move_to/1, T)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to update some phone_number fields.
%% Note: will always result in a phone_number save.
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binaries(), knm_phone_number:set_functions()) -> ret().
-spec update(ne_binaries(), knm_phone_number:set_functions(), knm_number_options:options()) -> ret().
update(Nums, Routines) ->
    update(Nums, Routines, knm_number_options:default()).

-ifdef(TEST).
update([?NE_BINARY|_]=Nums, Routines, Options) ->
    Reason = not_reconcilable,  %% FIXME: unify to atom OR knm_error.
    do_update(do_get_pn(Nums, Options, Reason), Routines);
update(Ns, Routines, Options) ->
    T0 = new(Options, Ns),
    T1 = do_in_wrap(fun (T) -> knm_phone_number:setters(T, Routines) end, T0),
    ret(do(fun save_numbers/1, T1)).
-else.
update(Nums, Routines, Options) ->
    Reason = not_reconcilable,  %% FIXME: unify to atom OR knm_error.
    do_update(do_get_pn(Nums, Options, Reason), Routines).
-endif.

do_update(T0, Routines) ->
    ret(pipe(T0
            ,[fun (T) -> knm_phone_number:setters(T, Routines) end
             ,fun knm_number:new/1
             ,fun save_numbers/1
             ])).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec release(ne_binaries()) -> ret().
-spec release(ne_binaries(), knm_number_options:options()) -> ret().
release(Nums) ->
    release(Nums, knm_number_options:default()).

release(Nums, Options) ->
    ret(pipe(do_get_pn(Nums, Options)
            ,[fun knm_phone_number:release/1
             ,fun knm_number:new/1
             ,fun knm_providers:delete/1
             ,fun unwind_or_disconnect/1
             ,fun save_phone_numbers/1
             ])).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Remove numbers from the system without doing any state checking.
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binaries(), knm_number_options:options()) -> ret().
delete(Nums, Options) ->
    AuthBy = knm_number_options:auth_by(Options),
    case ?KNM_DEFAULT_AUTH_BY =:= AuthBy
        orelse kz_util:is_system_admin(AuthBy)
    of
        false ->
            Error = knm_errors:to_json(unauthorized),
            ret(new(Options, [], Nums, Error));
        true ->
            T0 = do_get(Nums, Options),
            F1 = fun knm_providers:delete/1,
            F2 = fun knm_phone_number:delete/1,
            ret(do_in_wrap(F2, do(F1, T0)))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Note: option 'assign_to' needs to be set.
%% @end
%%--------------------------------------------------------------------
-spec reconcile(ne_binaries(), knm_number_options:options()) -> ret().
reconcile(Nums, Options0) ->
    case knm_number_options:assign_to(Options0) =:= undefined of
        true ->
            Error = knm_errors:to_json(assign_failure, undefined, field_undefined),
            ret(new(Options0, [], Nums, Error));
        false ->
            Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY} | Options0],
            {T0, NotFounds} = take_not_founds(do_get(Nums, Options)),
            %% Ensures state to be IN_SERVICE
            Ta = do_move_not_founds(NotFounds, Options),
            Tb = do(fun (T) -> reconcile_number(T, Options) end, T0),
            ret(merge_okkos(Ta, Tb))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches then transitions existing numbers to the reserved state.
%% @end
%%--------------------------------------------------------------------
-spec reserve(ne_binaries(), knm_number_options:options()) -> ret().
reserve(Nums, Options) ->
    ret(do(fun to_reserved/1, do_get(Nums, Options))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binaries(), api_ne_binary()) -> ret().
-spec assign_to_app(ne_binaries(), api_ne_binary(), knm_number_options:options()) -> ret().
assign_to_app(Nums, App) ->
    assign_to_app(Nums, App, knm_number_options:default()).

assign_to_app(Nums, App, Options) ->
    Setters = [{fun knm_phone_number:set_used_by/2, App}],
    ret(pipe(do_get_pn(Nums, Options)
            ,[fun (T) -> knm_phone_number:setters(T, Setters) end
             ,fun knm_phone_number:save/1
             ,fun knm_number:new/1
             ])).

%%--------------------------------------------------------------------
%% @public
%% @doc Release all of an account's numbers
%% @end
%%--------------------------------------------------------------------
-spec free(ne_binary()) -> 'ok'.
free(Account=?NE_BINARY) ->
    AccountDb = kz_util:format_account_db(Account),
    {Numbers, _NumbersData} = lists:unzip(account_listing(AccountDb)),
    #{ok := Ns, ko := KOs} = release(Numbers),
    lager:debug("successfully released ~p from ~s", [[num(N) || N <- Ns], Account]),
    lists:foreach(fun ({Num, R}) ->
                          lager:error("error when releasing ~s from ~s: ~p", [Num, Account, R])
                  end
                 ,maps:to_list(KOs)
                 ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find an account's phone numbers that have emergency services enabled
%% @end
%%--------------------------------------------------------------------
-spec emergency_enabled(ne_binary()) -> ne_binaries().
emergency_enabled(AccountId=?MATCH_ACCOUNT_RAW(_)) ->
    AccountDb = kz_util:format_account_db(AccountId),
    [Num || {Num, JObj} <- account_listing(AccountDb),
            Features <- [kz_json:get_list_value(<<"features">>, JObj, [])],
            lists:member(?FEATURE_E911, Features)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% List an account's phone numbers & statuses.
%% Does not go through sub accounts.
%% @end
%%--------------------------------------------------------------------
-spec account_listing(ne_binary()) -> [{ne_binary(), kz_json:object()}].
account_listing(AccountDb=?MATCH_ACCOUNT_ENCODED(_,_,_)) ->
    case kz_datamgr:get_results(AccountDb, <<"phone_numbers/crossbar_listing">>) of
        {'ok', []} ->
            lager:debug("account ~s holds no numbers", [AccountDb]),
            [];
        {'ok', JObjs} ->
            [{kz_doc:id(JObj), kz_json:get_value(<<"value">>, JObj)}
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("error listing numbers for ~s: ~p", [AccountDb, _R])
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-type reason_t() :: atom() | fun((num())-> knm_errors:error()).
-spec new(knm_number_options:options(), nums()) -> t().
-spec new(knm_number_options:options(), nums(), nums()) -> t().
-spec new(knm_number_options:options(), nums(), nums(), reason_t()) -> t().
new(Options, ToDos) -> new(Options, ToDos, []).
new(Options, ToDos, KOs) -> new(Options, ToDos, KOs, not_reconcilable).
new(Options, ToDos, KOs, Reason) ->
    #{todo => ToDos
     ,ok => []
     ,ko => case is_function(Reason, 1) of %%FIXME: find something better than Reason/1.
                false -> maps:from_list([{KO, Reason} || KO <- KOs]);
                true -> maps:from_list([{KO, Reason(KO)} || KO <- KOs])
            end

     ,options => Options
     ,plan => undefined
     ,services => undefined
     ,transactions => []
     ,charges => []
     }.

%% @private
%% @doc
%% Apply something to "todo" if not empty,
%% if empty use "ok" as the new "todo".
%% If "ok" is empty, return.
%% Exported ONLY for inside-app use.
%% @end
-spec pipe(t(), appliers()) -> t();
          (t_pn(), appliers(t_pn())) -> t_pn().
pipe(T, []) -> T;
pipe(T=#{todo := [], ok := []}, _) -> T;
pipe(T=#{todo := [], ok := OK}, Fs) ->
    NewT = T#{todo => OK, ok => []},
    pipe(NewT, Fs);
pipe(T, [F|Fs]) ->
    case do(F, T) of
        NewT=#{ok := []} -> NewT;
        NewT -> pipe(NewT, Fs)
    end.

%% @private
%% @doc
%% Exported ONLY for knm_number_states use.
%% @end
-spec do(applier(), t()) -> t().
do(_, T=#{todo := [], ok := []}) -> T;
do(F, T=#{todo := [], ok := OK}) ->
    %% For calls not from pipe/2
    do(F, T#{todo => OK, ok => []});
do(F, T) ->
    lager:debug("applying ~p", [F]),
    NewT = F(T),
    NewT#{todo => []}.

%% @private
-spec do_in_wrap(applier(t_pn()), t()) -> t().
do_in_wrap(_, T=#{todo := [], ok := []}) -> T;
do_in_wrap(F, T=#{todo := [], ok := OK}) ->
    %% For calls not from pipe/2
    do_in_wrap(F, T#{todo => OK, ok => []});
do_in_wrap(F, T0=#{todo := Ns}) ->
    {NumsMap, PNs} = unwrap_phone_numbers(Ns),
    T1 = do(F, T0#{todo => PNs}),
    rewrap_phone_numbers(NumsMap, T1).

%% @private
%% Exported ONLY for knm_number_states use.
-spec merge_okkos(t(), t()) -> t().
merge_okkos(#{ok := OKa, ko := KOa}
           ,#{ok := OKb, ko := KOb} = B) ->
    B#{ok => OKa ++ OKb
      ,ko => maps:merge(KOa, KOb)
      }.

%% @private
%% Exported ONLY for knm_number_states use.
-spec merge_okkos([t()]) -> t().
merge_okkos([T]) -> T;
merge_okkos([T0|Ts]) ->
    lists:foldl(fun merge_okkos/2, T0, Ts).

%% @private
-spec id(t()) -> t().
id(T=#{todo := Todo}) -> ok(Todo, T).

%% @private
-spec ret(t()) -> ret().
ret(#{ok := OKs
     ,ko := KOs
     ,services := Services
     ,charges := Charges
     ,options := Options
     }) ->
    #{ok => OKs
     ,ko => KOs %%FIXME Convert to error format
     ,services => Services
     ,charges => Charges
     ,options => Options
     }.

%% @public
-spec to_json(ret()) -> kz_json:object().
to_json(#{ok := Ns, ko := KOs}) ->
    Successes = [{num(N), knm_number:to_public_json(N)} || N <- Ns],
    kz_json:from_list(
      props:filter_empty(
        [{<<"success">>, kz_json:from_list(Successes)}
        ,{<<"error">>, kz_json:from_map(KOs)}
        ])).

%% @private
-spec unwrap_phone_numbers(knm_number:knm_numbers()) ->
                                  {#{num() => knm_number:knm_number()}
                                  ,knm_phone_number:knm_phone_numbers()
                                  }.
unwrap_phone_numbers(Ns) ->
    F = fun (N, {M, PNs}) ->
                PN = knm_number:phone_number(N),
                Num = knm_phone_number:number(PN),
                {M#{Num => N}, [PN|PNs]}
        end,
    lists:foldl(F, {#{}, []}, Ns).

%% @private
-spec rewrap_phone_numbers(#{num() => knm_number:knm_number()}, t()) -> t().
rewrap_phone_numbers(NumsMap, T=#{ok := PNs}) ->
    F = fun (PN, Ns) ->
                N = maps:get(knm_phone_number:number(PN), NumsMap),
                [knm_number:set_phone_number(N, PN) | Ns]
        end,
    T#{ok => lists:foldl(F, [], PNs)}.


are_reconcilable(Nums) ->
    knm_converters:are_reconcilable(lists:usort(Nums)).

take_not_founds(T=#{ko := KOs}) ->
    F = fun ({_Num, Reason}) -> not_found =:= Reason end,
    {NumsNotFound, NewKOs} = lists:partition(F, maps:to_list(KOs)),
    Nums = [Num || {Num,not_found} <- NumsNotFound],
    {T#{ko := maps:from_list(NewKOs)}, Nums}.

-spec maybe_create(ne_binaries(), t_pn()) -> t_pn().
maybe_create(NotFounds, T) ->
    Ta = do(fun knm_number:ensure_can_create/1, new(options(T), NotFounds)),
    Tb = pipe(T, [fun knm_number:ensure_can_load_to_create/1
                 ,fun update_for_create/1
                 ]),
    merge_okkos(Ta, Tb).

-spec update_for_create(t_pn()) -> t_pn().
update_for_create(T=#{todo := _PNs, options := Options}) ->
    Updates = knm_number_options:to_phone_number_setters(
                props:delete('state', Options)
               ),
    knm_phone_number:setters(T, Updates).

-spec update_for_reconcile(t_pn(), knm_number_options:options()) -> t_pn().
update_for_reconcile(T, Options) ->
    S = [{fun knm_phone_number:set_assigned_to/2, knm_number_options:assign_to(Options)}
        ,{fun knm_phone_number:set_auth_by/2,     knm_number_options:auth_by(Options)}
        ,{fun knm_phone_number:update_doc/2,      knm_number_options:public_fields(Options)}
        ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
         | case props:is_defined(module_name, Options) of
               false -> [];
               true ->
                   [{fun knm_phone_number:set_module_name/2, knm_number_options:module_name(Options)}]
           end
        ],
    knm_phone_number:setters(T, S).

save_phone_numbers(T) ->
    do_in_wrap(fun knm_phone_number:save/1, T).

save_numbers(T) ->
    pipe(T, [fun knm_providers:save/1
            ,fun save_phone_numbers/1
            ,fun knm_services:update_services/1
            ]).

reconcile_number(T0, Options) ->
    F1 = fun (T) -> update_for_reconcile(T, Options) end,
    %%FIXME: create a pipe_in_wrap that does not create the superfluous Numbers.
    pipe(do_in_wrap(F1, T0), [fun save_phone_numbers/1]).

do_move_not_founds(Nums, Options) ->
    pipe(new([{state, ?NUMBER_STATE_IN_SERVICE} | Options], Nums)
        ,[fun knm_phone_number:new/1
         ,fun knm_number:new/1
         ]).

discover(T0=#{todo := Nums, options := Options}) ->
    F = fun (Num, T) ->
                case knm_search:discovery(Num, Options) of
                    {ok, N} -> ok(N, T);
                    {error, R} -> ko(Num, R, T)
                end
        end,
    lists:foldl(F, T0, Nums).

move_to(T) ->
    NewOptions = [{state, ?NUMBER_STATE_IN_SERVICE} | options(T)],
    pipe(options(NewOptions, T)
        ,[fun knm_number_states:to_options_state/1
         ,fun save_numbers/1
         ]).

to_reserved(T) ->
    NewOptions = [{state, ?NUMBER_STATE_RESERVED} | options(T)],
    pipe(options(NewOptions, T)
        ,[fun knm_number_states:to_options_state/1
         ,fun save_numbers/1
         ]).

unwind_or_disconnect(T) ->
    #{ok := Ns} = T0 = do_in_wrap(fun knm_phone_number:unwind_reserve_history/1, T),
    {ToDisconnect, ToUnwind} = lists:partition(fun is_history_empty/1, Ns),
    Ta = do_in_wrap(fun unwind/1, ok(ToUnwind, T0)),
    Tb = pipe(ok(ToDisconnect, T0)
             ,[fun knm_carriers:disconnect/1
              ,fun delete_maybe_age/1
              ]),
    merge_okkos(Ta, Tb).

unwind(T0=#{todo := PNs}) ->
    BaseRoutines = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_RESERVED}],
    F = fun (PN, T) ->
                [NewAssignedTo|_] = knm_phone_number:reserve_history(PN),
                Routines = [{fun knm_phone_number:set_assigned_to/2, NewAssignedTo} | BaseRoutines],
                {ok, NewPN} = knm_phone_number:setters(PN, Routines),
                ok(NewPN, T)
        end,
    lists:foldl(F, T0, PNs).

delete_maybe_age(T=#{todo := _Ns, options := Options}) ->
    case knm_config:should_permanently_delete(
           knm_number_options:should_delete(Options))
    of
        true -> delete_permanently(T);
        false ->
            {DeleteNs, OtherNs} = split_on(fun is_carrier_local_or_mdn/1, T),
            merge_okkos(delete_permanently(DeleteNs), maybe_age(OtherNs))
    end.

delete_permanently(T) ->
    do_in_wrap(fun knm_phone_number:delete/1, T).

split_on(Pred, T=#{todo := Ns}) ->
    {Yes, No} = lists:partition(Pred, Ns),
    {T#{todo => Yes}, T#{todo => No}}.

is_carrier_local_or_mdn(N) ->
    Carrier = knm_phone_number:module_name(knm_number:phone_number(N)),
    ?CARRIER_LOCAL =:= Carrier
        orelse ?CARRIER_MDN =:= Carrier.

maybe_age(T=#{todo := Ns}) ->
    case knm_config:should_age() of
        false -> ok(Ns, T);
        true ->
            lager:debug("aging for some time"),
            {Yes, No} = lists:partition(fun is_state_available/1, Ns),
            Ta = do(fun id/1, T#{todo => No}),
            NewOptions = [{state, ?NUMBER_STATE_AGING} | options(T)],
            Tb = do(fun knm_number_states:to_options_state/1
                   ,options(NewOptions, T#{todo => Yes})),
            merge_okkos(Ta, Tb)
    end.

is_history_empty(N) ->
    [] =:= knm_phone_number:reserve_history(knm_number:phone_number(N)).

is_state_available(N) ->
    ?NUMBER_STATE_AVAILABLE =:= knm_phone_number:state(knm_number:phone_number(N)).
