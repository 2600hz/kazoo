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
        ,transactions/1, transactions/2
        ,charges/1, charges/2
        ]).
-export([ok/2, ko/3]).

-export([get/1, get/2
        ,create/2
        ,move/2, move/3
        ,update/2, update/3
        ,release/1, release/2
        ,delete/2
        ,reconcile/2
        ,reserve/2

        ,to_state/2, to_state/3
        ,assign_to_app/2, assign_to_app/3

        ,free/1
        ,emergency_enabled/1

        ,account_listing/1
        ]).

-include("knm.hrl").

-type num() :: ne_binary().  %%TODO: support ranges?
-type nums() :: [num()].
-type ret(T) :: {num(), T}.
-type ok() :: knm_number:knm_number().
-type oks() :: [ok()].
-type ko() :: ret(knm_errors:error() | atom()).
-type kos() :: [ko()].
-type ret() :: #{ok => oks()
                ,ko => kz_json:object() | atom()
                ,dry_run => kz_json:object()
                }.

-export_type([ret/0, ret/1
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
-type transactions() :: kz_transaction:transactions().
-type charges() :: [{ne_binary(), non_neg_integer()}].

-export_type([options/0
             ,plan/0
             ,services/0
             ,transactions/0
             ,charges/0
             ]).

-type applier() :: fun((t()) -> t()).
-type appliers() :: [applier()].

-type number_return() :: {ne_binary(), knm_number:knm_number_return()}.
-type numbers_return() :: [number_return()].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Either `nums()' xor `oks()'.
%% @end
%%--------------------------------------------------------------------
-spec todo(t()) -> nums() | oks().
todo(#{todo := ToDo}) -> ToDo.

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
-spec charges(t()) -> charges().
-spec charges(charges(), t()) -> t().
charges(#{charges := V}) -> V.
charges(V, T) -> T#{charges => V}.

%% @public
-spec ok(ok() | oks(), t()) -> t().
ok(Numbers, T) when is_list(Numbers) ->
    T#{ok => Numbers};
ok(Number, T) -> T#{ok => [Number | maps:get(ok, T)]}.

%% @public
-spec ko(num() | knm_number:knm_number(), ko(), t()) -> t().
ko(?NE_BINARY=Num, Reason, T) ->
    KOs = maps:get(ko, T),
    T#{ko => KOs#{Num => Reason}};
ko(N, Reason, T) ->
    Num = knm_phone_number:number(knm_number:phone_number(N)),
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
    pipe(new(Options, Yes, No), [%% fetch/1 puts PNs in "ok"!
                                 fun knm_phone_number:fetch/1
                                ,fun knm_number:new/1
                                ]).

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
    {Yes, No} = are_reconcilable(Nums),
    FIXME = fun (Num) -> knm_errors:to_json('not_reconcilable', Num) end,
    T0 = do(fun knm_phone_number:fetch/1, new(Options, Yes, No, FIXME)),
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
    {TDiscovered, NotExisting} = take_not_founds(do(fun discover/1, new(Options, NotFounds))),
    TNew = pipe(new(Options, NotExisting), [fun knm_phone_number:new/1
                                           ,fun knm_number:new/1
                                           ]),
    T = merge_okkos([TFound, TDiscovered, TNew]),
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

update(Nums, Routines, Options) ->
    ret(pipe(do_get(Nums, Options), [fun (T) -> update_existing(T, Routines) end
                                    ,fun save_numbers/1
                                    ])).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec release(ne_binaries()) ->
                     numbers_return().
-spec release(ne_binaries(), knm_number_options:options()) ->
                     numbers_return().
release(Nums) ->
    release(Nums, knm_number_options:default()).

release(Nums, Options) ->
    [{Num, knm_number:release(Num, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binaries(), knm_number_options:options()) ->
                    numbers_return().
delete(Nums, Options) ->
    [{Num, knm_number:delete(Num, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reconcile(ne_binaries(), knm_number_options:options()) ->
                       numbers_return().
reconcile(Nums, Options) ->
    [{Num, knm_number:reconcile(Num, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reserve(ne_binaries(), knm_number_options:options()) ->
                     numbers_return().
reserve(Nums, Options) ->
    [{Num, knm_number:reserve(Num, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_state(ne_binaries(), ne_binary()) ->
                      numbers_return().
-spec to_state(ne_binaries(), ne_binary(), knm_number_options:options()) ->
                      numbers_return().
to_state(Nums, ToState) ->
    to_state(Nums, ToState, knm_number_options:default()).

to_state(Nums, ToState, Options) ->
    [{Num, change_state(Num, ToState, Options)} || Num <- Nums].

-spec change_state(ne_binary(), ne_binary(), knm_number_options:options()) ->
                          {'ok', knm_number:knm_number()} |
                          knm_errors:thrown_error().
change_state(Num, ToState, Options) ->
    try knm_number_states:to_state(Num, ToState, Options) of
        Number -> {'ok', Number}
    catch
        'throw':R -> R
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binaries(), api_binary()) ->
                           numbers_return().
-spec assign_to_app(ne_binaries(), api_binary(), knm_number_options:options()) ->
                           numbers_return().
assign_to_app(Nums, App) ->
    assign_to_app(Nums, App, knm_number_options:default()).

assign_to_app(Nums, App, Options) ->
    [{Num, knm_number:assign_to_app(Num, App, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc Release all of an account's numbers
%% @end
%%--------------------------------------------------------------------
-spec free(ne_binary()) -> 'ok'.
free(Account=?NE_BINARY) ->
    AccountDb = kz_util:format_account_db(Account),
    {Numbers, _NumbersData} = lists:unzip(account_listing(AccountDb)),
    lists:foreach(fun ({Num, {'ok', _PhoneNumber}}) ->
                          lager:debug("successfully released ~s from ~s", [Num, Account]);
                      ({Num, {'error', _R}}) ->
                          lager:error("error when releasing ~s from ~s: ~p", [Num, Account, _R])
                  end
                 ,release(Numbers)
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
-spec new(knm_number_options:options(), nums()) -> t().
-spec new(knm_number_options:options(), nums(), nums()) -> t().
-spec new(knm_number_options:options(), nums(), nums(), atom() | fun((num())->kz_json:object())) -> t().
new(Options, ToDos) -> new(Options, ToDos, []).
new(Options, ToDos, KOs) -> new(Options, ToDos, KOs, not_reconcilable).
new(Options, ToDos, KOs, Reason) ->
    #{todo => ToDos
     ,ok => []
     ,ko => case is_atom(Reason) of
                true -> maps:from_list([{KO, Reason} || KO <- KOs]);
                false -> maps:from_list([{KO, Reason(KO)} || KO <- KOs])
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
%% @end
-spec pipe(t(), appliers()) -> t().
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
-spec do_in_wrap(applier(), t()) -> t().
do_in_wrap(_, T=#{todo := [], ok := []}) -> T;
do_in_wrap(F, T=#{todo := [], ok := OK}) ->
    %% For calls not from pipe/2
    do_in_wrap(F, T#{todo => OK, ok => []});
do_in_wrap(F, T0=#{todo := Ns}) ->
    {NumsMap, PNs} = unwrap_phone_numbers(Ns),
    T1 = do(F, T0#{todo => PNs}),
    rewrap_phone_numbers(NumsMap, T1).

%% @private
-spec merge_okkos(t(), t()) -> t().
merge_okkos(#{ok := OKa, ko := KOa}
           ,#{ok := OKb, ko := KOb} = B) ->
    B#{ok => OKa ++ OKb
      ,ko => maps:merge(KOa, KOb)
      }.

%% @private
-spec ret(t()) -> ret().
ret(#{ok := OKs
     ,ko := KOs
     }) ->
    %%FIXME: use the collection()'s services.
    ServicesList = [Services || N <- OKs,
                                Services <- [knm_number:services(N)],
                                Services =/= undefined
                   ],
    F = fun (Services, JObj) -> kz_json:sum(kz_services:dry_run(Services), JObj) end,
    DryRun = lists:foldl(F, kz_json:new(), ServicesList),
    #{ok => OKs
     ,ko => KOs %%FIXME Convert to error format
     ,dry_run => DryRun
     ,services => case ServicesList of [] -> undefined; [S|_] -> S end
     }.

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

update_for_create(T=#{todo := _PNs, options := Options}) ->
    Updates = knm_number_options:to_phone_number_setters(
                props:delete('state', Options)
               ),
    knm_phone_number:setters(T, Updates).

update_existing(T0, Setters) ->
    do_in_wrap(fun (T) -> knm_phone_number:setters(T, Setters) end, T0).

save_phone_numbers(T) ->
    do_in_wrap(fun knm_phone_number:save/1, T).

save_numbers(T) ->
    pipe(T, [fun knm_providers:save/1
            ,fun save_phone_numbers/1
            ,fun knm_services:update_services/1
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
    pipe(options(NewOptions, T), [fun knm_number_states:to_options_state/1
                                 ,fun save_numbers/1
                                 ]).
