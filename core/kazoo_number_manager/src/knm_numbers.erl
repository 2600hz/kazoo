%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Bulk operations on numbers.
%%%
%%% <div class="notice">Functions should not throw, instead should return
%%% {@link ret()}.</div>
%%%
%%% @author Peter Defebvre
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_numbers).

-export([todo/1, set_todo/2
        ,options/1, options/2
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
-export([do_in_wrap/2]).
-export([merge_okkos/2, merge_okkos/1]).
-export([from_jobjs/1]).

-include("knm.hrl").

-type num() :: kz_term:ne_binary().  %%TODO: support ranges?
-type nums() :: [num()].
-type ok() :: knm_number:knm_number() | knm_phone_number:knm_phone_number().
-type oks() :: [ok()].
-type ko() :: knm_errors:error() | atom().
-type kos() :: #{num() => ko()}.

-type ret() :: #{'ok' => oks()
                ,'ko' => kos()
                ,'todo' => nums() | oks()
                ,'quotes' => quotes()
                ,'options' => options()
                }.

-export_type([ret/0
             ,ok/0, oks/0
             ,ko/0, kos/0
             ,num/0, nums/0
             ]).

-type t() :: t(oks()).
-type t(OKs) :: t(OKs, OKs).
-type t(OKs, TODOs) :: #{'todo' => nums() | TODOs
                        ,'ok' => OKs
                        ,'ko' => kos()
                        ,'options' => options()
                        ,'quotes' => quotes()
                        }.

-type t_pn() :: t(knm_phone_number:knm_phone_number()).

-type collection() :: t().
-type pn_collection() :: t_pn().
-export_type([collection/0, pn_collection/0]).

-type options() :: knm_number_options:options().
-export_type([options/0]).

-type applier() :: applier(t()).
-type applier(A) :: fun((A) -> A).
-type appliers() :: [applier()].
-type appliers(A) :: [applier(A)].

-spec num(knm_number:knm_number()) -> num().
num(N) ->
    knm_phone_number:number(knm_number:phone_number(N)).

%%------------------------------------------------------------------------------
%% @doc Returns either {@link nums()} `xor' {@link oks()}.
%% @end
%%------------------------------------------------------------------------------
-spec todo(t()) -> nums() | oks().
todo(#{'todo' := ToDo}) -> ToDo.

-spec set_todo(t(), nums() | oks()) -> t().
set_todo(T, ToDo) ->
    T#{'todo' => ToDo}.

%%------------------------------------------------------------------------------
%% @doc Set of numbers' `assigned_to' fields.
%% @end
%%------------------------------------------------------------------------------
-spec assigned_to(t()) -> kz_term:api_ne_binary().
assigned_to(#{'todo' := Ns}) ->
    F = fun (N, S) ->
                case knm_phone_number:assigned_to(knm_number:phone_number(N)) of
                    'undefined' -> S;
                    ?MATCH_ACCOUNT_RAW(AccountId) -> sets:add_element(AccountId, S)
                end
        end,
    case sets:to_list(lists:foldl(F, sets:new(), Ns)) of
        [] -> 'undefined';
        [AccountId] -> AccountId
    end.

%%------------------------------------------------------------------------------
%% @doc Set of numbers' `prev_assigned_to' fields.
%% @end
%%------------------------------------------------------------------------------
-spec prev_assigned_to(t()) -> kz_term:api_ne_binary().
prev_assigned_to(#{'todo' := Ns}) ->
    F = fun (N, S) ->
                case knm_phone_number:prev_assigned_to(knm_number:phone_number(N)) of
                    'undefined' -> S;
                    ?MATCH_ACCOUNT_RAW(AccountId) -> sets:add_element(AccountId, S)
                end
        end,
    case sets:to_list(lists:foldl(F, sets:new(), Ns)) of
        [] -> 'undefined';
        [AccountId] -> AccountId
    end.


-spec options(t()) -> options().
options(#{'options' := V}) -> V.

-spec options(options(), t()) -> t().
options(V, T) -> T#{'options' => V}.

-spec ok(ok() | oks(), t()) -> t().
ok(Numbers, T) when is_list(Numbers) -> T#{'ok' => Numbers};
ok(Number, T) when not is_list(Number) ->
    T#{'ok' => [Number | maps:get('ok', T)]}.

-spec add_oks(oks(), t()) -> t().
%%FIXME: unify with ok/2.
add_oks(Numbers, T=#{'ok' := OKs}) when is_list(Numbers) ->
    T#{'ok' => Numbers ++ OKs}.

-spec ko(num() | knm_number:knm_number() | nums() | [knm_number:knm_number()], ko(), t()) -> t().
ko(?NE_BINARY=Num, Reason, T) ->
    lager:debug("number ~s error: ~p", [Num, Reason]),
    KOs = maps:get('ko', T),
    T#{'ko' => KOs#{Num => Reason}};
ko(Ns=[_|_], Reason, T0) ->
    F = fun (N, T) -> ko(N, Reason, T) end,
    lists:foldl(F, T0, Ns);
ko(N, Reason, T) ->
    PN = knm_number:phone_number(N),
    Num = knm_phone_number:number(PN),
    lager:debug("number ~s state: ~s", [Num, knm_phone_number:state(PN)]),
    ko(Num, Reason, T).

%%------------------------------------------------------------------------------
%% @doc Attempts to get numbers from DB.
%%
%% <div class="notice">Each number in `Nums' has to be normalized.</div>
%% @end
%%------------------------------------------------------------------------------

-spec get(kz_term:ne_binaries()) -> ret().
get(Nums) -> get(Nums, knm_number_options:default()).

-spec get(kz_term:ne_binaries(), knm_number_options:options()) -> ret().
get(Nums, Options) -> ret(do_get(Nums, Options)).

-spec do_get(kz_term:ne_binaries(), knm_number_options:options()) -> t().
do_get(Nums, Options) ->
    {Yes, No} = are_reconcilable(Nums),
    pipe(new(Options, Yes, No)
        ,[fun knm_phone_number:fetch/1  %% fetch/1 puts PNs in "ok"!
         ,fun knm_number:new/1
         ]).

-spec do_get_pn(kz_term:ne_binaries(), knm_number_options:options()) -> t_pn().
do_get_pn(Nums, Options) ->
    {Yes, No} = are_reconcilable(Nums),
    do(fun knm_phone_number:fetch/1, new(Options, Yes, No)).

-spec do_get_pn(kz_term:ne_binaries(), knm_number_options:options(), reason_t()) -> t_pn().
do_get_pn(Nums, Options, Error) ->
    {Yes, No} = are_reconcilable(Nums),
    do(fun knm_phone_number:fetch/1, new(Options, Yes, No, Error)).

-spec from_jobjs(kz_json:objects()) -> t().
from_jobjs(JObjs) ->
    Options = knm_number_options:default(),
    PNs = [knm_phone_number:from_json_with_options(Doc, Options)
           || JObj <- JObjs,
              Doc <- [kz_json:get_value(<<"doc">>, JObj)],
              kz_doc:type(Doc) =:= <<"number">>
          ],
    new(Options, PNs, []).

-ifdef(TEST).
-define(OPTIONS_FOR_LOAD(Nums, Options),
        case knm_number_options:ported_in(Options) of
            'false' -> Options;
            'true' ->
                case Nums of
                    [?TEST_PORT_IN2_NUM] -> [{'module_name', <<"knm_telnyx">>}|Options];
                    [?TEST_AVAILABLE_NUM] -> [{'module_name', <<"knm_bandwidth2">>}|Options];
                    _ -> [{'module_name', ?PORT_IN_MODULE_NAME}|Options]
                end
        end).
-else.
-define(OPTIONS_FOR_LOAD(_Nums, Options),
        case knm_number_options:ported_in(Options) of
            'false' -> Options;
            'true' -> [{'module_name', ?PORT_IN_MODULE_NAME}|Options]
        end).
-endif.

%%------------------------------------------------------------------------------
%% @doc Attempts to create new numbers in DB or modify existing ones.
%%
%% <div class="notice">`assign_to' number option MUST be set.</div>
%%
%% <div class="notice">Creating numbers with `ported_in' option set to true will
%% attempt to create them with state `in_service'.</div>
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binaries(), knm_number_options:options()) -> ret().
create(Nums, Options) ->
    T0 = pipe(do_get_pn(Nums
                       ,?OPTIONS_FOR_LOAD(Nums, props:delete('state', Options))
                       )
             ,[fun fail_if_assign_to_is_not_an_account_id/1]
             ),
    case take_not_founds(T0) of
        {#{'ok' := []}, []} -> T0;
        {T1, NotFounds} ->
            try knm_number:state_for_create(Options) of
                ToState ->
                    lager:debug("picked state ~s for ~s for ~p", [ToState, knm_number_options:assign_to(Options), Nums]),
                    NewOptions = [{'state', ToState} | Options],
                    ret(pipe(maybe_create(NotFounds, options(NewOptions, T1))
                            ,[fun knm_number:new/1
                             ,fun knm_number_states:to_options_state/1
                             ,fun save_numbers/1
                             ]))
            catch 'throw':{'error', 'unauthorized'} ->
                    Reason = knm_errors:to_json('unauthorized', 'undefined', 'state_for_create'),
                    F = fun (T=#{'todo' := PNs}) ->
                                ko([knm_phone_number:number(PN) || PN <- PNs], Reason, T)
                        end,
                    ret(do(F, ko(NotFounds, Reason, T1)))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec move(kz_term:ne_binaries(), kz_term:ne_binary()) -> ret().
move(Nums, MoveTo) ->
    move(Nums, MoveTo, knm_number_options:default()).

-spec move(kz_term:ne_binaries(), kz_term:ne_binary(), knm_number_options:options()) -> ret().
move(Nums, ?MATCH_ACCOUNT_RAW(MoveTo), Options0) ->
    Options = props:set_value('assign_to', MoveTo, Options0),
    {TFound, NotFounds} = take_not_founds(do_get(Nums, Options)),
    Updates = knm_number_options:to_phone_number_setters(Options0),
    TUpdated = do_in_wrap(fun (T) -> knm_phone_number:setters(T, Updates) end, TFound),
    TDiscovered = do(fun discover/1, new(Options, NotFounds)),
    T = merge_okkos(TUpdated, TDiscovered),
    ret(do(fun move_to/1, T)).

%%------------------------------------------------------------------------------
%% @doc Attempts to update some phone_number fields.
%%
%% <div class="notice">Will always result in a phone_number save.</div>
%% @end
%%------------------------------------------------------------------------------

-spec update(kz_term:ne_binaries(), knm_phone_number:set_functions()) -> ret().
update(Nums, Routines) ->
    update(Nums, Routines, knm_number_options:default()).

-ifdef(TEST).

-spec update(kz_term:ne_binaries(), knm_phone_number:set_functions(), knm_number_options:options()) -> ret().
update([?NE_BINARY|_]=Nums, Routines, Options) ->
    Reason = 'not_reconcilable',  %% FIXME: unify to atom OR knm_error.
    do_update(do_get_pn(Nums, Options, Reason), Routines);
update(Ns, Updates, Options) ->
    Routines = [{fun knm_phone_number:set_is_dirty/2, 'false'}
                | knm_number_options:to_phone_number_setters(Options)
                ++ Updates
               ],
    T0 = new(Options, Ns),
    T1 = do_in_wrap(fun (T) -> knm_phone_number:setters(T, Routines) end, T0),
    ret(do(fun save_numbers/1, T1)).
-else.

-spec update(kz_term:ne_binaries(), knm_phone_number:set_functions(), knm_number_options:options()) -> ret().
update(Nums, Routines, Options) ->
    Reason = 'not_reconcilable',  %% FIXME: unify to atom OR knm_error.
    do_update(do_get_pn(Nums, Options, Reason), Routines).
-endif.

do_update(T0, Routines) ->
    ret(pipe(T0
            ,[fun (T) -> knm_phone_number:setters(T, Routines) end
             ,fun knm_number:new/1
             ,fun save_numbers/1
             ]
            )).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec release(kz_term:ne_binaries()) -> ret().
release(Nums) ->
    release(Nums, knm_number_options:default()).

-spec release(kz_term:ne_binaries(), knm_number_options:options()) -> ret().
release(Nums, Options) ->
    ret(pipe(do_get_pn(Nums, Options)
            ,[fun try_release/1
             ,fun knm_number:new/1
             ,fun knm_providers:delete/1
             ,fun unwind_maybe_disconnect/1
             ,fun save_phone_numbers/1
             ])).

%%------------------------------------------------------------------------------
%% @doc Remove numbers from the system without doing any state checking.
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_term:ne_binaries(), knm_number_options:options()) -> ret().
delete(Nums, Options) ->
    case knm_phone_number:is_admin(knm_number_options:auth_by(Options)) of
        'false' ->
            Error = knm_errors:to_json('unauthorized'),
            ret(new(Options, [], Nums, Error));
        'true' ->
            T0 = do_get(Nums, Options),
            F1 = fun knm_providers:delete/1,
            F2 = fun knm_phone_number:delete/1,
            ret(do_in_wrap(F2, do(F1, T0)))
    end.

%%------------------------------------------------------------------------------
%% @doc Note: option `assign_to' needs to be set.
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_term:ne_binaries(), knm_number_options:options()) -> ret().
reconcile(Nums, Options0) ->
    Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY} | Options0],
    T0 = pipe(do_get(Nums, Options)
             ,[fun fail_if_assign_to_is_not_an_account_id/1]
             ),
    {T1, NotFounds} = take_not_founds(T0),
    %% Ensures state to be IN_SERVICE
    Ta = do_move_not_founds(NotFounds, Options),
    Tb = do(fun (T) -> reconcile_number(T, Options) end, T1),
    ret(merge_okkos(Ta, Tb)).

%%------------------------------------------------------------------------------
%% @doc Fetches then transitions existing numbers to the reserved state.
%% @end
%%------------------------------------------------------------------------------
-spec reserve(kz_term:ne_binaries(), knm_number_options:options()) -> ret().
reserve(Nums, Options) ->
    ret(pipe(do_get(Nums, Options)
            ,[fun fail_if_assign_to_is_not_an_account_id/1
             ,fun to_reserved/1
             ])).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec assign_to_app(kz_term:ne_binaries(), kz_term:api_ne_binary()) -> ret().
assign_to_app(Nums, App) ->
    assign_to_app(Nums, App, knm_number_options:default()).

-spec assign_to_app(kz_term:ne_binaries(), kz_term:api_ne_binary(), knm_number_options:options()) -> ret().
assign_to_app(Nums, App, Options) ->
    Setters = [{fun knm_phone_number:set_used_by/2, App}],
    ret(pipe(do_get_pn(Nums, Options)
            ,[fun (T) -> knm_phone_number:setters(T, Setters) end
             ,fun knm_phone_number:save/1
             ,fun knm_number:new/1
             ])).

%%------------------------------------------------------------------------------
%% @doc Release all of an account's numbers
%% @end
%%------------------------------------------------------------------------------
-spec free(kz_term:ne_binary()) -> 'ok'.
free(Account=?NE_BINARY) ->
    AccountDb = kz_util:format_account_db(Account),
    {Numbers, _NumbersData} = lists:unzip(account_listing(AccountDb)),
    #{'ok' := Ns, 'ko' := KOs} = release(Numbers),
    lager:debug("successfully released ~p from ~s", [[num(N) || N <- Ns], Account]),
    lists:foreach(fun ({Num, R}) ->
                          lager:error("error when releasing ~s from ~s: ~p", [Num, Account, R])
                  end
                 ,maps:to_list(KOs)
                 ).

%%------------------------------------------------------------------------------
%% @doc Find an account's phone numbers that have emergency services enabled
%% @end
%%------------------------------------------------------------------------------
-spec emergency_enabled(kz_term:ne_binary()) -> kz_term:ne_binaries().
emergency_enabled(AccountId=?MATCH_ACCOUNT_RAW(_)) ->
    AccountDb = kz_util:format_account_db(AccountId),
    [Num || {Num, JObj} <- account_listing(AccountDb),
            Features <- [kz_json:get_list_value(<<"features">>, JObj, [])],
            lists:member(?FEATURE_E911, Features)
    ].

%%------------------------------------------------------------------------------
%% @doc List an account's phone numbers and statuses.
%% Does not go through sub accounts.
%% @end
%%------------------------------------------------------------------------------
-spec account_listing(kz_term:ne_binary()) -> [{kz_term:ne_binary(), kz_json:object()}].
account_listing(AccountDb=?MATCH_ACCOUNT_ENCODED(_,_,_)) ->
    case kz_datamgr:get_results(AccountDb, <<"phone_numbers/crossbar_listing">>) of
        {'ok', []} ->
            lager:debug("account ~s holds no numbers", [AccountDb]),
            [];
        {'ok', JObjs} ->
            [{kz_doc:id(JObj), kz_json:get_value(<<"value">>, JObj)}
             || JObj <- JObjs
            ];
        {'error', 'not_found'=_R} ->
            lager:error("error listing numbers for ~s: ~p", [AccountDb, _R]),
            [];
        {'error', R} ->
            lager:error("error listing numbers for ~s: ~p", [AccountDb, R]),
            throw(R)
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-type reason_t() :: atom() |
                    fun((num()) -> knm_errors:error()) |
                    knm_errors:error().

-spec new(knm_number_options:options(), nums()) -> t().
new(Options, ToDos) -> new(Options, ToDos, []).

-spec new(knm_number_options:options(), nums(), nums()) -> t().
new(Options, ToDos, KOs) -> new(Options, ToDos, KOs, 'not_reconcilable').

-spec new(knm_number_options:options(), nums(), nums(), reason_t()) -> t().
new(Options, ToDos, KOs, Reason) ->
    #{'todo' => ToDos
     ,'ok' => []
     ,'ko' => case is_function(Reason, 1) of %%FIXME: find something better than Reason/1.
                  'false' -> maps:from_list([{KO, Reason} || KO <- KOs]);
                  'true' -> maps:from_list([{KO, Reason(KO)} || KO <- KOs])
              end
     ,'options' => Options
     ,'quotes' => 'undefined'
     }.

%%------------------------------------------------------------------------------
%% @doc Apply something to `todo' if not empty
%% If empty use `ok' as the new `todo'.
%% If `ok' is empty, return.
%% Exported ONLY for inside-app use.
%% @end
%%------------------------------------------------------------------------------
-spec pipe(t() | t_pn(), appliers() | appliers(t_pn())) -> t() | t_pn().
pipe(T, []) -> T;
pipe(T=#{'todo' := [], 'ok' := []}, _) -> T;
pipe(T=#{'todo' := [], 'ok' := OK}, Fs) ->
    NewT = T#{'todo' => OK, 'ok' => []},
    pipe(NewT, Fs);
pipe(T, [F|Fs]) ->
    case do(F, T) of
        NewT=#{'ok' := []} -> NewT;
        NewT -> pipe(NewT, Fs)
    end.

%% @doc
%% Exported ONLY for {@link knm_number_states} use.
%% @end
-spec do(applier(), t()) -> t().
do(_, T=#{'todo' := [], 'ok' := []}) -> T;
do(F, T=#{'todo' := [], 'ok' := OK}) ->
    %% For calls not from pipe/2
    do(F, T#{'todo' => OK, 'ok' => []});
do(F, T) ->
    ?LOG_DEBUG("applying ~p", [F]),
    NewT = F(T),
    NewT#{'todo' => []}.

%% @doc Exported ONLY for `knm_number_states' use.
%% @end
-spec do_in_wrap(applier(t_pn()), t()) -> t().
do_in_wrap(_, T=#{'todo' := [], 'ok' := []}) -> T;
do_in_wrap(F, T=#{'todo' := [], 'ok' := OK}) ->
    %% For calls not from pipe/2
    do_in_wrap(F, T#{'todo' => OK, 'ok' => []});
do_in_wrap(F, T0=#{'todo' := Ns}) ->
    {NumsMap, PNs} = unwrap_phone_numbers(Ns),
    T1 = do(F, T0#{'todo' => PNs}),
    rewrap_phone_numbers(NumsMap, T1).

%% Exported ONLY for `knm_number_states' use.
-spec merge_okkos(t(), t()) -> t().
merge_okkos(#{'ok' := OKa, 'ko' := KOa}
           ,#{'ok' := OKb, 'ko' := KOb} = B) ->
    B#{'ok' => OKa ++ OKb
      ,'ko' => maps:merge(KOa, KOb)
      }.

%% Exported ONLY for `knm_number_states' use.
-spec merge_okkos([t()]) -> t().
merge_okkos([T]) -> T;
merge_okkos([T0|Ts]) ->
    lists:foldl(fun merge_okkos/2, T0, Ts).

-spec id(t()) -> t().
id(T=#{'todo' := Todo}) -> ok(Todo, T).

-spec ret(t()) -> ret().
ret(#{'ok' := OKs
     ,'ko' := KOs
     ,'options' := Options
     ,'quotes' := Quotes
     }) ->
    #{'ok' => OKs
     ,'ko' => KOs %%FIXME Convert to error format
     ,'options' => Options
     ,'quotes' => Quotes
     }.

-spec to_json(ret()) -> kz_json:object().
to_json(#{'ok' := Ns, 'ko' := KOs}) ->
    Successes = [{num(N), knm_number:to_public_json(N)} || N <- Ns],
    kz_json:from_list(
      props:filter_empty(
        [{<<"success">>, kz_json:from_list(Successes)}
        ,{<<"error">>, kz_json:from_map(KOs)}
        ])).

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

-spec rewrap_phone_numbers(#{num() => knm_number:knm_number()}, t()) -> t().
rewrap_phone_numbers(NumsMap, T=#{'ok' := PNs}) ->
    F = fun (PN, Ns) ->
                N = maps:get(knm_phone_number:number(PN), NumsMap),
                [knm_number:set_phone_number(N, PN) | Ns]
        end,
    T#{'ok' => lists:foldl(F, [], PNs)}.


are_reconcilable(Nums) ->
    knm_converters:are_reconcilable(lists:usort(Nums)).

take_not_founds(T=#{'ko' := KOs}) ->
    F = fun ({_Num, Reason}) -> 'not_found' =:= Reason end,
    {NumsNotFound, NewKOs} = lists:partition(F, maps:to_list(KOs)),
    Nums = [Num || {Num, 'not_found'} <- NumsNotFound],
    {T#{'ko' := maps:from_list(NewKOs)}, Nums}.

-spec maybe_create(nums(), t()) -> t().
maybe_create(NotFounds, T) ->
    Ta = do(fun knm_number:ensure_can_create/1, new(options(T), NotFounds)),
    Tb = pipe(T, [fun knm_number:ensure_can_load_to_create/1
                 ,fun update_for_create/1
                 ]),
    merge_okkos(Ta, Tb).

-spec update_for_create(t_pn()) -> t_pn().
update_for_create(T=#{'todo' := _PNs, 'options' := Options}) ->
    Updates = knm_number_options:to_phone_number_setters(
                props:delete('state', Options)
               ),
    knm_phone_number:setters(T, Updates).

-spec update_for_reconcile(t_pn(), knm_number_options:options()) -> t_pn().
update_for_reconcile(T, Options) ->
    S = [{fun knm_phone_number:set_assigned_to/2, knm_number_options:assign_to(Options)}
        ,{fun knm_phone_number:set_auth_by/2, knm_number_options:auth_by(Options)}
        ,{fun knm_phone_number:update_doc/2, knm_number_options:public_fields(Options)}
        ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
         | case props:is_defined('module_name', Options) of
               'false' -> [];
               'true' ->
                   [{fun knm_phone_number:set_module_name/2, knm_number_options:module_name(Options)}]
           end
        ],
    knm_phone_number:setters(T, S).

save_phone_numbers(T) ->
    do_in_wrap(fun knm_phone_number:save/1, T).

-spec save_numbers(t()) -> t_pn().
save_numbers(T) ->
    pipe(T, [fun knm_providers:save/1
            ,fun save_phone_numbers/1
            ,fun update_services/1
            ]).

-spec update_services(collection()) -> collection().
-ifdef(TEST).
update_services(T=#{todo := Ns}) -> ok(Ns, T).
-else.
update_services(T=#{todo := Numbers, options := Options}) ->
    case {knm_number_options:batch_run(Options)
         ,knm_number_options:dry_run(Options)
         }
    of
        {'true', _} ->
            lager:debug("batch_run-ing btw"),
            ok(Numbers, T);
        {_, 'true'} -> maybe_dry_run_services(T);
        {_, _} -> run_services(T)
    end.

-spec run_services(collection()) -> collection().
run_services(T=#{todo := Numbers}) ->
    Updates = services_group_numbers(Numbers),
    AccountIds = kz_json:get_keys(Updates),
    try run_services(AccountIds, Updates, []) of
        'ok' ->
            ok(Numbers, T)
    catch
        'throw':{'error', 'not_enough_credit', AccountId, Units} ->
            Reason = knm_errors:to_json('not_enough_credit', AccountId, Units),
            ko(Numbers, Reason, T)
    end.

-spec run_services(kz_term:ne_binaries(), kz_json:object(), [kz_services:services()]) -> 'ok'.
run_services([], _Updates, UpdatedServicesAcc) ->
    _ = [kz_services:commit(UpdatedServices) || UpdatedServices <- lists:reverse(UpdatedServicesAcc)],
    'ok';
run_services([AccountId|AccountIds], Updates, UpdatedServicesAcc) ->
    CurrentJObjs = kz_json:get_value([AccountId, <<"current">>], Updates),
    ProposedJObjs = kz_json:get_value([AccountId, <<"proposed">>], Updates),
    Services = kz_services:fetch(AccountId),
    UpdatedServices = kz_services:set_updates(Services
                                             ,AccountId
                                             ,CurrentJObjs
                                             ,ProposedJObjs
                                             ),
    Quotes = kz_services_invoices:create(UpdatedServices),
    HasAdditions = kz_services_invoices:has_billable_additions(Quotes),
    check_creditably(Services, Quotes, HasAdditions),
    run_services(AccountIds, Updates, [UpdatedServices | UpdatedServicesAcc]).

-spec check_creditably(kz_services:services(), kz_services_invoices:invoices(), boolean() | number()) -> 'ok'.
check_creditably(_Services, _Quotes, 'false') ->
    'ok';
check_creditably(Services, Quotes, 'true') ->
    Key = [<<"difference">>, <<"billable">>],
    Additions = [begin
                     Changes = kz_services_item:changes(Item),
                     BillableQuantity = kz_json:get_integer_value(Key, Changes, 0),
                     Rate = kz_services_item:rate(Item),
                     BillableQuantity * Rate
                 end
                 || Invoice <- kz_services_invoices:billable_additions(Quotes),
                    Item <- kz_services_invoice:items(Invoice),
                    kz_services_item:has_billable_additions(Item)
                ],
    check_creditably(Services, Quotes, lists:sum(Additions));
check_creditably(_Services, _Quotes, Amount) when Amount =< 0 ->
    'ok';
check_creditably(Services, Quotes, Amount) ->
    Options = #{amount => kz_currency:dollars_to_units(Amount)
               ,quotes => Quotes
               },
    case kz_services_standing:acceptable(Services, Options) of
        {'true', _} -> 'ok';
        {'false', Reason} ->
            knm_errors:billing_issue(kz_services:account_id(Services)
                                    ,kz_json:from_map(Reason)
                                    )
    end.

-spec maybe_dry_run_services(collection()) -> collection().
maybe_dry_run_services(T=#{todo := Numbers, options := Options}) ->
    case knm_number_options:crossbar(Options) of
        'undefined' -> ok(Numbers, T);
        CrossbarOptions -> dry_run_services(T, CrossbarOptions)
    end.

-spec dry_run_services(collection(), kz_term:proplist()) -> collection().
dry_run_services(T=#{todo := Numbers}, CrossbarOptions) ->
    Services = props:get_value('services', CrossbarOptions),
    AccountId = props:get_value('account_id', CrossbarOptions),
    Updates = services_group_numbers(Numbers),
    CurrentJObjs = kz_json:get_value([AccountId, <<"current">>], Updates),
    ProposedJObjs = kz_json:get_value([AccountId, <<"proposed">>], Updates),
    UpdatedServices = kz_services:set_updates(Services
                                             ,AccountId
                                             ,CurrentJObjs
                                             ,ProposedJObjs
                                             ),
    Quotes = kz_services_invoices:create(UpdatedServices),
    case kz_services_invoices:has_changes(Quotes) of
        'false' -> ok(Numbers, T);
        'true' ->
            JObj = kz_services_invoices:public_json(Quotes),
            ok(Numbers, T#{'quotes' => JObj})
    end.

-spec services_group_numbers(knm_number:knm_numbers()) -> kz_json:object().
services_group_numbers(Numbers) ->
    %% TODO: sort these so the account with the largest pvt_tree is first...
    services_group_numbers(Numbers, dict:new()).

-spec services_group_numbers(knm_number:knm_numbers(), dict:dict()) -> kz_json:object().
services_group_numbers([], Updates) ->
    kz_json:set_values(dict:to_list(Updates), kz_json:new());
services_group_numbers([Number|Numbers], Updates) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignedTo = kz_term:to_api_term(
                   knm_phone_number:assigned_to(PhoneNumber)
                  ),
    PrevAssignedTo = kz_term:to_api_term(
                       knm_phone_number:prev_assigned_to(PhoneNumber)
                      ),
    Props = services_group_number(PhoneNumber, AssignedTo, PrevAssignedTo),
    UpdatedGroups = lists:foldl(fun({Key, Value}, U) ->
                                        dict:append(Key, Value, U)
                                end
                               ,Updates
                               ,Props
                               ),
    services_group_numbers(Numbers, UpdatedGroups).

-spec services_group_number(knm_phone_number:knm_phone_number(), kz_term:api_binary(), kz_term:api_binary()) -> kz_term:proplist().
services_group_number(_PhoneNumber, 'undefined', 'undefined') -> [];
services_group_number(PhoneNumber, 'undefined', PrevAssignedTo) ->
    ProposedJObj = kz_json:new(),
    CurrentJObj = knm_phone_number:current_doc(PhoneNumber),
    [{[PrevAssignedTo, <<"proposed">>], ProposedJObj}
    ,{[PrevAssignedTo, <<"current">>], CurrentJObj}
    ];
services_group_number(PhoneNumber, AssignedTo, 'undefined') ->
    ProposedJObj = knm_phone_number:to_json(PhoneNumber),
    CurrentJObj = kz_json:new(),
    [{[AssignedTo, <<"proposed">>], ProposedJObj}
    ,{[AssignedTo, <<"current">>], CurrentJObj}
    ];
services_group_number(PhoneNumber, AssignedTo, AssignedTo) ->
    ProposedJObj = knm_phone_number:to_json(PhoneNumber),
    CurrentJObj = knm_phone_number:current_doc(PhoneNumber),
    [{[AssignedTo, <<"proposed">>], ProposedJObj}
    ,{[AssignedTo, <<"current">>], CurrentJObj}
    ];
services_group_number(PhoneNumber, AssignedTo, PrevAssignedTo) ->
    ProposedJObj = knm_phone_number:to_json(PhoneNumber),
    CurrentJObj = knm_phone_number:current_doc(PhoneNumber),
    [{[AssignedTo, <<"proposed">>], ProposedJObj}
    ,{[AssignedTo, <<"current">>], kz_json:new()}
    ,{[PrevAssignedTo, <<"proposed">>], kz_json:new()}
    ,{[PrevAssignedTo, <<"current">>], CurrentJObj}
    ].
-endif.

reconcile_number(T0, Options) ->
    F1 = fun (T) -> update_for_reconcile(T, Options) end,
    %%FIXME: create a pipe_in_wrap that does not create the superfluous Numbers.
    pipe(do_in_wrap(F1, T0), [fun save_phone_numbers/1]).

do_move_not_founds(Nums, Options) ->
    pipe(new([{'state', ?NUMBER_STATE_IN_SERVICE} | Options], Nums)
        ,[fun knm_phone_number:new/1
         ,fun knm_number:new/1
         ]).

discover(T0=#{'todo' := Nums, 'options' := Options}) ->
    F = fun (Num, T) ->
                case knm_search:discovery(Num, Options) of
                    {'ok', N} -> ok(N, T);
                    {'error', R} -> ko(Num, R, T)
                end
        end,
    lists:foldl(F, T0, Nums).

move_to(T) ->
    NewOptions = [{'state', ?NUMBER_STATE_IN_SERVICE} | options(T)],
    pipe(options(NewOptions, T)
        ,[fun knm_number_states:to_options_state/1
         ,fun save_numbers/1
         ]).

to_reserved(T) ->
    NewOptions = [{'state', ?NUMBER_STATE_RESERVED} | options(T)],
    pipe(options(NewOptions, T)
        ,[fun knm_number_states:to_options_state/1
         ,fun save_numbers/1
         ]).

-spec fail_if_assign_to_is_not_an_account_id(t() | t_pn()) -> t() | t_pn().
fail_if_assign_to_is_not_an_account_id(T=#{'todo' := NsOrPNs, 'options' := Options}) ->
    case knm_number_options:assign_to(Options) of
        ?MATCH_ACCOUNT_RAW(_) -> ok(NsOrPNs, T);
        _ ->
            Reason = knm_errors:to_json('assign_failure', 'undefined', 'field_undefined'),
            NsOrNums = case knm_phone_number:is_phone_number(hd(NsOrPNs)) of
                           'false' -> NsOrPNs;
                           'true' -> [knm_phone_number:number(PN) || PN <- NsOrPNs]
                       end,
            ko(NsOrNums, Reason, T)
    end.

-spec try_release(t_pn()) -> t_pn().
try_release(T) ->
    pipe(T
        ,[fun can_release/1
         ,fun knm_phone_number:is_authorized/1
         ,fun reset_features/1
         ]).

-spec can_release(t_pn()) -> t_pn().
can_release(T0=#{'todo' := PNs}) ->
    ToState = knm_config:released_state(),
    F = fun (PN, T) ->
                FromState = knm_phone_number:state(PN),
                case can_release(FromState, knm_phone_number:module_name(PN)) of
                    'true' -> ok(PN, T);
                    'false' ->
                        {'error', A, B, C} = (catch knm_errors:invalid_state_transition('undefined', FromState, ToState)),
                        Reason = knm_errors:to_json(A, B, C),
                        ko(knm_phone_number:number(PN), Reason, T)
                end
        end,
    lists:foldl(F, T0, PNs).

-spec can_release(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
can_release(?NUMBER_STATE_RELEASED, _) -> 'true';
can_release(?NUMBER_STATE_RESERVED, _) -> 'true';
can_release(?NUMBER_STATE_PORT_IN, _) -> 'true';
can_release(?NUMBER_STATE_IN_SERVICE, _) -> 'true';
can_release(_, ?CARRIER_LOCAL) -> 'true';
can_release(_, _) -> 'false'.

-spec reset_features(t_pn()) -> t_pn().
reset_features(T) ->
    Routines = [fun knm_phone_number:reset_features/1
               ,fun knm_phone_number:reset_doc/1
               ],
    knm_phone_number:setters(T, Routines).

-spec unwind_maybe_disconnect(t()) -> t().
unwind_maybe_disconnect(T) ->
    #{'ok' := Ns} = T0 = do_in_wrap(fun knm_phone_number:unwind_reserve_history/1, T),
    {ToDisconnect, DontDisconnect} = lists:partition(fun should_disconnect/1, Ns),
    Ta = ok(DontDisconnect, T0),
    Tb = pipe(ok(ToDisconnect, T0)
             ,[fun knm_carriers:disconnect/1
              ,fun delete_maybe_age/1
              ]),
    merge_okkos(Ta, Tb).

-spec should_disconnect(knm_number:knm_number()) -> boolean().
should_disconnect(N) ->
    'undefined' =:= knm_phone_number:assigned_to(knm_number:phone_number(N)).

-spec delete_maybe_age(t()) -> t().
delete_maybe_age(T) ->
    case knm_config:should_permanently_delete() of
        'true' -> delete_permanently(T);
        'false' ->
            {DeleteNs, OtherNs} = split_on(fun is_carrier_local_or_mdn/1, T),
            merge_okkos(delete_permanently(DeleteNs), maybe_age(OtherNs))
    end.

-spec delete_permanently(t()) -> t().
delete_permanently(T) ->
    do_in_wrap(fun knm_phone_number:delete/1, T).

split_on(Pred, T=#{'todo' := Ns}) ->
    {Yes, No} = lists:partition(Pred, Ns),
    {T#{'todo' => Yes}, T#{'todo' => No}}.

is_carrier_local_or_mdn(N) ->
    Carrier = knm_phone_number:module_name(knm_number:phone_number(N)),
    ?CARRIER_LOCAL =:= Carrier
        orelse ?CARRIER_MDN =:= Carrier.

maybe_age(T=#{'todo' := Ns}) ->
    case knm_config:should_age() of
        'false' -> ok(Ns, T);
        'true' ->
            lager:debug("aging for some time"),
            {Yes, No} = lists:partition(fun is_state_available/1, Ns),
            Ta = do(fun id/1, T#{'todo' => No}),
            NewOptions = [{'state', ?NUMBER_STATE_AGING} | options(T)],
            Tb = do(fun knm_number_states:to_options_state/1
                   ,options(NewOptions, T#{'todo' => Yes})),
            merge_okkos(Ta, Tb)
    end.

is_state_available(N) ->
    ?NUMBER_STATE_AVAILABLE =:= knm_phone_number:state(knm_number:phone_number(N)).
