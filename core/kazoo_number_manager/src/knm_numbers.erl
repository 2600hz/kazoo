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

-type t() :: #{todo => nums() | oks()
              ,ok => oks()
              ,ko => kos()
              ,dry_run => kz_json:object()

              ,options => options()
              ,plan => plan()
              ,services => services()
              ,transactions => transactions()
              ,charges => charges()
              }.

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
-spec ok(ok(), t()) -> t().
ok(Number, T) -> T#{ok => [Number | maps:get(ok, T)]}.

%% @public
-spec ko(num(), ko(), t()) -> t().
ko(Num, Reason, T) ->
    KOs = maps:get(ko, T),
    T#{ko => KOs#{Num => Reason}}.

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
    {Yes, No} = knm_converters:are_reconcilable(lists:usort(Nums)),
    do(fun knm_phone_number:fetch/1, new(Options, Yes, No)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binaries(), knm_number_options:options()) -> numbers_return().
create(Nums, Options) ->
    [{Num, knm_number:create(Num, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binaries(), ne_binary()) ->
                  numbers_return().
-spec move(ne_binaries(), ne_binary(), knm_number_options:options()) ->
                  numbers_return().
move(Nums, MoveTo) ->
    move(Nums, MoveTo, knm_number_options:default()).

move(Nums, MoveTo, Options) ->
    [{Num, knm_number:move(Num, MoveTo, Options)} || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binaries(), knm_phone_number:set_functions()) ->
                    numbers_return().
-spec update(ne_binaries(), knm_phone_number:set_functions(), knm_number_options:options()) ->
                    numbers_return().
update(Nums, Routines) ->
    update(Nums, Routines, knm_number_options:default()).

update(Nums, Routines, Options) ->
    [{Num, knm_number:update(Num, Routines, Options)} || Num <- Nums].

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
-spec new(knm_number_options:options(), nums(), nums()) -> t().
new(Options, ToDos, KOs) ->
    #{todo => ToDos
     ,ok => []
     ,ko => maps:from_list([{KO, not_reconcilable} || KO <- KOs])
     ,dry_run => kz_json:new()

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
-spec do(fun(), t()) -> t().
do(_, T = #{todo := [], ok := []}) -> T;
do(F, T = #{todo := [], ok := OK}) ->
    do(F, T#{todo => OK, ok => []});
do(F, T) ->
    NewT = F(T),
    NewT#{todo => []}.

%% @private
-spec ret(t()) -> ret().
ret(#{ok := OKs
     ,ko := KOs
     ,dry_run := DryRun
     }) ->
    #{ok => OKs
     ,ko => KOs %%FIXME Convert to error format
     ,dry_run => DryRun
     }.
