%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz INC
%%% @doc
%%%   Bulk operations on numbers. Follows knm_number's API.
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_numbers).

-export([get/1, get/2
         ,create/2
         ,move/2, move/3
         ,update/2, update/3
         ,release/1, release/2
         ,reconcile/2
         ,reserve/2

         ,to_state/2, to_state/3
         ,assign_to_app/2, assign_to_app/3

         ,free/1
         ,emergency_enabled/1

         ,account_listing/1
        ]).

-include_lib("kazoo_number_manager/src/knm.hrl").

-type number_return() :: {ne_binary(), knm_number:knm_number_return()}.
-type numbers_return() :: [number_return()].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binaries()) ->
                 numbers_return().
-spec get(ne_binaries(), knm_number_options:options()) ->
                 numbers_return().
get(Nums) ->
    get(Nums, knm_number_options:default()).

get(Nums, Options) ->
    [{Num, knm_number:get(Num, Options)} || Num <- Nums].

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
    _ = [case Result of
             {Num, {'ok', _PhoneNumber}} ->
                 lager:debug("successfully released ~s from ~s", [Num, Account]);
             {Num, {'error', _R}} ->
                 lager:error("error when releasing ~s from ~s: ~p", [Num, Account, _R])
         end
         || Result <- release(Numbers)],
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc Find an account's phone numbers that have emergency services enabled
%%--------------------------------------------------------------------
-spec emergency_enabled(ne_binary()) -> ne_binaries().
emergency_enabled(AccountId=?MATCH_ACCOUNT_RAW(_)) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Numbers =
        [Num || {Num, ShortJObj} <- account_listing(AccountDb),
                AccountId == kz_json:get_value(<<"assigned_to">>, ShortJObj)
        ],
    [Num || {Num, {'ok', KNMNumber}} <- ?MODULE:get(Numbers),
            knm_providers:has_emergency_services(KNMNumber)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc Use a view to list an account's phone numbers & statuses
%%--------------------------------------------------------------------
-spec account_listing(ne_binary()) -> [{ne_binary(), kz_json:object()}].
account_listing(AccountDb = ?MATCH_ACCOUNT_ENCODED(_,_,_)) ->
    case kz_datamgr:get_results(AccountDb, <<"phone_numbers/crossbar_listing">>) of
        {'ok', []} ->
            lager:debug("account ~s holds no numbers", [AccountDb]),
            [];
        {'ok', JObjs} ->
            [{kz_doc:id(JObj)
             ,kz_json:get_value(<<"value">>, JObj)
             } || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("error listing numbers for ~s: ~p", [AccountDb, _R])
    end.
