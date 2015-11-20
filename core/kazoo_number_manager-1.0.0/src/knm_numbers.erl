%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_numbers).

-export([get/1, get/2
         ,create/1
         ,move/1 ,move/2
         ,update/1 ,update/2
         ,delete/1 ,delete/2
         ,change_state/1 ,change_state/2
         ,assigned_to_app/1 ,assigned_to_app/2
         ,buy/2
         ,free/1
        ]).

-include("knm.hrl").

-define(KNM_PHONE_NUMBERS_DOC, <<"phone_numbers">>).

-type numbers_return() :: [{ne_binary(), knm_number_return()} | {'error', _}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binaries()) ->
                 numbers_return().
-spec get(ne_binaries(), knm_number_options:options()) ->
                 numbers_return().
-spec get(ne_binaries(), knm_number_options:options(), numbers_return()) ->
                 numbers_return().
get(Nums) ->
    get(Nums, knm_number_options:default()).

get(Nums, Options) ->
    get(Nums, Options, []).

get([], _Options, Acc) -> Acc;
get([Num|Nums], Options, Acc) ->
    Return = knm_number:get(Num, Options),
    get(Nums, Options, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(wh_proplist()) -> numbers_return().
-spec create(wh_proplist(), numbers_return()) -> numbers_return().
create(Props) ->
    create(Props, []).

create([], Acc) -> Acc;
create([{Num, Data}|Props], Acc) ->
    Return = knm_number:create(Num, Data),
    create(Props, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(wh_proplist()) ->
                  numbers_return().
-spec move(wh_proplist(), knm_number_options:options()) ->
                  numbers_return().
-spec move(wh_proplist(), knm_number_options:options(), numbers_return()) ->
                  numbers_return().
move(Props) ->
    move(Props, knm_number_options:default()).

move(Props, Options) ->
    move(Props, Options, []).

move([], _Options, Acc) -> Acc;
move([{Num, MoveTo}|Props], Options, Acc) ->
    Return = knm_number:move(Num, MoveTo, Options),
    move(Props, Options, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(wh_proplist()) ->
                    numbers_return().
-spec update(wh_proplist(), knm_number_options:options()) ->
                    numbers_return().
-spec update(wh_proplist(), knm_number_options:options(), numbers_return()) ->
                    numbers_return().
update(Props) ->
    update(Props, knm_number_options:default()).

update(Props, Options) ->
    update(Props, Options, []).

update([], _Options, Acc) -> Acc;
update([{Num, Data}|Props], Options, Acc) ->
    Return = knm_number:update(Num, Data, Options),
    update(Props, Options, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binaries()) ->
                    numbers_return().
-spec delete(ne_binaries(), knm_number_options:options()) ->
                    numbers_return().
-spec delete(ne_binaries(), knm_number_options:options(), numbers_return()) ->
                    numbers_return().
delete(Props) ->
    delete(Props, knm_number_options:default()).

delete(Props, Options) ->
    delete(Props, Options, []).

delete([], _Options, Acc) -> Acc;
delete([Num|Nums], Options, Acc) ->
    Return = knm_number:delete(Num, Options),
    delete(Nums, Options, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_state(wh_proplist()) ->
                          numbers_return().
-spec change_state(wh_proplist(), knm_number_options:options()) ->
                          numbers_return().
-spec change_state(wh_proplist(), knm_number_options:options(), numbers_return()) ->
                          numbers_return().
change_state(Props) ->
    change_state(Props, knm_number_options:default()).

change_state(Props, Options) ->
    change_state(Props, Options, []).

change_state([], _Options, Acc) -> Acc;
change_state([{Num, State}|Props], Options, Acc) ->
    try knm_number_states:to_state(Num, State, Options) of
        Number -> change_state(Props, Options, [{Num, {'ok', Number}}|Acc])
    catch
        'throw':R ->
            change_state(Props, Options, [{Num, R} | Acc])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to_app(wh_proplist()) ->
                             numbers_return().
-spec assigned_to_app(wh_proplist(), knm_number_options:options()) ->
                             numbers_return().
-spec assigned_to_app(wh_proplist(), knm_number_options:options(), numbers_return()) ->
                             numbers_return().
assigned_to_app(Props) ->
    assigned_to_app(Props, knm_number_options:default()).

assigned_to_app(Props, Options) ->
    assigned_to_app(Props, Options, []).

assigned_to_app([], _Options, Acc) -> Acc;
assigned_to_app([{Num, App}|Props], Options, Acc) ->
    Return = knm_number:assign_to_app(Num, App, Options),
    assigned_to_app(Props, Options, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec buy(ne_binaries(), ne_binary()) -> numbers_return().
buy(Nums, Account) ->
    [knm_number:buy(Num, Account) || Num <- Nums].

-spec free(ne_binary()) -> 'ok'.
free(<<__/binary>> = AccountId) ->
    case couch_mgr:open_cache_doc(
           wh_util:format_account_id(AccountId, 'encoded')
           ,?KNM_PHONE_NUMBERS_DOC
          )
    of
        {'ok', JObj} ->
            free_numbers(AccountId, JObj);
        {'error', _E} ->
            lager:debug("failed to open ~s in ~s: ~p"
                        ,[?KNM_PHONE_NUMBERS_DOC, AccountId, _E]
                       )
    end.

-spec free_numbers(ne_binary(), wh_json:object()) -> 'ok'.
free_numbers(AccountId, JObj) ->
    lists:foreach(fun(DID) ->
                          maybe_free_number(AccountId, DID)
                  end
                  ,wh_json:get_public_keys(JObj)
                 ).

-spec maybe_free_number(ne_binary(), ne_binary()) -> 'ok'.
maybe_free_number(AccountId, DID) ->
    case knm_number:get(DID) of
        {'error', _E} ->
            lager:debug("failed to release ~s for account ~s: ~p", [DID, AccountId, _E]);
        {'ok', Number} ->
            check_to_free_number(AccountId, Number)
    end.

-spec check_to_free_number(ne_binary(), knm_number:knm_number()) -> 'ok'.
check_to_free_number(AccountId, Number) ->
    check_to_free_number(AccountId
                         ,Number
                         ,knm_phone_number:assigned_to(knm_number:phone_number(Number))
                        ).

-spec check_to_free_number(ne_binary(), knm_number:knm_number(), api_binary()) -> 'ok'.
check_to_free_number(AccountId, Number, AccountId) ->
    try knm_number_states:to_state(Number, ?NUMBER_STATE_RELEASED) of
        _Number ->
            lager:debug("released ~s for account ~s"
                        ,[knm_phone_number:number(knm_number:phone_number(Number))
                          ,AccountId
                         ]
                       )
    catch
        'throw':_R ->
            lager:debug("failed to release ~s for account ~s: ~p"
                        ,[knm_phone_number:number(knm_number:phone_number(Number))
                          ,AccountId
                          ,_R
                         ]
                       )
    end;
check_to_free_number(_AccountId, Number, _OtherAccountId) ->
    lager:debug("not releasing ~s for account ~s; it is assigned to account ~s"
                ,[knm_phone_number:number(knm_number:phone_number(Number))
                  ,_AccountId
                  ,_OtherAccountId
                 ]
               ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
