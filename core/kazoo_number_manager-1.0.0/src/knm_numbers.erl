%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_numbers).

-export([
    get/1, get/2
    ,create/1
    ,move/1 ,move/2
    ,update/1 ,update/2
    ,delete/1 ,delete/2
    ,change_state/1 ,change_state/2
    ,assigned_to_app/1 ,assigned_to_app/2
]).

-include("knm.hrl").


-type numbers_return() :: [{ne_binary(), number_return()}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binaries()) -> numbers_return().
-spec get(ne_binaries(), ne_binary()) -> numbers_return().
-spec get(ne_binaries(), ne_binary(), numbers_return()) -> numbers_return().
get(Nums) ->
    get(Nums, <<"system">>).

get(Nums, AuthBy) ->
    get(Nums, AuthBy, []).

get([], _AuthBy, Acc) -> Acc;
get([Num|Nums], AuthBy, Acc) ->
    Return = knm_number:get(Num, AuthBy),
    get(Nums, AuthBy, [{Num, Return}|Acc]).


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
    Return  = knm_number:create(Num, Data),
    create(Props, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(wh_proplist()) -> numbers_return().
-spec move(wh_proplist(), ne_binary()) -> numbers_return().
-spec move(wh_proplist(), ne_binary(), numbers_return()) -> numbers_return().
move(Props) ->
    move(Props, <<"system">>).

move(Props, AuthBy) ->
    move(Props, AuthBy, []).

move([], _AuthBy, Acc) -> Acc;
move([{Num, MoveTo}|Props], AuthBy, Acc) ->
    Return  = knm_number:move(Num, MoveTo, AuthBy),
    move(Props, AuthBy, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(wh_proplist()) -> numbers_return().
-spec update(wh_proplist(), ne_binary()) -> numbers_return().
-spec update(wh_proplist(), ne_binary(), numbers_return()) -> numbers_return().
update(Props) ->
    update(Props, <<"system">>).

update(Props, AuthBy) ->
    update(Props, AuthBy, []).

update([], _AuthBy, Acc) -> Acc;
update([{Num, Data}|Props], AuthBy, Acc) ->
    Return  = knm_number:update(Num, Data, AuthBy),
    update(Props, AuthBy, [{Num, Return}|Acc]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binaries()) -> numbers_return().
-spec delete(ne_binaries(), ne_binary()) -> numbers_return().
-spec delete(ne_binaries(), ne_binary(), numbers_return()) -> numbers_return().
delete(Props) ->
    delete(Props, <<"system">>).

delete(Props, AuthBy) ->
    delete(Props, AuthBy, []).

delete([], _AuthBy, Acc) -> Acc;
delete([Num|Nums], AuthBy, Acc) ->
    Return = knm_number:delete(Num, AuthBy),
    delete(Nums, AuthBy, [{Num, Return}|Acc]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_state(wh_proplist()) -> numbers_return().
-spec change_state(wh_proplist(), ne_binary()) -> numbers_return().
-spec change_state(wh_proplist(), ne_binary(), numbers_return()) -> numbers_return().
change_state(Props) ->
    change_state(Props, <<"system">>).

change_state(Props, AuthBy) ->
    change_state(Props, AuthBy, []).

change_state([], _AuthBy, Acc) -> Acc;
change_state([{Num, State}|Props], AuthBy, Acc) ->
    Return  = knm_number:change_state(Num, State, AuthBy),
    change_state(Props, AuthBy, [{Num, Return}|Acc]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to_app(wh_proplist()) -> numbers_return().
-spec assigned_to_app(wh_proplist(), ne_binary()) -> numbers_return().
-spec assigned_to_app(wh_proplist(), ne_binary(), numbers_return()) -> numbers_return().
assigned_to_app(Props) ->
    assigned_to_app(Props, <<"system">>).

assigned_to_app(Props, AuthBy) ->
    assigned_to_app(Props, AuthBy, []).

assigned_to_app([], _AuthBy, Acc) -> Acc;
assigned_to_app([{Num, App}|Props], AuthBy, Acc) ->
    Return  = knm_number:assigned_to_app(Num, App, AuthBy),
    assigned_to_app(Props, AuthBy, [{Num, Return}|Acc]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

