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
-spec get(ne_binaries(), wh_proplist()) -> numbers_return().
-spec get(ne_binaries(), wh_proplist(), numbers_return()) -> numbers_return().
get(Nums) ->
    get(Nums, knm_phone_number:default_options()).

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
    Return  = knm_number:create(Num, Data),
    create(Props, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(wh_proplist()) -> numbers_return().
-spec move(wh_proplist(), wh_proplist()) -> numbers_return().
-spec move(wh_proplist(), wh_proplist(), numbers_return()) -> numbers_return().
move(Props) ->
    move(Props, knm_phone_number:default_options()).

move(Props, Options) ->
    move(Props, Options, []).

move([], _Options, Acc) -> Acc;
move([{Num, MoveTo}|Props], Options, Acc) ->
    Return  = knm_number:move(Num, MoveTo, Options),
    move(Props, Options, [{Num, Return}|Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(wh_proplist()) -> numbers_return().
-spec update(wh_proplist(), wh_proplist()) -> numbers_return().
-spec update(wh_proplist(), wh_proplist(), numbers_return()) -> numbers_return().
update(Props) ->
    update(Props, knm_phone_number:default_options()).

update(Props, Options) ->
    update(Props, Options, []).

update([], _Options, Acc) -> Acc;
update([{Num, Data}|Props], Options, Acc) ->
    Return  = knm_number:update(Num, Data, Options),
    update(Props, Options, [{Num, Return}|Acc]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binaries()) -> numbers_return().
-spec delete(ne_binaries(), wh_proplist()) -> numbers_return().
-spec delete(ne_binaries(), wh_proplist(), numbers_return()) -> numbers_return().
delete(Props) ->
    delete(Props, knm_phone_number:default_options()).

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
-spec change_state(wh_proplist()) -> numbers_return().
-spec change_state(wh_proplist(), wh_proplist()) -> numbers_return().
-spec change_state(wh_proplist(), wh_proplist(), numbers_return()) -> numbers_return().
change_state(Props) ->
    change_state(Props, knm_phone_number:default_options()).

change_state(Props, Options) ->
    change_state(Props, Options, []).

change_state([], _Options, Acc) -> Acc;
change_state([{Num, State}|Props], Options, Acc) ->
    Return  = knm_number:change_state(Num, State, Options),
    change_state(Props, Options, [{Num, Return}|Acc]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to_app(wh_proplist()) -> numbers_return().
-spec assigned_to_app(wh_proplist(), wh_proplist()) -> numbers_return().
-spec assigned_to_app(wh_proplist(), wh_proplist(), numbers_return()) -> numbers_return().
assigned_to_app(Props) ->
    assigned_to_app(Props, knm_phone_number:default_options()).

assigned_to_app(Props, Options) ->
    assigned_to_app(Props, Options, []).

assigned_to_app([], _Options, Acc) -> Acc;
assigned_to_app([{Num, App}|Props], Options, Acc) ->
    Return  = knm_number:assigned_to_app(Num, App, Options),
    assigned_to_app(Props, Options, [{Num, Return}|Acc]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

