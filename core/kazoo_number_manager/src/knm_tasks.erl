%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_tasks).

-export([number/1
        ]).

-export([assign_to/4
        ]).

-include("knm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Verifiers

-spec number(ne_binary()) -> boolean().
number(<<"+", _/binary>>) -> 'true';
number(_) -> 'false'.

%% Appliers

-spec assign_to(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> any().
assign_to(Props, Number, AccountId, 'undefined') ->
    AuthBy = props:get_value('auth_account_id', Props),
    assign_to(Props, Number, AccountId, AuthBy);
assign_to(_Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', AuthBy}
              ],
    knm_number:move(Number, AccountId, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% End of Module.
