%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Handle renaming module_name for admins
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_rename_carrier).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_RENAME_CARRIER).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(N) ->
    PN = knm_number:phone_number(N),
    Value = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PN)),
    case false =/= kz_util:try_load_module(Value) of
        false ->
            Msg = <<"'", Value/binary, "' is not known by the system">>,
            knm_errors:invalid(N, Msg);
        true ->
            NewPN = knm_phone_number:set_module_name(PN, Value),
            knm_number:set_phone_number(N, NewPN)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(N) -> N.
