%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number).

-export([move/2]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binary(), ne_binary()) -> {'ok', number()} | {'error', _}.
move(Num, MoveTo) ->
    lager:debug("trying to move ~s to ~s", [Num, MoveTo]),
    case knm_phone_number:fetch(Num) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            AccountId = wh_util:format_account_id(MoveTo, 'raw'),
            AssignedTo = knm_phone_number:assigned_to(Number),
            Props = [
                {fun knm_phone_number:set_assigned_to/2, AccountId}
                ,{fun knm_phone_number:set_prev_assigned_to/2, AssignedTo}
            ],
            UpdatedNumber = knm_phone_number:setters(Number, Props),
            knm_phone_number:save(UpdatedNumber)
    end.