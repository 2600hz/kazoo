%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number).

-export([
    get/1, get/2
    ,create/2
    ,move/2 ,move/3
    ,update/2 ,update/3
    ,delete/1 ,delete/2
    ,change_state/2 ,change_state/3
]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> number_return().
-spec get(ne_binary(), ne_binary()) -> number_return().
get(Num) ->
    get(Num, <<"system">>).

get(Num, AuthBy) ->
    case knm_converters:is_reconcilable(Num) of
        'false' -> {'error', 'not_reconcilable'};
        'true' -> knm_phone_number:fetch(Num, AuthBy)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), wh_proplist()) -> number_return().
create(Num, Props) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates =
        props:filter_undefined([
            {fun knm_phone_number:set_number/2, NormalizedNum}
            ,{fun knm_phone_number:set_number_db/2, NumberDb}
            ,{fun knm_phone_number:set_state/2, props:get_value(<<"state">>, Props, ?NUMBER_STATE_DISCOVERY)}
            ,{fun knm_phone_number:set_ported_in/2, props:get_is_true(<<"ported_in">>, Props, 'false')}
            ,{fun knm_phone_number:set_assigned_to/2, props:get_value(<<"assigned_to">>, Props)}
            ,{fun knm_phone_number:set_auth_by/2, props:get_value(<<"auth_by">>, Props, <<"system">>)}
        ]),
    Number = knm_phone_number:setters(knm_phone_number:new(), Updates),
    knm_phone_number:save(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move(ne_binary(), ne_binary()) -> number_return().
-spec move(ne_binary(), ne_binary(), ne_binary()) -> number_return().
move(Num, MoveTo) ->
    move(Num, MoveTo, <<"system">>).

move(Num, MoveTo, AuthBy) ->
    lager:debug("trying to move ~s to ~s", [Num, MoveTo]),
    case ?MODULE:get(Num, AuthBy) of
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


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), wh_proplist()) -> number_return().
-spec update(ne_binary(), wh_proplist(), ne_binary()) -> number_return().
update(Num, Props) ->
    update(Num, Props, <<"system">>).

update(Num, Props, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            UpdatedNumber = knm_phone_number:setters(Number, Props),
            knm_phone_number:save(UpdatedNumber)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) -> number_return().
-spec delete(ne_binary(), ne_binary()) -> number_return().
delete(Num) ->
    delete(Num, <<"system">>).

delete(Num, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            knm_phone_number:delete(Number)
    end.



%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_state(ne_binary(), ne_binary()) -> number_return().
-spec change_state(ne_binary(), ne_binary(), ne_binary()) -> number_return().
change_state(Num, State) ->
    change_state(Num, State, <<"system">>).

change_state(Num, State, AuthBy) ->
    case ?MODULE:get(Num, AuthBy) of
        {'error', _R}=E -> E;
        {'ok', Number} ->
            maybe_change_state(Number, State)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_change_state(number(), ne_binary()) -> number_return().
maybe_change_state(Number, ToState) ->
    FromState = knm_phone_number:state(Number),
    case is_change_allowed(FromState, ToState) of
        {'error', _R}=E -> E;
        'ok' ->
            UpdatedNumber = knm_phone_number:set_state(Number, ToState),
            knm_phone_number:save(UpdatedNumber)
    end.

-spec is_change_allowed(ne_binary(), ne_binary() | ne_binaries()) -> 'ok' | {'error', _}.
is_change_allowed(_FromState, ?NUMBER_STATE_PORT_IN) ->
    'ok';
is_change_allowed(FromState, ?NUMBER_STATE_PORT_OUT) ->
    is_change_allowed(FromState, [?NUMBER_STATE_PORT_IN]);
is_change_allowed(_FromState, ?NUMBER_STATE_DISCOVERY) ->
    'ok';
is_change_allowed(FromState, ?NUMBER_STATE_IN_SERVICE) ->
    is_change_allowed(FromState, [?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_AVAILABLE, ?NUMBER_STATE_DISCOVERY]);
is_change_allowed(FromState, ?NUMBER_STATE_RELEASED) ->
    is_change_allowed(FromState, [?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED]);
is_change_allowed(FromState, ?NUMBER_STATE_RESERVED) ->
    is_change_allowed(FromState, [?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_AVAILABLE]);
is_change_allowed(FromState, ?NUMBER_STATE_AVAILABLE) ->
    is_change_allowed(FromState, [?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED, ?NUMBER_STATE_RELEASED]);
is_change_allowed(FromState, ?NUMBER_STATE_DISCONNECTED) ->
    is_change_allowed(FromState, []);
is_change_allowed(FromState, ?NUMBER_STATE_DELETED) ->
    is_change_allowed(FromState, []);
is_change_allowed(FromState, Allowed) when is_list(Allowed) ->
    case lists:member(FromState, Allowed) of
        'true' -> 'ok';
        'false' -> {'error', 'wrong_state'}
    end;
is_change_allowed(_FromState, _ToState) ->
    lager:error("unknown state ~s", [_ToState]),
    {'error', 'unknown_state'}.







