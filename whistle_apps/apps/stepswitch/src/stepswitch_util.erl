%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_util).

-export([lookup_number/1]).
-export([evaluate_number/2]).
-export([evaluate_flags/2]).
-export([get_dialstring/2]).

-include_lib("stepswitch/src/stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec lookup_number/1 :: (ne_binary()) -> {'ok', ne_binary(), proplist()} | {'error', term()}.
lookup_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, cache_key_number(Num)) of
        {ok, {AccountId, Props}} -> {ok, AccountId, Props};
        {error, not_found} -> fetch_number(Num)
    end.

-spec fetch_number/1 :: (ne_binary()) -> {'ok', ne_binary(), proplist()} | {'error', term()}.
fetch_number(Num) ->
    case wh_number_manager:lookup_account_by_number(Num) of
        {ok, AccountId, Props} ->
            _ = maybe_transition_port_in(Num, Props),
            wh_cache:store_local(?STEPSWITCH_CACHE, cache_key_number(Num), {AccountId, Props}),
            lager:debug("~s is associated with account ~s", [Num, AccountId]),
            {ok, AccountId, Props};
        {error, Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec maybe_transition_port_in/2 :: (ne_binary(), proplist()) -> false|pid().
maybe_transition_port_in(Num, Props) ->
    case props:get_value(pending_port, Props) of
        false -> false;
        true -> spawn(fun() -> wh_number_manager:ported(Num) end)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter the list of resources returning only those with a rule that
%% matches the number.  The list is of tuples with three elements,
%% the weight, the captured component of the number, and the gateways.
%% @end
%%--------------------------------------------------------------------
-spec evaluate_number/2 :: (ne_binary(), [#resrc{}]) -> endpoints().
evaluate_number(Number, Resrcs) ->
    sort_endpoints(get_endpoints(Number, Resrcs)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter the list of resources returning only those that have every
%% flag provided
%% @end
%%--------------------------------------------------------------------
-spec evaluate_flags/2 :: (list(), [#resrc{}]) -> [#resrc{}].
evaluate_flags(F1, Resrcs) ->
    [Resrc
     || #resrc{flags=F2}=Resrc <- Resrcs,
        lists:all(fun(Flag) -> 
                          wh_util:is_empty(Flag)
                              orelse lists:member(Flag, F2)
                  end, F1)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Build the sip url of a resource gateway
%% @end
%%--------------------------------------------------------------------
-spec get_dialstring/2 :: (#gateway{}, ne_binary()) -> ne_binary().
get_dialstring(#gateway{route = undefined, prefix=Prefix, suffix=Suffix, server=Server}, Number) ->
    list_to_binary(["sip:"
                    ,wh_util:to_binary(Prefix)
                    ,Number
                    ,wh_util:to_binary(Suffix)
                    ,"@"
                    ,wh_util:to_binary(Server)
                   ]);
get_dialstring(#gateway{route=Route}, _) ->
    Route.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sort the gateway tuples returned by evalutate_resrcs according to
%% weight.
%% @end
%%--------------------------------------------------------------------
-spec sort_endpoints/1 :: (endpoints()) -> endpoints().
sort_endpoints(Endpoints) ->
    lists:sort(fun({W1, _, _, _, _}, {W2, _, _, _, _}) ->
                       W1 =< W2
               end, Endpoints).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints/2 :: (ne_binary(), [#resrc{}]) -> endpoints().
get_endpoints(Number, Resrcs) ->
    EPs = [get_endpoint(Number, R) || R <- Resrcs],
    [Endpoint || Endpoint <- EPs, Endpoint =/= no_match].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint/2 :: (ne_binary(), #resrc{}) -> endpoint() | 'no_match'.
get_endpoint(Number, #resrc{weight_cost=WC, gateways=Gtws, rules=Rules
                            ,grace_period=GP, is_emergency=IsEmergency}) ->
    case evaluate_rules(Rules, Number) of
        {ok, DestNum} ->
            {WC, GP, DestNum, Gtws, IsEmergency};
        {error, no_match} ->
            no_match
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function loops over rules (regex) and until one matches
%% the destination number.  If the matching rule has a
%% capture group return the largest group, otherwise return the whole
%% number.  In the event that no rules match then return an error.
%% @end
%%--------------------------------------------------------------------
-spec evaluate_rules/2 :: (re:mp(), ne_binary()) -> {'ok', ne_binary()} |
                                                    {'error', 'no_match'}.
evaluate_rules([], _) ->
    {error, no_match};
evaluate_rules([Regex|T], Number) ->
    case re:run(Number, Regex) of
        {match, [{Start,End}]} ->
            {ok, binary:part(Number, Start, End)};
        {match, CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {ok, binary:part(Number, Start, End)};
        _ ->
            evaluate_rules(T, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec cache_key_number/1 :: (ne_binary()) -> {'stepswitch_number', ne_binary()}.
cache_key_number(Number) ->
    {stepswitch_number, Number}.
