%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% David Singer
%%%-------------------------------------------------------------------
-module(j5_flat_rate).

-export([authorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-define(DEFAULT_WHITELIST, <<"^\\+?1\\d{10}$">>).
-define(DEFAULT_BLACKLIST, <<"^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900|8(?:[0,2,3,4,5,6,7]{2}|8[0-9]))\\d{7}$">>).
-define(MAX_RECURSE_DEPTH, whapps_config:get_integer(?CONFIG_CAT, <<"dialed_region_max_recurse_depth">>, 20)).

-ifdef(TEST).
-define(WHITELIST, ?DEFAULT_WHITELIST).
-define(BLACKLIST, ?DEFAULT_BLACKLIST).
-define(TRUNK_ELIGIBLE_ON_REGEX_ERROR, 'false').
-else.
-define(WHITELIST, whapps_config:get(<<"jonny5">>, <<"flat_rate_whitelist">>, ?DEFAULT_WHITELIST)).
-define(BLACKLIST, whapps_config:get(<<"jonny5">>, <<"flat_rate_blacklist">>, ?DEFAULT_BLACKLIST)).
-define(TRUNK_ELIGIBLE_ON_REGEX_ERROR, fun() ->  wh_util:is_true(whapps_config:get(<<"jonny5">>, <<"default_trunk_eligible_on_regex_error">>, ?DEFAULT_TRUNK_ELIGIBLE_ON_REGEX_ERROR)) end()).
-define(DEFAULT_TRUNK_ELIGIBLE_ON_REGEX_ERROR
        ,whapps_config:get_is_true(?CONFIG_CAT, <<"is_trunk_eligible_on_regex_error">>, 'false')
       ).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    lager:debug("checking if account ~s has available flat rate trunks"
		,[j5_limits:account_id(Limits)]
               ),
    case eligible_for_flat_rate(Request, Limits) of
        'true' ->
            maybe_consume_flat_rate(Request, Limits);
        'false' ->
            lager:debug("number '~s' is not eligible for flat rate trunks"
                        ,[j5_request:number(Request)]
                       ),
            Request
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(_, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec eligible_for_flat_rate(j5_request:request(), j5_limits:limits()) -> boolean().
eligible_for_flat_rate(Request, Limits) ->
    try
        eligible_for_flat_rate(
            j5_limits:trunk_region_id(Limits)
            , wnm_util:to_e164(j5_request:number(Request))
            , j5_request:call_direction(Request)
            , j5_request:classification(Request)
            , []
            , ?MAX_RECURSE_DEPTH
        )
    catch
        _E:_R ->
            Allow = ?TRUNK_ELIGIBLE_ON_REGEX_ERROR,
            lager:debug("regex error processing trunk eligibility: ~s: ~p", [_E, _R]),
            lager:debug("default allowing: ~s", [Allow]),
            Allow
    end.

-spec eligible_for_flat_rate(api_binaries() | ne_binary()
                             ,ne_binary(), ne_binary()
                             ,ne_binary(), ne_binaries()
                             ,integer()
                            ) -> boolean().
eligible_for_flat_rate('undefined', Number, Direction, _, _, _) ->
    match_default_whiteblack_list(Number, Direction);
eligible_for_flat_rate(_TrunkDefIDs, _, _, _, _Path, Depth) when Depth =< 0 ->
    lager:debug("reached dialed_region_max_recurse_depth: ~p. No loop detected. Current trunk_region_id(s): ~p, path: ~p", [?MAX_RECURSE_DEPTH, _TrunkDefIDs, _Path]),
    'false';
eligible_for_flat_rate([], _Number, _Direction, _Class, _Path, _Depth) ->
    'false';
eligible_for_flat_rate(TrunkDefID, Number, Direction, Class, Path, Depth) when is_binary(TrunkDefID) ->
    eligible_for_flat_rate([TrunkDefID], Number, Direction, Class, Path, Depth);
eligible_for_flat_rate([TrunkDefID|TrunkDefIDs], Number, Direction, Class, Path, Depth) ->
    case trunk_def_loop_detect(TrunkDefID, Path) of
        'true' ->
            eligible_for_flat_rate(TrunkDefIDs, Number, Direction, Class, Path, Depth);
        'false' ->
            CustomTrunkDefs = whapps_config:get(?CONFIG_CAT, [<<"dialed_region_definitions">>, <<"default">>, TrunkDefID]),
            BlackClass  = wh_json:get_value(<<Direction/binary, "_black_num_classifications">>, CustomTrunkDefs),
            WhiteClass  = wh_json:get_value(<<Direction/binary, "_white_num_classifications">>, CustomTrunkDefs),
            WhiteRecurseDefs = wh_json:get_value(<<"include_defs">>, CustomTrunkDefs),
            BlackRecurseDefs = wh_json:get_value(<<"exclude_defs">>, CustomTrunkDefs),
            %% check called number class against black_num_classifications.
            %% If matched reject. Else try the other methods that might authorize the trunk.
            lager:debug("checking def id: ~s, others this level: ~p. trunk def: ~p"
                        ,[TrunkDefID, TrunkDefIDs, CustomTrunkDefs]
                       ),
            not (
              maybe_class(Class, BlackClass, <<"black">>)
              orelse eligible_for_flat_rate(BlackRecurseDefs, Number, Direction, Class, [TrunkDefID|Path], Depth - 1)
             )
                andalso (
                  maybe_class(Class, WhiteClass, <<"white">>)
                  orelse match_custom_whiteblack_list(CustomTrunkDefs, Number, Direction)
                  orelse eligible_for_flat_rate(WhiteRecurseDefs, Number, Direction, Class, [TrunkDefID|Path], Depth - 1)
                  orelse eligible_for_flat_rate(TrunkDefIDs, Number, Direction, Class, Path, Depth)
                 )
    end.

-spec trunk_def_loop_detect(ne_binary(), ne_binaries()) -> boolean().
trunk_def_loop_detect(TrunkDefID, Path) ->
    case lists:member(TrunkDefID, Path) of
        'true' ->
            lager:warning("WARNING Trunk definition loop detected. Current ID: ~s, Path: ~p", [TrunkDefID, Path]),
            'true';
        'false' -> 'false'
    end.

-spec maybe_class(ne_binary(), list(), ne_binary()) -> boolean().
maybe_class(Class, [_|_]=Classes, Color) ->
    Matched = lists:member(Class, Classes),
    case Matched of
        'true' ->
            lager:debug("Num class ~s found in ~s classifications ~p", [Class, Color, Classes]);
        'false' ->
            lager:debug("Num class ~s Not found in ~s classifications ~p", [Class, Color, Classes])
    end,
    Matched;
maybe_class(_, 'undefined', _) ->
    'false';
maybe_class(_, BadFormat, Color) ->
    lager:debug("Bad format of ~s class list. Found ~p. Should be like ['us_did','uk_did'].", [Color, BadFormat]),
    'false'.

-spec match_custom_whiteblack_list(wh_json:object(), ne_binary(), ne_binary()) -> boolean().
match_custom_whiteblack_list(CustomTrunkDefs, Number, Direction) ->
    TrunkWhitelist = wh_json:get_value(<<Direction/binary, "_whitelist">>, CustomTrunkDefs),
    TrunkBlacklist = wh_json:get_value(<<Direction/binary, "_blacklist">>, CustomTrunkDefs),
    match_whiteblack_list(TrunkWhitelist, TrunkBlacklist, Number).

-spec match_default_whiteblack_list(ne_binary(), ne_binary()) -> boolean().
match_default_whiteblack_list(Number, Direction) ->
    lager:debug("Using system white and black lists"),
    TrunkWhitelist = get_default_whiteblack_list(Direction, <<"_whitelist">>, ?DEFAULT_WHITELIST),
    TrunkBlacklist = get_default_whiteblack_list(Direction, <<"_blacklist">>, ?DEFAULT_BLACKLIST),
    match_whiteblack_list(TrunkWhitelist, TrunkBlacklist, Number).


-spec get_default_whiteblack_list(ne_binary(), ne_binary(), ne_binary()) -> binary().
get_default_whiteblack_list(Direction, Color, Default) ->
    case whapps_config:get(?CONFIG_CAT, <<"flat_rate_", Direction/binary, Color/binary>>) of
        'undefined' ->
            whapps_config:get(?CONFIG_CAT, <<"flat_rate", Color/binary>>, Default);
        _Result ->
            _Result
    end.

-spec match_whiteblack_list(ne_binary(), ne_binary(), ne_binary()) -> boolean().
match_whiteblack_list(TrunkWhitelist, TrunkBlacklist, Number) ->
    try wh_util:is_empty(TrunkWhitelist) orelse re:run(Number, TrunkWhitelist) =/= 'nomatch' of
        'true' ->
            lager:debug("Matched trunk whitelist or empty whitelist.", []),
            match_black_list(TrunkBlacklist, Number);
        'false' ->
            lager:debug("Did NOT match trunk whitelist.", []),
            'false'
    catch
        Err ->
            _Allow = ?TRUNK_ELIGIBLE_ON_REGEX_ERROR,
            lager:debug("Error in whitelist. trunk eligible anyway: ~p, bad regex /~s/ error: ~p", [_Allow, TrunkWhitelist, Err]),
            _Allow
    end.

-spec match_black_list(ne_binary(), ne_binary()) -> boolean().
match_black_list(TrunkBlacklist, Number) ->
    try (wh_util:is_empty(TrunkBlacklist) orelse re:run(Number, TrunkBlacklist) =:= 'nomatch') of
        'true' ->
            lager:debug("Did NOT match trunk blacklist or empty blacklist.", []),
            'true';
        'false' ->
            lager:debug("Matched trunk blacklist.", []),
            'false'
    catch
        Err ->
            _Allow = ?TRUNK_ELIGIBLE_ON_REGEX_ERROR,
            lager:debug("Error in blacklist. trunk eligible anyway: ~p, bad regex: /~s/ error: ~p", [_Allow, TrunkBlacklist, Err]),
            _Allow
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_consume_flat_rate(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_consume_flat_rate(Request, Limits) ->
    RemainingInbound = consume_inbound_limits(Request, Limits),
    RemainingOutbound = consume_outbound_limits(Request, Limits),
    case consume_twoway_limits(RemainingInbound + RemainingOutbound, Limits) of
        0 -> j5_request:authorize(<<"flat_rate">>, Request, Limits);
        Remaining -> maybe_consume_flat_rate_burst(Remaining, Request, Limits)
    end.

-spec maybe_consume_flat_rate_burst(non_neg_integer(), j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_consume_flat_rate_burst(Remaining, Request, Limits) ->
    Limit = j5_limits:burst_trunks(Limits),
    case consume_limit(Limit, Remaining, <<"burst">>) of
        0 -> j5_request:authorize(<<"flat_rate_burst">>, Request, Limits);
        _Else -> Request
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec consume_inbound_limits(j5_request:request(), j5_limits:limits()) -> integer().
consume_inbound_limits(Request, Limits) ->
    Used = get_inbound_resources(Request, Limits),
    Limit = j5_limits:inbound_trunks(Limits),
    consume_limit(Limit, Used, <<"inbound">>).

-spec get_inbound_resources(j5_request:request(), j5_limits:limits()) -> integer().
get_inbound_resources(Request, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    CurrentUsage = j5_channels:inbound_flat_rate(AccountId),
    case j5_request:call_direction(Request) of
        <<"inbound">> -> CurrentUsage + 1;
        <<"outbound">> -> CurrentUsage
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec consume_outbound_limits(j5_request:request(), j5_limits:limits()) -> integer().
consume_outbound_limits(Request, Limits) ->
    Used = get_outbound_resources(Request, Limits),
    Limit = j5_limits:outbound_trunks(Limits),
    consume_limit(Limit, Used, <<"outbound">>).

-spec get_outbound_resources(j5_request:request(), j5_limits:limits()) -> integer().
get_outbound_resources(Request, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    CurrentUsage = j5_channels:outbound_flat_rate(AccountId),
    case j5_request:call_direction(Request) of
        <<"inbound">> -> CurrentUsage;
        <<"outbound">> -> CurrentUsage + 1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec consume_twoway_limits(integer(), j5_limits:limits()) -> integer().
consume_twoway_limits(Used, Limits) ->
    Limit = j5_limits:twoway_trunks(Limits),
    consume_limit(Limit, Used, <<"twoway">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec consume_limit(integer(), integer(), ne_binary()) -> integer().
consume_limit(-1, _, Type) ->
    lager:debug("account has unlimited ~s trunks", [Type]),
    0;
consume_limit(0, Used, Type) ->
    lager:debug("account has no ~s trunks", [Type]),
    Used;
consume_limit(Limit, 0, Type) ->
    lager:debug("account is consuming 0/~p ~s trunks", [Limit, Type]),
    0;
consume_limit(Limit, Used, Type) ->
    case Used - Limit of
        Remaining when Remaining > 0 ->
            lager:debug("all ~p ~s trunks consumed leaving ~p channels unaccounted for"
                        ,[Limit, Type, Remaining]
                       ),
            Remaining;
        _Else ->
            lager:debug("account is consuming ~p/~p ~s trunks"
                        ,[Used - 1, Limit, Type]
                       ),
            0
    end.
