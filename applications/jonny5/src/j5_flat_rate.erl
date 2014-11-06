%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_flat_rate).

-export([authorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-define(DEFAULT_TRUNK_ELIGIBLE_ON_REGEX_ERROR, 'true').
-define(DEFAULT_WHITELIST, <<"^\\+?1\\d{10}$">>).
-define(DEFAULT_BLACKLIST, <<"^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900|8(?:[0,2,3,4,5,6,7]{2}|8[0-9]))\\d{7}$">>).
-define(DEFAULT_INBOUND_WHITELIST, <<".*">>).
-define(DEFAULT_INBOUND_BLACKLIST, <<"^\\+?1(800|888|877|866|855|844)\\d{7}$">>).

-ifdef(TEST).
-define(TRUNK_ELIGIBLE_ON_REGEX_ERROR, ?DEFAULT_TRUNK_ELIGIBLE_ON_REGEX_ERROR).
-define(WHITELIST, ?DEFAULT_WHITELIST).
-define(BLACKLIST, ?DEFAULT_BLACKLIST).
-define(INBOUND_WHITELIST, ?DEFAULT_INBOUND_WHITELIST).
-define(INBOUND_BLACKLIST, ?DEFAULT_INBOUND_BLACKLIST).
-else.
-define(TRUNK_ELIGIBLE_ON_REGEX_ERROR, fun() ->  wh_util:is_true(whapps_config:get(<<"jonny5">>, <<"default_trunk_eligible_on_regex_error">>, ?DEFAULT_TRUNK_ELIGIBLE_ON_REGEX_ERROR)) end()).
-define(WHITELIST, fun() ->  whapps_config:get(<<"jonny5">>, <<"flat_rate_whitelist">>, ?DEFAULT_WHITELIST) end()).
-define(BLACKLIST, fun() ->  whapps_config:get(<<"jonny5">>, <<"flat_rate_blacklist">>, ?DEFAULT_BLACKLIST) end()).
-define(INBOUND_WHITELIST, fun() ->  whapps_config:get(<<"jonny5">>, <<"flat_rate_inbound_whitelist">>, ?DEFAULT_INBOUND_WHITELIST) end()).
-define(INBOUND_BLACKLIST, fun() ->  whapps_config:get(<<"jonny5">>, <<"flat_rate_inbound_blacklist">>, ?DEFAULT_INBOUND_BLACKLIST) end()).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    lager:debug("Checking account, ~s, for available flat rate trunks. Req Info: ~p"
		,[j5_limits:account_id(Limits), Request]),
    case eligible_for_flat_rate(Request) of
        'true' ->
            maybe_consume_flat_rate(Request, Limits);
        'false' ->
            lager:debug("number is not eligible for flat rate trunks", []),
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
-spec eligible_for_flat_rate(j5_request:request()) -> boolean().
eligible_for_flat_rate(Request) ->
    Direction = j5_request:call_direction(Request),
    case Direction of
        <<"inbound">>  -> 
	    TrunkWhitelist = ?INBOUND_WHITELIST,
	    TrunkBlacklist = ?INBOUND_BLACKLIST;
        <<"outbound">> -> 
	    TrunkWhitelist = ?WHITELIST,
	    TrunkBlacklist = ?BLACKLIST
    end,
    Number = wnm_util:to_e164(j5_request:number(Request)),
    lager:debug("Checking if number, ~s, matches to ~s white and black lists.", [Number, Direction]),
    lager:debug("whitelist: /~s/.", [TrunkWhitelist]),
    lager:debug("blacklist: /~s/.", [TrunkBlacklist]),
    case catch wh_util:is_empty(TrunkWhitelist) orelse re:run(Number, TrunkWhitelist) =/= nomatch of
	'true' -> 
		lager:debug("Matched trunk whitelist or empty whitelist.", []),
		case catch (wh_util:is_empty(TrunkBlacklist) orelse re:run(Number, TrunkBlacklist) =:= nomatch) of
			'true' -> 
				lager:debug("Did NOT match trunk blacklist or empty blacklist.", []),
				'true';
			'false' ->
				lager:debug("Matched trunk blacklist.", []),
				'false';
			Err ->
			    _Allow = ?TRUNK_ELIGIBLE_ON_REGEX_ERROR,
			    lager:debug("Error in ~s blacklist. trunk eligible anyway: ~p, bad regex: /~s/ error: ~p", [Direction, _Allow, TrunkBlacklist, Err]),
			    _Allow
		end;
	'false' ->
		lager:debug("Did NOT match trunk whitelist.", []),
		'false';
	Err ->
	    _Allow = ?TRUNK_ELIGIBLE_ON_REGEX_ERROR,
            lager:debug("Error in ~s whitelist. trunk eligible anyway: ~p, bad regex /~s/ error: ~p", [Direction, _Allow, TrunkWhitelist, Err]),
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
                        ,[Limit, Type, Remaining]),
            Remaining;
        _Else ->
            lager:debug("account is consuming ~p/~p ~s trunks"
                        ,[Used - 1, Limit, Type]),
            0
    end.

