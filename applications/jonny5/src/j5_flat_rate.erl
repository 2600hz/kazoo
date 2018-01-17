%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_flat_rate).

-export([authorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-define(DEFAULT_WHITELIST, <<"^\\+1\\d{10}$">>).
-define(DEFAULT_BLACKLIST, <<"^\\+1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900|800|888|877|866|855|844)\\d{7}$">>).

-define(WHITELIST, kapps_config:get_ne_binary(?APP_NAME, <<"flat_rate_whitelist">>, ?DEFAULT_WHITELIST)).
-define(BLACKLIST, kapps_config:get_ne_binary(?APP_NAME, <<"flat_rate_blacklist">>, ?DEFAULT_BLACKLIST)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    case eligible_for_flat_rate(Request) of
        'true' ->
            lager:debug("checking if account ~s has available flat rate trunks"
                       ,[j5_limits:account_id(Limits)]
                       ),
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
-spec eligible_for_flat_rate(j5_request:request()) -> boolean().
eligible_for_flat_rate(Request) ->
    Number = knm_converters:normalize(j5_request:number(Request)),
    {TrunkWhitelist, TrunkBlacklist} = maybe_get_resource_flat_rate(Request),
    lager:debug("using whitelist: ~p for flat rate check", [TrunkWhitelist]),
    lager:debug("using blacklist: ~p for flat rate check", [TrunkBlacklist]),
    (kz_term:is_empty(TrunkWhitelist)
     orelse re:run(Number, TrunkWhitelist) =/= 'nomatch'
    )
        andalso
          (kz_term:is_empty(TrunkBlacklist)
           orelse re:run(Number, TrunkBlacklist) =:= 'nomatch'
          ).

-define(SHOULD_LOOKUP_FLAT_RATE
       ,kapps_config:get_is_true(?APP_NAME, <<"resource_flat_rate_lookup">>, 'false')
       ).

-spec maybe_get_resource_flat_rate(j5_request:request()) ->
                                          {kz_term:ne_binary(), kz_term:ne_binary()}.
-spec maybe_get_resource_flat_rate(j5_request:request(), boolean()) ->
                                          {kz_term:ne_binary(), kz_term:ne_binary()}.
maybe_get_resource_flat_rate(Request) ->
    maybe_get_resource_flat_rate(Request, ?SHOULD_LOOKUP_FLAT_RATE).

maybe_get_resource_flat_rate(_Request, 'false') ->
    {?WHITELIST, ?BLACKLIST};
maybe_get_resource_flat_rate(Request, 'true') ->
    ResourceId = j5_request:resource_id(Request),
    case kz_datamgr:open_cache_doc(?KZ_OFFNET_DB, ResourceId) of
        {'ok', JObj} ->
            {kzd_resource:flat_rate_whitelist(JObj, ?WHITELIST)
            ,kzd_resource:flat_rate_blacklist(JObj, ?BLACKLIST)
            };
        {'error', _E} ->
            {?WHITELIST, ?BLACKLIST}
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
-spec consume_limit(integer(), integer(), kz_term:ne_binary()) -> integer().
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
