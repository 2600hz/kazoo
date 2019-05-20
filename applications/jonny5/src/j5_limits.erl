%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_limits).

-export([get/1]).
-export([fetch/1]).
-export([cached/0]).
-export([to_props/1]).
-export([account_id/1]).
-export([enabled/1]).
-export([soft_limit_outbound/1]).
-export([soft_limit_inbound/1]).
-export([allotments/1]).
-export([calls/1]).
-export([resource_consuming_calls/1]).
-export([inbound_trunks/1]).
-export([outbound_trunks/1]).
-export([twoway_trunks/1]).
-export([burst_trunks/1]).
-export([allow_prepay/1]).
-export([allow_postpay/1]).
-export([reserve_amount/1]).
-export([max_postpay/1]).
-export([authz_resource_types/1]).
-export([inbound_channels_per_did_rules/1]).

-include("jonny5.hrl").

-record(limits, {account_id  :: kz_term:api_binary()
                ,account_db :: kz_term:api_binary()
                ,enabled = 'true' :: boolean()
                ,calls = -1 :: tristate_integer()
                ,resource_consuming_calls = -1 :: tristate_integer()
                ,inbound_trunks = 0 :: tristate_integer()
                ,outbound_trunks = 0 :: tristate_integer()
                ,twoway_trunks = -1 :: tristate_integer()
                ,bundled_inbound_trunks = 0 :: non_neg_integer()
                ,bundled_outbound_trunks = 0 :: non_neg_integer()
                ,bundled_twoway_trunks = 0 :: non_neg_integer()
                ,burst_trunks = 0 :: tristate_integer()
                ,max_postpay_amount = 0 :: tristate_integer()
                ,reserve_amount = 0 :: non_neg_integer()
                ,allow_prepay = 'true' :: boolean()
                ,allow_postpay = 'false' :: boolean()
                ,allotments = kz_json:new() :: kz_json:object()
                ,soft_limit_inbound = 'false' :: boolean()
                ,soft_limit_outbound = 'false' :: boolean()
                ,authz_resource_types = [] :: list()
                ,inbound_channels_per_did_rules = kz_json:new() :: kz_json:object()
                }).

-type limits() :: #limits{}.
-export_type([limits/0]).

-define(LIMITS_KEY(AccountId), {'limits', AccountId}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get(kz_term:ne_binary()) -> limits().
get(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_cache:peek_local(?CACHE_NAME, ?LIMITS_KEY(AccountId)) of
        {'ok', Limits} -> Limits;
        {'error', 'not_found'} -> fetch(AccountId)
    end.

-spec fetch(kz_term:ne_binary()) -> limits().
fetch(Account) ->
    AccountId = kz_util:format_account_id(Account),
    AccountDb = kz_util:format_account_db(Account),
    JObj = kz_services_limits:fetch(AccountId),
    CacheOrigins = kz_json:get_ne_value(<<"pvt_cache_origins">>, JObj, []),
    case kz_term:is_empty(JObj) of
        'true' ->
            create_limits(AccountId, AccountDb, kzd_limits:new(AccountId));
        'false' when CacheOrigins =/= [] ->
            Limits = create_limits(AccountId, AccountDb, kz_json:delete_key(<<"pvt_cache_origins">>,JObj)),
            CacheProps = [{'origin', CacheOrigins}],
            kz_cache:store_local(?CACHE_NAME, ?LIMITS_KEY(AccountId), Limits, CacheProps),
            Limits;
        'false' ->
            create_limits(AccountId, AccountDb, kz_json:delete_key(<<"pvt_cache_origins">>,JObj))
    end.

-spec cached() -> [limits()].
cached() ->
    IsLimit = fun (_, #limits{}) -> 'true';
                  (_, _) -> 'false'
              end,
    [Limit || {_, Limit} <- kz_cache:filter_local(?CACHE_NAME, IsLimit)].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_props(kz_term:ne_binary() | limits()) -> kz_term:proplist().
to_props(#limits{}=Limits) ->
    lists:zip(record_info('fields', 'limits'), tl(tuple_to_list(Limits)));
to_props(Account) -> to_props(?MODULE:get(Account)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(limits()) -> kz_term:api_binary().
account_id(#limits{account_id=AccountId}) -> AccountId;
account_id(_) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec enabled(limits()) -> boolean().
enabled(#limits{enabled=Enabled}) -> Enabled.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec soft_limit_outbound(limits()) -> boolean().
soft_limit_outbound(#limits{soft_limit_outbound=SoftLimit}) -> SoftLimit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec soft_limit_inbound(limits()) -> boolean().
soft_limit_inbound(#limits{soft_limit_inbound=SoftLimit}) -> SoftLimit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allotments(limits()) -> kz_json:object().
allotments(#limits{allotments=Allotments}) -> Allotments.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calls(limits()) -> tristate_integer().
calls(#limits{calls=Calls}) -> Calls.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_consuming_calls(limits()) -> tristate_integer().
resource_consuming_calls(#limits{resource_consuming_calls=Calls}) -> Calls.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inbound_trunks(limits()) -> tristate_integer().
inbound_trunks(#limits{inbound_trunks=-1}) -> -1;
inbound_trunks(#limits{bundled_inbound_trunks=BundledTrunks
                      ,inbound_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec outbound_trunks(limits()) -> tristate_integer().
outbound_trunks(#limits{outbound_trunks=-1}) -> -1;
outbound_trunks(#limits{bundled_outbound_trunks=BundledTrunks
                       ,outbound_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec twoway_trunks(limits()) -> tristate_integer().
twoway_trunks(#limits{twoway_trunks=-1}) -> -1;
twoway_trunks(#limits{bundled_twoway_trunks=BundledTrunks
                     ,twoway_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec burst_trunks(limits()) -> tristate_integer().
burst_trunks(#limits{burst_trunks=Trunks}) -> Trunks.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allow_prepay(limits()) -> boolean().
allow_prepay(#limits{allow_prepay=AllowPrepay}) -> AllowPrepay.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allow_postpay(limits()) -> boolean().
allow_postpay(#limits{allow_postpay=AllowPostpay}) -> AllowPostpay.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reserve_amount(limits()) -> non_neg_integer().
reserve_amount(#limits{reserve_amount=ReserveAmount}) -> ReserveAmount.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec max_postpay(limits()) -> non_neg_integer().
max_postpay(#limits{max_postpay_amount=MaxPostpay}) -> MaxPostpay.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inbound_channels_per_did_rules(limits()) -> kz_json:object().
inbound_channels_per_did_rules(#limits{inbound_channels_per_did_rules=JObj}) -> JObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authz_resource_types(limits()) -> list().
authz_resource_types(#limits{authz_resource_types=AuthzResourceTypes}) ->
    AuthzResourceTypes.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_limits(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> limits().
create_limits(AccountId, AccountDb, JObj) ->
    #limits{account_id = AccountId
           ,account_db = AccountDb
           ,enabled = kzd_limits:enabled(JObj)
           ,calls = kzd_limits:calls(JObj)
           ,resource_consuming_calls = kzd_limits:resource_consuming_calls(JObj)
           ,inbound_trunks = kzd_limits:inbound_trunks(JObj)
           ,outbound_trunks = kzd_limits:outbound_trunks(JObj)
           ,twoway_trunks = kzd_limits:twoway_trunks(JObj)
           ,bundled_inbound_trunks = kzd_limits:bundled_inbound_trunks(JObj, AccountDb)
           ,bundled_outbound_trunks = kzd_limits:bundled_outbound_trunks(JObj, AccountDb)
           ,bundled_twoway_trunks = kzd_limits:bundled_twoway_trunks(JObj, AccountDb)
           ,burst_trunks = kzd_limits:burst_trunks(JObj)
           ,max_postpay_amount = kzd_limits:max_postpay_units(JObj) * -1
           ,reserve_amount = kzd_limits:reserve_units(JObj, ?DEFAULT_RATE)
           ,allow_prepay = kzd_limits:allow_prepay(JObj)
           ,allow_postpay = kzd_limits:allow_postpay(JObj)
           ,allotments = kzd_limits:allotments(JObj)
           ,soft_limit_inbound = kzd_limits:soft_limit_inbound(JObj)
           ,soft_limit_outbound = kzd_limits:soft_limit_outbound(JObj)
           ,authz_resource_types = kzd_limits:authz_resource_types(JObj)
           ,inbound_channels_per_did_rules = kzd_limits:inbound_channels_per_did_rules(JObj)
           }.
