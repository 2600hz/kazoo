%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

-include_lib("jonny5.hrl").

-record(limits, {account_id  :: api_binary()
                 ,account_db :: api_binary()
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
                 ,allotments = wh_json:new() :: wh_json:object()
                 ,soft_limit_inbound = 'false' :: boolean()
                 ,soft_limit_outbound = 'false' :: boolean()
                }).

-type limits() :: #limits{}.
-export_type([limits/0]).

-define(LIMITS_KEY(AccountId), {'limits', AccountId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> limits().
get(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case wh_cache:peek_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId)) of
        {'ok', Limits} -> Limits;
        {'error', 'not_found'} -> fetch(AccountId)
    end.

-spec fetch(ne_binary()) -> limits().
fetch(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    JObj = get_limit_jobj(AccountDb),
    Limits = #limits{account_id = AccountId
                     ,account_db = AccountDb
                     ,enabled = get_limit_boolean(<<"enabled">>, JObj, 'true')
                     ,calls = get_limit(<<"calls">>, JObj, -1)
                     ,resource_consuming_calls = get_limit(<<"resource_consuming_calls">>, JObj, -1)
                     ,inbound_trunks = get_limit(<<"inbound_trunks">>, JObj, 0)
                     ,outbound_trunks = get_limit(<<"outbound_trunks">>, JObj, 0)
                     ,twoway_trunks = get_limit(<<"twoway_trunks">>, JObj, -1)
                     ,bundled_inbound_trunks = get_bundled_inbound_limit(AccountDb, JObj)
                     ,bundled_outbound_trunks = get_bundled_outbound_limit(AccountDb, JObj)
                     ,bundled_twoway_trunks = get_bundled_twoway_limit(AccountDb, JObj)
                     ,burst_trunks = get_limit(<<"burst_trunks">>, JObj, 0)
                     ,max_postpay_amount = get_limit_units(<<"max_postpay_amount">>, JObj, 0.0) * -1
                     ,reserve_amount = get_limit_units(<<"reserve_amount">>, JObj, ?DEFAULT_RATE)
                     ,allow_prepay = get_limit_boolean(<<"allow_prepay">>, JObj, 'true')
                     ,allow_postpay = get_limit_boolean(<<"allow_postpay">>, JObj, 'false')
                     ,allotments = wh_json:get_value(<<"pvt_allotments">>, JObj, wh_json:new())
                     ,soft_limit_inbound = get_limit_boolean(<<"soft_limit_inbound">>, JObj, 'false')
                     ,soft_limit_outbound = get_limit_boolean(<<"soft_limit_outbound">>, JObj, 'false')
                    },
    CacheProps = [{'origin', {'db', AccountDb}}],
    wh_cache:store_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId), Limits, CacheProps),
    Limits.

-spec cached() -> [limits()].
cached() ->
    IsLimit = fun (_, #limits{}) -> 'true';
                  (_, _) -> 'false'
              end,
    [Limit || {_, Limit} <- wh_cache:filter_local(?JONNY5_CACHE, IsLimit)].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_props(ne_binary() | limits()) -> wh_proplist().
to_props(#limits{}=Limits) ->
    lists:zip(record_info('fields', 'limits'), tl(tuple_to_list(Limits)));
to_props(Account) -> to_props(?MODULE:get(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_id(limits()) -> api_binary().
account_id(#limits{account_id=AccountId}) -> AccountId;
account_id(_) -> 'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec enabled(limits()) -> boolean().
enabled(#limits{enabled=Enabled}) -> Enabled.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec soft_limit_outbound(limits()) -> boolean().
soft_limit_outbound(#limits{soft_limit_outbound=SoftLimit}) -> SoftLimit.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec soft_limit_inbound(limits()) -> boolean().
soft_limit_inbound(#limits{soft_limit_inbound=SoftLimit}) -> SoftLimit.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allotments(limits()) -> wh_json:object().
allotments(#limits{allotments=Allotments}) -> Allotments.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calls(limits()) -> tristate_integer().
calls(#limits{calls=Calls}) -> Calls.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_consuming_calls(limits()) -> tristate_integer().
resource_consuming_calls(#limits{resource_consuming_calls=Calls}) -> Calls.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec inbound_trunks(limits()) -> tristate_integer().
inbound_trunks(#limits{inbound_trunks=-1}) -> -1;
inbound_trunks(#limits{bundled_inbound_trunks=BundledTrunks
                       ,inbound_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec outbound_trunks(limits()) -> tristate_integer().
outbound_trunks(#limits{outbound_trunks=-1}) -> -1;
outbound_trunks(#limits{bundled_outbound_trunks=BundledTrunks
                        ,outbound_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec twoway_trunks(limits()) -> tristate_integer().
twoway_trunks(#limits{twoway_trunks=-1}) -> -1;
twoway_trunks(#limits{bundled_twoway_trunks=BundledTrunks
                      ,twoway_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec burst_trunks(limits()) -> tristate_integer().
burst_trunks(#limits{burst_trunks=Trunks}) -> Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allow_prepay(limits()) -> boolean().
allow_prepay(#limits{allow_prepay=AllowPrepay}) -> AllowPrepay.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allow_postpay(limits()) -> boolean().
allow_postpay(#limits{allow_postpay=AllowPostpay}) -> AllowPostpay.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reserve_amount(limits()) -> non_neg_integer().
reserve_amount(#limits{reserve_amount=ReserveAmount}) -> ReserveAmount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec max_postpay(limits()) -> non_neg_integer().
max_postpay(#limits{max_postpay_amount=MaxPostpay}) -> MaxPostpay.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_limit(ne_binary(), wh_json:object(), tristate_integer()) ->
                       tristate_integer().
get_limit(Key, JObj, Default) ->
    PrivateValue = get_private_limit(Key, JObj),
    PublicValue = get_public_limit(Key, JObj, Default),
    case PrivateValue =/= 'undefined'
        andalso PrivateValue < PublicValue
    of
        'true' -> PrivateValue;
        'false' -> PublicValue
    end.

-spec get_public_limit(ne_binary(), wh_json:object(), tristate_integer()) ->
                              non_neg_integer().
get_public_limit(Key, JObj, Default) ->
    case wh_json:get_integer_value(Key, JObj) of
        'undefined' -> get_default_limit(Key, Default);
        Value when Value < 0 -> 0;
        Value -> Value
    end.

-spec get_private_limit(ne_binary(), wh_json:object()) -> tristate_integer().
get_private_limit(Key, JObj) ->
    wh_json:get_integer_value(<<"pvt_", Key/binary>>, JObj).

-spec get_default_limit(ne_binary(), tristate_integer()) -> tristate_integer().
get_default_limit(Key, Default) ->
    whapps_config:get_integer(<<"jonny5">>, <<"default_", Key/binary>>, Default).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_limit_units(ne_binary(), wh_json:object(), float()) -> non_neg_integer().
get_limit_units(Key, JObj, Default) ->
    case wh_json:get_float_value(<<"pvt_", Key/binary>>, JObj) of
        'undefined' -> get_default_limit_units(Key, Default);
        Value -> wht_util:dollars_to_units(abs(Value))
    end.

-spec get_default_limit_units(ne_binary(), float()) -> non_neg_integer().
get_default_limit_units(Key, Default) ->
    Value = whapps_config:get_float(<<"jonny5">>, <<"default_", Key/binary>>, Default),
    wht_util:dollars_to_units(abs(Value)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_limit_boolean(ne_binary(), wh_json:object(), boolean()) -> boolean().
get_limit_boolean(Key, JObj, Default) ->
    case wh_json:get_value(<<"pvt_", Key/binary>>, JObj) of
        'undefined' -> get_public_limit_boolean(Key, JObj, Default);
        Value -> wh_util:is_true(Value)
    end.

-spec get_public_limit_boolean(ne_binary(), wh_json:object(), boolean()) -> boolean().
%% NOTE: all other booleans (inbound_soft_limit, allow_postpay, ect) should
%%  not be made public via this helper.
get_public_limit_boolean(<<"allow_prepay">> = Key, JObj, Default) ->
    case wh_json:get_value(Key, JObj) of
        'undefined' -> get_default_limit_boolean(Key, Default);
        Value -> wh_util:is_true(Value)
    end;
get_public_limit_boolean(Key, _, Default) ->
    get_default_limit_boolean(Key, Default).

-spec get_default_limit_boolean(ne_binary(), boolean()) -> boolean().
get_default_limit_boolean(Key, Default) ->
    whapps_config:get_is_true(<<"jonny5">>, <<"default_", Key/binary>>, Default).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_bundled_inbound_limit(ne_binary(), wh_json:object()) -> non_neg_integer().
get_bundled_inbound_limit(AccountDb, JObj) ->
    case wh_json:get_ne_value(<<"pvt_bundled_inbound_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_outbound_limit(ne_binary(), wh_json:object()) -> non_neg_integer().
get_bundled_outbound_limit(AccountDb, JObj) ->
    case wh_json:get_ne_value(<<"pvt_bundled_outbound_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_twoway_limit(ne_binary(), wh_json:object()) -> non_neg_integer().
get_bundled_twoway_limit(AccountDb, JObj) ->
    case wh_json:get_ne_value(<<"pvt_bundled_twoway_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_limit(ne_binary(), ne_binary()) -> non_neg_integer().
get_bundled_limit(AccountDb, View) ->
    case couch_mgr:get_results(AccountDb, View, []) of
        {'ok', JObjs} -> filter_bundled_limit(JObjs);
        {'error', _R} ->
            lager:debug("failed get bundled limit from ~s in ~s: ~p"
                        ,[View, AccountDb, _R]),
            0
    end.

-spec filter_bundled_limit(wh_json:objects()) -> non_neg_integer().
filter_bundled_limit(JObjs) ->
    length([JObj
            || JObj <- JObjs
                   ,wh_json:is_true([<<"value">>, <<"enabled">>]
                                    ,JObj
                                    ,'true')
           ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_limit_jobj(ne_binary()) -> wh_json:object().
get_limit_jobj(AccountDb) ->
    case couch_mgr:open_doc(AccountDb, <<"limits">>) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            lager:debug("limits doc in account db ~s not found", [AccountDb]),
            create_limit_jobj(AccountDb);
        {'error', _R} ->
            lager:debug("failed to open limits doc in account db ~s: ~p"
                        ,[AccountDb, _R]),
            wh_json:new()
    end.

-spec create_limit_jobj(ne_binary()) -> wh_json:object().
create_limit_jobj(AccountDb) ->
    TStamp = wh_util:current_tstamp(),
    JObj = wh_json:from_list(
             [{<<"_id">>, <<"limits">>}
              ,{<<"pvt_account_db">>, AccountDb}
              ,{<<"pvt_account_id">>, wh_util:format_account_id(AccountDb, 'raw')}
              ,{<<"pvt_type">>, <<"limits">>}
              ,{<<"pvt_created">>, TStamp}
              ,{<<"pvt_modified">>, TStamp}
              ,{<<"pvt_vsn">>, 1}
             ]),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {'ok', J} ->
            lager:debug("created initial limits document in db ~s", [AccountDb]),
            J;
         {'error', _R} ->
            lager:debug("failed to create initial limits document in db ~s: ~p", [AccountDb, _R]),
            wh_json:new()
    end.
