%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_flat_rate).

-export([is_available/2]).

-export([limits/2]).
-export([outbound_jobj/2]).
-export([inbound_jobj/2]).

-include("jonny5.hrl").

-define(DEFAULT_WHITELIST, <<"^\\+?1\\d{10}$">>).
-define(DEFAULT_BLACKLIST, <<"^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900|8(?:[0,2,3,4,5,6,7]{2}|8[0-9]))\\d{7}$">>).

-ifdef(TEST).
-define(WHITELIST, ?DEFAULT_WHITELIST).
-define(BLACKLIST, ?DEFAULT_BLACKLIST).
-else.
-define(WHITELIST, fun() ->  whapps_config:get(<<"jonny5">>, <<"flat_rate_whitelist">>, ?DEFAULT_WHITELIST) end()).
-define(BLACKLIST, fun() ->  whapps_config:get(<<"jonny5">>, <<"flat_rate_blacklist">>, ?DEFAULT_BLACKLIST) end()).
-endif.

-spec is_available(j5_limits(), wh_json:object()) -> boolean().
is_available(Limits, JObj) ->
    case eligible_for_flat_rate(JObj) of
        false -> false;
        true -> maybe_consume_flat_rate(Limits, JObj)
    end.

-spec maybe_consume_flat_rate(j5_limits(), wh_json:object()) -> boolean().
maybe_consume_flat_rate(Limits, JObj) ->
    RemainingInbound = consume_inbound_limits(Limits, get_inbound_resources(JObj)),
    UnconsumedResources = consume_twoway_limits(Limits, RemainingInbound + get_outbound_resources(JObj)),
    UnconsumedResources =:= 0.

-spec eligible_for_flat_rate(wh_json:object()) -> boolean().
eligible_for_flat_rate(JObj) ->
    [Num, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
    Number = wnm_util:to_e164(Num), 
    TrunkWhitelist = ?WHITELIST,
    TrunkBlacklist = ?BLACKLIST,
    (wh_util:is_empty(TrunkWhitelist) orelse re:run(Number, TrunkWhitelist) =/= nomatch)
        andalso 
          (wh_util:is_empty(TrunkBlacklist) orelse re:run(Number, TrunkBlacklist) =:= nomatch).

-spec get_inbound_resources(wh_json:object()) -> integer().
get_inbound_resources(JObj) ->
    CurrentUsage = wh_json:get_integer_value([<<"Usage">>, <<"Inbound-Flat-Rate">>], JObj, 0),
    case wh_json:get_value(<<"Call-Direction">>, JObj) of
        <<"inbound">> -> CurrentUsage + 1;
        _Else -> CurrentUsage
    end.

-spec get_outbound_resources(wh_json:object()) -> integer().
get_outbound_resources(JObj) ->
    CurrentUsage = wh_json:get_integer_value([<<"Usage">>, <<"Outbound-Flat-Rate">>], JObj, 0),
    case wh_json:get_value(<<"Call-Direction">>, JObj) of
        <<"outbound">> -> CurrentUsage + 1;
        _Else -> CurrentUsage
    end.            

-spec consume_inbound_limits(j5_limits(), integer()) -> integer().
consume_inbound_limits(_, 0) ->
    0;
consume_inbound_limits(#limits{inbound_trunks=-1}, _) ->
    lager:debug("account has unlimited inbound trunks", []),
    0;
consume_inbound_limits(#limits{inbound_trunks=0}, Resources) ->
    lager:debug("account has no inbound only trunks", []),
    Resources;
consume_inbound_limits(#limits{inbound_trunks=Trunks}, Resources) ->
    case Resources - Trunks of
        Count when Count > 0 -> 
            lager:debug("all ~p inbound trunks consumed leaving ~p channels unaccounted for", [Trunks, Count]),  
            Count;
        _Else -> 
            lager:debug("account is consuming ~p/~p inbound trunks", [abs(_Else), Trunks]),
            0
    end.

-spec consume_twoway_limits(j5_limits(), integer()) -> integer().
consume_twoway_limits(_, 0) -> 0;
consume_twoway_limits(#limits{twoway_trunks=-1}, _) ->
    lager:debug("account has unlimited twoway trunks", []),
    0;
consume_twoway_limits(#limits{twoway_trunks=0}, Resources) -> 
    lager:debug("account has no two-way trunks", []),
    Resources;
consume_twoway_limits(#limits{twoway_trunks=Trunks}, Resources) ->
    case Resources - Trunks of
        Count when Count > 0 -> 
            lager:debug("all ~p two-way trunks consumed leaving ~p channels unaccounted for", [Trunks, Count]),  
            Count;
        _Else ->
            lager:debug("account is consuming ~p/~p two-way trunks", [abs(_Else), Trunks]),
            0
    end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

limits(I, T) ->
    #limits{inbound_trunks = I
            ,twoway_trunks = T
           }.

outbound_jobj(I, T) ->
    Props = [{<<"Inbound-Flat-Rate">>, I}
             ,{<<"Outbound-Flat-Rate">>, T}
            ],
    wh_json:from_list([{<<"Call-Direction">>, <<"outbound">>}
                       ,{<<"Usage">>, wh_json:from_list(Props)}
                       ,{<<"Request">>, <<"+14158867900@2600hz.com">>}
                      ]).

inbound_jobj(I, T) ->
    Props = [{<<"Inbound-Flat-Rate">>, I}
             ,{<<"Outbound-Flat-Rate">>, T}
            ],
    wh_json:from_list([{<<"Call-Direction">>, <<"inbound">>}
                       ,{<<"Usage">>, wh_json:from_list(Props)}
                       ,{<<"Request">>, <<"+14158867900@2600hz.com">>}
                      ]).

request_jobj(R) ->
    wh_json:from_list([{<<"Request">>, <<R/binary, "@2600hz.com">>}]).

get_inbound_resources_test() ->
    ?assertEqual(1, get_inbound_resources(inbound_jobj(0, 0))),
    ?assertEqual(2, get_inbound_resources(inbound_jobj(1, 4))),
    ?assertEqual(0, get_inbound_resources(outbound_jobj(0, 8))),
    ok.

get_outbound_resources_test() ->
    ?assertEqual(1, get_outbound_resources(outbound_jobj(0, 0))),
    ?assertEqual(2, get_outbound_resources(outbound_jobj(4, 1))),
    ?assertEqual(0, get_outbound_resources(inbound_jobj(8, 0))),
    ok.

consume_inbound_limits_test() ->
    %% unlimited inbound
    ?assertEqual(0, consume_inbound_limits(limits(-1, 0), 0)),
    %% no inbound with no inbound resources
    ?assertEqual(0, consume_inbound_limits(limits(0, 0), 0)),
    %% enough inbound resources
    ?assertEqual(0, consume_inbound_limits(limits(2, 0), 1)),
    %% just enough inbound resources
    ?assertEqual(0, consume_inbound_limits(limits(2, 0), 2)),
    %% not enough inbound resources
    ?assertEqual(1, consume_inbound_limits(limits(2, 0), 3)),
    ok.

consume_twoway_limits_test() ->
    %% unlimited twoway
    ?assertEqual(0, consume_twoway_limits(limits(0, -1), 0)),
    %% no twoway with no twoway resources
    ?assertEqual(0, consume_twoway_limits(limits(0, 0), 0)),
    %% enough twoway resources
    ?assertEqual(0, consume_twoway_limits(limits(0, 2), 1)),
    %% just enough twoway resources
    ?assertEqual(0, consume_twoway_limits(limits(0, 2), 2)),
    %% not enough twoway resources
    ?assertEqual(1, consume_twoway_limits(limits(0, 2), 3)),
    ok.

inbound_limits_test() ->
    %% New inbound, no current, unlimited
    ?assertEqual(true, is_available(limits(-1, 0), inbound_jobj(0, 0))),
    %% New inbound, no current, limit to 0
    ?assertEqual(false, is_available(limits(0, 0), inbound_jobj(0, 0))),
    %% New inbound, no current, limit to 1
    ?assertEqual(true, is_available(limits(1, 0), inbound_jobj(0, 0))),
    %% New inbound, 1 current, limit to 0 (should never happen)
    ?assertEqual(false, is_available(limits(0, 0), inbound_jobj(1, 0))),
    %% New inbound, 1 current, limit to 1
    ?assertEqual(false, is_available(limits(1, 0), inbound_jobj(1, 0))),
    ok.

outbound_limits_test() ->
    %% New outbound, no current, unlimited
    ?assertEqual(true, is_available(limits(0, -1), outbound_jobj(0, 0))),
    %% New outbound, no current, limit to 0
    ?assertEqual(false, is_available(limits(0, 0), outbound_jobj(0, 0))),
    %% New outbound, no current, limit to 1
    ?assertEqual(true, is_available(limits(0, 1), outbound_jobj(0, 0))),
    %% New outbound, 1 current, limit to 0 (should never happen)
    ?assertEqual(false, is_available(limits(0, 0), outbound_jobj(0, 1))),
    %% New outbound, 1 current, limit to 1
    ?assertEqual(false, is_available(limits(0, 1), outbound_jobj(0, 1))),
    ok.

inbound_spillover_test() ->
    %% New inbound, no current, 0 inbound unlimited two-way limit
    ?assertEqual(true, is_available(limits(0, -1), inbound_jobj(0, 0))),
    %% New inbound, no current, 0 inbound 1 two-way limit
    ?assertEqual(true, is_available(limits(0, 1), inbound_jobj(0, 0))),
    %% New inbound, 1 current, 1 inbound and 1 outbound limit
    ?assertEqual(true, is_available(limits(1, 1), inbound_jobj(1, 0))),
    %% New outbound, 1 current inbound, 0 inbound and 1 outbound limit
    ?assertEqual(false, is_available(limits(0, 1), outbound_jobj(1, 0))),
    %% New outbound, 1 current inbound, 0 inbound and 2 outbound limit
    ?assertEqual(true, is_available(limits(0, 2), outbound_jobj(1, 0))),
    ok.

eligible_test() ->
    ?assertEqual(false, is_available(limits(-1, -1), request_jobj(<<"+18445555555">>))),
    ?assertEqual(true, eligible_for_flat_rate(request_jobj(<<"+14158867900">>))),
    ?assertEqual(true, eligible_for_flat_rate(request_jobj(<<"+12038699901">>))),
    ?assertEqual(true, eligible_for_flat_rate(request_jobj(<<"+15103384922">>))),
    ?assertEqual(true, eligible_for_flat_rate(request_jobj(<<"+15185712222">>))),
    ?assertEqual(true, eligible_for_flat_rate(request_jobj(<<"+12038699978">>))),
    %% US 900 numbers
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+19005555555">>))),
    %% US 800 numbers
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18005555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18885555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18775555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18665555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18555555555">>))),
    %% Future US 800 numbers
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18445555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18335555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18225555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18805555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18815555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18825555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18835555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18845555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18855555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18865555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18875555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18895555555">>))),
    %% America Somoa
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+16845555555">>))),
    %% Anguilla
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+12645555555">>))),
    %% Antigua and Barbuda
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+12685555555">>))),
    %% Bahamas
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+12425555555">>))),
    %% Barbados
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+12465555555">>))),
    %% Bermuda
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+14415555555">>))),
    %% British Virgin Islands (awesome)
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+12845555555">>))),
    %% Cayman Islands
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+13455555555">>))),
    %% Dominica
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+17675555555">>))),
    %% Dominican Republic
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18095555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18295555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18495555555">>))),
    %% Grenada
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+14735555555">>))),
    %% Guam
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+16715555555">>))),
    %% Jamaica
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18765555555">>))),
    %% Montserrat
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+16645555555">>))),
    %% Northern Mariana Islans
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+16705555555">>))),
    %% Puerto Rico
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+17875555555">>))),
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+19395555555">>))),
    %% Saint Kitts nad Nevis
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18695555555">>))),
    %% Saint Lucia
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+17585555555">>))),
    %% Saint Vincent and the Grendadines
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+17845555555">>))),
    %% Sint Maarten
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+17215555555">>))),
    %% Trinidad and Tobago
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+18685555555">>))),
    %% Turks and Caicos Islands
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+16495555555">>))),
    %% United States Virgin Islands
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+13405555555">>))),
    %% US International
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+447775707800">>))), 
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+33667790037">>))), 
    ?assertEqual(false, eligible_for_flat_rate(request_jobj(<<"+4916096900000">>))), 
    ok.

-endif.
