%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_util).

-export([lookup_number/1]).
-export([maybe_gateway_by_address/2]).
-export([evaluate_number/2]).
-export([evaluate_flags/2]).
-export([get_dialstring/2]).
-export([get_outbound_t38_settings/1, get_outbound_t38_settings/2]).
-export([create_resrc/1]).
-export([maybe_transition_port_in/2]).

-include("stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_number(ne_binary()) ->
                           {'ok', ne_binary(), wh_proplist()} |
                           {'error', term()}.
lookup_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, cache_key_number(Num)) of
        {'ok', {AccountId, Props}} -> {'ok', AccountId, Props};
        {'error', 'not_found'} -> fetch_number(Num)
    end.

-spec fetch_number(ne_binary()) ->
                          {'ok', ne_binary(), wh_proplist()} |
                          {'error', term()}.
fetch_number(Num) ->
    case wh_number_manager:lookup_account_by_number(Num) of
        {'ok', AccountId, Props} ->
            CacheProps = [{'origin', {'db', wnm_util:number_to_db_name(Num), Num}}],
            wh_cache:store_local(?STEPSWITCH_CACHE, cache_key_number(Num), {AccountId, Props}, CacheProps),
            lager:debug("~s is associated with account ~s", [Num, AccountId]),
            {'ok', AccountId, Props};
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec maybe_transition_port_in(ne_binary(), wh_proplist()) -> 'false' | pid().
maybe_transition_port_in(Num, Props) ->
    case props:get_value('pending_port', Props) of
        'false' -> 'false';
        'true' -> spawn('wh_number_manager', 'ported', [Num])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_gateway_by_address(ne_binary(), #resrc{}|[#gateway{},...]|[]) -> 'undefined' | #gateway{}.
maybe_gateway_by_address(_, []) -> 'undefined';
maybe_gateway_by_address(Address, [#resrc{gateways=Gateways}|Resources]) ->
    case maybe_gateway_by_address(Address, Gateways) of
        'undefined' -> maybe_gateway_by_address(Address, Resources);
        Gateway -> Gateway
    end;
maybe_gateway_by_address(Address, [#gateway{server=Address}=Gateway|_]) ->
    Gateway;
maybe_gateway_by_address(Address, [_G|Gateways]) ->
    maybe_gateway_by_address(Address, Gateways).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter the list of resources returning only those with a rule that
%% matches the number.  The list is of tuples with three elements,
%% the weight, the captured component of the number, and the gateways.
%% @end
%%--------------------------------------------------------------------
-spec evaluate_number(ne_binary(), [#resrc{}]) -> endpoints().
evaluate_number(Number, Resrcs) ->
    sort_endpoints(get_endpoints(Number, Resrcs)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter the list of resources returning only those that have every
%% flag provided
%% @end
%%--------------------------------------------------------------------
-spec evaluate_flags(list(), [#resrc{}]) -> [#resrc{}].
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
-spec get_dialstring(#gateway{}, ne_binary()) -> ne_binary().
get_dialstring(#gateway{route='undefined'
                        ,prefix=Prefix
                        ,suffix=Suffix
                        ,server=Server
                       }, Number) ->
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
%% Get the t38 settings for an endpoint based on carrier and device
%% @end
%%--------------------------------------------------------------------
-spec get_outbound_t38_settings(boolean(), api_binary() | boolean()) -> wh_proplist().
get_outbound_t38_settings(CarrierFlag, 'undefined') ->
    get_outbound_t38_settings(CarrierFlag);
get_outbound_t38_settings('true', <<"auto">>) ->
    get_outbound_t38_settings('true', 'true');
get_outbound_t38_settings('true', 'true') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'true'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('true', 'false') ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, <<"self">>}
    ];
get_outbound_t38_settings('false', 'false') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'true'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('false','true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, <<"peer">>}
    ].

-spec get_outbound_t38_settings(boolean()) -> wh_proplist().
get_outbound_t38_settings('true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('false') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sort the gateway tuples returned by evalutate_resrcs according to
%% weight.
%% @end
%%--------------------------------------------------------------------
-spec sort_endpoints(endpoints()) -> endpoints().
sort_endpoints(Endpoints) ->
    lists:sort(fun({W1, _, _, _, _}, {W2, _, _, _, _}) ->
                       W1 =< W2
               end, Endpoints).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints(ne_binary(), [#resrc{}]) -> endpoints().
get_endpoints(Number, Resrcs) ->
    EPs = [get_endpoint(Number, R) || R <- Resrcs],
    [Endpoint || Endpoint <- EPs, Endpoint =/= 'no_match'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint(ne_binary(), #resrc{}) -> endpoint() | 'no_match'.
get_endpoint(Number, #resrc{weight_cost=WC
                            ,gateways=Gtws
                            ,rules=Rules
                            ,grace_period=GP
                            ,is_emergency=IsEmergency
                           }) ->
    case evaluate_rules(Rules, Number) of
        {'ok', DestNum} -> {WC, GP, DestNum, Gtws, IsEmergency};
        {'error', 'no_match'} -> 'no_match'
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
-spec evaluate_rules(re:mp(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'no_match'}.
evaluate_rules([], _) -> {'error', 'no_match'};
evaluate_rules([Regex|T], Number) ->
    case re:run(Number, Regex) of
        {'match', [{Start,End}]} ->
            {'ok', binary:part(Number, Start, End)};
        {'match', CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {'ok', binary:part(Number, Start, End)};
        _ ->
            evaluate_rules(T, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cache_key_number(ne_binary()) -> {'stepswitch_number', ne_binary()}.
cache_key_number(Number) ->
    {'stepswitch_number', Number}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a resrc JSON object it builds a resrc record and
%% populates it with all enabled gateways
%% @end
%%--------------------------------------------------------------------
-spec create_resrc(wh_json:object()) -> #resrc{}.
create_resrc(JObj) ->
    Default = #resrc{},
    Id = wh_json:get_value(<<"_id">>, JObj),
    lager:debug("loading resource ~s", [Id]),

    #resrc{id = Id
           ,rev =
               wh_json:get_value(<<"_rev">>, JObj)
           ,weight_cost =
               constrain_weight(wh_json:get_value(<<"weight_cost">>, JObj, Default#resrc.weight_cost))
           ,grace_period =
               wh_json:get_integer_value(<<"grace_period">>, JObj, Default#resrc.grace_period)
           ,flags =
               wh_json:get_value(<<"flags">>, JObj, Default#resrc.flags)
           ,rules =
               [R2 || R1 <- wh_json:get_value(<<"rules">>, JObj, Default#resrc.rules)
                          ,(R2 = compile_rule(R1, Id)) =/= error]
           ,gateways =
               [create_gateway(G, Id) || G <- wh_json:get_value(<<"gateways">>, JObj, []),
                                         wh_json:is_true(<<"enabled">>, G, 'true')]
           ,is_emergency =
               wh_json:is_true(<<"emergency">>, JObj)
               orelse (wh_json:get_value([<<"caller_id_options">>, <<"type">>], JObj) =:= <<"emergency">>)
          }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec create_gateway(wh_json:object(), ne_binary()) -> #gateway{}.
create_gateway(JObj, Id) ->
    Default = #gateway{},
    EndpointType = endpoint_type(JObj, Default),
    EndpointOptions = endpoint_options(JObj, EndpointType),
    #gateway{'resource_id' = Id
             ,'server' =
                 wh_json:get_value(<<"server">>, JObj, Default#gateway.server)
             ,'realm' =
                 wh_json:get_value(<<"realm">>, JObj, Default#gateway.realm)
             ,'username' =
                 wh_json:get_value(<<"username">>, JObj, Default#gateway.username)
             ,'password' =
                 wh_json:get_value(<<"password">>, JObj, Default#gateway.password)
             ,'route' =
                 wh_json:get_value(<<"route">>, JObj, Default#gateway.route)
             ,'prefix' =
                 wh_json:get_binary_value(<<"prefix">>, JObj, Default#gateway.prefix)
             ,'suffix' =
                 wh_json:get_binary_value(<<"suffix">>, JObj, Default#gateway.suffix)
             ,'codecs' =
                 wh_json:get_value(<<"codecs">>, JObj, Default#gateway.codecs)
             ,'t38_setting' =
                 wh_json:get_value(<<"t38_setting">>, JObj, Default#gateway.t38_setting)
             ,'bypass_media' =
                 wh_json:get_value(<<"bypass_media">>, JObj)
             ,'caller_id_type' =
                 wh_json:get_value(<<"caller_id_type">>, JObj, Default#gateway.caller_id_type)
             ,'sip_headers' =
                 wh_json:get_value(<<"custom_sip_headers">>, JObj, Default#gateway.sip_headers)
             ,'sip_interface' =
                 wh_json:get_ne_value(<<"custom_sip_interface">>, JObj)
             ,'progress_timeout' =
                 wh_json:get_integer_value(<<"progress_timeout">>, JObj, Default#gateway.progress_timeout)
             ,'invite_format' =
                 wh_json:get_value(<<"invite_format">>, JObj, Default#gateway.invite_format)
             ,'endpoint_type' = EndpointType
             ,'endpoint_options' = EndpointOptions
             ,'format_from_uri' = wh_json:is_true(<<"format_from_uri">>, JObj, Default#gateway.format_from_uri)
            }.

endpoint_type(JObj, #gateway{endpoint_type=ET}) ->
    wh_json:get_value(<<"endpoint_type">>, JObj, ET).

endpoint_options(JObj, <<"freetdm">>) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Span">>, wh_json:get_value(<<"span">>, JObj)}
         ,{<<"Channel-Selection">>, wh_json:get_value(<<"channel_selection">>, JObj, <<"ascending">>)}
        ]));
endpoint_options(JObj, <<"skype">>) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Skype-Interface">>, wh_json:get_value(<<"interface">>, JObj)}
         ,{<<"Skype-RR">>, wh_json:is_true(<<"skype_rr">>, JObj, true)}
        ]));
endpoint_options(_, _) ->
    wh_json:new().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for re:compile so we can log rules that fail (including
%% which resource it was on).
%% @end
%%--------------------------------------------------------------------
-spec compile_rule(ne_binary(), ne_binary()) -> re:mp() | 'error'.
compile_rule(Rule, Id) ->
    case re:compile(Rule) of
        {ok, MP} ->
            lager:debug("compiled ~s on resource ~s", [Rule, Id]),
            MP;
        {error, R} ->
            lager:debug("bad rule '~s' on resource ~s, ~p", [Rule, Id, R]),
            error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% constrain the weight on a scale from 1 to 100
%% @end
%%--------------------------------------------------------------------
-spec constrain_weight(ne_binary() | integer()) -> integer().
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(wh_util:to_integer(W));
constrain_weight(W) when W > 100 -> 100;
constrain_weight(W) when W < 1 -> 1;
constrain_weight(W) -> W.
