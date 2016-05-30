%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_resource_selectors).

-export([endpoints/2]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"resource_selectors">>).
-define(SRS_CONFIG_CAT, <<?SS_CONFIG_CAT/binary, ".", ?MOD_NAME/binary>>).
-define(MOD_PREFIX, "kz_srs_").
-define(DEFAULT_SRS_RULES, [{[{<<"get_resources">>
                               ,{[]}}
                               %,{[{<<"source">>,<<"database">>}]}}
                             ]}
                            ,{[{<<"filter_flags">>
                                ,{[{<<"source">>,<<"resource">>}]}}
                              ]}
                            ,{[{<<"filter_regex_rules">>
                                ,{[{<<"source">>,<<"resource">>}
                                   ,{<<"deny_on_empty_rules">>,'true'}
                                   ,{<<"update_number">>,'true'}
                                  ]}
                               }]}
                            ,{[{<<"filter_regex_cid_rules">>
                                ,{[{<<"source">>,<<"resource">>}
                                   ,{<<"deny_on_empty_rules">>,'false'}
                                   ,{<<"update_cid_number">>,'false'}
                                  ]}
                               }]}
                            ,{[{<<"order_weight">>
                                ,{[{<<"source">>,<<"resource">>}
                                   ,{<<"sort_order">>,<<"ascend">>}
                                  ]}
                               }]}
                           ]
       ).


-spec endpoints(ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
endpoints(Number, OffnetJObj) ->
    HuntAccountId  = get_hunt_account(OffnetJObj),
    SelectorsDb = get_selectors_db(HuntAccountId),
    case get_selector_rules(HuntAccountId) of
        {'ok', SelectorRules} ->
            Resources = foldl_modules(Number, OffnetJObj, SelectorsDb, SelectorRules),
            stepswitch_util:resources_to_endpoints(Resources, Number, OffnetJObj);
        {'error', _E} -> []
    end.

-spec foldl_modules(ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:objects()) -> stepswitch_resources:resources().
foldl_modules(Number, OffnetJObj, SelectorsDb, SelectorRules) ->
    lists:foldl(fun(JObj, Resources) ->
                        [Module|_] = kz_json:get_keys(JObj),
                        ModuleName = real_module_name(Module),
                        ModuleParams = kz_json:get_value(Module, JObj),
                        try Res = ModuleName:handle_req(Resources
                                                        ,Number
                                                        ,OffnetJObj
                                                        ,SelectorsDb
                                                        ,ModuleParams
                                                       ),
                            lager:info("module ~p return resources: ~p"
                                       ,[Module, [ stepswitch_resources:get_resrc_id(R) ||
                                                   R <- Res ]
                                        ]),
                            Res
                        catch
                            E:R ->
                                ST = erlang:get_stacktrace(),
                                lager:error("failed to run module: ~p, ~p:~p",[Module, E, R]),
                                kz_util:log_stacktrace(ST),
                                Resources
                        end
                end
                ,[]
                ,SelectorRules
               ).

-spec get_hunt_account(kapi_offnet_resource:req()) -> api_binary().
get_hunt_account(OffnetJObj) ->
    HuntAccountId = kapi_offnet_resource:hunt_account_id(OffnetJObj),
    AccountId = kapi_offnet_resource:account_id(OffnetJObj),
    case kz_util:is_not_empty(HuntAccountId)
         andalso kz_util:is_in_account_hierarchy(HuntAccountId, AccountId, 'true')
    of
        'true' -> HuntAccountId;
        'false' -> 'undefined'
    end.

-spec get_selectors_db(api_binary()) -> ne_binary().
get_selectors_db('undefined') -> ?KZ_RESOURCE_SELECTORS_DB;
get_selectors_db(HuntAccountId) -> kz_util:format_resource_selectors_db(HuntAccountId).

-spec get_selector_rules(api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_selector_rules('undefined') ->
    Rules = kapps_config:get(?SRS_CONFIG_CAT, <<"rules">>, ?DEFAULT_SRS_RULES),
    {'ok', Rules};
get_selector_rules(HuntAccountId) ->
    Db = kz_util:format_account_db(HuntAccountId),
    case kz_datamgr:open_doc(Db, ?MOD_NAME) of
        {'ok', Doc} -> {'ok', Doc};
        {'error', 'not_found'} ->
            %% TODO: save default rules to AccoutnDB
            {'ok', ?DEFAULT_SRS_RULES};
        {'error', E} ->
            lager:error("failed to get resource selector rules from ~s: ~p", [Db, E]),
            {'error', E}
    end.

-spec real_module_name(binary()) -> atom().
real_module_name(Module) when is_binary(Module) ->
    kz_util:to_atom(<<?MOD_PREFIX, Module/binary>>, 'true').
