%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_resource_selectors).

-export([endpoints/2]).

-include("stepswitch_resource_selectors.hrl").

-define(MOD_NAME, <<"resource_selectors">>).
-define(SRS_CONFIG_CAT, <<?SS_CONFIG_CAT/binary, ".", ?MOD_NAME/binary>>).
-define(MOD_PREFIX, "kz_srs_").
-define(SRS_RULES_DOC, <<"resource_selector_rules">>).
-define(DEFAULT_SRS_RULES, [{[{<<"get_resources">>
			      ,{[]}
                              }]}
			   ,{[{<<"filter_list">>
			      ,{[{<<"value_a">>,<<"request:Flags">>}
				,{<<"value_b">>,<<"resource:flags">>}
				,{<<"action">>,<<"keep">>}
				]}
			      }]}
			   ,{[{<<"filter_regex">>
			      ,{[{<<"value_a">>,<<"number">>}
				,{<<"value_b">>,<<"resource:rules">>}
				,{<<"action">>,<<"keep">>}
				,{<<"mode">>,<<"empty_fail">>}
				]}
			      }]}
			   ,{[{<<"filter_regex">>
			      ,{[{<<"value_a">>,<<"cid_number">>}
				,{<<"value_b">>,<<"resource:cid_rules">>}
				,{<<"action">>,<<"keep">>}
				,{<<"mode">>,<<"empty_ok">>}
				]}
			      }]}
			   ,{[{<<"order">>
			      ,{[{<<"value">>, <<"resource:weight_cost">>}
				,{<<"direction">>, <<"ascend">>}
				]}
			      }]}
                           ]
       ).

-spec endpoints(ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
endpoints(Number, OffnetJObj) ->
    HuntAccountId  = maybe_get_hunt_account(OffnetJObj),
    SelectorsDb = kz_util:format_resource_selectors_db(HuntAccountId),
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
                            'error':R ->
                                ST = erlang:get_stacktrace(),
                                lager:error("failed to run module: ~p, error: ~p",[Module, R]),
                                kz_util:log_stacktrace(ST),
                                [];
                            'throw':T ->
                                lager:error("module ~p (~p) throw exception: ~p",[Module, ModuleName, T]),
                                []
                        end
                end
	       ,[]
	       ,SelectorRules
               ).

-spec maybe_get_hunt_account(kapi_offnet_resource:req()) -> api_binary().
maybe_get_hunt_account(OffnetJObj) ->
    HuntAccountId = kapi_offnet_resource:hunt_account_id(OffnetJObj),
    AccountId = kapi_offnet_resource:account_id(OffnetJObj),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    case kz_util:is_not_empty(HuntAccountId)
	andalso kz_util:is_in_account_hierarchy(HuntAccountId, AccountId, 'true')
    of
        'true' -> HuntAccountId;
        'false' -> MasterAccountId
    end.

-spec get_selector_rules(api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_selector_rules(HuntAccountId) ->
    Db = kz_util:format_account_db(HuntAccountId),
    case kz_datamgr:open_doc(Db, ?SRS_RULES_DOC) of
        {'ok', Doc} ->
            Rules = kz_json:get_value(<<"rules">>, Doc, ?DEFAULT_SRS_RULES),
            {'ok', Rules};
        {'error', 'not_found'} ->
            Doc = {[{<<"_id">>, ?SRS_RULES_DOC}
                   ,{<<"rules">>, ?DEFAULT_SRS_RULES}
                   ]},
            _ = kz_datamgr:save_doc(Db, Doc),
            {'ok', ?DEFAULT_SRS_RULES};
        {'error', E} ->
            lager:error("failed to get resource selector rules from ~s: ~p", [Db, E]),
            {'error', E}
    end.

-spec real_module_name(binary()) -> atom().
real_module_name(Module) when is_binary(Module) ->
    kz_util:to_atom(<<?MOD_PREFIX, Module/binary>>, 'true').
