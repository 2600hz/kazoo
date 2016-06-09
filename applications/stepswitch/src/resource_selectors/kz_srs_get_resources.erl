%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_get_resources).

-export([handle_req/5]).

-include("stepswitch.hrl").

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_proplist()) ->
    stepswitch_resources:resources().
handle_req(Resources, _Number, OffnetJObj, _DB, _Params) ->
    NewResources = case kapi_offnet_resource:hunt_account_id(OffnetJObj) of
                       'undefined' -> get_resources('undefined');
                       HuntAccount ->
                           AccountId = kapi_offnet_resource:account_id(OffnetJObj),
                           maybe_get_local_resources(HuntAccount, AccountId)
                   end,
    lists:append(Resources, NewResources).

-spec maybe_get_local_resources(ne_binary(), ne_binary()) -> kz_json:objects().
maybe_get_local_resources(HuntAccount, AccountId) ->
    case kz_util:is_in_account_hierarchy(HuntAccount, AccountId, 'true') of
        'false' ->
            lager:info("account ~s attempted to use local resources of ~s, but it is not allowed"
                       ,[AccountId, HuntAccount]
                      ),
            [];
        'true' ->
            lager:info("account ~s is using the local resources of ~s", [AccountId, HuntAccount]),
            get_resources(HuntAccount)
    end.

-spec get_resources(api_binary()) -> stepswitch_resources:resources().
get_resources('undefined') ->
    case kz_cache:fetch_local(?CACHE_NAME, 'global_resources') of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} -> stepswitch_resources:fetch_global_resources()
    end;
get_resources(AccountId) ->
    case kz_cache:fetch_local(?CACHE_NAME, {'local_resources', AccountId}) of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} -> stepswitch_resources:fetch_local_resources(AccountId)
    end.
