%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Sponsored by Conversant Ltd,
%%%     implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cb_dialplans).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").
-define(ACCOUNT_CONFIG, <<"config">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.dialplans">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.dialplans">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.dialplans">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Doc = maybe_add_name(get_joined_dialplans(cb_context:account_id(Context))),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, Doc}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).

-spec get_joined_dialplans(api_binary()) -> [kz_json:object()].
get_joined_dialplans('undefined') -> get_dialplans('undefined');
get_joined_dialplans(AccountId) ->
    get_dialplans(AccountId) ++ get_dialplans('undefined').

-spec get_dialplans(api_binary()) -> [kz_json:object()].
get_dialplans('undefined') ->
    kapps_config:get_all_kvs(<<"dialplans">>);
get_dialplans(AccountId) ->
    JObj = kapps_account_config:get_global(AccountId, ?ACCOUNT_CONFIG, <<"dialplans">>, {[]}),
    kz_json:to_proplist(JObj).

-spec maybe_add_name(kz_proplist()) -> kz_json:object().
maybe_add_name(KVs) ->
    maybe_add_name(KVs, kz_json:new()).

-spec maybe_add_name(kz_proplist(), kz_json:object()) -> kz_json:object().
maybe_add_name([], Acc) -> Acc;
maybe_add_name([{K, V} | KVs], Acc0)
  when is_list(V) ->
    Acc = lists:foldl(fun(V1, Acc1) -> maybe_add_name([{K, V1}], Acc1) end, Acc0, V),
    maybe_add_name(KVs, Acc);
maybe_add_name([{K, V} | KVs], Acc0) ->
    Acc = case kz_json:get_ne_binary_value(<<"name">>, V) of
              'undefined' ->
                  JObj = kz_json:set_value(<<"name">>, K, V),
                  kz_json:set_value(K, JObj, Acc0);
              Name ->
                  kz_json:set_value(Name, kz_json:set_value(<<"regex">>, K, V), Acc0)
          end,
    maybe_add_name(KVs, Acc).
