%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Sponsored by Conversant Ltd, Implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_dialplans).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.dialplans">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.dialplans">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.dialplans">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Doc = maybe_add_name(kapps_config:get_all_kvs(<<"dialplans">>)),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, Doc}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).

-spec maybe_add_name(kz_term:proplist()) -> kz_json:object().
maybe_add_name(KVs) ->
    maybe_add_name(KVs, kz_json:new()).

-spec maybe_add_name(kz_term:proplist(), kz_json:object()) -> kz_json:object().
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
