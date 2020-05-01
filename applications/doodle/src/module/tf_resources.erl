%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tf_resources).

-include("doodle.hrl").

-export([handle/2]).

-define(CID_EXT_KEY, [<<"caller_id">>
                     ,<<"external">>
                     ,<<"number">>
                     ]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_im:im()) -> 'ok'.
handle(Data, Im) ->
    API = [{<<"Message-ID">>, kapps_im:message_id(Im)}
          ,{<<"Body">>, kapps_im:body(Im)}
          ,{<<"From">>, get_from_did(Data, Im)}
          ,{<<"To">>, get_to_did(Data, Im)}
          ,{<<"Account-ID">>, kapps_im:account_id(Im)}
          ,{<<"Route-Type">>, <<"offnet">>}
           | kz_api:default_headers(<<"sms">>, <<"outbound">>, ?APP_NAME, ?APP_VERSION)
          ],
    kapi_im:publish_outbound(API),
    tf_exe:stop(Im, 'offnet').

-spec get_to_did(kz_json:object(), kapps_im:im()) -> kz_term:ne_binary().
get_to_did(Data, Im) ->
    case kz_json:is_true(<<"do_not_normalize">>, Data) of
        'false' -> get_normalized_did(Im, kapps_im:to(Im));
        'true' -> kapps_im:to(Im)
    end.

-spec get_normalized_did(kapps_im:im(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_normalized_did(Im, Number) ->
    case kz_json:get_value(<<"dial_plan">>, kapps_im:endpoint(Im), []) of
        [] -> Number;
        DialPlan -> knm_converters:normalize(Number, kapps_im:account_id(Im), DialPlan)
    end.

-spec get_from_did(kz_json:object(), kapps_im:im()) -> kz_term:ne_binary().
get_from_did(_Data, Im) ->
    case knm_converters:is_reconcilable(kapps_im:from(Im)) of
        'true' -> get_normalized_did(Im, kapps_im:from(Im));
        'false' -> kz_json:get_ne_value(?CID_EXT_KEY, kapps_im:endpoint(Im))
    end.
