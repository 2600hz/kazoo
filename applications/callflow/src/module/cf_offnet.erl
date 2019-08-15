%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc User Global resource.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`to_did'</dt>
%%%   <dd>Statically dial DID.</dd>
%%%
%%%   <dt>`media'</dt>
%%%   <dd>Media ID</dd>
%%%
%%%   <dt>`ringback'</dt>
%%%   <dd>Ringback ID</dd>
%%%
%%%   <dt>`format_from_did'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`timeout'</dt>
%%%   <dd>`integer()'</dd>
%%%
%%%   <dt>`do_not_normalize'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`bypass_e164'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`from_uri_realm'</dt>
%%%   <dd>`binary'</dd>
%%%
%%%   <dt>`caller_id_type'</dt>
%%%   <dd>Can use custom caller id properties on endpoints, e.g. `external'.</dd>
%%%
%%%   <dt>`use_local_resources'</dt>
%%%   <dd>`boolean()'.</dd>
%%%
%%%   <dt>`hunt_account_id'</dt>
%%%   <dd>Use `Account' local carriers instead of current account.</dd>
%%%
%%%   <dt>`emit_account_id'</dt>
%%%   <dd>Puts account id in SIP header `X-Account-ID'. `boolean()'.</dd>
%%%
%%%   <dt>`custom_sip_headers'</dt>
%%%   <dd>`[{"header":"value",...}]'</dd>
%%%
%%%   <dt>`ignore_early_media'</dt>
%%%   <dd>`boolean()'</dd>
%%%
%%%   <dt>`outbound_flags'</dt>
%%%   <dd>Used to match flags on carrier docs. `["flag_1","flag_2"]'.</dd>
%%% </dl>
%%%
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_offnet).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    cf_resources:handle(kz_json:set_value(<<"use_local_resources">>, 'false', Data), Call).
