%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Allows to control presence feature by calling this Callflow.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`presence_id'</dt>
%%%   <dd>Presence ID, e.g. `foo@bar.com' or `foo' (account's realm will be added instead).</dd>
%%%
%%%   <dt>`status'</dt>
%%%   <dd>One of: `idle', `ringing' or 'busy'.</dd>
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
-module(cf_manual_presence).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    PresenceId =
        case binary:match((P = kzd_devices:presence_id(Data, CaptureGroup)), <<"@">>) of
            'nomatch' -> <<P/binary, "@", (kapps_call:request_realm(Call))/binary>>;
            _Else -> P
        end,
    Status = kz_json:get_value(<<"status">>, Data, <<"idle">>),
    update_presence(Status, PresenceId, Call),
    cf_exe:continue(Call).

-spec update_presence(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
update_presence(<<"idle">>, PresenceId, Call) ->
    _ = update_doc(Call, PresenceId, <<"terminated">>),
    kapps_call_command:presence(<<"terminated">>, PresenceId, kz_term:to_hex_binary(crypto:hash('md5', PresenceId)));
update_presence(<<"ringing">>, PresenceId, Call) ->
    _ = update_doc(Call, PresenceId, <<"early">>),
    kapps_call_command:presence(<<"early">>, PresenceId, kz_term:to_hex_binary(crypto:hash('md5', PresenceId)));
update_presence(<<"busy">>, PresenceId, Call) ->
    _ = update_doc(Call, PresenceId, <<"confirmed">>),
    kapps_call_command:presence(<<"confirmed">>, PresenceId, kz_term:to_hex_binary(crypto:hash('md5', PresenceId))).

-spec update_doc(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                        {'ok', kz_json:object()} |
                        kz_datamgr:data_error().
update_doc(Call, PresenceId, State) ->
    Update = [{[PresenceId], State}],
    UpdateOptions = [{'update', Update}],
    kz_datamgr:update_doc(kapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, UpdateOptions).
