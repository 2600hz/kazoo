%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hangups_util).

-export([meter_name/1, meter_name/2
        ,meter_prefix/0
        ,is_hangup_meter/1, is_hangup_meter/2, is_hangup_meter/3
        ,meter_account_id/1
        ,meter_hangup_cause/1
        ]).

-include("hangups.hrl").

-define(METER_PREFIX_LIST, "hangups").
-define(METER_PREFIX, <<?METER_PREFIX_LIST>>).

-spec meter_name(kz_term:ne_binary()) -> kz_term:ne_binary().
meter_name(<<"*">>) -> <<"*">>;
meter_name(HangupCause) ->
    <<?METER_PREFIX_LIST, ".", HangupCause/binary>>.

-spec meter_name(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
meter_name(HangupCause, 'undefined') ->
    meter_name(HangupCause);
meter_name(HangupCause, AccountId) ->
    <<?METER_PREFIX_LIST, ".", HangupCause/binary, ".", AccountId/binary>>.

-spec meter_prefix() -> kz_term:ne_binary().
meter_prefix() ->
    ?METER_PREFIX.

-spec meter_account_id(kz_term:ne_binary()) -> kz_term:api_binary().
meter_account_id(Name) ->
    case binary:split(Name, <<".">>, ['global']) of
        [?METER_PREFIX, _HC, AccountId] -> AccountId;
        _ -> 'undefined'
    end.

-spec meter_hangup_cause(kz_term:ne_binary()) -> kz_term:api_binary().
meter_hangup_cause(Name) ->
    case binary:split(Name, <<".">>, ['global']) of
        [?METER_PREFIX, HangupCause, _AccountId] -> HangupCause;
        [?METER_PREFIX, HangupCause] -> HangupCause;
        _ -> 'undefined'
    end.

-spec is_hangup_meter(kz_term:ne_binary()) -> boolean().
is_hangup_meter(<<?METER_PREFIX_LIST, ".", _/binary>>) -> 'true';
is_hangup_meter(_) -> 'false'.

-spec is_hangup_meter(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_hangup_meter(Name, HangupCause) ->
    Size = byte_size(HangupCause),
    case Name of
        <<?METER_PREFIX_LIST, ".", HangupCause:Size/binary, ".", _/binary>> -> 'true';
        <<?METER_PREFIX_LIST, ".", HangupCause:Size/binary>> -> 'true';
        _ -> 'false'
    end.

-spec is_hangup_meter(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_hangup_meter(Name, <<"*">>, AccountId) ->
    case binary:split(Name, <<".">>, ['global']) of
        [?METER_PREFIX, _HC, AccountId] -> 'true';
        _ -> 'false'
    end;
is_hangup_meter(Name, HangupCause, AccountId) ->
    meter_name(HangupCause, AccountId) =:= Name.
