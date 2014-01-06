%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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

-spec meter_name(ne_binary()) -> ne_binary().
-spec meter_name(ne_binary(), api_binary()) -> ne_binary().
meter_name(HangupCause) ->
    <<?METER_PREFIX_LIST, ".", HangupCause/binary>>.
meter_name(HangupCause, 'undefined') ->
    meter_name(HangupCause);
meter_name(HangupCause, AccountId) ->
    <<?METER_PREFIX_LIST, ".", HangupCause/binary, ".", AccountId/binary>>.

-spec meter_prefix() -> ne_binary().
meter_prefix() ->
    ?METER_PREFIX.

-spec is_hangup_meter(ne_binary()) -> boolean().
is_hangup_meter(<<?METER_PREFIX_LIST, _/binary>>) ->
    'true';
is_hangup_meter(_) ->
    'false'.

-spec meter_account_id(ne_binary()) -> api_binary().
meter_account_id(Name) ->
    case binary:split(Name, <<".">>, ['global']) of
        [?METER_PREFIX, _HC, AccountId] -> AccountId;
        _ -> 'undefined'
    end.

-spec meter_hangup_cause(ne_binary()) -> api_binary().
meter_hangup_cause(Name) ->
    case binary:split(Name, <<".">>, ['global']) of
        [?METER_PREFIX, HangupCause, _AccountId] -> HangupCause;
        [?METER_PREFIX, HangupCause] -> HangupCause;
        _ -> 'undefined'
    end.

-spec is_hangup_meter(ne_binary(), ne_binary()) -> boolean().
is_hangup_meter(Name, HangupCause) ->
    Size = byte_size(HangupCause),
    case Name of
        <<?METER_PREFIX_LIST, ".", HC:Size/binary, ".", _/binary>> when HC =:= HangupCause -> 'true';
        <<?METER_PREFIX_LIST, ".", HC:Size/binary>> when HC =:= HangupCause -> 'true';
        _ -> 'false'
    end.

-spec is_hangup_meter(ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_hangup_meter(Name, <<"*">>, AccountId) ->
    case binary:split(Name, <<".">>, ['global']) of
        [?METER_PREFIX, _HC, AccountId] -> 'true';
        _ -> 'false'
    end;
is_hangup_meter(Name, HangupCause, AccountId) ->
    meter_name(HangupCause, AccountId) =:= Name.
