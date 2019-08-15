%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_util).

-export([fax_properties/1]).
-export([collect_channel_props/1]).
-export([notify_email_list/1, notify_email_list/2, notify_email_list/3]).
-export([filter_numbers/1]).
-export([is_valid_caller_id/2]).

-include("fax.hrl").

-spec fax_properties(kz_json:object()) -> kz_term:proplist().
fax_properties(JObj) ->
    [{kz_json:normalize_key(K), V} || {<<"Fax-", K/binary>>, V} <- kz_json:to_proplist(JObj)].

-spec collect_channel_props(kz_json:object()) ->
                                   kz_term:proplist().
collect_channel_props(JObj) ->
    collect_channel_props(JObj, ?FAX_CHANNEL_DESTROY_PROPS).

-spec collect_channel_props(kz_json:object(), kz_term:proplist() | kz_term:ne_binaries()) ->
                                   kz_term:proplist().
collect_channel_props(JObj, List) ->
    collect_channel_props(JObj, List, []).

-spec collect_channel_props(kz_json:object(), kz_term:proplist() | kz_term:ne_binaries(), kz_term:proplist()) ->
                                   kz_term:proplist().
collect_channel_props(JObj, List, Acc) ->
    lists:foldl(fun({Key, Keys}, Acc0) ->
                        collect_channel_props(kz_json:get_value(Key, JObj), Keys, Acc0);
                   (Key, Acc0) ->
                        [collect_channel_prop(Key, JObj) | Acc0]
                end, Acc, List).

-spec collect_channel_prop(kz_term:ne_binary(), kz_json:object()) ->
                                  {kz_json:path(), kz_json:json_term()}.
collect_channel_prop(<<"Hangup-Code">> = Key, JObj) ->
    <<"sip:", Code/binary>> = kz_json:get_value(Key, JObj, <<"sip:500">>),
    {Key, Code};
collect_channel_prop(Key, JObj) ->
    {Key, kz_json:get_value(Key, JObj)}.

-spec notify_email_list(kz_term:ne_binary() | list()) -> list().
notify_email_list(Email) ->
    notify_email_list('undefined', 'undefined', Email).

-spec notify_email_list(kz_term:api_binary(), kz_term:ne_binary() | list()) -> list().
notify_email_list(OwnerEmail, Email) ->
    notify_email_list('undefined', OwnerEmail, Email).

-spec notify_email_list(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary() | list()) -> list().
notify_email_list(From, OwnerEmail, Email) when is_binary(Email) ->
    notify_email_list(From, OwnerEmail, [Email]);
notify_email_list('undefined', 'undefined', List) ->
    lists:usort(List);
notify_email_list(From, 'undefined', List) ->
    lists:usort([From | List]);
notify_email_list('undefined', OwnerEmail, List) ->
    lists:usort([OwnerEmail | List]);
notify_email_list(From, OwnerEmail, List) ->
    lists:usort([From, OwnerEmail | List]).

-spec filter_numbers(binary()) -> binary().
filter_numbers(Number) ->
    << <<X>> || <<X>> <= Number, is_digit(X)>>.

-spec is_valid_caller_id(kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
is_valid_caller_id('undefined', _) -> 'false';
is_valid_caller_id(<<>>, _) -> 'false';
is_valid_caller_id(Number, AccountId) ->
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, _} -> 'true';
        _Else -> 'false'
    end.

-spec is_digit(integer()) -> boolean().
is_digit(N) when is_integer(N),
                 N >= $0,
                 N =< $9 -> true;
is_digit(_) -> false.
