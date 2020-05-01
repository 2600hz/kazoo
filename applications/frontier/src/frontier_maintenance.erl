%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(frontier_maintenance).

-include("frontier.hrl").

%% API
-export([lookup_acls/1
        ,lookup_rate_limits/1
        ,update_system_default/2
        ]).

-spec lookup_acls(kz_term:ne_binary()) -> 'ok'.
lookup_acls(Entity) ->
    io:format("looking for ACL records for ~s:~n", [Entity]),
    lists:foreach(fun print_acl_record/1, frontier_handle_acl:lookup_acl_records(Entity)).

-spec print_acl_record(kz_json:object()) -> 'ok'.
print_acl_record(Record) ->
    Name = kz_json:get_value(<<"key">>, Record),
    Type = kz_json:get_value([<<"value">>, <<"type">>], Record),
    Policy = kz_json:get_value([<<"value">>, <<"acls">>, <<"order">>], Record),
    CIDRs = kz_json:get_value([<<"value">>, <<"acls">>, <<"cidrs">>], Record),
    io:format("~s ~s use policy ~s for cidrs:~n",[Type, Name, Policy]),
    lists:foreach(fun(CIDR) -> io:format(" ~s~n", [CIDR]) end, CIDRs).

-spec lookup_rate_limits(kz_term:ne_binary()) -> 'ok'.
lookup_rate_limits(Entity) ->
    Limits = frontier_handle_rate:lookup_rate_limit_records(Entity),
    lists:foreach(fun(S) -> print_limits(S, kz_json:get_value(S, Limits)) end
                 ,[<<"Device">>, <<"Realm">>]
                 ).

-spec print_limits(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
print_limits(Section, Rates) ->
    Name = kz_json:get_value(<<"Name">>, Rates),
    Min = kz_json:get_value(<<"Minute">>, Rates, kz_json:new()),
    Sec = kz_json:get_value(<<"Second">>, Rates, kz_json:new()),
    io:format("~s rates for ~s~n", [Section, Name]),
    Keys = [frontier_handle_rate:name_to_method(N)
            || N <- frontier_handle_rate:names()],
    lists:foreach(fun(Key) ->
                          io:format("~-15s: ~7.10B/m ~7.10B/s~n"
                                   ,[Key, kz_json:get_value(Key, Min), kz_json:get_value(Key, Sec)]
                                   )
                  end, Keys).

-spec update_system_default(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_system_default(Path, Value) ->
    Keys = binary:split(Path, <<".">>, ['global']),
    NewRate = kz_term:to_integer(Value),
    Rates = frontier_init:default_rate_limits(),
    OldRate = kz_json:get_value(Keys, Rates),
    io:format("Updating ~s from ~p to ~p~n", [Path, OldRate, NewRate]),
    {'ok', _} = kapps_config:set(?APP_NAME, <<"rate_limits">>, kz_json:set_value(Keys, NewRate, Rates)),
    'ok'.
