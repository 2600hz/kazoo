%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(frontier_maintenance).

-include("frontier.hrl").

%% API
-export([lookup_acls/1
         ,lookup_ratelimits/1
        ]).

-spec lookup_acls(ne_binary()) -> 'ok'.
lookup_acls(Entity) ->
    io:format("looking for ACL records for ~s:~n", [Entity]),
    lists:foreach(fun print_acl_record/1, frontier_handle_acl:lookup_acl_records(Entity)).

-spec print_acl_record(wh_json:object()) -> 'ok'.
print_acl_record(Record) ->
    Name = wh_json:get_value(<<"key">>, Record),
    Type = wh_json:get_value([<<"value">>, <<"type">>], Record),
    Policy = wh_json:get_value([<<"value">>, <<"acls">>, <<"order">>], Record),
    CIDRs = wh_json:get_value([<<"value">>, <<"acls">>, <<"cidrs">>], Record),
    io:format("~s ~s use policy ~s for cidrs:~n",[Type, Name, Policy]),
    lists:foreach(fun (CIDR) -> io:format("~s~n", [CIDR]) end, CIDRs).

-spec lookup_ratelimits(ne_binary()) -> 'ok'.
lookup_ratelimits(Entity) ->
    Limits = frontier_handle_rate:lookup_rate_limit_records(Entity),
    lists:foreach(fun (S) -> print_limits(S, wh_json:get_value(S, Limits)) end,[<<"Device">>, <<"Realm">>]).

-spec print_limits(ne_binary(), wh_json:object()) -> 'ok'.
print_limits(Section, Rates) ->
    Name = wh_json:get_value(<<"Name">>, Rates),
    Min = wh_json:get_value(<<"Minute">>, Rates, wh_json:new()),
    Sec = wh_json:get_value(<<"Second">>, Rates, wh_json:new()),
    io:format("~s rates for ~s~n", [Section, Name]),
    Keys = lists:map(fun frontier_handle_rate:name_to_method/1, frontier_handle_rate:names()),
    lists:foreach(fun (Key) ->
                      io:format("~-15s: ~7.10B/m ~7.10B/s~n", [Key, wh_json:get_value(Key, Min), wh_json:get_value(Key, Sec)])
                  end, Keys).
