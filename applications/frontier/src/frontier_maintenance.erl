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
    CIDRs = wh_json:get_value([<<"value">>, <<"acls">>, <<"cidr">>], Record),
    io:format("~s ~s use policy ~s for cidrs:~n",[Type, Name, Policy]),
    lists:foreach(fun (CIDR) -> io:format("~s~n", [CIDR]) end, CIDRs).

-spec lookup_ratelimits(ne_binary()) -> 'ok'.
lookup_ratelimits(Entity) ->
    Limits = frontier_handle_rate:lookup_rate_limit_records(Entity),
    wh_json:foreach(fun print_limits/1, Limits).

-spec print_limits({ne_binary(), wh_json:object()}) -> 'ok'.
print_limits({Type, Rates}) ->
    Name = wh_json:get_value(<<"name">>, Rates),
    Min = wh_json:get_value(?MINUTE, Rates, wh_json:new()),
    Sec = wh_json:get_value(?SECOND, Rates, wh_json:new()),
    io:format("~s rates for ~s~n", [Type, Name]),
    lists:foreach(fun (Key) ->
                      io:format("~-15s: ~7.10B/m ~7.10B/s~n", [Key, wh_json:get_value(Key, Min), wh_json:get_value(Key, Sec)])
                  end, frontier_handle_rate:names()).
