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
         ,lookup_rate_limits/1
         ,update_system_default/2
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
    lists:foreach(fun(CIDR) -> io:format(" ~s~n", [CIDR]) end, CIDRs).

-spec lookup_rate_limits(ne_binary()) -> 'ok'.
lookup_rate_limits(Entity) ->
    Limits = frontier_handle_rate:lookup_rate_limit_records(Entity),
    lists:foreach(fun(S) -> print_limits(S, wh_json:get_value(S, Limits)) end
                  ,[<<"Device">>, <<"Realm">>]
                 ).

-spec print_limits(ne_binary(), wh_json:object()) -> 'ok'.
print_limits(Section, Rates) ->
    Name = wh_json:get_value(<<"Name">>, Rates),
    Min = wh_json:get_value(<<"Minute">>, Rates, wh_json:new()),
    Sec = wh_json:get_value(<<"Second">>, Rates, wh_json:new()),
    io:format("~s rates for ~s~n", [Section, Name]),
    Keys = lists:map(fun frontier_handle_rate:name_to_method/1, frontier_handle_rate:names()),
    lists:foreach(fun(Key) ->
                          io:format("~-15s: ~7.10B/m ~7.10B/s~n"
                                    ,[Key, wh_json:get_value(Key, Min), wh_json:get_value(Key, Sec)]
                                   )
                  end, Keys).

-spec update_system_default(ne_binary(), ne_binary()) -> 'ok'.
update_system_default(Path, Value) ->
    Keys = binary:split(Path, <<".">>, ['global']),
    NewRate = wh_util:to_integer(Value),
    Rates = frontier_init:default_rate_limits(),
    OldRate = wh_json:get_value(Keys, Rates),
    io:format("Updating ~s from ~p to ~p~n", [Path, OldRate, NewRate]),
    {'ok', _} = whapps_config:set(?APP_NAME, <<"rate_limits">>, wh_json:set_value(Keys, NewRate, Rates)),
    'ok'.
