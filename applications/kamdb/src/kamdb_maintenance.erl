%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_maintenance).

-include("kamdb.hrl").

%% API
-export([lookup_acls/1]).

-spec lookup_acls(ne_binary()) -> 'ok'.
lookup_acls(Entity) ->
    io:format("looking for ACL records for ~s:~n", [Entity]),
    lists:foreach(fun print_acl_record/1, kamdb_handle_acl:lookup_acl_records(Entity)).

-spec print_acl_record(wh_json:object()) -> 'ok'.
print_acl_record(Record) ->
    Name = wh_json:get_value(<<"key">>, Record),
    Type = wh_json:get_value([<<"value">>, <<"type">>], Record),
    Policy = wh_json:get_value([<<"value">>, <<"acls">>, <<"order">>], Record),
    CIDRs = wh_json:get_value([<<"value">>, <<"acls">>, <<"cidr">>], Record),
    io:format("~s ~s use policy ~s for cidrs:~n",[Type, Name, Policy]),
    lists:foreach(fun (CIDR) -> io:format("~s~n", [CIDR]) end, CIDRs).
