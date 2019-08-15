%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_ips_maintenance).

-include("kazoo_ips.hrl").

-export([refresh/0]).
-export([add/0
        ,add/3
        ]).
-export([assign/0
        ,assign/2
        ]).
-export([release/0
        ,release/1
        ]).
-export([delete/0
        ,delete/1
        ]).
-export([summary/0
        ,summary/1
        ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'no_return'.
refresh() ->
    _ = kz_ip_utils:refresh_database(),
    refresh_assigned().

-spec refresh_assigned() -> 'no_return'.
refresh_assigned() ->
    case kz_ips:assigned() of
        {'ok', IPs} ->
            refresh_assigned(IPs);
        _Else ->
            'no_return'
    end.

-spec refresh_assigned(kz_json:objects()) -> 'no_return'.
refresh_assigned([]) -> 'no_return';
refresh_assigned([IP|IPs]) ->
    AssignedTo = kz_ip:assigned_to(IP),
    AccountDb = kz_util:format_account_db(AssignedTo),
    JObj = kz_json:delete_key(<<"_rev">>, kz_ip:to_json(IP)),
    _ = kz_datamgr:save_doc(AccountDb, JObj),
    refresh_assigned(IPs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add() -> 'no_return'.
add() ->
    io:format("Please use: sup kazoo_ips_maintenance add <ip> <zone> <host>~n", []),
    'no_return'.

-spec add(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
add(IPAddress, Zone, Host) ->
    case kz_ip:create(IPAddress, Zone, Host) of
        {'ok', _IP} ->
            io:format("added IP ~s to available dedicated ips~n"
                     ,[IPAddress]
                     );
        {'error', _R} ->
            io:format("unable to add IP ~s: ~p~n", [IPAddress, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign() -> 'no_return'.
assign() ->
    io:format("Please use: sup kazoo_ips_maintenance assign <ip> <account>~n", []),
    'no_return'.

-spec assign(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
assign(IP, Account) ->
    case kzd_accounts:fetch(Account) of
        {'ok', _} -> do_assignment(Account, IP);
        {'error', _R} ->
            io:format("unable to find account: ~p~n", [_R])
    end,
    'no_return'.

-spec do_assignment(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
do_assignment(Account, IP) ->
    case kz_ip:assign(Account, IP) of
        {'ok', _} ->
            io:format("assigned IP ~s to ~s~n", [IP, Account]);
        {'error', _R} ->
            io:format("unable to assign IP: ~p~n", [_R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec release() -> 'no_return'.
release() ->
    io:format("Please use: sup kazoo_ips_maintenance release <ip>~n", []),
    'no_return'.

-spec release(kz_term:ne_binary()) -> 'no_return'.
release(IP) ->
    _ = case kz_ip:release(IP) of
            {'ok', _} ->
                io:format("released IP ~s~n", [IP]);
            {'error', _R} ->
                io:format("unable to release IP: ~p~n", [_R])
        end,
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete() -> 'no_return'.
delete() ->
    io:format("Please use: sup kazoo_ips_maintenance delete <ip>~n", []),
    'no_return'.

-spec delete(kz_term:ne_binary()) -> 'no_return'.
delete(IP) ->
    _ = case kz_ip:delete(IP) of
            {'ok', _} ->
                io:format("deleted IP ~s~n", [IP]);
            {'error', _R} ->
                io:format("unable to delete IP: ~p~n", [_R])
        end,
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summary() -> 'no_return'.
summary() -> summary('undefined').

-spec summary(kz_term:api_binary()) -> 'no_return'.
summary(Host) ->
    _ = case kz_ips:summary(Host) of
            {'ok', []} ->
                io:format("No IPs found~n", []);
            {'ok', JObjs} -> print_summary(JObjs);
            {'error', _Reason} ->
                io:format("Unable to list IPs assigned to host ~s: ~p~n"
                         ,[Host, _Reason]
                         )
        end,
    'no_return'.

-spec print_summary(kz_json:objects()) -> 'ok'.
print_summary(JObjs) ->
    FormatString = "| ~-15s | ~-10s | ~-10s | ~-30s | ~-32s |~n",
    io:format("+-----------------+------------+------------+--------------------------------+----------------------------------+~n", []),
    print_summary_headers(FormatString),
    io:format("+=================+============+============+================================+==================================+~n", []),
    print_summary_row(JObjs, FormatString),
    io:format("+-----------------+------------+------------+--------------------------------+----------------------------------+~n", []).

-spec print_summary_headers(string()) -> 'ok'.
print_summary_headers(FormatString) ->
    Headers = [<<"IP">>
              ,<<"Status">>
              ,<<"Zone">>
              ,<<"Host">>
              ,<<"Account">>
              ],
    io:format(FormatString, Headers).

-spec print_summary_row(kz_json:objects(), string()) -> 'ok'.
print_summary_row([], _) -> 'ok';
print_summary_row([JObj|JObjs], FormatString) ->
    io:format(FormatString,
              [kz_json:get_value(<<"ip">>, JObj)
              ,kz_json:get_value(<<"status">>, JObj)
              ,kz_json:get_value(<<"zone">>, JObj)
              ,kz_json:get_value(<<"host">>, JObj)
              ,kz_json:get_value(<<"assigned_to">>, JObj)
              ]),
    print_summary_row(JObjs, FormatString).
