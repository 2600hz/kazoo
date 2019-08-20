%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ip).

-include("kazoo_ips.hrl").

-export([to_json/1
        ,from_json/1
        ]).
-export([create/3]).
-export([fetch/1]).
-export([assign/2]).
-export([release/1]).
-export([delete/1]).
-export([ip/1]).
-export([zone/1]).
-export([modified/1]).
-export([created/1]).
-export([assigned_to/1]).
-export([host/1]).
-export([is_dedicated_ip/1]).
-export([is_available/1]).

-type ip() :: kz_json:object().
-export_type([ip/0]).

-type std_return() :: {'ok', ip()} | {'error', any()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(ip()) -> kz_json:object().
to_json(IP) -> IP.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_json(kz_json:object()) -> ip().
from_json(JObj) -> JObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                    std_return().
create(IP, Zone, Host) ->
    Timestamp = kz_time:now_s(),
    JObj = kz_json:from_list(
             [{<<"_id">>, IP}
             ,{<<"pvt_vsn">>, <<"1">>}
             ,{<<"pvt_status">>, ?AVAILABLE}
             ,{<<"pvt_type">>, ?PVT_TYPE}
             ,{<<"pvt_zone">>, Zone}
             ,{<<"pvt_host">>, Host}
             ,{<<"pvt_modified">>, Timestamp}
             ,{<<"pvt_created">>, Timestamp}
             ]
            ),
    case kz_datamgr:save_doc(?KZ_DEDICATED_IP_DB, JObj) of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> create(IP, Zone, Host) end);
        {'ok', SavedJObj} ->
            lager:debug("created dedicated ip ~s in zone ~s on host ~s"
                       ,[IP, Zone, Host]
                       ),
            {'ok', from_json(SavedJObj)};
        {'error', _R}=E ->
            lager:debug("unable to create dedicated ip ~s: ~p"
                       ,[IP, _R]
                       ),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary()) -> std_return().
fetch(IP) ->
    case kz_datamgr:open_cache_doc(?KZ_DEDICATED_IP_DB, IP) of
        {'ok', JObj} -> {'ok', from_json(JObj)};
        {'error', _R}=E ->
            lager:debug("unable to fetch dedicated ip ~s: ~p"
                       ,[IP, _R]
                       ),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign(kz_term:ne_binary(), kz_term:ne_binary() | ip()) -> std_return().
assign(Account, <<_/binary>> = RawIP) ->
    case fetch(RawIP) of
        {'ok', IPDoc} -> assign(Account, IPDoc);
        {'error', _}=E -> E
    end;
assign(Account, IPDoc) ->
    'true' = is_dedicated_ip(IPDoc),
    case is_available(IPDoc) of
        {'error', _}=E -> E;
        'false' -> {'error', 'already_assigned'};
        'true' ->
            IPJObj = to_json(IPDoc),
            AccountId = kz_util:format_account_id(Account, 'raw'),
            Props = [{<<"pvt_assigned_to">>, AccountId}
                    ,{<<"pvt_modified">>, kz_time:now_s()}
                    ,{<<"pvt_status">>, ?ASSIGNED}
                    ],
            JObj = kz_json:set_values(Props, IPJObj),
            maybe_save_in_account(AccountId, save(JObj))
    end.

-spec maybe_save_in_account(kz_term:ne_binary(), std_return()) -> std_return().
maybe_save_in_account(AccountId, {'ok', JObj}=Ok) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_doc(AccountDb, kz_doc:id(JObj)) of
        {'error', 'not_found'} ->
            _ = kz_datamgr:save_doc(AccountDb, kz_doc:delete_revision(JObj)),
            Ok;
        {'error', _R}=E ->
            lager:info("failed to save ip doc to accounts: ~p", [_R]),
            E;
        {'ok', CurrentJObj} ->
            Update = [{kz_doc:path_revision(), kz_doc:revision(CurrentJObj)}
                      | kz_json:to_proplist(JObj)
                     ],
            UpdateOptions = [{'update', Update}
                            ,{'ensure_saved', 'true'}
                            ],
            _ = kz_datamgr:update_doc(AccountDb, kz_doc:id(JObj), UpdateOptions),
            Ok
    end;
maybe_save_in_account(_, Return) -> Return.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec release(kz_term:ne_binary() | ip()) -> std_return().
release(<<_/binary>> = Ip) ->
    case fetch(Ip) of
        {'ok', IP} -> release(IP);
        {'error', _}=E -> E
    end;
release(IP) ->
    'true' = is_dedicated_ip(IP),
    RemoveKeys = [<<"pvt_assigned_to">>],
    Props = [{<<"pvt_status">>, ?AVAILABLE}
            ,{<<"pvt_modified">>, kz_time:now_s()}
            ],
    IPJObj = to_json(IP),
    AccountId = kz_json:get_ne_value(<<"pvt_assigned_to">>, IPJObj),
    JObj = kz_json:delete_keys(RemoveKeys, kz_json:set_values(Props, IPJObj)),
    maybe_remove_from_account(AccountId, save(JObj)).

-spec maybe_remove_from_account(kz_term:ne_binary(), std_return()) -> std_return().
maybe_remove_from_account(AccountId, {'ok', IP}=Ok) ->
    AccountDb = kz_util:format_account_db(AccountId),
    _ = case kz_datamgr:open_doc(AccountDb, ip(IP)) of
            {'ok', JObj} -> kz_datamgr:del_doc(AccountDb, JObj);
            {'error', _} -> 'ok'
        end,
    Ok;
maybe_remove_from_account(_, Return) -> Return.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_term:ne_binary() | ip()) -> std_return().
delete(<<_/binary>> = IP) ->
    case kz_datamgr:open_doc(?KZ_DEDICATED_IP_DB, IP) of
        {'ok', JObj} -> delete(from_json(JObj));
        {'error', _}=E -> E
    end;
delete(IP) ->
    'true' = is_dedicated_ip(IP),
    kz_datamgr:del_doc(?KZ_DEDICATED_IP_DB, to_json(IP)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ip(ip()) -> kz_term:ne_binary().
ip(IP) -> kz_doc:id(IP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec zone(ip()) -> kz_term:ne_binary().
zone(IP) -> kz_json:get_value(<<"pvt_zone">>, IP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec modified(ip()) -> kz_term:api_integer().
modified(IP) -> kz_doc:modified(IP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec created(ip()) -> kz_term:api_integer().
created(IP) -> kz_doc:created(IP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec host(ip()) -> kz_term:ne_binary().
host(IP) -> kz_json:get_value(<<"pvt_host">>, IP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assigned_to(ip()) -> kz_term:ne_binary().
assigned_to(IP) -> kz_json:get_value(<<"pvt_assigned_to">>, IP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_dedicated_ip(ip()) -> boolean().
is_dedicated_ip(IP) ->
    kz_doc:type(IP) =:= ?PVT_TYPE.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_available(ip() | kz_term:ne_binary()) -> boolean() | {'error', any()}.
is_available(Ip) when is_binary(Ip) ->
    case fetch(Ip) of
        {'ok', IP} -> is_available(IP);
        {'error', _}=E -> E
    end;
is_available(IP) ->
    kz_json:get_value(<<"pvt_status">>, IP) =:= ?AVAILABLE.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(kz_json:object()) -> std_return().
save(JObj) ->
    case kz_datamgr:save_doc(?KZ_DEDICATED_IP_DB, JObj) of
        {'ok', J} -> {'ok', from_json(J)};
        {'error', _R}=E ->
            lager:debug("failed to save dedicated ip ~s: ~p", [kz_doc:id(JObj), _R]),
            E
    end.
