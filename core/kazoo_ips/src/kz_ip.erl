%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_json(ip()) -> kz_json:object().
to_json(IP) -> IP.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_json(kz_json:object()) -> ip().
from_json(JObj) -> JObj.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), ne_binary(), ne_binary()) ->
                    {'ok', ip()} |
                    {'error', any()}.
create(IP, Zone, Host) ->
    Timestamp = kz_time:current_tstamp(),
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) ->
                   {'ok', ip()} |
                   {'error', any()}.
fetch(IP) ->
    case kz_datamgr:open_cache_doc(?KZ_DEDICATED_IP_DB, IP) of
        {'ok', JObj} -> {'ok', from_json(JObj)};
        {'error', _R}=E ->
            lager:debug("unable to fetch dedicated ip ~s: ~p"
                       ,[IP, _R]
                       ),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec assign(ne_binary(), ne_binary() | ip()) ->
                    {'ok', ip()} |
                    {'error', any()}.
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
                    ,{<<"pvt_modified">>, kz_time:current_tstamp()}
                    ,{<<"pvt_status">>, ?ASSIGNED}
                    ],
            save(kz_json:set_values(Props, IPJObj)
                ,kz_json:get_ne_binary_value(<<"pvt_assigned_to">>, IPJObj)
                )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec release(ne_binary() | ip()) ->
                     {'ok', ip()} |
                     {'error', any()}.
release(<<_/binary>> = Ip) ->
    case fetch(Ip) of
        {'ok', IP} -> release(IP);
        {'error', _}=E -> E
    end;
release(IP) ->
    'true' = is_dedicated_ip(IP),
    RemoveKeys = [<<"pvt_assigned_to">>],
    Props = [{<<"pvt_status">>, ?AVAILABLE}
            ,{<<"pvt_modified">>, kz_time:current_tstamp()}
            ],
    JObj = to_json(IP),
    save(kz_json:delete_keys(RemoveKeys, kz_json:set_values(Props, JObj))
        ,kz_json:get_ne_binary_value(<<"pvt_assigned_to">>, JObj)
        ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary() | ip()) ->
                    {'ok', ip()} |
                    {'error', any()}.
delete(<<_/binary>> = IP) ->
    case kz_datamgr:open_doc(?KZ_DEDICATED_IP_DB, IP) of
        {'ok', JObj} -> delete(from_json(JObj));
        {'error', _}=E -> E
    end;
delete(IP) ->
    'true' = is_dedicated_ip(IP),
    kz_datamgr:del_doc(?KZ_DEDICATED_IP_DB, to_json(IP)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec ip(ip()) -> ne_binary().
ip(IP) -> kz_doc:id(IP).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec zone(ip()) -> ne_binary().
zone(IP) -> kz_json:get_value(<<"pvt_zone">>, IP).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec modified(ip()) -> api_integer().
modified(IP) -> kz_doc:modified(IP).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec created(ip()) -> api_integer().
created(IP) -> kz_doc:created(IP).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec host(ip()) -> ne_binary().
host(IP) -> kz_json:get_value(<<"pvt_host">>, IP).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec assigned_to(ip()) -> ne_binary().
assigned_to(IP) -> kz_json:get_value(<<"pvt_assigned_to">>, IP).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_dedicated_ip(ip()) -> boolean().
is_dedicated_ip(IP) ->
    kz_doc:type(IP) =:= ?PVT_TYPE.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_available(ip() | ne_binary()) -> boolean() | {'error', any()}.
is_available(Ip) when is_binary(Ip) ->
    case fetch(Ip) of
        {'ok', IP} -> is_available(IP);
        {'error', _}=E -> E
    end;
is_available(IP) ->
    kz_json:get_value(<<"pvt_status">>, IP) =:= ?AVAILABLE.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save(kz_json:object(), api_binary()) ->
                  {'ok', ip()} |
                  {'error', any()}.
save(JObj, PrevAccountId) ->
    case kz_datamgr:save_doc(?KZ_DEDICATED_IP_DB, JObj) of
        {'ok', J} ->
            AccountId = kz_json:get_value(<<"pvt_assigned_to">>, J),
            _ = reconcile_services(PrevAccountId, AccountId),
            {'ok', from_json(J)};
        {'error', _R}=E ->
            lager:debug("failed to save dedicated ip ~s: ~p", [kz_doc:id(JObj), _R]),
            E
    end.

-spec reconcile_services(api_binary(), api_binary()) -> 'false' | kz_services:services().
reconcile_services('undefined', AccountId) ->
    kz_services:reconcile(AccountId, <<"ips">>);
reconcile_services(AccountId, 'undefined') ->
    kz_services:reconcile(AccountId, <<"ips">>);
reconcile_services(AccountId, AccountId) ->
    kz_services:reconcile(AccountId, <<"ips">>);
reconcile_services(PrevAccountId, AccountId) ->
    _ = kz_services:reconcile(PrevAccountId, <<"ips">>),
    kz_services:reconcile(AccountId, <<"ips">>).
