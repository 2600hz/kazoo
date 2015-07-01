%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf_acls).

-export([build/1]).

-export([resolve_hostname/4
         ,builder/2
        ]).

-include("sysconf.hrl").

-define(REQUEST_TIMEOUT, whapps_config:get_integer(?APP_NAME, <<"acl_request_timeout_ms">>, 2 * ?MILLISECONDS_IN_SECOND)).
-define(REQUEST_TIMEOUT_FUDGE, whapps_config:get_integer(?APP_NAME, <<"acl_request_timeout_fudge_ms">>, 100)).
-define(IP_REGEX, <<"^(\\d{1,3}\\\.\\d{1,3}\\\.\\d{1,3}\\\.\\d{1,3}).*">>).
-define(ACL_RESULT(IP, ACL), {'acl', IP, ACL}).

-type acls() :: wh_json:object().

-spec build(ne_binary()) -> acls().
build(Node) ->
    Routines = [fun offnet_resources/1
                ,fun local_resources/1
                ,fun sip_auth_ips/1
               ],
    Self = self(),
    PidRefs = [spawn_monitor(?MODULE, 'builder', [Self, F]) || F <- Routines],
    collect(system_config_acls(Node), PidRefs).

-spec collect(wh_json:object(), pid_refs()) ->
                     wh_json:object().
-spec collect(wh_json:object(), pid_refs(), wh_timeout()) ->
                     wh_json:object().
collect(ACLs, PidRefs) ->
    collect(ACLs, PidRefs, request_timeout()).

-spec request_timeout() -> pos_integer().
request_timeout() ->
    case whapps_config:get(<<"ecallmgr">>, <<"fetch_timeout">>) of
        'undefined' -> ?REQUEST_TIMEOUT;
        Timeout -> Timeout - ?REQUEST_TIMEOUT_FUDGE
    end.

collect(ACLs, [], _Timeout) ->
    lager:debug("acls built with ~p ms to spare", [_Timeout]),
    ACLs;
collect(ACLs, _PidRefs, Timeout) when Timeout < 0 ->
    lager:debug("timed out waiting for ACLs, returning what we got"),
    ACLs;
collect(ACLs, PidRefs, Timeout) ->
    Start = os:timestamp(),
    receive
        ?ACL_RESULT(IP, ACL) ->
            lager:debug("adding acl for '~s' to ~s", [IP, wh_json:get_value(<<"network-list-name">>, ACL)]),
            collect(wh_json:set_value(IP, ACL, ACLs), PidRefs, wh_util:decr_timeout(Timeout, Start));
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            case lists:keytake(Pid, 1, PidRefs) of
                'false' -> collect(ACLs, PidRefs, wh_util:decr_timeout(Timeout, Start));
                {'value', {Pid, Ref}, NewPidRefs} ->
                    collect(ACLs, NewPidRefs, wh_util:decr_timeout(Timeout, Start))
            end
    after Timeout ->
            lager:debug("timed out collecting acls, working with what we have"),
            ACLs
    end.

-spec builder(pid(), fun((pid()) -> any())) -> any().
builder(Collector, Fun) ->
    Fun(Collector).

-spec system_config_acls(ne_binary()) -> acls().
system_config_acls(Node) ->
    whapps_config:get(<<"ecallmgr">>, <<"acls">>, wh_json:new(), Node).

-spec sip_auth_ips(pid()) -> 'ok'.
sip_auth_ips(Collector) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get view results for sip_auth resources: ~p", [_R]);
        {'ok', JObjs} ->
            {RawIPs, RawHosts} = lists:foldl(fun needs_resolving/2, {[], []}, JObjs),
            _ = [handle_sip_auth_result(Collector, JObj, IPs) || {IPs, JObj} <- RawIPs],
            PidRefs = [spawn_monitor(?MODULE
                                     ,'resolve_hostname'
                                     ,[Collector
                                       ,Host
                                       ,JObj
                                       ,fun handle_sip_auth_result/3
                                      ])
                       || {Host, JObj} <- RawHosts
                      ],
            wait_for_pid_refs(PidRefs)
    end.

-spec needs_resolving(wh_json:object(), {list(), list()}) -> {list(), list()}.
needs_resolving(JObj, {IPs, ToResolve}) ->
    IP = wh_json:get_value(<<"key">>, JObj),
    case wh_network_utils:is_ipv4(IP) of
        'true' -> {[{[IP], JObj}|IPs], ToResolve};
        'false' -> {IPs, [{IP, JObj} | ToResolve]}
    end.

-spec wait_for_pid_refs(pid_refs()) -> 'ok'.
wait_for_pid_refs(PidRefs) ->
    wait_for_pid_refs(PidRefs, ?REQUEST_TIMEOUT).
wait_for_pid_refs([], _Timeout) -> 'ok';
wait_for_pid_refs(_PidRefs, Timeout) when Timeout < 0 -> 'ok';
wait_for_pid_refs(PidRefs, Timeout) ->
    Start = os:timestamp(),
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            case lists:keytake(Pid, 1, PidRefs) of
                'false' -> wait_for_pid_refs(PidRefs, wh_util:decr_timeout(Timeout, Start));
                {'value', {Pid, Ref}, NewPidRefs} ->
                    wait_for_pid_refs(NewPidRefs, wh_util:decr_timeout(Timeout, Start))
            end
    after Timeout ->
            lager:debug("timed out waiting for pid refs: ~p", [PidRefs])
    end.

-spec resolve_hostname(pid(), ne_binary(), wh_json:object(), fun()) -> 'ok'.
resolve_hostname(Collector, ResolveMe, JObj, ACLBuilderFun) ->
    lager:debug("attempting to resolve '~s'", [ResolveMe]),
    case (not wh_network_utils:is_ipv4(ResolveMe))
        andalso wh_network_utils:resolve(ResolveMe)
    of
        'false' ->
            maybe_capture_ip(Collector, ResolveMe, JObj, ACLBuilderFun);
        [] ->
            lager:debug("no IPs returned, checking for raw IP"),
            maybe_capture_ip(Collector, ResolveMe, JObj, ACLBuilderFun);
        IPs ->
            ACLBuilderFun(Collector, JObj, IPs),
            lager:debug("resolved '~s' for ~p: '~s'", [ResolveMe, Collector, wh_util:join_binary(IPs, <<"','">>)])
    end.

-spec maybe_capture_ip(pid(), ne_binary(), wh_json:object(), fun()) -> 'ok'.
maybe_capture_ip(Collector, CaptureMe, JObj, ACLBuilderFun) ->
    case re:run(CaptureMe, ?IP_REGEX, [{'capture', 'all', 'binary'}]) of
        {'match', [_All, IP]} ->
            ACLBuilderFun(Collector, JObj, [IP]),
            lager:debug("captured '~s' from ~s", [IP, CaptureMe]);
        'nomatch' ->
            lager:debug("failed to find IP at start of '~s'", [CaptureMe])
    end.

-spec handle_sip_auth_result(pid(), wh_json:object(), ne_binaries()) -> 'ok'.
handle_sip_auth_result(Collector, JObj, IPs) ->
    AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
    AuthorizingId = wh_doc:id(JObj),
    AuthorizingType = wh_json:get_value([<<"value">>, <<"authorizing_type">>], JObj),
    add_trusted_objects(Collector, AccountId, AuthorizingId, AuthorizingType, IPs).

-spec local_resources(pid()) -> 'ok'.
local_resources(Collector) ->
    ViewOptions = ['include_docs'],
    case couch_mgr:get_results(?WH_SIP_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get view results for local resources: ~p", [_R]);
        {'ok', JObjs} ->
            handle_resource_results(Collector, JObjs)
    end.

-spec offnet_resources(pid()) -> 'ok'.
offnet_resources(Collector) ->
    ViewOptions = ['include_docs'],
    case couch_mgr:get_results(?WH_OFFNET_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("Unable to get view results for offnet resources: ~p", [_R]);
        {'ok', JObjs} ->
            handle_resource_results(Collector, JObjs)
    end.

-spec handle_resource_results(pid(), wh_json:objects()) -> 'ok'.
handle_resource_results(Collector, JObjs) ->
    _ = [handle_resource_result(Collector, JObj) || JObj <- JObjs],
    'ok'.

-spec handle_resource_result(pid(), wh_json:object()) -> 'ok'.
handle_resource_result(Collector, JObj) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    InboundPidRefs = resource_inbound_ips(Collector, Doc),
    ServerPidRefs = resource_server_ips(Collector, Doc),
    wait_for_pid_refs(InboundPidRefs ++ ServerPidRefs).

-spec handle_resource_result(pid(), wh_json:object(), ne_binaries()) -> 'ok'.
handle_resource_result(Collector, JObj, IPs) ->
    AuthorizingId = wh_json:get_first_defined([<<"_id">>, <<"id">>], JObj),
    add_trusted_objects(Collector, 'undefined', AuthorizingId, <<"resource">>, IPs).

-spec resource_inbound_ips(pid(), wh_json:object()) -> pid_refs().
resource_inbound_ips(Collector, JObj) ->
    [spawn_monitor(?MODULE
                   ,'resolve_hostname'
                   ,[Collector
                     ,IP
                     ,JObj
                     ,fun handle_resource_result/3
                    ])
     || IP <- wh_json:get_value(<<"inbound_ips">>, JObj, [])
    ].

-spec resource_server_ips(pid(), wh_json:object()) -> pid_refs().
resource_server_ips(Collector, JObj) ->
    [spawn_monitor(?MODULE
                   ,'resolve_hostname'
                   ,[Collector
                     ,wh_json:get_value(<<"server">>, Gateway)
                     ,JObj
                     ,fun handle_resource_result/3
                    ])
     || Gateway <- wh_json:get_value(<<"gateways">>, JObj, []),
        wh_json:is_true(<<"enabled">>, Gateway, 'false')
    ].

-spec add_trusted_objects(pid(), api_binary(), ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
add_trusted_objects(_Collector, _AccountId, _AuthorizingId, _AuthorizingType, []) -> 'ok';
add_trusted_objects(Collector, AccountId, AuthorizingId, AuthorizingType, [IP|IPs]) ->
    Props = [{<<"type">>, <<"allow">>}
             ,{<<"network-list-name">>, <<"trusted">>}
             ,{<<"cidr">>, <<IP/binary, "/32">>}
             ,{<<"account_id">>, AccountId}
             ,{<<"authorizing_id">>, AuthorizingId}
             ,{<<"authorizing_type">>, AuthorizingType}
            ],
    Collector ! ?ACL_RESULT(IP, wh_json:from_list(props:filter_undefined(Props))),
    add_trusted_objects(Collector, AccountId, AuthorizingId, AuthorizingType, IPs).
