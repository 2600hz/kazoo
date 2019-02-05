%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(frontier_handle_acl).

-export([handle_acl_req/2
        ,lookup_acl_records/1, lookup_acl_records/2
        ]).

-include("frontier.hrl").

-spec handle_acl_req(kz_json:object(), kz_term:proplist()) -> any().
handle_acl_req(Reqest, _Props) ->
    'true' = kapi_frontier:acls_req_v(Reqest),
    Entity = kz_json:get_value(<<"Entity">>, Reqest),
    IncludeRealm  = kz_json:is_true(<<"With-Realm">>, Reqest, 'false'),
    Payload = lookup_acl_records(Entity, IncludeRealm),
    send_response(Reqest, Payload).

-spec send_response(kz_json:object(), kz_json:objects()) -> any().
send_response(Reqest, Responses) ->
    RespStub = kz_json:from_list([{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, Reqest)}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = kz_json:get_value(<<"Server-ID">>, Reqest),
    {DeviceACLs, RealmACLs} = lists:partition(fun frontier_utils:is_device/1, Responses),
    Resp = lists:foldl(fun kz_json:merge_jobjs/2, RespStub, [make_section(DeviceACLs, <<"Device">>)
                                                            ,make_section(RealmACLs, <<"Realm">>)
                                                            ]),
    lager:debug("publishing response"),
    kapi_frontier:publish_acls_resp(ServerID, Resp).

-spec make_section(kz_json:objects(), kz_term:ne_binary()) -> kz_json:object().
make_section([], _) ->
    kz_json:new();
make_section([JObj], Section) ->
    Order = kz_json:get_value([<<"value">>, <<"acls">>, <<"order">>], JObj),
    CIDRs = kz_json:get_value([<<"value">>, <<"acls">>, <<"cidrs">>], JObj),
    UserAgent = kz_json:get_value([<<"value">>, <<"acls">>, <<"user_agent">>], JObj),
    make_section(Section, Order, CIDRs, UserAgent).

-spec make_section(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binaries(), kz_term:api_binary()) -> kz_json:object().
make_section(_, Order, CIDRs, _) when Order =:= 'undefined'
                                      orelse CIDRs =:= 'undefined' ->
    kz_json:new();
make_section(Section, Order, CIDRs, UserAgent) ->
    Props = props:filter_undefined([{<<"Order">>, Order}
                                   ,{<<"CIDR">>, CIDRs}
                                   ,{<<"User-Agent">>, UserAgent}
                                   ]),
    kz_json:from_list([{Section, kz_json:from_list(Props)}]).

-spec lookup_acl_records(kz_term:ne_binary(), boolean()) -> kz_json:objects().
lookup_acl_records(Entity, IncludeRealm) ->
    lager:debug("handle acl request for ~s", [Entity]),
    Realm = frontier_utils:extract_realm(Entity),
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', _} ->
            lager:debug("found realm, try to send response"),
            run_acl_query(Entity, IncludeRealm);
        _ ->
            lager:info("can't find realm ~s. Sending deny ACL.", [Realm]),
            make_deny_acl(Entity, IncludeRealm)
    end.

-spec lookup_acl_records(kz_term:ne_binary()) -> kz_json:objects().
lookup_acl_records(Entity) ->
    lookup_acl_records(Entity, 'true').

-spec run_acl_query(kz_term:ne_binary(), boolean()) -> kz_json:objects().
run_acl_query(Entity, IncludeRealm) ->
    ViewOpts = build_view_options(Entity, IncludeRealm),
    {'ok', UserDb} = kapps_util:get_account_by_realm(frontier_utils:extract_realm(Entity)),
    lager:debug("looking for ~s's acls in ~s", [Entity, UserDb]),
    case kz_datamgr:get_results(UserDb, <<"access_lists/crossbar_listing">>, ViewOpts) of
        {'ok', Results} ->
            lager:debug("found ~p records", [length(Results)]),
            Results;
        _ ->
            lager:info("can't fetch records"),
            []
    end.

-spec build_view_options(kz_term:ne_binary(), boolean()) -> kz_term:proplist().
build_view_options(Entity, IncludeRealm) ->
    case binary:split(Entity, <<"@">>) of
        [User, _OnRealm] = Keys ->
            case IncludeRealm of
                'true' -> [{'keys', Keys}];
                _ -> [{'key', User}]
            end;
        [JustRealm] -> [{'key', JustRealm}]
    end.

-spec make_deny_acl(kz_term:ne_binary(), boolean()) -> kz_json:objects().
make_deny_acl(Entity, IncludeRealm) ->
    Realm = frontier_utils:extract_realm(Entity),
    IsDevice = Realm =/= Entity,
    Type = case IsDevice of
               'true' -> <<"device">>;
               _ -> <<"realm">>
           end,
    ACL = kz_json:from_list([{<<"order">>, <<"allow,deny">>}
                            ,{<<"cidrs">>, [<<"0.0.0.0/0">>]}
                            ]),
    Value = kz_json:from_list([{<<"type">>, Type}
                              ,{<<"acls">>, ACL}
                              ]),
    Record = kz_json:from_list([{<<"id">>, 'undefined'}
                               ,{<<"key">>, Entity}
                               ,{<<"value">>, Value}
                               ]),
    case IsDevice
        andalso IncludeRealm
    of
        'true' -> [Record | make_deny_acl(Realm)];
        _ -> [Record]
    end.

-spec make_deny_acl(kz_term:ne_binary()) -> kz_json:objects().
make_deny_acl(Entity) ->
    make_deny_acl(Entity, 'true').
