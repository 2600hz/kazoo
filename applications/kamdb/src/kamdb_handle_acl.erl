%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_handle_acl).

-export([handle_acl_req/2
        ]).

-include("kamdb.hrl").

-spec handle_acl_req(wh_json:object(), wh_proplist()) -> any().
handle_acl_req(JObj, _Props) ->
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    case whapps_util:get_account_by_realm(kamdb_utils:extract_realm(Entity)) of
        {'ok', _} -> send_response(JObj);
        _ -> deny(JObj)
    end.

-spec deny(wh_json:object()) -> any().
deny(JObj) ->
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, JObj),
    Section = case Entity =/= kamdb_utils:extract_realm(Entity) of
                  'true' -> <<"Device">>;
                  _ -> <<"Realm">>
              end,
    DenySubObj = wh_json:from_list([{<<"Order">>, <<"AD">>}
                                    ,{<<"CIDR">>, <<"0.0.0.0/0">>}
                                   ]),
    DenyObj = wh_json:from_list([{Section, DenySubObj}]),
    Resp = case wh_json:is_true(<<"With-Realm">>, JObj) of
               'true' ->
                   RealmDeny = wh_json:from_list([{<<"Realm">>, DenySubObj}]),
                   wh_json:merge_jobjs(DenyObj, RealmDeny);
               _ -> DenyObj
           end,
    wapi_kamdb:publish_ratelimits_resp(ServerID, wh_json:merge_jobjs(Resp, RespStub)).

-spec send_response(wh_json:object()) -> any().
send_response(JObj) ->
    'true' = wapi_kamdb:acls_req_v(JObj),
    RespStub = wh_json:from_list([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ServerID = wh_json:get_value(<<"Server-ID">>, JObj),
    Entity = wh_json:get_value(<<"Entity">>, JObj),
    Keys = binary:split(Entity, <<"@">>),
    ViewOpts = case Keys of
                   [User, OnRealm] ->
                       case wh_json:is_true(<<"With-Realm">>, JObj) of
                           'true' -> [{'keys', [User, OnRealm]}];
                           _ -> [{'key', User}]
                       end;
                   [JustRealm] -> [{'key', JustRealm}]
               end,
    {'ok', UserDb} = whapps_util:get_account_by_realm(kamdb_utils:extract_realm(Entity)),
    {'ok', Results} = couch_mgr:get_results(UserDb, <<"acls/crossbar_listing">>, ViewOpts),
    {DeviceACLs, RealmACLs} = lists:partition(fun is_device/1, Results),
    Resp = lists:foldl(fun wh_json:merge_jobjs/2, RespStub, [make_section(DeviceACLs, <<"Device">>)
                                                             ,make_section(RealmACLs, <<"Realm">>)
                                                            ]),
    wapi_kamdb:publish_acls_resp(ServerID, Resp).

-spec is_device(wh_json:object()) -> boolean().
is_device(JObj) ->
    <<"device">> =:= wh_json:get_value([<<"value">>, <<"type">>], JObj).

-spec make_section(wh_json:objects(), ne_binary()) -> wh_json:object().
-spec make_section(ne_binary(), api_object(), api_object()) -> wh_json:object().
make_section([], _) ->
    wh_json:new();
make_section([JObj], Section) ->
    Order = wh_json:get_value([<<"value">>, <<"acls">>, <<"order">>], JObj),
    CIDRs = wh_json:get_value([<<"value">>, <<"acls">>, <<"cidr">>], JObj),
    make_section(Section, Order, CIDRs).
make_section(_, Order, CIDRs)
    when Order =:= 'undefined' orelse CIDRs =:= 'undefined' ->
        wh_json:new();
make_section(Section, Order, CIDRs) ->
    wh_json:from_list([{Section, wh_json:from_list([{<<"Order">>, Order}, {<<"CIDR">>, CIDRs}])}]).
