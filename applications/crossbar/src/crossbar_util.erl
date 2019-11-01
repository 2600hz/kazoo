%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_util).

-export([response/2
        ,response/3
        ,response/4
        ,response/5
        ]).
-export([response_deprecated/1]).
-export([response_deprecated_redirect/2
        ,response_deprecated_redirect/3
        ]).
-export([response_redirect/3
        ,response_redirect/4
        ]).
-export([response_202/2, response_202/3]).
-export([response_400/3
        ,response_401/1
        ,response_402/2
        ]).
-export([response_faulty_request/1]).
-export([response_bad_identifier/2]).
-export([response_conflicting_docs/1]).
-export([response_datastore_timeout/1]).
-export([response_datastore_conn_refused/1]).
-export([response_invalid_data/2]).
-export([response_missing_view/1]).
-export([response_db_missing/1]).
-export([response_db_fatal/1]).
-export([response_auth/1
        ,response_auth/2
        ,response_auth/3
        ]).
-export([flush_registrations/1
        ,flush_registration/1, flush_registration/2
        ]).
-export([move_account/2]).
-export([get_descendants/1]).
-export([get_tree/1]).
-export([disable_account/1, maybe_disable_account/1
        ,enable_account/1
        ,change_pvt_enabled/2
        ]).
-export([load_apps/2]).
-export([get_path/2]).
-export([get_user_lang/2
        ,get_account_lang/1
        ,get_language/1
        ,get_language/2
        ,get_token_restrictions/3
        ]).
-export([get_user_timezone/2
        ]).
-export([apply_response_map/2]).
-export([maybe_remove_attachments/1]).

-export([create_auth_token/2]).

-export([descendants_count/0, descendants_count/1]).

-export([format_emergency_caller_id_number/1]).

-export([get_devices_by_owner/2]).
-export([maybe_refresh_fs_xml/2
        ,refresh_fs_xml/1
        ]).

-ifdef(TEST).
-export([trunkstore_servers_changed/2]).
-endif.

-include_lib("kazoo_services/include/kazoo_services.hrl").
-include("crossbar.hrl").

-define(DEFAULT_LANGUAGE
       ,kapps_config:get_ne_binary(?CONFIG_CAT, <<"default_language">>, <<"en-US">>)
       ).

-define(KEY_EMERGENCY, <<"emergency">>).

-type fails() :: 'error' | 'fatal'.

%%------------------------------------------------------------------------------
%% @doc This function set the response status to success, and load the provided
%% data.
%% @end
%%------------------------------------------------------------------------------
-spec response(kz_json:json_term(), cb_context:context()) ->
                      cb_context:context().
response(JTerm, Context) ->
    create_response('success', 'undefined', 'undefined', JTerm, Context).

-spec response_202(kz_term:api_ne_binary(), cb_context:context()) ->
                          cb_context:context().
response_202(Msg, Context) ->
    response_202(Msg, Msg, Context).

-spec response_202(kz_term:api_ne_binary(), kz_json:json_term(), cb_context:context()) ->
                          cb_context:context().
response_202(Msg, JTerm, Context) ->
    create_response('accepted', Msg, 202, JTerm, Context).

-spec response_400(kz_term:ne_binary(), kz_json:json_term(), cb_context:context()) ->
                          cb_context:context().
response_400(Message, Data, Context) ->
    create_response('error', Message, 400, Data, Context).

-spec response_401(cb_context:context()) -> cb_context:context().
response_401(Context) ->
    response('error', <<"invalid credentials">>, 401, Context).

-spec response_402(kz_json:json_term(), cb_context:context()) ->
                          cb_context:context().
response_402(Data, Context) ->
    create_response('error', <<"accept charges">>, 402, Data, Context).

%%------------------------------------------------------------------------------
%% @doc This function load the error message into a 500 response, of type
%% fatal or error.
%% @end
%%------------------------------------------------------------------------------
-spec response(fails(), kz_term:api_ne_binary(), cb_context:context()) ->
                      cb_context:context().
response('error', Msg, Context) ->
    create_response('error', Msg, 500, kz_json:new(), Context);
response('fatal', Msg, Context) ->
    create_response('fatal', Msg, 500, kz_json:new(), Context).

%%------------------------------------------------------------------------------
%% @doc This function load the error message into a specific code response,
%% of type fatal or error.
%% @end
%%------------------------------------------------------------------------------
-spec response(fails(), kz_term:api_ne_binary(), kz_term:api_integer(), cb_context:context()) ->
                      cb_context:context().
response('error', Msg, Code, Context) ->
    create_response('error', Msg, Code, kz_json:new(), Context);
response('fatal', Msg, Code, Context) ->
    create_response('fatal', Msg, Code, kz_json:new(), Context).

%%------------------------------------------------------------------------------
%% @doc This function load the error message into a specific code response,
%% of type fatal or error with additional data.
%% @end
%%------------------------------------------------------------------------------
-spec response(fails(), kz_term:api_ne_binary(), kz_term:api_integer(), kz_json:json_term(), cb_context:context()) -> cb_context:context().
response('error', Msg, Code, JTerm, Context) ->
    create_response('error', Msg, Code, JTerm, Context);
response('fatal', Msg, Code, JTerm, Context) ->
    create_response('fatal', Msg, Code, JTerm, Context).

%%------------------------------------------------------------------------------
%% @doc This function loads the response vars in Context, soon it will
%% make smarter choices about formatting resp_data and filtering
%% other parameters.
%% @end
%%------------------------------------------------------------------------------
-spec create_response(crossbar_status(), kz_term:api_ne_binary(), kz_term:api_integer()
                     ,kz_json:json_term(), cb_context:context()
                     ) -> cb_context:context().
create_response(Status, Msg, Code, JTerm, Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, Status}
                       ,{fun cb_context:set_resp_error_msg/2, Msg}
                       ,{fun cb_context:set_resp_error_code/2, Code}
                       ,{fun cb_context:set_resp_data/2, JTerm}
                       ]).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the request is faulty (doesn't have a
%% match in validation, or some other issue with it keeps it from being
%% processed, like nonsensical chains).
%% @end
%%------------------------------------------------------------------------------
-spec response_faulty_request(cb_context:context()) -> cb_context:context().
response_faulty_request(Context) ->
    response('error', <<"faulty request">>, 404, Context).

%%------------------------------------------------------------------------------
%% @doc When a module is no longer valid, alert the client of the deprecated status
%% by either sending a 410 Gone or a 301 Redirect (when using the arity
%% 3 version.
%%
%% The RedirectUrl should be relative to the accessed URL. So, if the
%% URL accessed that is deprecated is:
%% `/v1/account/{AID}/module/{MID}'
%% and that MID moved to `module2', the RedirectURL should be:
%% `../../module2/{MID}'
%%
%% If redirecting from `module1' to `module2', RedirectURL should be:
%% `../module2'.
%% @end
%%------------------------------------------------------------------------------
-spec response_deprecated(cb_context:context()) -> cb_context:context().
response_deprecated(Context) ->
    create_response('error', <<"deprecated">>, 410, kz_json:new(), Context).

-spec response_deprecated_redirect(cb_context:context(), kz_term:ne_binary()) ->
                                          cb_context:context().
response_deprecated_redirect(Context, RedirectUrl) ->
    response_deprecated_redirect(Context, RedirectUrl, kz_json:new()).

-spec response_deprecated_redirect(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
                                          cb_context:context().
response_deprecated_redirect(Context, RedirectUrl, JObj) ->
    create_response('error', <<"deprecated">>, 301, JObj
                   ,cb_context:add_resp_header(Context, <<"Location">>, RedirectUrl)
                   ).

-spec response_redirect(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
                               cb_context:context().
response_redirect(Context, RedirectUrl, JObj) ->
    response_redirect(Context, RedirectUrl, JObj, 301).

-spec response_redirect(cb_context:context(), kz_term:ne_binary(), kz_json:object(), pos_integer()) ->
                               cb_context:context().
response_redirect(Context, RedirectUrl, JObj, Redirect) ->
    create_response('error', <<"redirect">>, Redirect, JObj
                   ,cb_context:add_resp_header(Context, <<"Location">>, RedirectUrl)
                   ).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the requested ID did not match a
%% data record. Using 404 as 410 is a permanent Gone, while 404 is
%% a softer not found now.
%% @end
%%------------------------------------------------------------------------------
-spec response_bad_identifier(atom() | kz_term:ne_binary(), cb_context:context()) ->
                                     cb_context:context().
response_bad_identifier(Id, Context) when is_atom(Id) ->
    response('error', <<"bad identifier">>, 404, [kz_term:to_binary(Id)], Context);
response_bad_identifier(?NE_BINARY = Id, Context) ->
    response('error', <<"bad identifier">>, 404, [Id], Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the requested resource update fails
%% because of a conflict in the DB.
%% @end
%%------------------------------------------------------------------------------
-spec response_conflicting_docs(cb_context:context()) ->
                                       cb_context:context().
response_conflicting_docs(Context) ->
    response('error', <<"conflicting documents">>, 409, Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the requested data query was missing.
%% @end
%%------------------------------------------------------------------------------
-spec response_missing_view(cb_context:context()) ->
                                   cb_context:context().
response_missing_view(Context) ->
    response('fatal', <<"datastore missing view">>, 503, Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the datastore timed out.
%% @end
%%------------------------------------------------------------------------------
-spec response_datastore_timeout(cb_context:context()) ->
                                        cb_context:context().
response_datastore_timeout(Context) ->
    response('error', <<"datastore timeout">>, 503, Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the datastore timed out.
%% @end
%%------------------------------------------------------------------------------
-spec response_datastore_conn_refused(cb_context:context()) ->
                                             cb_context:context().
response_datastore_conn_refused(Context) ->
    response('error', <<"datastore connection refused">>, 503, Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the provided data did not validate.
%% @end
%%------------------------------------------------------------------------------
-spec response_invalid_data(kz_json:json_term(), cb_context:context()) ->
                                   cb_context:context().
response_invalid_data(JTerm, Context) ->
    response('error', <<"invalid data">>, 400, JTerm, Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the datastore does not have the requested
%% record collection.
%% @end
%%------------------------------------------------------------------------------
-spec response_db_missing(cb_context:context()) -> cb_context:context().
response_db_missing(Context) ->
    response('fatal', <<"data collection missing: database not found">>, 503, Context).

%%------------------------------------------------------------------------------
%% @doc Create a standard response if the datastore does not have the requested
%% record collection.
%% @end
%%------------------------------------------------------------------------------
-spec response_db_fatal(cb_context:context()) -> cb_context:context().
response_db_fatal(Context) ->
    response('fatal', <<"datastore fatal error">>, 503, Context).

-spec flush_registrations(kz_term:ne_binary() | cb_context:context()) -> 'ok'.
flush_registrations(<<_/binary>> = Realm) ->
    FlushCmd = [{<<"Realm">>, Realm}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ],
    kz_amqp_worker:cast(FlushCmd, fun kapi_registration:publish_flush/1);
flush_registrations(Context) ->
    flush_registrations(kzd_accounts:fetch_realm(cb_context:account_id(Context))).

-spec flush_registration(kz_term:api_binary(), kz_term:ne_binary() | cb_context:context()) -> 'ok'.
flush_registration('undefined', _Realm) ->
    lager:debug("did not flush registration: username is undefined");
flush_registration(Username, <<_/binary>> = Realm) ->
    FlushCmd = [{<<"Event">>, <<"check-sync">>}
               ,{<<"Realm">>, Realm}
               ,{<<"Username">>, Username}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ],
    _ = kz_amqp_worker:cast(FlushCmd, fun kapi_switch:publish_notify/1),
    kz_amqp_worker:cast(FlushCmd, fun kapi_registration:publish_flush/1);
flush_registration(Username, Context) ->
    Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    flush_registration(Username, Realm).

-spec flush_registration(cb_context:context()) -> 'ok'.
flush_registration(Context) ->
    OldDevice = cb_context:fetch(Context, 'db_doc'),
    NewDevice = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),
    Realm = kzd_accounts:fetch_realm(AccountId),

    Routins = [fun maybe_flush_registration_on_password/3
              ,fun maybe_flush_registration_on_username/3
              ,fun maybe_flush_registration_on_ownerid/3
              ,fun maybe_flush_registration_on_enabled/3
              ,fun maybe_flush_registration_on_deleted/3
              ],
    Fun = fun(_, 'ok') -> 'ok';
             (F, 'next') -> F(Realm, OldDevice, NewDevice)
          end,
    _ = lists:foldl(Fun, 'next', Routins),
    'ok'.

-spec maybe_flush_registration_on_password(kz_term:api_binary(), kz_json:object(), kz_json:object()) -> 'ok' | 'next'.
maybe_flush_registration_on_password(Realm, OldDevice, NewDevice) ->
    case kzd_devices:sip_password(OldDevice) =:= kzd_devices:sip_password(NewDevice) of
        'true' -> 'next';
        'false' ->
            lager:debug("the SIP password has changed, sending a registration flush"),
            flush_registration(kzd_devices:sip_username(OldDevice), Realm)
    end.

-spec maybe_flush_registration_on_username(kz_term:api_binary(), kz_json:object(), kz_json:object()) -> 'ok' | 'next'.
maybe_flush_registration_on_username(Realm, OldDevice, NewDevice) ->
    OldUsername = kzd_devices:sip_username(OldDevice),

    case kzd_devices:sip_username(NewDevice) of
        OldUsername -> 'next';
        NewUsername ->
            lager:debug("the SIP username has changed, sending a registration flush for both"),
            flush_registration(OldUsername, Realm),
            flush_registration(NewUsername, Realm)
    end.

-spec maybe_flush_registration_on_ownerid(kz_term:api_binary(), kz_json:object(), kz_json:object()) -> 'ok' | 'next'.
maybe_flush_registration_on_ownerid(Realm, OldDevice, NewDevice) ->
    OldOwnerId = kzd_devices:owner_id(OldDevice),

    case kzd_devices:owner_id(NewDevice) of
        OldOwnerId -> 'next';
        _NewOwnerId ->
            lager:debug("the device owner_id has changed, sending a registration flush"),
            flush_registration(kzd_devices:sip_username(OldDevice), Realm)
    end.

-spec maybe_flush_registration_on_enabled(kz_term:api_binary(), kz_json:object(), kz_json:object()) -> 'ok' | 'next'.
maybe_flush_registration_on_enabled(Realm, OldDevice, NewDevice) ->
    OldEnabled = kzd_devices:enabled(OldDevice),

    case kzd_devices:enabled(NewDevice) of
        OldEnabled -> 'next';
        _NewEnabled ->
            lager:debug("the device enabled has changed, sending a registration flush"),
            flush_registration(kzd_devices:sip_username(OldDevice), Realm)
    end.

-spec maybe_flush_registration_on_deleted(kz_term:api_binary(), kz_json:object(), kz_json:object()) -> 'ok' | 'next'.
maybe_flush_registration_on_deleted(Realm, _OldDevice, NewDevice) ->
    case kz_doc:is_soft_deleted(NewDevice)
        orelse kz_doc:is_deleted(NewDevice)
    of
        'false' -> 'next';
        'true' ->
            lager:debug("the device has been deleted, sending a registration flush"),
            flush_registration(kzd_devices:sip_username(NewDevice), Realm)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec move_account(kz_term:ne_binary(), kz_term:ne_binary()) ->
                          {'ok', kz_json:object()} |
                          {'error', any()}.
move_account(?MATCH_ACCOUNT_RAW(AccountId), ToAccount=?NE_BINARY) ->
    case validate_move(AccountId, ToAccount) of
        {'error', _E}=Error -> Error;
        {'ok', JObj, ToTree} ->
            move_account(AccountId, JObj, ToAccount, ToTree)
    end.

-spec move_account(kz_term:ne_binary(), kzd_accounts:doc(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
                          {'ok', kzd_services:doc()} |
                          {'error', any()}.
move_account(AccountId, JObj, ToAccount, ToTree) ->
    PreviousTree = kzd_accounts:tree(JObj),
    Updates = [{[?SERVICES_PVT_TREE], ToTree}
              ,{[?SERVICES_PVT_TREE_PREVIOUSLY], PreviousTree}
              ,{[?SERVICES_PVT_MODIFIED], kz_time:now_s()}
              ],

    %%FIXME: do something about setting pvt_auth_*_id
    case kzd_accounts:update(AccountId, Updates) of
        {'error', _E}=Error -> Error;
        {'ok', _AccountDoc} ->
            lager:info("moved account ~s (~s)", [AccountId, kz_doc:revision(_AccountDoc)]),
            NewResellerId = kz_services_reseller:get_id(ToAccount),
            {'ok', _} = move_descendants(AccountId, ToTree, NewResellerId),
            Services = kazoo_services_maintenance:update_tree(AccountId, ToTree, NewResellerId),
            {'ok', kz_services:services_jobj(Services)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_move(kz_term:ne_binary(), kz_term:ne_binary()) ->
                           {'error', kz_datamgr:data_error() | 'forbidden'} |
                           {'ok', kzd_accounts:doc(), kz_term:ne_binaries()}.
validate_move(AccountId, ToAccount) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _E}=Error -> Error;
        {'ok', JObj} ->
            ToTree = get_tree(ToAccount) ++ [ToAccount],
            case lists:member(AccountId, ToTree) of
                'true' -> {'error', 'forbidden'};
                'false' -> {'ok', JObj, ToTree}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec move_descendants(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) ->
                              {'ok', 'done'} |
                              {'error', any()}.
move_descendants(?MATCH_ACCOUNT_RAW(AccountId), Tree, NewResellerId) ->
    update_descendants_tree(get_descendants(AccountId), Tree, NewResellerId, AccountId).

-spec update_descendants_tree(kz_term:ne_binaries(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                     {'ok', 'done'} |
                                     {'error', any()}.
update_descendants_tree([], _, _, _) -> {'ok', 'done'};
update_descendants_tree([Descendant|Descendants], Tree, NewResellerId, MovedAccountId) ->
    case kzd_accounts:fetch(Descendant) of
        {'error', _E}=Error -> Error;
        {'ok', AccountJObj} ->
            PreviousTree = kzd_accounts:tree(AccountJObj),
            %% Preserve tree below and including common ancestor
            {_, Tail} = lists:splitwith(fun(X) -> X =/= MovedAccountId end, PreviousTree),
            ToTree = Tree ++ Tail,
            Updates = [{[?SERVICES_PVT_TREE], ToTree}
                      ,{[?SERVICES_PVT_TREE_PREVIOUSLY], PreviousTree}
                      ,{[?SERVICES_PVT_MODIFIED], kz_time:now_s()}
                      ],
            %%FIXME: do something about setting pvt_auth_*_id
            case kzd_accounts:update(Descendant, Updates) of
                {'error', _E}=Error -> Error;
                {'ok', _DescendantAccountJObj} ->
                    AccountId = kz_util:format_account_id(Descendant),
                    lager:debug("updated descendant ~s (~s)", [AccountId, kz_doc:revision(_DescendantAccountJObj)]),
                    _ = kazoo_services_maintenance:update_tree(AccountId, ToTree, NewResellerId),
                    update_descendants_tree(Descendants, Tree, NewResellerId, MovedAccountId)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Return all descendants of the account id.
%% @end
%%------------------------------------------------------------------------------
-spec get_descendants(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_descendants(?MATCH_ACCOUNT_RAW(AccountId)) ->
    kapps_util:account_descendants(AccountId) -- [AccountId].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_tree(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_tree(<<_/binary>> = Account) ->
    case kzd_accounts:fetch(Account) of
        {'ok', JObj} -> kzd_accounts:tree(JObj);
        {'error', _E} ->
            lager:error("could not load account doc ~s : ~p", [Account, _E]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Flag all descendants of the account id as disabled.
%% @end
%%------------------------------------------------------------------------------
-spec disable_account(kz_term:api_binary()) -> 'ok' | {'error', any()}.
disable_account('undefined') -> 'ok';
disable_account(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            _ = [change_pvt_enabled('false', kz_doc:id(JObj)) || JObj <- JObjs],
            'ok';
        {'error', R}=E ->
            lager:debug("unable to disable descendants of ~s: ~p", [AccountId, R]),
            E
    end.

-spec maybe_disable_account(kz_term:ne_binary()) ->
                                   'ok' |
                                   {'ok', kzd_accounts:doc()} |
                                   kz_datamgr:data_error().
maybe_disable_account(<<AccountId/binary>>) ->
    {'ok', AccountJObj} = kzd_accounts:fetch(AccountId),
    case kzd_accounts:is_enabled(AccountJObj) of
        'false' -> {'ok', AccountJObj};
        'true' ->
            Update = [{kzd_accounts:path_enabled(), 'false'}],
            case kzd_accounts:update(AccountId, Update) of
                {'ok', _UpdatedAccount}=OK ->
                    lager:debug("disabled account ~s (~s)", [AccountId, kz_doc:revision(_UpdatedAccount)]),
                    OK;
                {'error', _E}=Error ->
                    lager:info("failed to disable account ~s on save: ~p", [AccountId, _E]),
                    Error
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Flag all descendants of the account id as enabled.
%% @end
%%------------------------------------------------------------------------------
-spec enable_account(kz_term:api_binary()) -> 'ok' | {'error', any()}.
enable_account('undefined') -> 'ok';
enable_account(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            _ = [change_pvt_enabled('true', kz_doc:id(JObj)) || JObj <- JObjs],
            'ok';
        {'error', R}=E ->
            lager:debug("unable to enable descendants of ~s: ~p", [AccountId, R]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc Helper to set data for all auth type.
%% @end
%%------------------------------------------------------------------------------

-spec response_auth(kz_json:object()) ->
                           kz_json:object().
response_auth(JObj) ->
    AccountId = kz_json:get_first_defined([<<"account_id">>, <<"pvt_account_id">>], JObj),
    OwnerId = kz_json:get_first_defined([<<"owner_id">>, <<"user_id">>], JObj),
    response_auth(JObj, AccountId, OwnerId).

-spec response_auth(kz_json:object(), kz_term:api_binary()) ->
                           kz_json:object().
response_auth(JObj, AccountId) ->
    OwnerId = kz_json:get_first_defined([<<"owner_id">>, <<"user_id">>], JObj),
    response_auth(JObj, AccountId, OwnerId).

-spec response_auth(kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) ->
                           kz_json:object().
response_auth(JObj, AccountId, UserId) ->
    populate_resp(JObj, AccountId, UserId).

-spec populate_resp(kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) -> kz_json:object().
populate_resp(JObj, 'undefined', _UserId) -> JObj;
populate_resp(JObj, AccountId, UserId) ->
    Language = get_language(AccountId, UserId),
    Props = props:filter_undefined(
              [{<<"apps">>, load_apps(AccountId, UserId, Language)}
              ,{<<"language">>, Language}
              ,{<<"account_name">>, kzd_accounts:fetch_name(AccountId)}
              ,{<<"is_reseller">>, kz_services_reseller:is_reseller(AccountId)}
              ,{<<"reseller_id">>, kz_services_reseller:get_id(AccountId)}
              ]),
    kz_json:set_values(Props, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_apps(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
load_apps(AccountId, UserId) ->
    Language = get_language(AccountId, UserId),
    load_apps(AccountId, UserId, Language).

-spec load_apps(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
load_apps(AccountId, UserId, Language) ->
    AppJObjs = cb_apps_util:allowed_apps(AccountId, UserId),
    [format_app(Language, AppJObj)
     || AppJObj <- AppJObjs
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_app(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
format_app(Lang, AppJObj) ->
    I18N = kzd_app:i18n(AppJObj),
    DefaultLabel = kz_json:get_value([?DEFAULT_LANGUAGE, <<"label">>], I18N),
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(AppJObj)}
      ,{<<"name">>, kzd_app:name(AppJObj)}
      ,{<<"api_url">>, kzd_app:api_url(AppJObj)}
      ,{<<"source_url">>, kzd_app:source_url(AppJObj)}
      ,{<<"label">>, kz_json:get_value([Lang, <<"label">>], I18N, DefaultLabel)}
      ]).

%%------------------------------------------------------------------------------
%% @doc Update all descendants of the account id pvt_enabled flag with State.
%% @end
%%------------------------------------------------------------------------------
-spec change_pvt_enabled(boolean(), kz_term:api_ne_binary()) ->
                                'ok' | {'error', any()}.
change_pvt_enabled(_, 'undefined') -> 'ok';
change_pvt_enabled(IsEnabled, <<AccountId/binary>>) ->
    Update = [{kzd_accounts:path_enabled(), IsEnabled}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _UpdatedAccountDoc} ->
            lager:debug("set pvt_enabled to ~s on account ~s", [IsEnabled, AccountId]);
        {'error', _R}=Error ->
            lager:debug("unable to set pvt_enabled to ~s on account ~s: ~p", [IsEnabled, AccountId, _R]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Get user/account language
%% @end
%%------------------------------------------------------------------------------

-spec get_language(kz_term:ne_binary()) -> kz_term:ne_binary().
get_language(AccountId) ->
    case get_account_lang(AccountId) of
        {'ok', Lang} -> Lang;
        'error' -> ?DEFAULT_LANGUAGE
    end.

-spec get_language(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
get_language(AccountId, 'undefined') ->
    case get_account_lang(AccountId) of
        {'ok', Lang} -> Lang;
        'error' -> ?DEFAULT_LANGUAGE
    end;
get_language(AccountId, UserId) ->
    case get_user_lang(AccountId, UserId) of
        {'ok', Lang} -> Lang;
        'error' ->
            case get_account_lang(AccountId) of
                {'ok', Lang} -> Lang;
                'error' -> ?DEFAULT_LANGUAGE
            end
    end.

-spec get_user_lang(kz_term:ne_binary(), kz_term:ne_binary()) -> 'error' | {'ok', kz_term:ne_binary()}.
get_user_lang(AccountId, UserId) ->
    case kzd_users:fetch(AccountId, UserId) of
        {'ok', UserJObj} ->
            case kz_json:get_value(<<"language">>, UserJObj) of
                'undefined' -> 'error';
                Lang -> {'ok', Lang}
            end;
        {'error', _E} ->
            lager:error("failed to lookup user ~p in ~p: ~p", [UserId, AccountId, _E]),
            'error'
    end.

-spec get_account_lang(kz_term:ne_binary()) -> 'error' | {'ok', kz_term:ne_binary()}.
get_account_lang(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountJObj} ->
            case kzd_accounts:language(AccountJObj) of
                'undefined' -> 'error';
                Lang -> {'ok', Lang}
            end;
        {'error', _E} ->
            lager:error("failed to lookup account ~p: ~p", [AccountId, _E]),
            'error'
    end.

-spec get_user_timezone(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_user_timezone(AccountId, UserId) ->
    case kzd_users:fetch(AccountId, UserId) of
        {'ok', UserJObj} -> kzd_users:timezone(UserJObj);
        {'error', _E} -> kzd_accounts:timezone(AccountId)
    end.

-spec apply_response_map(cb_context:context(), kz_term:proplist()) -> cb_context:context().
apply_response_map(Context, Map) ->
    JObj = cb_context:doc(Context),
    RespData = lists:foldl(fun(MapItem, J) ->
                                   apply_response_map_item(MapItem, J, JObj)
                           end
                          ,cb_context:resp_data(Context)
                          ,Map
                          ),
    cb_context:set_resp_data(Context, RespData).

-spec apply_response_map_item({kz_json:path(), kz_json:path() | fun()}, kz_json:object(), kz_json:object()) ->
                                     kz_json:object().
apply_response_map_item({Key, Fun}, J, JObj) when is_function(Fun, 1) ->
    kz_json:set_value(Key, Fun(JObj), J);
apply_response_map_item({Key, Fun}, J, JObj) when is_function(Fun, 2) ->
    Id = kz_doc:id(JObj, kz_json:get_value(<<"Id">>, JObj)),
    kz_json:set_value(Key, Fun(Id, JObj), J);
apply_response_map_item({Key, ExistingKey}, J, JObj) ->
    kz_json:set_value(Key, kz_json:get_value(ExistingKey, JObj), J).

-spec get_path(cowboy_req:req() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_path(<<_/binary>> = RawPath, Relative) ->
    kz_http_util:resolve_uri(RawPath, Relative);
get_path(Req, Relative) ->
    get_path(cowboy_req:path(Req), Relative).

-spec maybe_remove_attachments(cb_context:context()) -> cb_context:context().
maybe_remove_attachments(Context) ->
    case kz_doc:maybe_remove_attachments(cb_context:doc(Context)) of
        {'false', _} -> Context;
        {'true', RemovedJObj} ->
            lager:debug("deleting attachments from doc"),
            crossbar_doc:save(
              cb_context:set_doc(Context, RemovedJObj)
             )
    end.

-spec create_auth_token(cb_context:context(), atom()) ->
                               cb_context:context().
create_auth_token(Context, AuthModule) ->
    JObj = cb_context:doc(Context),
    case kz_json:is_empty(JObj) of
        'true' ->
            lager:debug("empty doc, no auth token created"),
            response_401(Context);
        'false' ->
            create_auth_token(Context, AuthModule, JObj)
    end.

-spec create_auth_token(cb_context:context(), atom(), kz_json:object()) ->
                               cb_context:context().
create_auth_token(Context, AuthModule, JObj) ->
    Data = cb_context:req_data(Context),

    AccountId = kz_json:get_value(<<"account_id">>, JObj),
    OwnerId = kz_json:get_value(<<"owner_id">>, JObj),

    Token = props:filter_undefined(
              [{<<"account_id">>, AccountId}
              ,{<<"owner_id">>, OwnerId}
              ,{<<"as">>, kz_json:get_value(<<"as">>, Data)}
              ,{<<"api_key">>, kz_json:get_value(<<"api_key">>, Data)}
              ,{<<"method">>, kz_term:to_binary(AuthModule)}
              ]),
    JObjToken = kz_doc:update_pvt_parameters(kz_json:from_list(Token)
                                            ,kz_util:format_account_id(AccountId, 'encoded')
                                            ,Token
                                            ),

    %%FIXME: do something about setting pvt_auth_*_id
    case kz_datamgr:save_doc(?KZ_TOKEN_DB, JObjToken) of
        {'ok', Doc} ->
            AuthToken = kz_doc:id(Doc),
            lager:debug("created new local auth token ~s", [AuthToken]),
            response(response_auth(JObj, AccountId, OwnerId)
                    ,cb_context:setters(Context
                                       ,[{fun cb_context:set_auth_token/2, AuthToken}
                                        ,{fun cb_context:set_auth_doc/2, Doc}
                                        ])
                    );
        {'error', R} ->
            lager:debug("could not create new local auth token, ~p", [R]),
            cb_context:add_system_error('invalid_credentials', Context)
    end.

-spec get_token_restrictions(atom(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                    kz_term:api_object().
get_token_restrictions(AuthModule, AccountId, OwnerId) ->
    case kzd_accounts:is_superduper_admin(AccountId) of
        'true' -> 'undefined';
        'false' ->
            Restrictions =
                case get_account_token_restrictions(AccountId, AuthModule) of
                    'undefined' -> get_system_token_restrictions(AuthModule);
                    AccountRestrictions -> AccountRestrictions
                end,
            PrivLevel = get_priv_level(AccountId, OwnerId),
            get_priv_level_restrictions(Restrictions, PrivLevel)
    end.

-spec get_priv_level(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_binary().
%% for api_auth tokens we force "admin" priv_level
get_priv_level(_AccountId, 'undefined') ->
    cb_token_restrictions:default_priv_level();
get_priv_level(AccountId, OwnerId) ->
    AccountDB = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDB, OwnerId) of
        {'ok', Doc} -> kzd_users:priv_level(Doc);
        {'error', _} -> cb_token_restrictions:default_priv_level()
    end.

-spec get_system_token_restrictions(atom()) -> kz_term:api_object().
get_system_token_restrictions(AuthModule) ->
    case cb_token_restrictions:method_restrictions(AuthModule) of
        'undefined' -> cb_token_restrictions:default_method_restrictions();
        MethodRestrictions -> MethodRestrictions
    end.

-spec get_account_token_restrictions(kz_term:ne_binary(), atom()) -> kz_term:api_object().
get_account_token_restrictions(AccountId, AuthModule) ->
    AccountDB = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDB, ?CB_ACCOUNT_TOKEN_RESTRICTIONS) of
        {'error', _} -> 'undefined';
        {'ok', RestrictionsDoc} ->
            kz_json:get_first_defined([[<<"restrictions">>, kz_term:to_binary(AuthModule)]
                                      ,[<<"restrictions">>, ?CATCH_ALL]
                                      ]
                                     ,RestrictionsDoc
                                     )
    end.

-spec get_priv_level_restrictions(kz_term:api_object(), kz_term:ne_binary()) -> kz_term:api_object().
get_priv_level_restrictions('undefined', _PrivLevel) ->
    'undefined';
get_priv_level_restrictions(Restrictions, PrivLevel) ->
    RestrictionLevels = kz_json:get_keys(Restrictions),
    case lists:member(PrivLevel, RestrictionLevels) of
        'true' -> kz_json:get_json_value(PrivLevel, Restrictions);
        'false' -> kz_json:get_json_value(?CATCH_ALL, Restrictions)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec descendants_count() -> 'ok'.
descendants_count() ->
    Limit = kapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, 1000),
    ViewOptions = [{'limit', Limit}
                  ,{'skip', 0}
                  ],
    descendants_count(ViewOptions).

-spec descendants_count(kz_term:proplist() | kz_term:ne_binary()) -> 'ok'.
descendants_count(<<Account/binary>>) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    descendants_count([{'key', AccountId}]);
descendants_count(Opts) ->
    ViewOptions = [{'group_level', 1}
                   | props:delete('group_level', Opts)
                  ],

    case load_descendants_count(ViewOptions) of
        {'error', 'no_descendants'} ->
            handle_no_descendants(ViewOptions);
        {'error', _E} ->
            io:format("could not load view listing_by_descendants_count: ~p~n", [_E]);
        {'ok', Counts} ->
            handle_descendant_counts(ViewOptions, Counts)
    end.

-spec handle_descendant_counts(kz_term:proplist(), kz_term:proplist()) -> 'ok'.
handle_descendant_counts(ViewOptions, Counts) ->
    _ = [maybe_update_descendants_count(AccountId, Count)
         || {AccountId, Count} <- Counts
        ],

    case props:get_value('skip', ViewOptions) of
        'undefined' -> 'ok';
        Skip ->
            Limit = props:get_value('limit', ViewOptions),
            descendants_count(props:set_value('skip', Skip+Limit, ViewOptions))
    end.

-spec handle_no_descendants(kz_term:proplist()) -> 'ok'.
handle_no_descendants(ViewOptions) ->
    case props:get_value('key', ViewOptions) of
        'undefined' -> 'ok';
        AccountId ->
            maybe_update_descendants_count(AccountId, 0)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec format_emergency_caller_id_number(cb_context:context()) ->
                                               cb_context:context().
format_emergency_caller_id_number(Context) ->
    case cb_context:req_value(Context, [<<"caller_id">>, ?KEY_EMERGENCY]) of
        'undefined' -> Context;
        Emergency ->
            format_emergency_caller_id_number(Context, Emergency)
    end.

-spec format_emergency_caller_id_number(cb_context:context(), kz_json:object()) ->
                                               cb_context:context().
format_emergency_caller_id_number(Context, Emergency) ->
    case kz_json:get_ne_binary_value(<<"number">>, Emergency) of
        'undefined' -> Context;
        Number ->
            NEmergencyJObj = kz_json:set_value(<<"number">>, knm_converters:normalize(Number), Emergency),
            CallerIdJObj = cb_context:req_value(Context, <<"caller_id">>),
            NCallerIdJObj = kz_json:set_value(?KEY_EMERGENCY, NEmergencyJObj, CallerIdJObj),

            lager:debug("setting emergency caller id from ~s to ~s", [Number, knm_converters:normalize(Number)]),
            cb_context:set_req_data(Context
                                   ,kz_json:set_value(<<"caller_id">>, NCallerIdJObj, cb_context:req_data(Context))
                                   )
    end.

-type refresh_type() :: 'user' | 'device' | 'sys_info' | 'account'.

-spec maybe_refresh_fs_xml(refresh_type(), cb_context:context()) -> 'ok'.
maybe_refresh_fs_xml(Kind, Context) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    Doc = cb_context:doc(Context),
    Precondition =
        (kzd_devices:presence_id(DbDoc) =/= kzd_devices:presence_id(Doc))
        or (kz_json:get_value([<<"media">>, <<"encryption">>, <<"enforce_security">>], DbDoc) =/=
                kz_json:get_value([<<"media">>, <<"encryption">>, <<"enforce_security">>], Doc)
           ),
    maybe_refresh_fs_xml(Kind, Context, Precondition).

-spec maybe_refresh_fs_xml(refresh_type(), cb_context:context(), boolean()) -> 'ok'.
maybe_refresh_fs_xml('user', _Context, 'false') -> 'ok';
maybe_refresh_fs_xml('user', Context, 'true') ->
    Doc = cb_context:doc(Context),
    AccountDb = cb_context:account_db(Context),
    Realm = kzd_accounts:fetch_realm(AccountDb),
    Id = kz_doc:id(Doc),
    Devices = get_devices_by_owner(AccountDb, Id),
    lists:foreach(fun (DevDoc) -> refresh_fs_xml(Realm, DevDoc) end, Devices);
maybe_refresh_fs_xml('device', Context, Precondition) ->
    Doc   = cb_context:doc(Context),
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    ( Precondition
      or (kzd_devices:sip_username(DbDoc) =/= kzd_devices:sip_username(Doc))
      or (kzd_devices:sip_password(DbDoc) =/= kzd_devices:sip_password(Doc))
      or (kz_json:get_value(<<"owner_id">>, DbDoc) =/=
              kz_json:get_value(<<"owner_id">>, Doc))
      or (kz_json:is_true(<<"enabled">>, DbDoc)
          andalso not kz_json:is_true(<<"enabled">>, Doc)
         )
    )
        andalso refresh_fs_xml(kzd_accounts:fetch_realm(cb_context:account_db(Context))
                              ,DbDoc
                              ),
    'ok';
maybe_refresh_fs_xml('account', Context, _) ->
    Devices = get_account_devices(cb_context:account_id(Context)),
    Realm = kzd_accounts:fetch_realm(cb_context:account_db(Context)),
    lists:foreach(fun(DevDoc) -> refresh_fs_xml(Realm, DevDoc) end, Devices);
maybe_refresh_fs_xml('sys_info', Context, Precondition) ->
    Doc = cb_context:doc(Context),
    Servers = kz_json:get_value(<<"servers">>, Doc, []),

    DbDoc = cb_context:fetch(Context, 'db_doc'),
    DbServers = kz_json:get_value(<<"servers">>, DbDoc, []),

    ( Precondition
      or trunkstore_servers_changed(Servers, DbServers)
    ).

-spec trunkstore_servers_changed(kz_json:objects(), kz_json:objects()) -> boolean().
trunkstore_servers_changed([], []) -> 'false';
trunkstore_servers_changed([], _DbServers) -> 'true';
trunkstore_servers_changed(_Servers, []) -> 'true';
trunkstore_servers_changed(Servers, DbServers) ->
    MappedServers = map_servers(Servers),
    DbMappedServers = map_servers(DbServers),

    servers_changed(MappedServers, DbMappedServers)
        orelse servers_changed(DbMappedServers, MappedServers).

-spec servers_changed(kz_json:object(), kz_json:object()) -> boolean().
servers_changed(Servers1, Servers2) ->
    kz_json:any(fun({Name, S}) ->
                        server_changed(S, kz_json:get_value(Name, Servers2))
                end
               ,Servers1
               ).

-spec server_changed(kz_json:object(), kz_term:api_object()) -> boolean().
server_changed(_Server, 'undefined') ->
    lager:debug("server ~s existence has changed", [kz_json:get_value(<<"server_name">>, _Server)]),
    'true';
server_changed(Server1, Server2) ->
    Keys = [ [<<"auth">>, <<"auth_method">>]
           ,[<<"auth">>, <<"ip">>]
           ,[<<"auth">>, <<"auth_user">>]
           ,[<<"auth">>, <<"auth_password">>]
           ,[<<"options">>, <<"enabled">>]
           ],
    lists:any(fun(K) ->
                      kz_json:get_value(K, Server1) =/= kz_json:get_value(K, Server2)
              end
             ,Keys
             ).

-spec map_servers(kz_json:objects()) -> kz_json:object().
map_servers(Servers) ->
    lists:foldl(fun map_server/2, kz_json:new(), Servers).

-spec map_server(kz_json:object(), kz_json:object()) -> kz_json:object().
map_server(Server, Acc) ->
    Name = kz_json:get_value(<<"server_name">>, Server),
    kz_json:set_value(Name, Server, Acc).

-spec refresh_fs_xml(cb_context:context()) -> 'ok'.
refresh_fs_xml(Context) ->
    Realm = kzd_accounts:fetch_realm(cb_context:account_db(Context)),
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    refresh_fs_xml(Realm, DbDoc).

-spec refresh_fs_xml(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
refresh_fs_xml(Realm, Doc) ->
    case kzd_devices:sip_username(Doc, kz_json:get_value(<<"username">>, Doc)) of
        'undefined' -> 'ok';
        Username ->
            lager:debug("flushing fs xml for user '~s' at '~s'", [Username,Realm]),
            Req = [{<<"Username">>, Username}
                  ,{<<"Realm">>, Realm}
                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                  ],
            kz_amqp_worker:cast(Req, fun kapi_switch:publish_fs_xml_flush/1)
    end.

-spec get_devices_by_owner(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binaries().
get_devices_by_owner(_AccountDb, 'undefined') -> [];
get_devices_by_owner(AccountDb, OwnerId) ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]},
                   'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [OwnerId, _R]),
            []
    end.

-spec get_account_devices(kz_term:api_binary()) -> kz_term:ne_binaries().
get_account_devices('undefined') -> [];
get_account_devices(Account) ->
    AccountDb = kz_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, <<"devices/crossbar_listing">>, []) of
        {'ok', JObjs} -> [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find device documents owned by ~s: ~p", [AccountDb, _R]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_descendants_count(kz_term:proplist()) ->
                                    {'ok', kz_term:proplist()} |
                                    {'error', any()}.
load_descendants_count(ViewOptions) ->
    lager:debug("counting descendants with ~p", [ViewOptions]),
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants_count">>, ViewOptions) of
        {'error', _E}=Resp -> Resp;
        {'ok', []} -> {'error', 'no_descendants'};
        {'ok', JObjs} ->
            {'ok', [{kz_json:get_value(<<"key">>, JObj)
                    ,kz_json:get_value(<<"value">>, JObj)
                    }
                    || JObj <- JObjs
                   ]}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_descendants_count(kz_term:ne_binary(), integer()) -> 'ok'.
maybe_update_descendants_count(AccountId, NewCount) ->
    maybe_update_descendants_count(AccountId, NewCount, 3).

-spec maybe_update_descendants_count(kz_term:ne_binary(), integer(), integer()) -> 'ok'.
maybe_update_descendants_count(AccountId, _, Try) when Try =< 0 ->
    io:format("too many attempts to update descendants count for ~s~n", [AccountId]);
maybe_update_descendants_count(AccountId, NewCount, Try) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _E} ->
            io:format("could not load account ~s: ~p~n", [AccountId, _E]);
        {'ok', AccountJObj} ->
            maybe_update_descendants_count(AccountId, AccountJObj, NewCount, Try)
    end.

-spec maybe_update_descendants_count(kz_term:ne_binary(), kzd_accounts:doc(), integer(), integer()) -> 'ok'.
maybe_update_descendants_count(AccountId, AccountJObj, NewCount, Try) ->
    OldCount = kz_json:get_integer_value(<<"descendants_count">>, AccountJObj),
    compare_descendants_count(AccountId, NewCount, OldCount, Try).

-spec compare_descendants_count(kz_term:ne_binary(), integer(), integer(), integer()) -> 'ok'.
compare_descendants_count(_, Count, Count, _) -> 'ok';
compare_descendants_count(AccountId, NewCount, _, Try) ->
    case update_descendants_count(AccountId, NewCount) of
        'ok' -> 'ok';
        'error' ->
            maybe_update_descendants_count(AccountId, NewCount, Try-1)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_descendants_count(kz_term:ne_binary(), integer()) -> 'ok' | 'error'.
update_descendants_count(AccountId, NewCount) ->
    Update = [{[<<"descendants_count">>], NewCount}],
    case kzd_accounts:update(AccountId, Update) of
        {'error', _E} -> 'error';
        {'ok', _AccountDoc} -> lager:debug("updated descendants count to ~p for ~s", [NewCount, AccountId])
    end.
