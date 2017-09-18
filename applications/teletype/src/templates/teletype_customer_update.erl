%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Kirill Sysoev
%%%-------------------------------------------------------------------
-module(teletype_customer_update).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"customer_update">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).
-define(ACC_USERS_LIST, <<"users/crossbar_listing">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
          ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
           | ?USER_MACROS
           ++ ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Customer update">>).
-define(TEMPLATE_CATEGORY, <<"user">>).
-define(TEMPLATE_NAME, <<"Customer update">>).
-define(THIRD_PARTY_DATA, <<"databag">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                          ,{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'friendly_name', ?TEMPLATE_NAME}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"customer_update">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:customer_update_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case teletype_util:is_notice_enabled(AccountId, JObj, maybe_expand_template_id(DataJObj)) of
        'false' -> teletype_util:notification_disabled(DataJObj, maybe_expand_template_id(DataJObj));
        'true' ->
            Result = process_req(DataJObj, teletype_util:is_preview(DataJObj)),
            case lists:partition(fun('ok') -> 'true'; (_) -> 'false' end, lists:flatten(Result)) of
                {[], []} ->
                    lager:debug("no success no failure, I'm done"),
                    teletype_util:send_update(DataJObj, <<"completed">>);
                {[], [{'error', Reason}|_]} ->
                    teletype_util:send_update(DataJObj, <<"failed">>, Reason);
                _ ->
                    teletype_util:send_update(DataJObj, <<"completed">>)
            end
    end.

-spec process_req(kz_json:object(), boolean()) -> kz_proplist().
process_req(DataJObj, 'true') ->
    [send_update_to_user(kz_json:new(), DataJObj)];
process_req(DataJObj, 'false') ->
    case kz_json:get_value(<<"recipient_id">>, DataJObj) of
        ?MATCH_ACCOUNT_RAW(RecipientId) -> process_account(RecipientId, DataJObj);
        'undefined' -> process_accounts(DataJObj)
    end.

-spec process_accounts(kz_json:object()) -> kz_proplist().
process_accounts(DataJObj) ->
    SenderId = kz_json:get_value(<<"account_id">>, DataJObj),
    ViewOpts = [{'startkey', [SenderId]}
               ,{'endkey', [SenderId, kz_json:new()]}
               ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?ACC_CHILDREN_LIST, ViewOpts) of
        {'ok', Accounts} ->
            [process_account(kz_doc:id(Account), DataJObj) || Account <- Accounts];
        {'error', Reason} ->
            Msg = io_lib:format("failed to load children. error: ~p", [Reason]),
            lager:info(Msg),
            [{'error', kz_term:to_binary(Msg)}]
    end.

-spec process_account(ne_binary(), kz_json:object()) -> kz_proplist().
process_account(AccountId, DataJObj) ->
    case kz_json:get_value(<<"user_type">>, DataJObj) of
        ?MATCH_ACCOUNT_RAW(UserId) ->
            {'ok', UserJObj} = kzd_user:fetch(AccountId, UserId),
            [send_update_to_user(UserJObj, DataJObj)];
        _ ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            {'ok', Users} = kz_datamgr:get_results(AccountDb, ?ACC_USERS_LIST, []),
            select_users_to_update([kz_json:get_value(<<"value">>, User) || User <- Users], DataJObj)
    end.

-spec select_users_to_update(kz_proplist(), kz_json:object()) -> kz_proplist().
select_users_to_update(Users, DataJObj) ->
    case kz_json:get_value(<<"user_type">>, DataJObj) of
        <<"all_users">> ->
            [send_update_to_user(User, DataJObj) || User <- Users];
        _ ->
            [send_update_to_user(User, DataJObj) || User <- Users, kzd_user:is_account_admin(User)]
    end.

-spec send_update_to_user(kz_json:object(), kz_json:object()) -> 'ok' | {'error', ne_binary()}.
send_update_to_user(UserJObj, DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ]
        ++ build_macro_data(UserJObj, DataJObj)
        ++ [{?THIRD_PARTY_DATA, kz_json:get_value(?THIRD_PARTY_DATA, DataJObj, kz_json:new())}],

    RenderedTemplates =
        teletype_templates:render(maybe_expand_template_id(DataJObj), Macros, DataJObj, maybe_tpls_provided(DataJObj)),
    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(maybe_expand_template_id(DataJObj), kapi_notifications:account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
               ),
    Emails = maybe_replace_to_field(
               teletype_util:find_addresses(DataJObj, TemplateMetaJObj, maybe_expand_mod_config_cat(DataJObj))
                                   ,kz_json:get_value(<<"email">>, UserJObj)
              ),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> 'ok';
        {'error', Reason} -> {error, kz_term:to_binary(Reason)}
    end.

-spec maybe_replace_to_field(email_map(), api_binary()) -> email_map().
maybe_replace_to_field(Emails, 'undefined') -> Emails;
maybe_replace_to_field(Emails, To) -> props:set_value(<<"to">>, [To], Emails).

-spec build_macro_data(kz_json:object(), kz_json:object()) -> kz_proplist().
build_macro_data(UserJObj, DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            kz_json:foldl(fun(MacroKey, _V, Acc) ->
                                  maybe_add_macro_key(MacroKey, Acc, UserJObj)
                          end
                         ,[]
                         ,?TEMPLATE_MACROS
                         )
    end.

-spec maybe_add_macro_key(kz_json:path(), kz_proplist(), kz_json:object()) -> kz_proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, UserJObj) ->
    maybe_add_user_data(UserKey, Acc, UserJObj);
maybe_add_macro_key(_Key, Acc, _UserJObj) ->
    lager:debug("unprocessed macro key ~s: ~p", [_Key, _UserJObj]),
    Acc.

-spec maybe_add_user_data(kz_json:path(), kz_proplist(), kz_json:object()) -> kz_proplist().
maybe_add_user_data(Key, Acc, UserJObj) ->
    UserMacros = props:get_value(<<"user">>, Acc, []),
    case kz_json:get_value(Key, UserJObj) of
        'undefined' ->
            lager:debug("unprocessed user macro key ~s: ~p", [Key, UserJObj]),
            Acc;
        V -> props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.

-spec maybe_expand_template_id(kz_json:object()) -> ne_binary().
maybe_expand_template_id(DataJObj) ->
    case kz_json:get_value(<<"template_id">>, DataJObj) of
        <<"customer_update_", _/binary>> = TemplateId ->
            TemplateId;
        _ ->
            ?TEMPLATE_ID
    end.

-spec maybe_expand_mod_config_cat(kz_json:object()) -> ne_binary().
maybe_expand_mod_config_cat(DataJObj) ->
    case kz_json:get_value(<<"template_id">>, DataJObj) of
        <<"customer_update_", _/binary>> = TemplateId ->
            <<(?NOTIFY_CONFIG_CAT)/binary, ".", TemplateId/binary>>;
        _ ->
            ?MOD_CONFIG_CAT
    end.

-spec maybe_tpls_provided(kz_json:object()) -> boolean().
maybe_tpls_provided(DataJObj) ->
    case kz_json:get_first_defined([<<"html">>, <<"text">>], DataJObj) of
        'undefined' -> false;
        _ -> 'true'
    end.
