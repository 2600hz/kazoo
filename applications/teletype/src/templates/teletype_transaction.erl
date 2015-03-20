%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_transaction).

-export([init/0
         ,handle_transaction/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"transaction">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?NOTIFY_CONFIG_CAT)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
            ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
            | ?SERVICE_MACROS ++ ?ACCOUNT_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"KAZOO: transaction notice for {{account.name}} - ${{transaction.amount}} (ID #{{account.pvt_account_id}})\n\n{% if transaction %}Transaction\n{% for key, value in transaction %}{{ key }}: {{ value }}\n{% endfor %}\n{% endif %}{% if plan %}Service Plan\nID: {{plan.id}}\nCategory: {{plan.category}}\nItem: {{plan.item}}\nActivation-Charge: {{plan.activation_charge}}\n\n{% endif %}Account\nAccount ID: {{account.pvt_account_id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\nService\nURL: {{service.url}}\nName: {{service.name}}\nService Provider: {{service.provider}}\n\nSent from {{service.host}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h1>KAZOO: transaction notice for {{account.name}} - ${{transaction.amount}} (ID #{{account.pvt_account_id}})</h1><br/>{% if transaction %}<h2>Transaction</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in transaction %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% endif %}{% if plan %}<h2>Service Plan</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>ID: </td><td>{{plan.id}}</td></tr><tr><td>Category: </td><td>{{plan.category}}</td></tr><tr><td>Item: </td><td>{{plan.item}}</td></tr><tr><td>Activation-Charge: </td><td>{{plan.activation_charge}}</td></tr></table>{% endif %}<h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.pvt_account_id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table><h2>Service</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>URL: </td><td>{{service.url}}</td></tr><tr><td>Name: </td><td>{{service.name}}</td></tr><tr><td>Service Provider: </td><td>{{service.provider}}</td></tr></table><p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{service.host}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"KAZOO: transaction notice (account ID #{{account.pvt_account_id}})">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Transaction">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),

    teletype_util:init_template(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                               ,{'text', ?TEMPLATE_TEXT}
                                               ,{'html', ?TEMPLATE_HTML}
                                               ,{'subject', ?TEMPLATE_SUBJECT}
                                               ,{'category', ?TEMPLATE_CATEGORY}
                                               ,{'friendly_name', ?TEMPLATE_NAME}
                                               ,{'to', ?TEMPLATE_TO}
                                               ,{'from', ?TEMPLATE_FROM}
                                               ,{'cc', ?TEMPLATE_CC}
                                               ,{'bcc', ?TEMPLATE_BCC}
                                               ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                              ]).

-spec handle_transaction(wh_json:object(), wh_proplist()) -> 'ok'.
handle_transaction(JObj, _Props) ->
    'true' = wapi_notifications:transaction_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),

    case teletype_util:should_handle_notification(DataJObj) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(add_account(DataJObj))
    end.

-spec add_account(wh_json:object()) -> wh_json:object().
add_account(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),
    wh_json:set_value(<<"account">>, AccountJObj, DataJObj).

-spec handle_req(wh_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),

    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),
    Macros = [{<<"service">>, ServiceData}
             ,{<<"account">>, teletype_util:public_proplist(<<"account">>, DataJObj)}
             ,{<<"plan">>, notify_util:json_to_template_props(wh_json:get_value(<<"Service-Plan">>, DataJObj))}
             ,{<<"transaction">>, notify_util:json_to_template_props(wh_json:get_value(<<"Transaction">>, DataJObj))}
              | build_macro_data(DataJObj)],

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    %% Send email
    case teletype_util:send_email(Emails
                                  ,Subject
                                  ,ServiceData
                                  ,RenderedTemplates
                                 )
    of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_macro_data(wh_json:object()) -> wh_proplist().
build_macro_data(DataJObj) ->
    wh_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, DataJObj)
                  end
                  ,[]
                  ,?TEMPLATE_MACROS
                 ).

-spec maybe_add_macro_key(wh_json:key(), wh_proplist(), wh_json:object()) -> wh_proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, DataJObj) ->
    maybe_add_user_data(UserKey, Acc, DataJObj);
maybe_add_macro_key(_Key, Acc, _DataJObj) ->
    lager:debug("unprocessed macro key ~s: ~p", [_Key, _DataJObj]),
    Acc.

-spec maybe_add_user_data(wh_json:key(), wh_proplist(), wh_json:object()) -> wh_proplist().
maybe_add_user_data(Key, Acc, DataJObj) ->
    User = get_user(DataJObj),

    UserMacros = props:get_value(<<"user">>, Acc, []),

    case wh_json:get_value(Key, User) of
        'undefined' ->
            lager:debug("unprocessed user macro key ~s: ~p", [Key, User]),
            Acc;
        V -> props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.

-spec get_user(wh_json:object()) -> wh_json:object().
get_user(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    UserId = wh_json:get_value(<<"user_id">>, DataJObj),

    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            lager:debug("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            case wh_json:is_true(<<"preview">>, DataJObj) of
                'false' -> throw({'error', 'not_found'});
                'true' -> wh_json:new()
            end
    end.
