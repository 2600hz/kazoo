%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_inbound_to_email).

-export([init/0
         ,handle_inbound_fax/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_inbound_to_email">>).

-define(TEMPLATE_ID, <<"fax_inbound_to_email">>).
-define(TEMPLATE_MACROS, wh_json:from_list([{<<"user.first_name">>
                                             ,wh_json:from_list([{<<"i18n_label">>, <<"first_name">>}
                                                                 ,{<<"friendly_name">>, <<"First Name">>}
                                                                 ,{<<"description">>, <<"First name of the owner of the voicemail box">>}
                                                                ])
                                            }
                                            ,{<<"user.last_name">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"first_name">>}
                                                                  ,{<<"friendly_name">>, <<"First Name">>}
                                                                  ,{<<"description">>, <<"First name of the owner of the voicemail box">>}
                                                                 ])
                                             }
                                            ,{<<"user.email">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"email">>}
                                                                  ,{<<"friendly_name">>, <<"Email">>}
                                                                  ,{<<"description">>, <<"Email of the user">>}
                                                                 ])
                                             }
                                           ])).
-define(TEMPLATE_TEXT, <<"New Fax ({{fax.total_pages}} Pages)\n\nCaller ID: {{fax.caller_id_number}}\nCaller Name: {{fax.caller_id_name}}\n\nCalled To: {{fax.to_user}}   (Originally dialed number)\nCalled On: {{fax.date_called|date:\"l, F j, Y \\a\\t H:i\"}}\n\n\nFor help or questions about receiving faxes, please contact support at {{service.support_number}} or email {{service.support_email}}.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>New Fax ({{fax.total_pages}} Pages)</h3><table><tr><td>Caller ID</td><td>{{fax.caller_id_name}} ({{fax.caller_id_number}})</td></tr><tr><td>Callee ID</td><td>{{fax.to_user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{fax.date_called|date:\"l, F j, Y \\a\\t H:i\"}}</td></tr></table><p>For help or questions about receiving faxes, please contact {{service.support_number}} or email <a href=\"mailto:{{service.support_email}}\">Support</a></p><p style=\"font-size: 9px;color:#C0C0C0\">{{fax.call_id}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"New fax from {{fax.caller_id_name}} ({{fax.caller_id_number}})">>).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, ?TEMPLATE_MACROS, ?TEMPLATE_TEXT, ?TEMPLATE_HTML).

-spec handle_inbound_fax(wh_json:object(), wh_proplist()) -> 'ok'.
handle_inbound_fax(JObj, _Props) ->
    'true' = wapi_notifications:skel_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(wh_api:remove_defaults(JObj)),
    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),
    Macros = [{<<"service">>, ServiceData} | build_template_data(DataJObj)],

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, wh_json:get_value(<<"Account-ID">>, JObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [JObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),

    %% Send email
    teletype_util:send_email(?TEMPLATE_ID, DataJObj, ServiceData, Subject, RenderedTemplates).

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    [{<<"user">>, build_user_data(DataJObj)}
     | wh_json:to_proplist(wh_json:normalize(DataJObj))
    ].

-spec build_user_data(wh_json:object()) -> wh_proplist().
-spec build_user_data(wh_json:object(), wh_json:object()) -> wh_proplist().
build_user_data(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    UserId = wh_json:get_value(<<"user_id">>, DataJObj),

    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} ->
            build_user_data(DataJObj, UserJObj);
        {'error', _E} ->
            lager:debug("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            [{<<"first_name">>, <<"First">>}
             ,{<<"last_name">>, <<"Last">>}
             ,{<<"email">>, <<"Email">>}
            ]
    end.

build_user_data(_DataJObj, UserJObj) ->
    UserKeys = [<<"first_name">>, <<"last_name">>, <<"email">>],
    [{Key, wh_json:get_value(Key, UserJObj)} || Key <- UserKeys].
