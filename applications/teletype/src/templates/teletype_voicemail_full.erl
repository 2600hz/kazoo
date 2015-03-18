%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_voicemail_full).

-export([init/0
         ,handle_full_voicemail/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_full">>).

-define(TEMPLATE_ID, <<"voicemail_full">>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"voicemail.box">>, <<"voicemail_box">>, <<"Voicemail Box">>, <<"Which voicemail box was the message left in">>)
            ,?MACRO_VALUE(<<"voicemail.name">>, <<"voicemail_name">>, <<"Voicemail Name">>, <<"Name of the voicemail file">>)
            ,?MACRO_VALUE(<<"voicemail.number">>, <<"voicemail_number">>, <<"Voicemail Number">>, <<"Number of the voicemail box">>)
            ,?MACRO_VALUE(<<"owner.first_name">>, <<"first_name">>, <<"First Name">>, <<"First name of the owner of the voicemail box">>)
            ,?MACRO_VALUE(<<"owner.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last name of the owner of the voicemail box">>)
            ,?MACRO_VALUE(<<"voicemail.max_messages">>, <<"max_messages">>, <<"Maximum Messages">>, <<"The maximum number of messages this box can hold">>)
            ,?MACRO_VALUE(<<"voicemail.message_count">>, <<"message_count">>, <<"Message Count">>, <<"The current number of messages in the voicemail box">>)
            | ?SERVICE_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Your voicemail box '{{voicemail.name}}' is full.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>Your voicemail box '{{voicemail.name}}' is full.</h3></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Voicemail box {{voicemail.name}} is full">>).
-define(TEMPLATE_CATEGORY, <<"voicemail">>).
-define(TEMPLATE_NAME, <<"Full Voicemail Box">>).

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

-spec handle_full_voicemail(wh_json:object(), wh_proplist()) -> 'ok'.
handle_full_voicemail(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_full_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),

    AccountDb = wh_json:get_value(<<"account_db">>, DataJObj),
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    {'ok', AccountJObj} = teletype_util:open_doc(<<"account">>, AccountId, DataJObj),

    case teletype_util:should_handle_notification(DataJObj)
        andalso is_notice_enabled_on_account(AccountJObj, JObj)
    of
        'false' -> lager:debug("notification not enabled for account ~s", [wh_util:format_account_id(AccountDb, 'raw')]);
        'true' ->
            lager:debug("notification enabled for account ~s (~s)", [AccountId, AccountDb]),

            VMBox = get_vm_box(AccountDb, DataJObj),
            User = get_vm_box_owner(VMBox, DataJObj),
            ReqData =
                wh_json:set_values(
                    [{<<"voicemail">>, VMBox}
                      ,{<<"owner">>, User}
                      ,{<<"account">>, AccountJObj}
                      ,{<<"to">>, [wh_json:get_ne_value(<<"email">>, User)]}
                    ]
                    ,DataJObj
                ),

            case teletype_util:is_preview(DataJObj) of
                'false' -> process_req(ReqData);
                'true' ->
                    process_req(wh_json:merge_jobjs(DataJObj, ReqData))
            end
    end.

-spec get_vm_box(ne_binary(), wh_json:object()) -> wh_json:object().
get_vm_box(AccountDb, JObj) ->
    VMBoxId = wh_json:get_value(<<"voicemail_box">>, JObj),
    case teletype_util:open_doc(<<"voicemail">>, VMBoxId, JObj) of
        {'ok', VMBox} -> VMBox;
        {'error', _E} ->
            lager:debug("failed to load vm box ~s from ~s", [VMBoxId, AccountDb]),
            wh_json:new()
    end.

-spec get_vm_box_owner(wh_json:object(), wh_json:object()) -> wh_json:object().
get_vm_box_owner(VMBox, JObj) ->
    UserId = wh_json:get_value(<<"owner_id">>, VMBox),
    case teletype_util:open_doc(<<"user">>, UserId, JObj) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            lager:debug("failed to lookup owner, assuming none"),
            wh_json:new()
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    %% Load templates
    process_req(DataJObj, teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),

    Macros = [{<<"service">>, ServiceData}
              ,{<<"account">>, teletype_util:public_proplist(<<"account">>, DataJObj)}
              ,{<<"owner">>, teletype_util:public_proplist(<<"owner">>, DataJObj)}
              | build_template_data(DataJObj)
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_util:fetch_template_meta(?TEMPLATE_ID
                                          ,teletype_util:find_account_id(DataJObj)
                                         ),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
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

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    props:filter_undefined(
      [{<<"voicemail">>, build_voicemail_data(DataJObj)}]
     ).

-spec build_voicemail_data(wh_json:object()) -> wh_proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"box">>, wh_json:get_value(<<"voicemail_box">>, DataJObj)}
       ,{<<"number">>, wh_json:get_value(<<"voicemail_number">>, DataJObj)}
       ,{<<"max_messages">>, wh_json:get_binary_value(<<"max_message_count">>, DataJObj)}
       ,{<<"message_count">>, wh_json:get_binary_value(<<"message_count">>, DataJObj)}
       | props:delete(<<"pin">>, teletype_util:public_proplist(<<"voicemail">>, DataJObj))
      ]).

-spec is_notice_enabled_on_account(wh_json:object(), wh_json:object()) -> boolean().
is_notice_enabled_on_account(AccountJObj, ApiJObj) ->
    teletype_util:is_notice_enabled(AccountJObj, ApiJObj, <<"voicemail_full">>).
