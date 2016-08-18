%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
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

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"voicemail_full">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"voicemail.box">>, <<"voicemail_box">>, <<"Voicemail Box">>, <<"Which voicemail box was the message left in">>)
          ,?MACRO_VALUE(<<"voicemail.name">>, <<"voicemail_name">>, <<"Voicemail Name">>, <<"Name of the voicemail file">>)
          ,?MACRO_VALUE(<<"voicemail.number">>, <<"voicemail_number">>, <<"Voicemail Number">>, <<"Number of the voicemail box">>)
          ,?MACRO_VALUE(<<"owner.first_name">>, <<"first_name">>, <<"First Name">>, <<"First name of the owner of the voicemail box">>)
          ,?MACRO_VALUE(<<"owner.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last name of the owner of the voicemail box">>)
          ,?MACRO_VALUE(<<"voicemail.max_messages">>, <<"max_messages">>, <<"Maximum Messages">>, <<"The maximum number of messages this box can hold">>)
          ,?MACRO_VALUE(<<"voicemail.message_count">>, <<"message_count">>, <<"Message Count">>, <<"The current number of messages in the voicemail box">>)
          ])
       ).

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
                                          ]).

-spec handle_full_voicemail(kz_json:object(), kz_proplist()) -> 'ok'.
handle_full_voicemail(JObj, _Props) ->
    'true' = kapi_notifications:voicemail_full_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID)
        orelse teletype_util:stop_processing("template ~s not enabled for account ~s", [?TEMPLATE_ID, AccountId]),

    VMBox = get_vm_box(AccountId, DataJObj),
    User = get_vm_box_owner(VMBox, DataJObj),

    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails, kzd_user:email(User)),

    ReqData =
        kz_json:set_values(
          [{<<"voicemail">>, VMBox}
          ,{<<"owner">>, User}
          ,{<<"to">>, Emails}
          ]
                          ,DataJObj
         ),
    process_req(kz_json:merge_jobjs(DataJObj, ReqData)).

-spec maybe_add_user_email(ne_binaries(), api_binary()) -> ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined') -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail) -> [UserEmail | BoxEmails].

-spec get_vm_box(ne_binary(), kz_json:object()) -> kz_json:object().
get_vm_box(AccountId, JObj) ->
    VMBoxId = kz_json:get_value(<<"voicemail_box">>, JObj),
    case teletype_util:open_doc(<<"voicemail">>, VMBoxId, JObj) of
        {'ok', VMBox} -> VMBox;
        {'error', _E} ->
            lager:debug("failed to load vm box ~s from ~s", [VMBoxId, AccountId]),
            kz_json:new()
    end.

-spec get_vm_box_owner(kz_json:object(), kz_json:object()) -> kz_json:object().
get_vm_box_owner(VMBox, JObj) ->
    UserId = kz_json:get_value(<<"owner_id">>, VMBox),
    case teletype_util:open_doc(<<"user">>, UserId, JObj) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            lager:debug("failed to lookup owner, assuming none"),
            kz_json:new()
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"owner">>, teletype_util:public_proplist(<<"owner">>, DataJObj)}
              | build_template_data(DataJObj)
             ],

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_template_data(kz_json:object()) -> kz_proplist().
build_template_data(DataJObj) ->
    props:filter_undefined(
      [{<<"voicemail">>, build_voicemail_data(DataJObj)}]
     ).

-spec build_voicemail_data(kz_json:object()) -> kz_proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"box">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)}
      ,{<<"number">>, kz_json:get_value(<<"voicemail_number">>, DataJObj)}
      ,{<<"max_messages">>, kz_json:get_binary_value(<<"max_message_count">>, DataJObj)}
      ,{<<"message_count">>, kz_json:get_binary_value(<<"message_count">>, DataJObj)}
       | props:delete(<<"pin">>, teletype_util:public_proplist(<<"voicemail">>, DataJObj))
      ]).
