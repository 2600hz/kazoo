%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_voicemail_full).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"voicemail_full">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"voicemail.id">>, <<"id">>, <<"Voicemail Box Id">>, <<"Which voicemail box was the message left in">>)
          ,?MACRO_VALUE(<<"voicemail.name">>, <<"name">>, <<"Voicemail Box Name">>, <<"Name of the voicemail box">>)
          ,?MACRO_VALUE(<<"voicemail.mailbox">>, <<"mailbox">>, <<"Voicemail Box Number">>, <<"Number of the voicemail box">>)
          ,?MACRO_VALUE(<<"voicemail.max_messages">>, <<"max_messages">>, <<"Maximum Messages">>, <<"The maximum number of messages this box can hold">>)
          ,?MACRO_VALUE(<<"voicemail.message_count">>, <<"message_count">>, <<"Message Count">>, <<"The current number of messages in the voicemail box">>)
           | ?USER_MACROS
           ++ ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Voicemail box '{{voicemail.name}}' is full">>).
-define(TEMPLATE_CATEGORY, <<"voicemail">>).
-define(TEMPLATE_NAME, <<"Full Voicemail Box">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
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
    teletype_bindings:bind(<<"voicemail_full">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:voicemail_full_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(DataJObj, AccountId)
    end.

-spec process_req(kz_json:object(), kz_term:ne_binary()) -> template_response().
process_req(DataJObj, AccountId) ->
    VMBox = get_vm_box(AccountId, DataJObj),
    User = get_vm_box_owner(VMBox, DataJObj),

    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails, kzd_users:email(User)),

    ReqData =
        kz_json:set_values(
          [{<<"vmbox">>, VMBox}
          ,{<<"user">>, User}
          ,{<<"to">>, Emails}
          ], DataJObj
         ),
    process_req(kz_json:merge_jobjs(DataJObj, ReqData)).

-spec maybe_add_user_email(kz_term:ne_binaries(), kz_term:api_binary()) -> kz_term:ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined') -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail) -> [UserEmail | BoxEmails].

-spec get_vm_box(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
get_vm_box(AccountId, JObj) ->
    VMBoxId = kz_json:get_value(<<"voicemail_box">>, JObj),
    case teletype_util:open_doc(<<"vmbox">>, VMBoxId, JObj) of
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

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"user">>, teletype_util:user_params(kz_json:get_value(<<"user">>, DataJObj))}
              | build_template_data(DataJObj)
             ],

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
                                          ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

-spec build_template_data(kz_json:object()) -> kz_term:proplist().
build_template_data(DataJObj) ->
    [{<<"voicemail">>, build_voicemail_data(DataJObj)}
    ,{<<"owner">>, teletype_util:user_params(kz_json:get_value(<<"user">>, DataJObj))} %% backward compatibility
    ].

-spec build_voicemail_data(kz_json:object()) -> kz_term:proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"id">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)}
      ,{<<"box">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)} %% backward compatibility
      ,{<<"number">>, kz_json:get_binary_value([<<"vmbox">>, <<"mailbox">>], DataJObj)} %% backward compatibility
      ,{<<"max_messages">>, kz_json:get_binary_value(<<"max_message_count">>, DataJObj)}
      ,{<<"message_count">>, kz_json:get_binary_value(<<"message_count">>, DataJObj)}
       | props:delete(<<"pin">>, teletype_util:public_proplist(<<"vmbox">>, DataJObj))
      ]).
