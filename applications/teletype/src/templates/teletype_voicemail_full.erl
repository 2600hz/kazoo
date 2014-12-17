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
        ,wh_json:from_list([{<<"voicemail.box">>
                             ,wh_json:from_list([{<<"i18n_label">>, <<"voicemail_box">>}
                                                 ,{<<"friendly_name">>, <<"Voicemail Box">>}
                                                 ,{<<"description">>, <<"Which voicemail box was the message left in">>}
                                                ])
                            }
                            ,{<<"voicemail.name">>
                              ,wh_json:from_list([{<<"i18n_label">>, <<"voicemail_name">>}
                                                  ,{<<"friendly_name">>, <<"Voicemail Name">>}
                                                  ,{<<"description">>, <<"Name of the voicemail file">>}
                                                 ])
                             }
                            ,{<<"voicemail.number">>
                              ,wh_json:from_list([{<<"i18n_label">>, <<"voicemail_number">>}
                                                  ,{<<"friendly_name">>, <<"Voicemail Number">>}
                                                  ,{<<"description">>, <<"Number of the voicemail box">>}
                                                 ])
                             }
                            ,{<<"owner.first_name">>
                              ,wh_json:from_list([{<<"i18n_label">>, <<"first_name">>}
                                                  ,{<<"friendly_name">>, <<"First Name">>}
                                                  ,{<<"description">>, <<"First name of the owner of the voicemail box">>}
                                                 ])
                             }
                            ,{<<"owner.last_name">>
                              ,wh_json:from_list([{<<"i18n_label">>, <<"last_name">>}
                                                  ,{<<"friendly_name">>, <<"Last Name">>}
                                                  ,{<<"description">>, <<"Last name of the owner of the voicemail box">>}
                                                 ])
                             }
                            ,{<<"voicemail.max_messages">>
                              ,wh_json:from_list([{<<"i18n_label">>, <<"max_messages">>}
                                                  ,{<<"friendly_name">>, <<"Maximum Messages">>}
                                                  ,{<<"description">>, <<"The maximum number of messages this box can hold">>}
                                                 ])
                             }
                            ,{<<"voicemail.message_count">>
                              ,wh_json:from_list([{<<"i18n_label">>, <<"message_count">>}
                                                  ,{<<"friendly_name">>, <<"Message Count">>}
                                                  ,{<<"description">>, <<"The current number of messages in the voicemail box">>}
                                                 ])
                             }
                           ])
       ).

-define(TEMPLATE_TEXT, <<"Your voicemail box '{{voicemail.name}}' is full.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>Your voicemail box '{{voicemail.name}}' is full.</h3></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Voicemail box {{voicemail.name}} is full">>).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, ?TEMPLATE_MACROS, ?TEMPLATE_TEXT, ?TEMPLATE_HTML).

-spec handle_full_voicemail(wh_json:object(), wh_proplist()) -> 'ok'.
handle_full_voicemail(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_full_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),

    AccountDb = wh_json:get_value(<<"account_db">>, DataJObj),
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),

    {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),

    case is_notice_enabled_on_account(AccountJObj, JObj) of
        'false' -> lager:debug("notification not enabled for account ~s", [wh_util:format_account_id(AccountDb, 'raw')]);
        'true' ->
            lager:debug("notification enabled for account ~s (~s)", [AccountId, AccountDb]),

            VMBox = get_vm_box(AccountDb, DataJObj),
            User = get_vm_box_owner(AccountDb, VMBox),

            process_req(
              wh_json:set_values([{<<"voicemail">>, VMBox}
                                  ,{<<"owner">>, User}
                                  ,{<<"account">>, AccountJObj}
                                 ]
                                 ,DataJObj
                                )
             )
    end.

-spec get_vm_box(ne_binary(), wh_json:object()) -> wh_json:object().
get_vm_box(AccountDb, JObj) ->
    VMBoxId = wh_json:get_value(<<"voicemail_box">>, JObj),
    case couch_mgr:open_cache_doc(AccountDb, VMBoxId) of
        {'ok', VMBox} -> VMBox;
        {'error', _E} ->
            lager:debug("failed to load vm box ~s from ~s", [VMBoxId, AccountDb]),
            wh_json:new()
    end.

-spec get_vm_box_owner(ne_binary(), wh_json:object()) -> wh_json:object().
get_vm_box_owner(AccountDb, VMBox) ->
    case couch_mgr:open_cache_doc(AccountDb, wh_json:get_value(<<"owner_id">>, VMBox)) of
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
              ,{<<"account">>, public_proplist(<<"account">>, DataJObj)}
              ,{<<"owner">>, public_proplist(<<"owner">>, DataJObj)}
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

    %% Send email
    case teletype_util:send_email(?TEMPLATE_ID
                                  ,wh_json:set_value([<<"to">>, <<"email_addresses">>], to_email_addresses(DataJObj), DataJObj)
                                  ,ServiceData
                                  ,Subject
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

-spec to_email_addresses(wh_json:object()) -> api_binaries().
-spec to_email_addresses(wh_json:object(), api_binaries() | ne_binary()) -> api_binaries().
to_email_addresses(DataJObj) ->
    to_email_addresses(DataJObj
                       ,wh_json:get_first_defined([[<<"to">>, <<"email_addresses">>]
                                                   ,[<<"owner">>, <<"email">>]
                                                   ,[<<"owner">>, <<"username">>]
                                                  ]
                                                  ,DataJObj
                                                 )
                      ).

to_email_addresses(_DataJObj, <<_/binary>> = Email) ->
    [Email];
to_email_addresses(_DataJObj, [_|_] = Emails) ->
    Emails;
to_email_addresses(DataJObj, _) ->
    case teletype_util:find_account_rep_email(wh_json:get_value(<<"account">>, DataJObj)) of
        'undefined' ->
            lager:debug("failed to find account rep email, using defaults"),
            default_to_addresses();
        Emails ->
            lager:debug("using ~p for To", [Emails]),
            Emails
    end.

-spec default_to_addresses() -> api_binaries().
default_to_addresses() ->
    case whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>) of
        'undefined' -> 'undefined';
        <<_/binary>> = Email -> [Email];
        [_|_]=Emails -> Emails
    end.

-spec build_voicemail_data(wh_json:object()) -> wh_proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"box">>, wh_json:get_value(<<"voicemail_box">>, DataJObj)}
       ,{<<"number">>, wh_json:get_value(<<"voicemail_number">>, DataJObj)}
       ,{<<"max_messages">>, wh_json:get_binary_value(<<"max_message_count">>, DataJObj)}
       ,{<<"message_count">>, wh_json:get_binary_value(<<"message_count">>, DataJObj)}
       | props:delete(<<"pin">>, public_proplist(<<"voicemail">>, DataJObj))
      ]).

-spec is_notice_enabled_on_account(wh_json:object(), wh_json:object()) -> boolean().
is_notice_enabled_on_account(AccountJObj, ApiJObj) ->
    teletype_util:is_notice_enabled(AccountJObj, ApiJObj, <<"voicemail_full">>).

-spec public_proplist(wh_json:key(), wh_json:object()) -> wh_proplist().
public_proplist(Key, JObj) ->
    wh_json:to_proplist(
      wh_json:public_fields(
        wh_json:get_value(Key, JObj, wh_json:new())
       )
     ).
