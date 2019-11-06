%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_port_utils).

-export([port_request_data/2
        ]).

-include("teletype.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-spec port_request_data(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
port_request_data(DataJObj, TemplateId) ->
    PortReqId = kz_json:get_first_defined([<<"port_request_id">>
                                          ,[<<"port">>, <<"port_id">>]
                                          ]
                                         ,DataJObj
                                         ),
    case teletype_util:open_doc(<<"port_request">>, PortReqId, DataJObj) of
        {'ok', PortReqJObj} ->
            port_request_data(DataJObj, TemplateId, PortReqJObj);
        {'error', _Reason} ->
            lager:debug("failed to open port request ~s doc, throwing no_attachment here, don't panic: ~p"
                       ,[PortReqId, _Reason]
                       ),
            throw({'error', 'no_attachment'})
    end.

-spec port_request_data(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
port_request_data(DataJObj, TemplateId, PortReqJObj) ->
    Routines = [fun fix_port_authority/3
               ,fun fix_numbers/3
               ,fun fix_billing/3
               ,fun fix_reference_number/3
               ,fun fix_port_state/3
               ,fun fix_comments/3
               ,fun fix_dates/3
               ,fun fix_notifications/3
               ,fun fix_carrier/3
               ,fun fix_transfer_date/3
               ,fun fix_scheduled_date/3
               ,fun fix_ui_metadata/3
               ,fun maybe_add_reason/3
               ],
    JObj = lists:foldl(fun(F, AccJObj) -> F(DataJObj, TemplateId, AccJObj) end
                      ,PortReqJObj
                      ,Routines
                      ),
    NewData = kz_json:set_value(<<"port_request">>, JObj, DataJObj),
    maybe_add_attachments(maybe_fix_emails(NewData, TemplateId), TemplateId).

-spec fix_port_authority(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_port_authority(_DataJObj, _TemplateId, PortReqJObj) ->
    {'ok', MasterId} = kapps_util:get_master_account_id(),
    case kzd_port_requests:find_port_authority(PortReqJObj) of
        'undefined' ->
            lager:debug("port authority is undefined, using master as port authority"),
            kz_json:set_value(<<"port_authority">>
                             ,kz_json:from_list(teletype_util:find_account_params(MasterId))
                             ,PortReqJObj
                             );
        PortAuthority ->
            kz_json:set_value(<<"port_authority">>
                             ,kz_json:from_list(teletype_util:find_account_params(PortAuthority))
                             ,PortReqJObj
                             )
    end.

-spec fix_numbers(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_numbers(_DataJObj, _TemplateId, PortReqJObj) ->
    Numbers = kz_json:foldl(fun(Number, _Value, Acc) -> [Number|Acc] end
                           ,[]
                           ,get_numbers(PortReqJObj)
                           ),
    kzd_port_requests:set_numbers(PortReqJObj, Numbers).

-spec get_numbers(kz_json:object()) -> kz_json:object().
get_numbers(PortReqJObj) ->
    case kzd_port_requests:pvt_port_state(PortReqJObj) of
        ?PORT_COMPLETED ->
            %% in case the doc is not saved/replicated yet, use numbers key
            Default = kzd_port_requests:numbers(PortReqJObj, kz_json:new()),
            kzd_port_requests:pvt_ported_numbers(PortReqJObj, Default);
        _ ->
            kzd_port_requests:numbers(PortReqJObj, kz_json:new())
    end.

-spec fix_billing(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_billing(_DataJObj, _TemplateId, PortReqJObj) ->
    kz_json:foldl(fun (Key, Value, Acc) -> kz_json:set_value(<<"bill_", Key/binary>>, Value, Acc) end
                 ,fix_bill_object(PortReqJObj)
                 ,kzd_port_requests:bill(PortReqJObj, kz_json:new())
                 ).

-spec fix_bill_object(kz_json:object()) -> kz_json:object().
fix_bill_object(PortReqJObj) ->
    KeyMap = [{<<"account">>, [{<<"account_number">>, <<"number">>}
                              ,<<"pin">>
                              ,<<"btn">>
                              ]
              }
             ,{<<"address">>, [<<"name">>
                              ,<<"street_pre_dir">>
                              ,<<"street_number">>
                              ,{<<"street_address">>, <<"street_name">>}
                              ,<<"street_type">>
                              ,<<"street_post_dir">>
                              ,<<"extended_address">>
                              ,<<"locality">>
                              ,<<"region">>
                              ,<<"postal_code">>
                              ]
              }
             ],
    fix_bill_object(PortReqJObj, KeyMap).

-spec fix_bill_object(kz_json:object(), [{kz_term:ne_binary(), [kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:ne_binary()}]}]) ->
                             kz_json:object().
fix_bill_object(PortReqJObj, []) -> PortReqJObj;
fix_bill_object(PortReqJObj, [{Category, KeyMaps} | Rest]) ->
    NewPort = lists:foldl(fun(KeyMap, Acc) -> fix_bill_object(Acc, Category, KeyMap) end
                         ,PortReqJObj
                         ,KeyMaps
                         ),
    fix_bill_object(NewPort, Rest).

-spec fix_bill_object(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:ne_binary()}) ->
                             kz_json:object().
fix_bill_object(PortReqJObj, Category, {OldKeyName, NewKeyName}) ->
    kz_json:set_value([<<"bill">>, Category, NewKeyName]
                     ,kz_json:get_ne_binary_value([<<"bill">>, OldKeyName], PortReqJObj, <<"-">>)
                     ,kz_json:delete_key([<<"bill">>, OldKeyName], PortReqJObj)
                     );
fix_bill_object(PortReqJObj, Category, KeyName) ->
    fix_bill_object(PortReqJObj, Category, {KeyName, KeyName}).

-spec fix_reference_number(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_reference_number(_DataJObj, _TemplateId, PortReqJObj) ->
    kzd_port_requests:set_reference_number(PortReqJObj
                                          ,kzd_port_requests:reference_number(PortReqJObj, <<"-">>)
                                          ).

-spec fix_port_state(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_port_state(_DataJObj, _TemplateId, PortReqJObj) ->
    kz_json:set_value(<<"port_state">>
                     ,kzd_port_requests:pvt_port_state(PortReqJObj)
                     ,PortReqJObj
                     ).

-spec fix_comments(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_comments(DataJObj, _TemplateId, PortReqJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    case get_the_comment(DataJObj, PortReqJObj, IsPreview) of
        'undefined' -> kz_json:delete_key(<<"comments">>, PortReqJObj);
        Comment ->
            Timestamp = kzd_comment:timestamp(Comment),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, DataJObj)),
            Props = [{<<"date">>, Date}
                    ,{<<"timestamp">>, kz_json:get_value(<<"local">>, Date)} %% backward compatibility
                     | get_commenter_info(DataJObj)
                    ],
            kz_json:set_value(<<"comment">>
                             ,kz_json:set_values(Props, Comment)
                             ,kz_json:delete_key(<<"comments">>, PortReqJObj)
                             )
    end.

-spec get_the_comment(kz_json:object(), kz_json:object(), boolean()) -> kz_term:api_object().
get_the_comment(_, PortReqJObj, 'true') ->
    hd(kzd_port_requests:comments(PortReqJObj));
get_the_comment(DataJObj, _, 'false') ->
    kz_json:get_json_value(<<"comment">>, DataJObj).

-spec fix_dates(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_dates(DataJObj, _TemplateId, PortReqJObj) ->
    IsPreview= teletype_util:is_preview(DataJObj),
    lists:foldl(fun(Key, Value) -> fix_date_fold(Key, Value, IsPreview) end
               ,kz_json:set_value(<<"created">>, kz_doc:created(PortReqJObj), PortReqJObj)
               ,[<<"transfer_date">>, <<"scheduled_date">>
                ,<<"created">>, <<"ported_date">>
                ,<<"signing_date">>
                ]
               ).

-spec fix_date_fold(kz_json:path(), kz_json:object(), boolean()) -> kz_json:object().
fix_date_fold(Key, JObj, 'true') ->
    Date = kz_json:from_list(teletype_util:fix_timestamp(kz_time:now_s(), JObj)),
    kz_json:set_value(Key, Date, JObj);
fix_date_fold(<<"ported_date">> = Key, JObj, 'false') ->
    case kzd_port_requests:get_transition(JObj, ?PORT_COMPLETED) of
        [] -> JObj;
        [Completed|_] ->
            Timestamp = kz_json:get_integer_value([<<"transition">>, <<"timestamp">>], Completed),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, JObj)),
            kz_json:set_value(Key, Date, JObj)
    end;
fix_date_fold(Key, JObj, 'false') ->
    case kz_json:get_integer_value(Key, JObj) of
        'undefined' -> JObj;
        Timestamp ->
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, JObj)),
            kz_json:set_value(Key, Date, JObj)
    end.

-spec fix_notifications(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_notifications(_DataJObj, _TemplateId, PortReqJObj) ->
    case kzd_port_requests:notifications_email_send_to(PortReqJObj) of
        <<_/binary>> =Email -> kz_json:set_value(<<"customer_contact">>, [Email], PortReqJObj);
        [_|_]=Emails -> kz_json:set_value(<<"customer_contact">>, Emails, PortReqJObj);
        _ -> PortReqJObj
    end.

-spec fix_carrier(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_carrier(_DataJObj, _TemplateId, PortReqJObj) ->
    LosingKeys = [<<"carrier">>
                 ,[<<"billing">>, <<"carrier">>]
                 ,<<"bill_carrier">>
                 ],
    kz_json:set_values([{<<"losing_carrier">>
                        ,kz_json:get_first_defined(LosingKeys, PortReqJObj, <<"unknown carrier">>)
                        }
                       ,{<<"winning_carrier">>
                        ,kz_json:get_value(<<"winning_carrier">>, PortReqJObj, <<"unknown carrier">>)
                        }
                       ]
                      ,PortReqJObj %% not deleting the key for backward compatibility
                      ).

-spec fix_transfer_date(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_transfer_date(_DataJObj, _TemplateId, PortReqJObj) ->
    kz_json:set_values([{<<"requested_port_date">>, kz_json:get_value(<<"transfer_date">>, PortReqJObj)}
                       ,{<<"transfer_date">>, kz_json:get_value([<<"transfer_date">>, <<"local">>], PortReqJObj)} %% backward compatibility
                       ], PortReqJObj).

-spec fix_scheduled_date(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_scheduled_date(_DataJObj, _TemplateId, PortReqJObj) ->
    kz_json:set_values([{<<"port_scheduled_date">>, kz_json:get_value(<<"scheduled_date">>, PortReqJObj)}
                       ,{<<"scheduled_date">>, kz_json:get_value([<<"scheduled_date">>, <<"local">>], PortReqJObj)}
                       ], PortReqJObj).

-spec fix_ui_metadata(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_ui_metadata(_DataJObj, _TemplateId, PortReqJObj) ->
    kz_json:delete_keys([<<"ui_metadata">>, <<"ui_flags">>], PortReqJObj).

-spec maybe_add_reason(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
maybe_add_reason(DataJObj, _TemplateId, PortReqJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    case get_the_reason(DataJObj, PortReqJObj, IsPreview) of
        'undefined' -> PortReqJObj;
        Reason ->
            UserInfo = get_commenter_info(DataJObj),
            Timestamp = kzd_comment:timestamp(Reason),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, DataJObj)),
            Props = [{<<"content">>, kzd_comment:content(Reason)}
                    ,{<<"date">>, Date}
                    ,{<<"user">>, kz_json:from_list(UserInfo)}
                    ],
            kz_json:set_value(<<"transition_reason">>, kz_json:from_list(Props), PortReqJObj)
    end.

-spec get_the_reason(kz_json:object(), kz_json:object(), boolean()) -> kz_term:api_object().
get_the_reason(_, PortReqJObj, 'true') ->
    kz_json:get_json_value(<<"reason">>, PortReqJObj);
get_the_reason(DataJObj, _, 'false') ->
    kz_json:get_json_value(<<"reason">>, DataJObj).


-spec get_commenter_info(kz_json:object()) -> kz_term:proplist().
get_commenter_info(DataJObj) ->
    maybe_add_user_data(DataJObj
                       ,kz_json:get_first_defined([[<<"comment">>, <<"author">>]
                                                  ,[<<"reason">>, <<"author">>]
                                                  ], DataJObj)
                       ).

-spec maybe_add_user_data(kz_json:object(), kz_term:api_binary()) -> kz_term:proplist().
maybe_add_user_data(DataJObj, <<>>) ->
    maybe_add_user_data(DataJObj, 'undefined');
maybe_add_user_data(DataJObj, Author) ->
    maybe_add_user_data(DataJObj, Author, teletype_util:is_preview(DataJObj)).

-spec maybe_add_user_data(kz_json:object(), kz_term:api_ne_binary(), boolean()) -> kz_term:proplist().
maybe_add_user_data(DataJObj, Author, 'true') ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    case teletype_util:find_account_admin(AccountId) of
        'undefined' when Author =:= 'undefined' ->
            [{<<"author">>, <<"An agent">>}];
        'undefined' ->
            [{<<"author">>, Author}];
        UserDoc ->
            [{<<"author">>, kzd_users:full_name(UserDoc, <<"An agent">>)}
             | teletype_util:user_params(UserDoc)
            ]
    end;
maybe_add_user_data(DataJObj, Author, 'false') ->
    AccountId = kz_json:get_first_defined([[<<"comment">>, <<"account_id">>]
                                          ,[<<"reason">>, <<"account_id">>]
                                          ]
                                         ,DataJObj
                                         ),
    UserId = kz_json:get_first_defined([[<<"comment">>, <<"user_id">>]
                                       ,[<<"reason">>, <<"user_id">>]
                                       ]
                                      ,DataJObj
                                      ),
    case kzd_users:fetch(AccountId, UserId) of
        {'error', _} when Author =:= 'undefined' ->
            [{<<"author">>, <<"An agent">>}];
        {'error', _} ->
            [{<<"author">>, Author}];
        {'ok', UserDoc} ->
            [{<<"author">>, kzd_users:full_name(UserDoc, <<"An agent">>)}
             | teletype_util:user_params(UserDoc)
            ]
    end.

-spec is_attachable_template(kz_term:ne_binary()) -> boolean().
is_attachable_template(TemplateId) ->
    lists:member(TemplateId
                ,[teletype_port_request_admin:id()
                 ,teletype_port_request:id()
                 ]
                ).

-spec maybe_add_attachments(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_add_attachments(DataJObj, TemplateId) ->
    maybe_add_attachments(DataJObj, is_attachable_template(TemplateId), teletype_util:is_preview(DataJObj)).

-spec maybe_add_attachments(kz_json:object(), boolean(), boolean()) -> kz_json:object().
maybe_add_attachments(DataJObj, _, 'true') ->
    DataJObj;
maybe_add_attachments(DataJObj, 'true', 'false') ->
    PortReqId = kz_json:get_value(<<"port_request_id">>, DataJObj),
    Doc = kz_json:get_value(<<"port_request">>, DataJObj),
    Attachments = lists:foldl(fun(Name, Acc) -> get_attachment_fold(Name, Acc, PortReqId, Doc) end
                             ,[]
                             ,kz_doc:attachment_names(Doc)
                             ),
    kz_json:set_value(<<"attachments">>, Attachments, DataJObj);
maybe_add_attachments(DataJObj, _, 'false') ->
    DataJObj.

-spec get_attachment_fold(kz_json:key(), attachments(), kz_term:ne_binary(), kz_json:object()) ->
                                 attachments().
get_attachment_fold(Name, Acc, PortReqId, Doc) ->
    case kz_datamgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, PortReqId, Name) of
        {'ok', Attachment} ->
            ContentType = kz_doc:attachment_content_type(Doc, Name),
            {_, BinAttachment} = kz_attachment:decode_base64(Attachment),
            [{ContentType, Name, BinAttachment}|Acc];
        {'error', _Reason} ->
            lager:debug("failed to get port request ~s attchament ~s: ~p", [PortReqId, Name, _Reason]),
            throw({'error', 'no_attachment'})
    end.

-spec maybe_fix_emails(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_fix_emails(DataJObj, TemplateId) ->
    maybe_fix_emails(DataJObj, TemplateId, teletype_util:is_preview(DataJObj)).

-define(AUTHORITY_TEMPLATES, [teletype_port_cancel:id()
                             ,teletype_port_comment:id()
                             ,teletype_port_rejected:id()
                             ,teletype_port_request_admin:id()
                             ,teletype_port_request:id()
                             ]).

-spec maybe_fix_emails(kz_json:object(), kz_term:ne_binary(), boolean()) -> kz_json:object().
maybe_fix_emails(DataJObj, _, 'true') ->
    DataJObj;
maybe_fix_emails(DataJObj, TemplateId, 'false') ->
    IsAdminTemplate = teletype_port_request_admin:id() =:= TemplateId,
    SubmitterEmails = maybe_get_submitter(DataJObj, TemplateId, IsAdminTemplate),
    AuthorityEmails = maybe_get_port_authority(DataJObj, TemplateId, lists:member(TemplateId, ?AUTHORITY_TEMPLATES)),

    maybe_set_from_email(kz_json:set_values([{<<"to">>, SubmitterEmails}
                                            ,{<<"authority_emails">>, AuthorityEmails}
                                            ]
                                           ,DataJObj
                                           )
                        ,IsAdminTemplate
                        ).

-spec maybe_set_from_email(kz_json:object(), boolean()) -> kz_json:object().
maybe_set_from_email(DataJObj, 'true') ->
    DefaultFrom = teletype_util:default_from_address(),
    PortReq = kz_json:get_value(<<"port_request">>, DataJObj),
    Initiator = kzd_port_requests:notifications_email_send_to(PortReq, DefaultFrom),
    kz_json:set_value(<<"from">>, Initiator, DataJObj);
maybe_set_from_email(DataJObj, 'false') ->
    DataJObj.

-spec maybe_get_submitter(kz_json:object(), kz_term:ne_binary(), boolean()) -> kz_term:api_binaries().
maybe_get_submitter(_DataJObj, _TemplateId, 'true') ->
    lager:debug("admin template, not adding port submitter email"),
    'undefined';
maybe_get_submitter(DataJObj, TemplateId, 'false') ->
    case is_comment_private(DataJObj, TemplateId) of
        'true' ->
            lager:debug("comment is private, not adding port submitter email"),
            'undefined';
        'false' ->
            lager:debug("trying to find port submitter email"),
            get_port_submitter_emails(DataJObj)
    end.

-spec is_comment_private(kz_json:object(), kz_term:ne_binary()) -> boolean().
is_comment_private(DataJObj, TemplateId) ->
    teletype_port_comment:id() =:= TemplateId
        andalso kzd_comment:is_private_legacy(kz_json:get_json_value(<<"comment">>, DataJObj, kz_json:new())).

-spec get_port_submitter_emails(kz_json:object()) -> kz_term:api_binaries().
get_port_submitter_emails(DataJObj) ->
    Keys = [[<<"port_request">>, <<"customer_contact">>]
           ,[<<"port_request">>, <<"notifications">>, <<"email">>, <<"send_to">>]
           ],
    case kz_json:get_first_defined(Keys, DataJObj) of
        <<_/binary>> =Email ->
            [Email];
        Emails when is_list(Emails)
                    andalso Emails =/= [] ->
            Emails;
        _ ->
            lager:debug("port submitter email is not defined"),
            'undefined'
    end.

-spec maybe_get_port_authority(kz_json:object(), kz_term:ne_binary(), boolean()) -> kz_term:api_binaries().
maybe_get_port_authority(_, _TemplateId, 'false') ->
    lager:debug("template ~s is not meant for port authority", [_TemplateId]),
    'undefined';
maybe_get_port_authority(DataJObj, TemplateId, 'true') ->
    lager:debug("finding port authority emails for template ~s", [TemplateId]),
    get_authority_emails(DataJObj, TemplateId).

-spec get_authority_emails(kz_json:object(), kz_term:ne_binary()) -> kz_term:api_binaries().
get_authority_emails(DataJObj, TemplateId) ->
    PortDoc = kz_json:get_json_value(<<"port_request">>, DataJObj),
    case kz_json:get_ne_binary_value([<<"port_authority">>, <<"id">>], PortDoc) of
        'undefined' ->
            lager:debug("using master as port authority"),
            maybe_use_master_admins(TemplateId, 'undefined');
        PortAuthority ->
            maybe_use_port_support(PortAuthority)
    end.

-spec maybe_use_port_support(kz_term:ne_binary()) -> kz_term:api_binaries().
maybe_use_port_support(PortAuthority) ->
    case kzd_whitelabel:fetch(PortAuthority) of
        {'ok', JObj} ->
            case kzd_whitelabel:port_support_email(JObj) of
                ?NE_BINARY = Email ->
                    lager:debug("using port support emails from port authority ~s", [PortAuthority]),
                    [Email];
                [] ->
                    lager:debug("no port support, maybe using port authority admins"),
                    maybe_use_port_authority_admins(PortAuthority);
                Emails when is_list(Emails) ->
                    lager:debug("using port support emails from port authority ~s", [PortAuthority]),
                    Emails;
                _ ->
                    lager:debug("no port support, maybe using port authority admins"),
                    maybe_use_port_authority_admins(PortAuthority)
            end;
        {'error', _} ->
            lager:debug("can't open port authority whitelabel doc, maybe using port authority admins"),
            maybe_use_port_authority_admins(PortAuthority)
    end.

-spec maybe_use_port_authority_admins(kz_term:ne_binary()) -> kz_term:api_binaries().
maybe_use_port_authority_admins(PortAuthority) ->
    case teletype_util:find_account_admin_email(PortAuthority) of
        'undefined' ->
            lager:debug("~s doesn't have any admins, fallback to template emails", [PortAuthority]),
            'undefined';
        Admins ->
            lager:debug("using admin emails from ~s", [PortAuthority]),
            Admins
    end.

-spec maybe_use_master_admins(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_binaries().
maybe_use_master_admins(TemplateId, PortAuthority) ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            maybe_use_master_admins(TemplateId, PortAuthority, MasterAccountId);
        {'error', _} ->
            lager:debug("failed to find port authority, master account is undefined, maybe using system template"),
            maybe_use_system_emails(TemplateId)
    end.

-spec maybe_use_master_admins(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:api_binaries().
maybe_use_master_admins(TemplateId, MasterAccountId, MasterAccountId) ->
    lager:debug("reached to master account, maybe using default_to from system template"),
    maybe_use_system_emails(TemplateId);
maybe_use_master_admins(TemplateId, _, MasterAccountId) ->
    case teletype_util:find_account_admin_email(MasterAccountId) of
        'undefined' ->
            lager:debug("master doesn't have any admins, maybe using default_to from system template"),
            maybe_use_system_emails(TemplateId);
        Admins ->
            lager:debug("using admin emails from ~s", [MasterAccountId]),
            Admins
    end.

-spec maybe_use_system_emails(kz_term:ne_binary()) -> kz_term:api_binaries().
maybe_use_system_emails(TemplateId) ->
    case teletype_util:template_system_value(TemplateId, <<"default_to">>) of
        'undefined' ->
            lager:debug("system template doesn't have default_to"),
            'undefined';
        [] ->
            lager:debug("system template doesn't have default_to"),
            'undefined';
        ?NE_BINARY = To ->
            lager:debug("using default_to from system template"),
            [To];
        Emails when is_list(Emails) ->
            lager:debug("using default_to from system config template"),
            Emails;
        _ ->
            lager:debug("system template doesn't have default_to, using data emails"),
            'undefined'
    end.
