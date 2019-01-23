%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_port_utils).

-export([port_request_data/2]).

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
    Routines = [fun fix_numbers/3
               ,fun fix_billing/3
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
    maybe_get_attachments(maybe_fix_emails(NewData, TemplateId), TemplateId).

-spec fix_numbers(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_numbers(_DataJObj, _TemplateId, PortReqJObj) ->
    Numbers = kz_json:foldl(fun (Number, _Value, Acc) -> [Number|Acc] end
                           ,[]
                           ,get_numbers(PortReqJObj)
                           ),
    kz_json:set_value(<<"numbers">>, Numbers, PortReqJObj).

-spec get_numbers(kz_json:object()) -> kz_term:ne_binaries().
get_numbers(PortReqJObj) ->
    case kz_json:get_value(?PORT_PVT_STATE, PortReqJObj) of
        ?PORT_COMPLETED ->
            %% in case the doc is not saved/replicated yet, use numbers key
            Default = kz_json:get_json_value(<<"numbers">>, PortReqJObj, kz_json:new()),
            kz_json:get_json_value(<<"ported_numbers">>, PortReqJObj, Default);
        _ ->
            kz_json:get_json_value(<<"numbers">>, PortReqJObj, kz_json:new())
    end.

-spec fix_billing(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_billing(_DataJObj, _TemplateId, PortReqJObj) ->
    kz_json:foldl(fun (Key, Value, Acc) -> kz_json:set_value(<<"bill_", Key/binary>>, Value, Acc) end
                 ,kz_json:delete_key(<<"bill">>, PortReqJObj)
                 ,kz_json:get_json_value(<<"bill">>, PortReqJObj, kz_json:new())
                 ).

-spec fix_comments(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_comments(DataJObj, _TemplateId, PortReqJObj) ->
    case kz_json:get_json_value(<<"comment">>, DataJObj) of
        'undefined' -> kz_json:delete_key(<<"comments">>, PortReqJObj);
        Comment ->
            Timestamp = kz_json:get_integer_value(<<"timestamp">>, Comment),
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

-spec fix_dates(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_dates(_DataJObj, _TemplateId, PortReqJObj) ->
    lists:foldl(fun fix_date_fold/2
               ,kz_json:set_value(<<"created">>, kz_doc:created(PortReqJObj), PortReqJObj)
               ,[<<"transfer_date">>, <<"scheduled_date">>, <<"created">>, <<"ported_date">>]
               ).

-spec fix_date_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
fix_date_fold(<<"ported_date">> = Key, JObj) ->
    case [TransitionJObj
          || TransitionJObj <- kz_json:get_list_value(<<"pvt_transitions">>, JObj, []),
             kz_json:get_ne_binary_value([<<"transition">>, <<"new">>], TransitionJObj) =:= ?PORT_COMPLETED
         ]
    of
        [] -> JObj;
        [Completed|_] ->
            Timestamp = kz_json:get_integer_value([<<"transition">>, <<"timestamp">>], Completed),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, JObj)),
            kz_json:set_value(Key, Date, JObj)
    end;
fix_date_fold(Key, JObj) ->
    case kz_json:get_integer_value(Key, JObj) of
        'undefined' -> JObj;
        Timestamp ->
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, JObj)),
            kz_json:set_value(Key, Date, JObj)
    end.

-spec fix_notifications(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
fix_notifications(_DataJObj, _TemplateId, PortReqJObj) ->
    case kz_json:get_value([<<"notifications">>, <<"email">>, <<"send_to">>], PortReqJObj) of
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
                        ,kz_json:get_first_defined(LosingKeys, PortReqJObj, <<"Unknown Carrier">>)
                        }
                       ,{<<"winning_carrier">>
                        ,kz_json:get_value(<<"winning_carrier">>, PortReqJObj, <<"Unknown Carrier">>)
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
    case kz_json:get_ne_json_value(<<"reason">>, DataJObj) of
        'undefined' -> PortReqJObj;
        Reason ->
            UserInfo = get_commenter_info(DataJObj),
            Timestamp = kz_json:get_integer_value(<<"timestamp">>, Reason),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, DataJObj)),
            Props = [{<<"content">>, kz_json:get_ne_binary_value(<<"content">>, Reason)}
                    ,{<<"date">>, Date}
                    ,{<<"user">>, kz_json:from_list(UserInfo)}
                    ],
            kz_json:set_value(<<"transition_reason">>, kz_json:from_list(Props), PortReqJObj)
    end.

-spec get_commenter_info(kz_json:object()) -> kz_term:proplist().
get_commenter_info(DataJObj) ->
    maybe_add_user_data(DataJObj
                       ,kz_json:get_first_defined([[<<"comment">>, <<"author">>]
                                                  ,[<<"reason">>, <<"author">>]
                                                  ], DataJObj)
                       ).

-spec maybe_add_user_data(kz_json:object(), kz_term:api_binary()) -> kz_term:proplist().
maybe_add_user_data(DataJObj, Author) ->
    maybe_add_user_data(DataJObj, Author, teletype_util:is_preview(DataJObj)).

-spec maybe_add_user_data(kz_json:object(), kz_term:api_binary(), boolean()) -> kz_term:proplist().
maybe_add_user_data(DataJObj, Author, 'true') ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    case teletype_util:find_account_admin(AccountId) of
        'undefined' when Author =:= 'undefined' ->
            [{<<"author">>, <<"An agent">>}];
        'undefined' ->
            [{<<"author">>, Author}];
        UserDoc when Author =:= 'undefined' ->
            [{<<"author">>, first_last_name(kzd_users:first_name(UserDoc), kzd_users:last_name(UserDoc))}
             | teletype_util:user_params(UserDoc)
            ];
        UserDoc ->
            [{<<"author">>, Author}
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
        {'ok', UserDoc} when Author =:= 'undefined' ->
            [{<<"author">>, first_last_name(kzd_users:first_name(UserDoc), kzd_users:last_name(UserDoc))}
             | teletype_util:user_params(UserDoc)
            ];
        {'ok', UserDoc} ->
            [{<<"author">>, Author}
             | teletype_util:user_params(UserDoc)
            ]
    end.

-spec first_last_name(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
first_last_name(?NE_BINARY = First, ?NE_BINARY = Last) ->
    <<First/binary, " ", Last/binary>>;
first_last_name(_, ?NE_BINARY = Last) ->
    <<Last/binary>>;
first_last_name(?NE_BINARY = First, _) ->
    <<First/binary>>;
first_last_name(_, _) ->
    <<"An agent">>.

-spec is_attachable_template(kz_term:ne_binary()) -> boolean().
is_attachable_template(TemplateId) ->
    lists:member(TemplateId
                ,[teletype_port_request_admin:id()
                 ,teletype_port_request:id()
                 ]
                ).

-spec maybe_get_attachments(kz_json:object(), kz_term:ne_binary()) -> attachments().
maybe_get_attachments(DataJObj, TemplateId) ->
    maybe_get_attachments(DataJObj, is_attachable_template(TemplateId), teletype_util:is_preview(DataJObj)).

-spec maybe_get_attachments(kz_json:object(), boolean(), boolean()) -> attachments().
maybe_get_attachments(_DataJObj, _, 'true') -> [];
maybe_get_attachments(DataJObj, 'true', 'false') ->
    PortReqId = kz_json:get_value(<<"port_request_id">>, DataJObj),
    Doc = kz_json:get_value(<<"port_request">>, DataJObj),
    Attachments = lists:foldl(fun(Name, Acc) -> get_attachment_fold(Name, Acc, PortReqId, Doc) end
                             ,[]
                             ,kz_doc:attachment_names(Doc)
                             ),
    kz_json:set_value(<<"attachments">>, Attachments, DataJObj);
maybe_get_attachments(DataJObj, _, 'false') ->
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

-spec maybe_fix_emails(kz_json:object(), kz_term:ne_binary(), boolean()) -> kz_json:object().
maybe_fix_emails(DataJObj, _, 'true') ->
    DataJObj;
maybe_fix_emails(DataJObj, TemplateId, 'false') ->
    ToEmails = get_to_emails(DataJObj, TemplateId, is_comment_private(DataJObj, TemplateId)),

    IsAdminTemplate = teletype_port_request_admin:id() =:= TemplateId,

    maybe_set_from_email(kz_json:set_value(<<"to">>, ToEmails, DataJObj), IsAdminTemplate).

-spec get_to_emails(kz_json:object(), kz_term:ne_binary(), boolean()) -> kz_json:object().
get_to_emails(DataJObj, TemplateId, 'true') ->
    get_authority_emails(DataJObj, TemplateId);
get_to_emails(DataJObj, TemplateId, 'false') ->
    get_authority_emails(DataJObj, TemplateId)
        ++ get_port_submitter_emails(DataJObj).

-spec maybe_set_from_email(kz_json:object(), boolean()) -> kz_json:object().
maybe_set_from_email(DataJObj, 'true') ->
    DefaultFrom = teletype_util:default_from_address(),
    Initiator = kz_json:get_value([<<"port_request">>, <<"notifications">>, <<"email">>, <<"send_to">>], DataJObj, DefaultFrom),
    kz_json:set_value(<<"from">>, Initiator, DataJObj);
maybe_set_from_email(DataJObj, 'false') ->
    DataJObj.

-spec is_comment_private(kz_json:object(), kz_term:ne_binary()) -> boolean().
is_comment_private(DataJObj, TemplateId) ->
    teletype_port_comment:id() =:= TemplateId
        andalso kz_json:is_true([<<"comment">>, <<"superduper_comment">>], DataJObj, 'false').

-spec get_port_submitter_emails(kz_json:object()) -> kz_term:binaries().
get_port_submitter_emails(DataJObj) ->
    Keys = [[<<"port_request">>, <<"customer_contact">>]
           ,[<<"port_request">>, <<"notifications">>, <<"email">>, <<"send_to">>]
           ],
    case kz_json:get_first_defined(Keys, DataJObj) of
        <<_/binary>> =Email -> [Email];
        Emails when is_list(Emails) -> Emails;
        _ -> []
    end.

-spec get_authority_emails(kz_json:object(), kz_term:ne_binary()) -> kz_term:binaries().
get_authority_emails(DataJObj, TemplateId) ->
    PortDoc = kz_json:get_json_value(<<"port_request">>, DataJObj),
    case kzd_port_requests:find_port_authority(PortDoc) of
        'undefined' ->
            lager:debug("using master as port authority"),
            maybe_use_master_admins(TemplateId, 'undefined');
        PortAuthority ->
            case teletype_util:find_account_admin_email(PortAuthority) of
                'undefined' ->
                    lager:debug("~s doesn't have any admin maybe using system emails", [PortAuthority]),
                    maybe_use_master_admins(TemplateId, PortAuthority);
                [] ->
                    lager:debug("~s doesn't have any admin maybe using system emails", [PortAuthority]),
                    maybe_use_master_admins(TemplateId, PortAuthority);
                Admins ->
                    lager:debug("using admin emails from ~s", [PortAuthority]),
                    Admins
            end
    end.

-spec maybe_use_master_admins(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binaries().
maybe_use_master_admins(TemplateId, PortAuthority) ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            maybe_use_master_admins(TemplateId, PortAuthority, MasterAccountId);
        {'error', _} ->
            lager:debug("failed to find port authority, master account is undefined, maybe using system template"),
            maybe_use_system_emails(TemplateId)
    end.

-spec maybe_use_master_admins(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binaries().
maybe_use_master_admins(TemplateId, MasterAccountId, MasterAccountId) ->
    lager:debug("reached to master account, maybe using default_to from system template"),
    maybe_use_system_emails(TemplateId);
maybe_use_master_admins(TemplateId, _, MasterAccountId) ->
    case teletype_util:find_account_admin_email(MasterAccountId) of
        'undefined' ->
            lager:debug("master doesn't have any admins, maybe using default_to from system template"),
            maybe_use_system_emails(TemplateId);
        [] ->
            lager:debug("master doesn't have any admins, maybe using default_to from system template"),
            maybe_use_system_emails(TemplateId);
        Admins ->
            lager:debug("using admin emails from ~s", [MasterAccountId]),
            Admins
    end.

-spec maybe_use_system_emails(kz_term:ne_binary()) -> kz_term:api_ne_binaries().
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
