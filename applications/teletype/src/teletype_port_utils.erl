%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_port_utils).

-export([is_comment_private/1]).
-export([get_attachments/1]).
-export([fix_email/1, fix_email/2]).
-export([fix_port_request_data/2]).

-include("teletype.hrl").

-spec is_comment_private(kz_json:object()) -> boolean().
is_comment_private(DataJObj) ->
    kz_json:is_true([<<"comment">>, <<"superduper_comment">>], DataJObj, 'false').

-spec get_attachments(kz_json:object()) -> attachments().
get_attachments(DataJObj) ->
    get_attachments(DataJObj, teletype_util:is_preview(DataJObj)).

-spec get_attachments(kz_json:object(), boolean()) -> attachments().
get_attachments(_DataJObj, 'true') -> [];
get_attachments(DataJObj, 'false') ->
    PortReqId = kz_json:get_value(<<"port_request_id">>, DataJObj),
    Doc = kz_json:get_value(<<"port_request">>, DataJObj),
    lists:foldl(fun(Name, Acc) -> get_attachment_fold(Name, Acc, PortReqId, Doc) end
               ,[]
               ,kz_doc:attachment_names(Doc)
               ).

-spec get_attachment_fold(kz_json:key(), attachments(), kz_term:ne_binary(), kz_json:object()) ->
                                 attachments().
get_attachment_fold(Name, Acc, PortReqId, Doc) ->
    {'ok', Attachment} = kz_datamgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, PortReqId, Name),
    ContentType = kz_doc:attachment_content_type(Doc, Name),
    {_, BinAttachment} = kz_attachment:decode_base64(Attachment),
    [{ContentType, Name, BinAttachment}|Acc].

-spec fix_email(kz_json:object()) -> kz_json:object().
fix_email(ReqData) ->
    fix_email(ReqData, 'false').

-spec fix_email(kz_json:object(), boolean()) -> kz_json:object().
fix_email(ReqData, OnlyAdmin) ->
    AccountId = kz_json:get_value(<<"account_id">>, ReqData),
    Emails = get_emails(ReqData, AccountId, OnlyAdmin),
    kz_json:set_value(<<"to">>, Emails, ReqData).

-spec get_emails(kz_json:object(), kz_term:api_binary(), boolean()) -> kz_term:ne_binaries().
get_emails(_ReqData, AccountId, 'true') ->
    ResellerId = teletype_util:find_reseller_id(AccountId),

    ResellerEmail = find_reseller_port_email(ResellerId),
    AdminEmails = teletype_util:find_account_admin_email(ResellerId),

    case {ResellerEmail, AdminEmails} of
        {'undefined', 'undefined'} -> [];
        {'undefined', [_|_]} -> AdminEmails;
        {ResellerEmail, _} -> [ResellerEmail]
    end;
get_emails(ReqData, AccountId, 'false') ->
    ResellerId = teletype_util:find_reseller_id(AccountId),

    ResellerEmail = find_reseller_port_email(ResellerId),
    AdminEmails = teletype_util:find_account_admin_email(ResellerId),
    PortReqEmail = get_port_req_email(ReqData),

    case {ResellerEmail, AdminEmails} of
        {'undefined', 'undefined'} -> lists:usort(PortReqEmail);
        {'undefined', [_|_]} -> lists:usort(AdminEmails ++ PortReqEmail);
        {ResellerEmail, _} -> lists:usort([ResellerEmail] ++ PortReqEmail)
    end.

-spec find_reseller_port_email(kz_term:api_binary()) -> kz_term:api_binary().
find_reseller_port_email(AccountId) ->
    case kzd_whitelabel:fetch(AccountId) of
        {'error', _R} -> 'undefined';
        {'ok', JObj} ->
            kzd_whitelabel:port_support_email(JObj)
    end.

-spec get_port_req_email(kz_json:object()) -> kz_term:binaries().
get_port_req_email(ReqData) ->
    Keys = [[<<"port_request">>, <<"customer_contact">>]
           ,[<<"port_request">>, <<"notifications">>, <<"email">>, <<"send_to">>]
           ],
    case kz_json:get_first_defined(Keys, ReqData) of
        <<_/binary>> =Email -> [Email];
        [_|_]=Emails -> Emails;
        _ -> []
    end.

-spec fix_port_request_data(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_port_request_data(JObj, DataJObj) ->
    Routines = [fun fix_numbers/2
               ,fun fix_billing/2
               ,fun fix_comments/2
               ,fun fix_dates/2
               ,fun fix_notifications/2
               ,fun fix_carrier/2
               ,fun fix_transfer_date/2
               ,fun fix_scheduled_date/2
               ,fun fix_ui_metadata/2
               ,fun maybe_add_reason/2
               ],
    lists:foldl(fun(F, J) -> F(J, DataJObj) end, JObj, Routines).

-spec fix_numbers(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_numbers(JObj, _DataJObj) ->
    NumbersJObj = kz_json:get_value(<<"numbers">>, JObj, kz_json:new()),
    Numbers = kz_json:foldl(fun fix_number_fold/3, [], NumbersJObj),
    kz_json:set_value(<<"numbers">>, Numbers, JObj).

-spec fix_number_fold(kz_json:key(), kz_json:json_term(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
fix_number_fold(Number, _Value, Acc) ->
    [Number|Acc].

-spec fix_billing(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_billing(JObj, _DataJObj) ->
    kz_json:foldl(fun fix_billing_fold/3
                 ,kz_json:delete_key(<<"bill">>, JObj)
                 ,kz_json:get_json_value(<<"bill">>, JObj, kz_json:new())
                 ).

-spec fix_billing_fold(kz_term:ne_binary(), kz_json:json_term(), kz_json:object()) ->
                              kz_json:object().
fix_billing_fold(Key, Value, Acc) ->
    kz_json:set_value(<<"bill_", Key/binary>>, Value, Acc).

-spec fix_comments(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_comments(JObj, DataJObj) ->
    case kz_json:get_json_value(<<"comment">>, DataJObj) of
        'undefined' -> kz_json:delete_key(<<"comments">>, JObj);
        Comment ->
            Timestamp = kz_json:get_integer_value(<<"timestamp">>, Comment),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, DataJObj)),
            Props = [{<<"date">>, Date}
                    ,{<<"timestamp">>, kz_json:get_value(<<"local">>, Date)} %% backward compatibility
                    ],
            kz_json:set_value(<<"comment">>
                             ,kz_json:set_values(Props, Comment)
                             ,kz_json:delete_key(<<"comments">>, JObj)
                             )
    end.

-spec fix_dates(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_dates(JObj, _DataJObj) ->
    lists:foldl(fun fix_date_fold/2, JObj, [<<"transfer_date">>, <<"scheduled_date">>]).

-spec fix_date_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
fix_date_fold(Key, JObj) ->
    case kz_json:get_integer_value(Key, JObj) of
        'undefined' -> JObj;
        Timestamp ->
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, JObj)),
            kz_json:set_value(Key, Date, JObj)
    end.

-spec fix_notifications(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_notifications(JObj, _DataJObj) ->
    kz_json:set_value(<<"customer_contact">>
                     ,kz_json:get_value([<<"notifications">>, <<"email">>, <<"send_to">>], JObj)
                     ,JObj %% not deleting the key for backward compatibility
                     ).

-spec fix_carrier(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_carrier(JObj, _DataJObj) ->
    kz_json:set_value(<<"service_provider">>
                     ,kz_json:get_value(<<"carrier">>, JObj)
                     ,JObj %% not deleting the key for backward compatibility
                     ).

-spec fix_transfer_date(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_transfer_date(JObj, _DataJObj) ->
    kz_json:set_values([{<<"requested_port_date">>, kz_json:get_value(<<"transfer_date">>, JObj)}
                       ,{<<"transfer_date">>, kz_json:get_value([<<"transfer_date">>, <<"local">>], JObj)} %% backward compatibility
                       ], JObj).

-spec fix_scheduled_date(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_scheduled_date(JObj, _DataJObj) ->
    kz_json:set_values([{<<"port_scheduled_date">>, kz_json:get_value(<<"scheduled_date">>, JObj)}
                       ,{<<"scheduled_date">>, kz_json:get_value([<<"scheduled_date">>, <<"local">>], JObj)}
                       ], JObj).

-spec fix_ui_metadata(kz_json:object(), kz_json:object()) -> kz_json:object().
fix_ui_metadata(JObj, _DataJObj) ->
    kz_json:delete_keys([<<"ui_metadata">>, <<"ui_flags">>], JObj).

-spec maybe_add_reason(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_add_reason(JObj, DataJObj) ->
    case kz_json:get_ne_json_value(<<"reason">>, DataJObj) of
        'undefined' -> JObj;
        Reason ->
            UserInfo = get_commenter_info(kz_json:get_ne_binary_value(<<"account_id">>, Reason)
                                         ,kz_json:get_ne_binary_value(<<"user_id">>, Reason)
                                         ),
            Timestamp = kz_json:get_integer_value(<<"timestamp">>, Reason),
            Date = kz_json:from_list(teletype_util:fix_timestamp(Timestamp, DataJObj)),
            Props = [{<<"content">>, kz_json:get_ne_binary_value(<<"content">>, Reason)}
                    ,{<<"date">>, Date}
                    ,{<<"user">>, kz_json:from_list(UserInfo)}
                    ],
            kz_json:set_value(<<"transition_reason">>, kz_json:from_list(Props), JObj)
    end.

-spec get_commenter_info(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:proplist().
get_commenter_info(?NE_BINARY=AccountId, ?NE_BINARY=UserId) ->
    case kzd_users:fetch(AccountId, UserId) of
        {'ok', UserJObj} -> teletype_util:user_params(UserJObj);
        {'error', _Reason} ->
            lager:debug("failed to get commenter info: account_id ~s user_id ~s", [AccountId, UserId]),
            []
    end;
get_commenter_info(_, _) ->
    [].
