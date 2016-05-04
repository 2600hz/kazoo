%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_port_utils).

-export([is_comment_private/1]).
-export([get_attachments/1]).
-export([fix_email/1, fix_email/2]).
-export([fix_port_request_data/1]).

-include("teletype.hrl").

-spec is_comment_private(any()) -> boolean().
is_comment_private('undefined') -> 'false';
is_comment_private([]) -> 'false';
is_comment_private([_|_]=Comments) ->
    is_comment_private(lists:last(Comments));
is_comment_private(Comment) ->
    kz_json:is_true(<<"superduper_comment">>, Comment, 'false').

-spec get_attachments(kz_json:object()) -> attachments().
-spec get_attachments(kz_json:object(), boolean()) -> attachments().
get_attachments(DataJObj) ->
    get_attachments(DataJObj, teletype_util:is_preview(DataJObj)).

get_attachments(_DataJObj, 'true') -> [];
get_attachments(DataJObj, 'false') ->
    PortReqId = kz_json:get_value(<<"port_request_id">>, DataJObj),
    Doc = kz_json:get_value(<<"port_request">>, DataJObj),
    lists:foldl(
      fun(Name, Acc) -> get_attachment_fold(Name, Acc, PortReqId, Doc) end
      ,[]
      ,kz_doc:attachment_names(Doc)
     ).

-spec get_attachment_fold(kz_json:key(), attachments(), ne_binary(), kz_json:object()) ->
                                 attachments().
get_attachment_fold(Name, Acc, PortReqId, Doc) ->
    {'ok', Attachment} = kz_datamgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, PortReqId, Name),
    ContentType = kz_doc:attachment_content_type(Doc, Name),
    {_, BinAttachment} = kz_attachment:decode_base64(Attachment),
    [{ContentType, Name, BinAttachment}|Acc].

-spec fix_email(kz_json:object()) -> kz_json:object().
-spec fix_email(kz_json:object(), boolean()) -> kz_json:object().
fix_email(ReqData) ->
    fix_email(ReqData, 'false').

fix_email(ReqData, OnlyAdmin) ->
    AccountId = kz_json:get_value(<<"account_id">>, ReqData),
    Emails = get_emails(ReqData, AccountId, OnlyAdmin),
    kz_json:set_value(<<"to">>, Emails, ReqData).

-spec get_emails(kz_json:object(), maybe(binary()), boolean()) -> ne_binaries().
get_emails(_ReqData, AccountId, 'true') ->
    ResellerId = kz_services:find_reseller_id(AccountId),

    ResellerEmail = find_reseller_port_email(ResellerId),
    AdminEmails = teletype_util:find_account_admin_email(ResellerId),

    case {ResellerEmail, AdminEmails} of
        {'undefined', 'undefined'} -> [];
        {'undefined', [_|_]} -> AdminEmails;
        {ResellerEmail, _} -> [ResellerEmail]
    end;
get_emails(ReqData, AccountId, 'false') ->
    ResellerId = kz_services:find_reseller_id(AccountId),

    ResellerEmail = find_reseller_port_email(ResellerId),
    AdminEmails = teletype_util:find_account_admin_email(ResellerId),
    PortReqEmail = get_port_req_email(ReqData),

    case {ResellerEmail, AdminEmails} of
        {'undefined', 'undefined'} -> [PortReqEmail];
        {'undefined', [_|_]} -> AdminEmails ++ PortReqEmail;
        {ResellerEmail, _} -> [ResellerEmail] ++ PortReqEmail
    end.

-spec find_reseller_port_email(maybe(binary())) -> maybe(binary()).
find_reseller_port_email(AccountId) ->
    case kz_whitelabel:fetch(AccountId) of
        {'error', _R} -> 'undefined';
        {'ok', JObj} ->
            kz_whitelabel:port_email(JObj)
    end.

-spec get_port_req_email(kz_json:object()) -> ne_binaries().
get_port_req_email(ReqData) ->
    Key = [<<"port_request">>, <<"notifications">>, <<"email">>, <<"send_to">>],
    case kz_json:get_value(Key, ReqData) of
        <<_/binary>> =Email -> [Email];
        [_|_]=Emails -> Emails
    end.

-spec fix_port_request_data(kz_json:object()) -> kz_json:object().
fix_port_request_data(JObj) ->
    Routines = [fun fix_numbers/1
                ,fun fix_billing/1
                ,fun fix_comments/1
                ,fun fix_dates/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec fix_numbers(kz_json:object()) -> kz_json:object().
fix_numbers(JObj) ->
    Numbers =
        kz_json:foldl(
          fun fix_number_fold/3
          ,[]
          ,kz_json:get_value(<<"numbers">>, JObj, kz_json:new())
         ),
    kz_json:set_value(<<"numbers">>, Numbers, JObj).

-spec fix_number_fold(kz_json:object(), any(), kz_json:keys()) -> kz_json:keys().
fix_number_fold(Number, _Value, Acc) ->
    [Number|Acc].

-spec fix_billing(kz_json:object()) -> kz_json:object().
fix_billing(JObj) ->
    kz_json:foldl(
      fun fix_billing_fold/3
      ,kz_json:delete_key(<<"bill">>, JObj)
      ,kz_json:get_value(<<"bill">>, JObj)
     ).

-spec fix_billing_fold(kz_json:key(), kz_json:json_term(), kz_json:object()) ->
                              kz_json:object().
fix_billing_fold(Key, Value, Acc) ->
    kz_json:set_value(<<"bill_", Key/binary>>, Value, Acc).

-spec fix_comments(kz_json:object()) -> kz_json:object().
fix_comments(JObj) ->
    case kz_json:get_value(<<"comments">>, JObj) of
        'undefined' ->
            kz_json:delete_key(<<"comments">>, JObj);
        [] ->
            kz_json:delete_key(<<"comments">>, JObj);
        Comments ->
            LastComment = lists:last(Comments),

            Timestamp = kz_json:get_integer_value(<<"timestamp">>, LastComment),
            {Date, _Time} = calendar:gregorian_seconds_to_datetime(Timestamp),
            Comment = kz_json:set_value(<<"timestamp">>, Date, LastComment),

            kz_json:set_value(
              <<"comment">>
              ,kz_json:to_proplist(Comment)
              ,kz_json:delete_key(<<"comments">>, JObj)
             )
    end.

-spec fix_dates(kz_json:object()) -> kz_json:object().
fix_dates(JObj) ->
    lists:foldl(
      fun fix_date_fold/2
      ,JObj
      ,[<<"transfer_date">>, <<"scheduled_date">>]
     ).

-spec fix_date_fold(kz_json:key(), kz_json:object()) -> kz_json:object().
fix_date_fold(Key, JObj) ->
    case kz_json:get_integer_value(Key, JObj) of
        'undefined' -> JObj;
        Timestamp ->
            {Date, _Time} = calendar:gregorian_seconds_to_datetime(Timestamp),
            kz_json:set_value(Key, Date, JObj)
    end.
