%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_ported).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".ported">>).

-define(TEMPLATE_ID, <<"ported">>).
-define(TEMPLATE_MACROS
        ,wh_json:from_list(
            [?MACRO_VALUE(<<"port_request.carrier">>, <<"carrier">>, <<"Carrier">>, <<"Carrier">>)
             ,?MACRO_VALUE(<<"port_request.name">>, <<"name">>, <<"Name">>, <<"Name">>)
             ,?MACRO_VALUE(<<"port_request.bill_name">>, <<"bill_name">>, <<"Bill Name">>, <<"Name on the bill">>)
             ,?MACRO_VALUE(<<"port_request.bill_address">>, <<"bill_address">>, <<"Bill Address">>, <<"Address on the bill">>)
             ,?MACRO_VALUE(<<"port_request.bill_locality">>, <<"bill_locality">>, <<"Bill Locality">>, <<"City on the bill">>)
             ,?MACRO_VALUE(<<"port_request.bill_region">>, <<"bill_region">>, <<"Bill Region">>, <<"Region on the bill">>)
             ,?MACRO_VALUE(<<"port_request.bill_postal_code">>, <<"bill_postal_code">>, <<"Bill Postal Code">>, <<"Postal Code on the bill">>)
             ,?MACRO_VALUE(<<"port_request.transfer_date">>, <<"transfer_date">>, <<"Transfer Date">>, <<"Transfer Date">>)
             ,?MACRO_VALUE(<<"port_request.numbers">>, <<"numbers">>, <<"Numbers">>, <<"Numbers">>)
             | ?SERVICE_MACROS ++ ?ACCOUNT_MACROS
            ])
        ).

-define(TEMPLATE_TEXT, <<"Port request successful for {{account.name}}.\n\n Numbers: {% for number in port_request.numbers %} {{ number }} {% endfor %}.">>).
-define(TEMPLATE_HTML, <<"<p>Port request successful for {{account.name}}.</p><p>Numbers: {% for number in port_request.numbers %} {{ number }} {% endfor %}</p>">>).
-define(TEMPLATE_SUBJECT, <<"Port request successful for {{account.name}}">>).
-define(TEMPLATE_CATEGORY, <<"port_request">>).
-define(TEMPLATE_NAME, <<"Port Successful">>).

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

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:ported_v(JObj),
    wh_util:put_callid(JObj),
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    case teletype_util:should_handle_notification(DataJObj) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec handle_req(wh_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    {'ok', AccountJObj} = teletype_util:open_doc(<<"account">>, AccountId, DataJObj),

    PortReqId = wh_json:get_value(<<"port_request_id">>, DataJObj),
    {'ok', PortReqJObj} = teletype_util:open_doc(<<"port_request">>, PortReqId, DataJObj),

    ReqData =
        wh_json:set_values(
          [{<<"account">>, AccountJObj}
           ,{<<"port_request">>, fix_port_request_data(PortReqJObj)}
          ]
          ,DataJObj
         ),

    case teletype_util:is_preview(DataJObj) of
        'false' ->
            handle_port_request(ReqData);
        'true' ->
            handle_port_request(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec fix_port_request_data(wh_json:object()) -> wh_json:object().
fix_port_request_data(JObj) ->
    Routines = [fun fix_numbers/1
                ,fun fix_billing/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec fix_numbers(wh_json:object()) -> wh_json:object().
fix_numbers(JObj) ->
    Numbers =
        wh_json:foldl(
            fun(Number, _Value, Acc) ->
                [Number|Acc]
            end
            ,[]
            ,wh_json:get_value(<<"numbers">>, JObj)
        ),
    wh_json:set_value(<<"numbers">>, Numbers, JObj).

-spec fix_billing(wh_json:object()) -> wh_json:object().
fix_billing(JObj) ->
    wh_json:foldl(
        fun(Key, Value, Acc) ->
            wh_json:set_value(<<"bill_", Key/binary>>, Value, Acc)
        end
        ,wh_json:delete_key(<<"bill">>, JObj)
        ,wh_json:get_value(<<"bill">>, JObj)
    ).


-spec handle_port_request(wh_json:object()) -> 'ok'.
-spec handle_port_request(wh_json:object(), wh_proplist()) -> 'ok'.
handle_port_request(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    handle_port_request(DataJObj, teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj)).

handle_port_request(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
handle_port_request(DataJObj, Templates) ->
    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),

    Macros = [{<<"service">>, ServiceData}
              ,{<<"account">>, teletype_util:public_proplist(<<"account">>, DataJObj)}
              ,{<<"port_request">>, teletype_util:public_proplist(<<"port_request">>, DataJObj)}
             ],

    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_util:fetch_template_meta(?TEMPLATE_ID
                                          ,teletype_util:find_account_id(DataJObj)
                                         ),

    Subject =
        teletype_util:render_subject(
            wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
            ,Macros
        ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case
        teletype_util:send_email(
            Emails
            ,Subject
            ,ServiceData
            ,RenderedTemplates
            ,get_attachments(DataJObj)
        )
    of
        'ok' ->
            teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} ->
            teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec get_attachments(wh_json:object()) -> attachments().
-spec get_attachments(wh_json:object(), boolean()) -> attachments().
get_attachments(DataJObj) ->
    get_attachments(DataJObj, teletype_util:is_preview(DataJObj)).

get_attachments(_DataJObj, 'true') -> [];
get_attachments(DataJObj, 'false') ->
    PortReqId = wh_json:get_value(<<"port_request_id">>, DataJObj),
    Attachments = wh_doc:attachments(wh_json:get_value(<<"port_request">>, DataJObj)),
    AttachmentNames = wh_doc:attachment_names(Attachments),
    lists:foldl(
        fun(Name, Acc) ->
            {'ok', Attachment} = couch_mgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, PortReqId, Name),
            ContentType = wh_doc:attachment_content_type(Attachments, Name),
            [{ContentType, Name, Attachment}|Acc]
        end
        ,[]
        ,AttachmentNames
    ).
