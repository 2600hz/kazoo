%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with fax attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_fax).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_fax_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_fax_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_fax_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the fax template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_notifications:fax_v(JObj),
    _ = whapps_util:put_callid(JObj),

    lager:debug("new fax left, sending to email if enabled"),
    lager:debug("notification: ~p", [JObj]),

    AcctDB = wh_json:get_value(<<"Account-DB">>, JObj),
    FaxId = wh_json:get_value(<<"Fax-ID">>, JObj),

    lager:debug("account-db: ~s, fax-id: ~s", [AcctDB, FaxId]),

    {ok, FaxDoc} = couch_mgr:open_doc(AcctDB, FaxId),

    OwnerId = case wh_json:get_value(<<"owner_id">>, FaxDoc) of
                  undefined -> wh_json:get_value(<<"Owner-ID">>, JObj);
                  OID -> OID
              end,
    lager:debug("owner: ~s", [OwnerId]),

    {ok, UserJObj} = couch_mgr:open_doc(AcctDB, OwnerId),
    case {wh_json:get_ne_value(<<"email">>, UserJObj)
          ,wh_json:is_true(<<"fax_to_email_enabled">>, UserJObj)
         } of
        {undefined, _} ->
            lager:debug("no email found for user ~s", [wh_json:get_value(<<"username">>, UserJObj)]);
        {_Email, false} ->
            lager:debug("fax to email disabled for ~s", [_Email]);
        {Email, true} ->
            lager:debug("Fax->Email enabled for user, sending to ~s", [Email]),
            {ok, AcctObj} = couch_mgr:open_doc(?WH_ACCOUNTS_DB, wh_util:format_account_id(AcctDB, raw)),
            Docs = [FaxDoc, UserJObj, AcctObj],

            Props = [{<<"email_address">>, Email}
                     | create_template_props(JObj, Docs, AcctObj)
                    ],

            CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"fax_to_email">>, <<"email_text_template">>], AcctObj),
            {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

            CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"fax_to_email">>, <<"email_html_template">>], AcctObj),
            {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

            CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"fax_to_email">>, <<"email_subject_template">>], AcctObj),
            {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

            try build_and_send_email(TxtBody, HTMLBody, Subject, Email, props:filter_empty(Props)) of
                _ -> lager:debug("built and sent")
            catch
                C:R ->
                    lager:debug("failed: ~s:~p", [C, R]),
                    ST = erlang:get_stacktrace(),
                    wh_util:log_stacktrace(ST)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:objects(), wh_json:object()) -> wh_proplist().
create_template_props(Event, Docs, Account) ->
    Now = wh_util:current_tstamp(),

    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, Event),
    ToE164 = wh_json:get_value(<<"To-User">>, Event),
    FromE164 = wh_json:get_value(<<"From-User">>, Event),
    DateCalled = wh_json:get_integer_value(<<"Fax-Timestamp">>, Event, Now),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    Timezone = wh_util:to_list(wh_json:find(<<"timezone">>, Docs, <<"UTC">>)),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
     ,{<<"fax">>, [{<<"caller_id_number">>, wnm_util:pretty_print(CIDNum)}
                   ,{<<"caller_id_name">>, wnm_util:pretty_print(CIDName)}
                   ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                   ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                   ,{<<"from_user">>, wnm_util:pretty_print(FromE164)}
                   ,{<<"from_realm">>, wh_json:get_value(<<"From-Realm">>, Event)}
                   ,{<<"to_user">>, wnm_util:pretty_print(ToE164)}
                   ,{<<"to_realm">>, wh_json:get_value(<<"To-Realm">>, Event)}
                   ,{<<"fax_id">>, wh_json:get_value(<<"Fax-ID">>, Event)}
                   ,{<<"fax_media">>, wh_json:get_value(<<"Fax-Name">>, Event)}
                   ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, Event)}
                   | fax_values(Event)
                  ]}
     ,{<<"account_db">>, wh_json:get_value(<<"pvt_account_db">>, Account)}
    ].

fax_values(Event) ->
    [{wh_json:normalize_key(K), V}
     || {<<"Fax-", K/binary>>, V} <- wh_json:to_proplist(Event)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), wh_proplist()) -> any().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    To = props:get_value(<<"email_address">>, Props),

    From = props:get_value(<<"send_from">>, Service),

    {ContentType, AttachmentFileName, AttachmentBin} = get_attachment(Props),
    [ContentTypeA,ContentTypeB] = binary:split(ContentType,<<"/">>),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,[]
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               }
               ,{ContentTypeA, ContentTypeB
                     ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentFileName, "\""])}
                       ,{<<"Content-Type">>, list_to_binary([<<ContentType/binary, "; name=\"">>, AttachmentFileName, "\""])}
                       ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                      ]
                 ,[], AttachmentBin
                }
              ]
            },
    notify_util:send_email(From, To, Email).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(proplist(), string()) -> ne_binary().
get_file_name(Props, Ext) ->
    %% CallerID_Date_Time.mp3
    Fax = props:get_value(<<"fax">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Fax), props:get_value(<<"caller_id_number">>, Fax)} of
                   {undefined, undefined} -> <<"Unknown">>;
                   {undefined, Num} -> wh_util:to_binary(Num);
                   {Name, _} -> wh_util:to_binary(Name)
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Fax, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(wh_util:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{return, binary}, global]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_attachment(proplist()) -> {ne_binary(), ne_binary(), ne_binary()} | {'error', _}.
get_attachment(Props) ->
    {ok, AttachmentBin} = raw_attachment_binary(Props),
    case whapps_config:get_binary(?MOD_CONFIG_CAT, <<"attachment_format">>, <<"pdf">>) of
        <<"pdf">> -> convert_to_pdf(AttachmentBin, Props);
        _Else -> convert_to_tiff(AttachmentBin, Props)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec raw_attachment_binary(proplist()) -> {'ok', ne_binary()} | {'error', _}.
raw_attachment_binary(Props) ->
    DB = props:get_value(<<"account_db">>, Props),
    Fax = props:get_value(<<"fax">>, Props),
    FaxId = props:get_value(<<"fax_id">>, Fax),
    {ok, FaxJObj} = couch_mgr:open_doc(DB, FaxId),
    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, FaxJObj),
    couch_mgr:fetch_attachment(DB, FaxId, AttachmentId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_to_tiff(ne_binary(), proplist()) -> {ne_binary(), ne_binary(), ne_binary()}.
convert_to_tiff(AttachmentBin, Props) ->
    {<<"image/tiff">>, get_file_name(Props, "tiff"), AttachmentBin}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_to_pdf(ne_binary(), proplist()) -> {ne_binary(), ne_binary(), ne_binary()} | {'error', _}.
convert_to_pdf(AttachmentBin, Props) ->
    TiffFile = tmp_file_name(<<"tiff">>),
    PDFFile = tmp_file_name(<<"pdf">>),    
    _ = file:write_file(TiffFile, AttachmentBin),
    Cmd = io_lib:format("tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"", [PDFFile, TiffFile]),
    _ = os:cmd(Cmd),
    _ = file:delete(TiffFile),
    case file:read_file(PDFFile) of
        {ok, PDFBin} ->
            _ = file:delete(PDFFile),
            {<<"application/pdf">>, get_file_name(Props, "pdf"), PDFBin};
        {error, _R}=E ->
            lager:debug("unable to convert tiff: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec tmp_file_name(ne_binary()) -> string().
tmp_file_name(Ext) ->
    wh_util:to_list(<<"/tmp/", (wh_util:rand_hex_binary(10))/binary, "_notify_fax.", Ext/binary>>).
