%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%%
%%%
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_low_balance).

-export([init/0, send/2
        ,handle_req/2
        ]).
-export([collect_recipients/1]).


-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_low_balance_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_low_balance_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_low_balance_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".low_balance">>).

%%------------------------------------------------------------------------------
%% @doc initialize the module
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:low_balance_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("account low balance alert, sending email notification"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    {'ok', Account} = kzd_accounts:fetch(kz_json:get_value(<<"Account-ID">>, JObj)),

    notify_util:maybe_send_update(send(kz_json:get_integer_value(<<"Current-Balance">>, JObj), Account)
                                 ,RespQ
                                 ,MsgId
                                 ).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec send(integer(), kz_json:object()) -> any().
send(CurrentBalance, Account) ->
    AccountId = kz_doc:id(Account),
    case collect_recipients(AccountId) of
        'undefined' -> 'ok';
        To ->
            lager:debug("sending low balance alert for account ~s", [AccountId]),
            Props = create_template_props(CurrentBalance, Account),
            CustomTxtTemplate = kz_json:get_value([<<"notifications">>, <<"low_balance">>, <<"email_text_template">>], Account),
            {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
            CustomHtmlTemplate = kz_json:get_value([<<"notifications">>, <<"low_balance">>, <<"email_html_template">>], Account),
            {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
            CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"low_balance">>, <<"email_subject_template">>], Account),
            {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
            build_and_send_email(TxtBody, HTMLBody, Subject, To, Props)
    end.

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(integer(), kz_json:object()) -> kz_term:proplist().
create_template_props(CurrentBalance, Account) ->
    Threshold = kzd_accounts:low_balance_threshold(kz_doc:id(Account)),
    [{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"service">>, notify_util:get_service_props(kz_json:new(), Account, ?MOD_CONFIG_CAT)}
    ,{<<"current_balance">>, pretty_print_dollars(kz_currency:units_to_dollars(CurrentBalance))}
    ,{<<"threshold">>, pretty_print_dollars(Threshold)}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pretty_print_dollars(float()) -> kz_term:ne_binary().
pretty_print_dollars(Amount) ->
    kz_term:to_binary(io_lib:format("$~.2f", [Amount])).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:proplist()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),
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
             ]
            },
    notify_util:send_email(From, To, Email).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collect_recipients(kz_term:ne_binary()) -> kz_term:api_binaries() | kz_term:api_binary().
collect_recipients(AccountId) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    get_email(AccountId, MasterAccountId).

-spec get_email(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binaries() | kz_term:api_binary().
get_email(MasterAccountId, MasterAccountId) ->
    AccountDb = kz_util:format_account_id(MasterAccountId, 'encoded'),
    lager:debug("attempting to email low balance to master account ~s"
               ,[MasterAccountId]
               ),
    case kz_datamgr:open_doc(AccountDb, MasterAccountId) of
        {'ok', JObj} -> find_billing_email(JObj);
        {'error', _R} ->
            lager:error("could not open account ~s : ~p"
                       ,[MasterAccountId, _R]
                       ),
            'undefined'
    end;
get_email(AccountId, MasterAccountId) ->
    lager:debug("attempting to email low balance to account ~s"
               ,[AccountId]
               ),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} -> get_email(JObj, AccountId, MasterAccountId);
        {'error', _R} ->
            lager:error("could not open account ~s : ~p", [AccountId, _R]),
            get_email(MasterAccountId, MasterAccountId)
    end.

-spec get_email(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binaries() | kz_term:api_binary().
get_email(JObj, AccountId, MasterAccountId) ->
    case find_billing_email(JObj) of
        'undefined' ->
            lager:debug("billing contact email not set or low balance disabled for account ~s"
                       ,[AccountId]
                       ),
            ResellerId = kz_services_reseller:get_id(AccountId),
            get_email(ResellerId, MasterAccountId);
        Email -> Email
    end.

-spec find_billing_email(kz_json:object()) -> kz_term:api_binaries() | kz_term:api_binary().
find_billing_email(JObj) ->
    case is_notify_enabled(JObj) of
        'false' -> 'undefined';
        'true' ->
            kz_json:get_ne_value([<<"contact">>, <<"billing">>, <<"email">>], JObj)
    end.

-spec is_notify_enabled(kz_json:object()) -> boolean().
is_notify_enabled(JObj) ->
    case kz_json:get_value([<<"notifications">>
                           ,<<"low_balance">>
                           ,<<"enabled">>
                           ], JObj)
    of
        'undefined' -> is_notify_enabled_default();
        Value -> kz_term:is_true(Value)
    end.

-spec is_notify_enabled_default() -> boolean().
is_notify_enabled_default() ->
    kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"default_enabled">>, 'false').
