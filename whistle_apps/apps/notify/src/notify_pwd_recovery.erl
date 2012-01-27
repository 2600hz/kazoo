%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% James Aimonetti <james@2600hz.org>
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 22 Dec 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_pwd_recovery).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_pwd_recovery_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_pwd_recovery_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_pwd_recovery_subj_tmpl).

-define(NOTIFY_PWD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".password_recovery">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, ?DEFAULT_TEXT_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_PWD_CONFIG_CAT, default_text_template), ?DEFAULT_TEXT_TMPL),
    {ok, ?DEFAULT_HTML_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_PWD_CONFIG_CAT, default_html_template), ?DEFAULT_HTML_TMPL),
    {ok, ?DEFAULT_SUBJ_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_PWD_CONFIG_CAT, default_subject_template), ?DEFAULT_SUBJ_TMPL),
    ?LOG_SYS("init done for password recovery notify").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:pwd_recovery_v(JObj),
    whapps_util:put_callid(JObj),

    ?LOG_START("password has been reset, sending email notification"),

    {AcctDb, AcctId} = case {wh_json:get_value(<<"Account-DB">>, JObj), wh_json:get_value(<<"Account-ID">>, JObj)} of
                           {undefined, undefined} -> undefined;
                           {undefined, Id1} ->
                               {wh_util:format_account_id(Id1, encoded), Id1};
                           {Id2, undefined} ->
                               {Id2, wh_util:format_account_id(Id2, raw)};
                           Else -> Else 
                       end,

    ?LOG("attempting to load account doc ~s from ~s", [AcctId, AcctDb]),
    {ok, Account} = couch_mgr:open_doc(AcctDb, AcctId),
    
    To = wh_json:get_value(<<"Email">>, JObj, whapps_config:get(?NOTIFY_PWD_CONFIG_CAT, <<"default_to">>, <<"">>)),

    DefaultFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
    From = wh_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"send_from">>], Account
                             ,whapps_config:get(?NOTIFY_PWD_CONFIG_CAT, <<"default_from">>, DefaultFrom)),

    ?LOG("creating password reset notice"),
    
    Props = [{<<"From">>, From}
             |get_template_props(JObj, Account)
            ],

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    
    send_pwd_recovery_email(TxtBody, HTMLBody, Subject, To, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec get_template_props/2 :: (wh_json:json_object(), wh_json:json_objects()) -> proplist().
get_template_props(Event, Account) ->
    User = wh_json:delete_key(<<"Request">>, Event),
    Request = wh_json:get_value(<<"Request">>, Event, wh_json:new()),
    [{<<"user">>, notify_util:json_to_template_props(User)}
     ,{<<"request">>, notify_util:json_to_template_props(Request)}
     ,{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(Request, Account, ?NOTIFY_PWD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send_pwd_recovery_email/5 :: (iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist()) -> 'ok'.
send_pwd_recovery_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    [send_pwd_recovery_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To],
    ok;
send_pwd_recovery_email(TxtBody, HTMLBody, Subject, To, Props) ->
    From = props:get_value(<<"From">>, Props),
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
    ?LOG("sending password recovery notice to: ~p", [To]),
    notify_util:send_email(From, To, Email),
    ok.                
