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
-module(notify_deregister).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_deregister_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_deregister_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_deregister_subj_tmpl).

-define(NOTIFY_DEREG_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".deregister">>).

-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, ?DEFAULT_TEXT_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_DEREG_CONFIG_CAT, default_text_template), ?DEFAULT_TEXT_TMPL),
    {ok, ?DEFAULT_HTML_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_DEREG_CONFIG_CAT, default_html_template), ?DEFAULT_HTML_TMPL),
    {ok, ?DEFAULT_SUBJ_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_DEREG_CONFIG_CAT, default_subject_template), ?DEFAULT_SUBJ_TMPL),
    ?LOG_SYS("init done for dergister notify").

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:deregister_v(JObj),
    whapps_util:put_callid(JObj),

    ?LOG_START("endpoint has become unregistered, sending email notification"),

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

    To = wh_json:get_value([<<"notifications">>, <<"deregister">>, <<"send_to">>], Account
                           ,whapps_config:get(?NOTIFY_DEREG_CONFIG_CAT, <<"default_to">>, <<"">>)),

    From = wh_json:get_value([<<"notifications">>, <<"deregister">>, <<"send_from">>], Account
                             ,whapps_config:get(?NOTIFY_DEREG_CONFIG_CAT, <<"default_from">>, <<"no_reply@2600hz.com">>)),

    ?LOG("creating deregisted notice"),
    
    Props = [{<<"To">>, To}
             ,{<<"From">>, From}
             |get_template_props(JObj, Account)
            ],

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"deregister">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"deregister">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"deregister">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    
    send_deregister_email(TxtBody, HTMLBody, Subject, [{<<"To">>, To}|Props]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec get_template_props/2 :: (wh_json:json_object(), wh_json:json_objects()) -> proplist().
get_template_props(Event, Account) ->
    [{<<"last_registration">>, notify_util:json_to_template_props(Event)}
     ,{<<"account">>, notify_util:json_to_template_props(Account)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send_deregister_email/4 :: (iolist(), iolist(), iolist(), proplist()) -> 'ok'.
send_deregister_email(TxtBody, HTMLBody, Subject, Props) ->
    %% NOTE: the value in the From prop could be 'undefined' or not present...
    From = case props:get_value(<<"From">>, Props) of
               undefined -> 
                   list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]);
               Else ->
                   Else
           end,
    To = props:get_value(<<"To">>, Props),
    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,[]
             ,[
               {<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               }
              ]
            },
    ?LOG("sending deregistered notice to: ~p", [To]),
    notify_util:send_email(From, To, Email),
    ok.
