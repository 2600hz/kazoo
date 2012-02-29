%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 27 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_system_alert).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_system_alert_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_system_alert_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_system_alert_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".system_alert">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:system_alert_v(JObj),
    whapps_util:put_callid(JObj),

    lager:debug("an alert has occured, sending email notification"),

    Account = case notify_util:get_account_doc(JObj) of
                  undefined -> wh_json:new();
                  {ok, Else} -> Else
              end,

    {To, MinLevel} = case wh_json:get_value([<<"notifications">>, <<"alert">>, <<"send_to">>], Account) of
                         undefined ->
                             {whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>)
                              ,whapps_config:get(?MOD_CONFIG_CAT, <<"default_level">>, <<"critical">>)};
                         Admin ->
                             {Admin
                              ,wh_json:get_value([<<"notifications">>, <<"alert">>, <<"send_to">>], Account
                                                 ,whapps_config:get(?MOD_CONFIG_CAT, <<"default_level">>, <<"critical">>))}
                     end,

    AlertLevel = wh_json:get_value(<<"Level">>, JObj),
    
    case alert_level_to_integer(AlertLevel) >= alert_level_to_integer(MinLevel) of
        false -> ok;
        true ->
            DefaultFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
            From = wh_json:get_value([<<"notifications">>, <<"alert">>, <<"send_from">>], Account
                                     ,whapps_config:get(?MOD_CONFIG_CAT, <<"default_from">>, DefaultFrom)),

            lager:debug("creating system alert notice"),
            
            Props = [{<<"From">>, From}
                     ,{<<"Level">>, AlertLevel}
                     |create_template_props(JObj, Account)
                    ],
            
            CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"alert">>, <<"email_text_template">>], Account),
            {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
            
            CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"alert">>, <<"email_html_template">>], Account),
            {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
            
            CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"alert">>, <<"email_subject_template">>], Account),
            {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
            
            To = wh_json:get_value([<<"notifications">>, <<"alert">>, <<"send_to">>], Account
                                   ,whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>)),
            
            build_and_send_email(TxtBody, HTMLBody, Subject, To, Props)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props/2 :: (wh_json:json_object(), wh_json:json_objects()) -> proplist().
create_template_props(Event, Account) ->
    Admin = notify_util:find_admin(Account),    
    [{<<"request">>, notify_util:json_to_template_props(wh_json:delete_keys([<<"Details">>
                                                                                 ,<<"App-Version">>
                                                                                 ,<<"App-Name">>
                                                                                 ,<<"Event-Name">>
                                                                                 ,<<"Event-Category">>
                                                                                 ,<<"Server-ID">>
                                                                                 ,<<"Message">>
                                                                            ], Event))}
     ,{<<"message">>, wh_json:get_binary_value(<<"Message">>, Event)}
     ,{<<"details">>, notify_util:json_to_template_props(wh_json:get_value(<<"Details">>, Event))}
     ,{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
     ,{<<"service">>, notify_util:get_service_props(Account, ?MOD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email/5 :: (iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To],
    ok;
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
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
    notify_util:send_email(From, To, Email),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% convert the textual alert level to an interger value
%% @end
%%--------------------------------------------------------------------
-spec alert_level_to_integer/1 :: (ne_binary()) -> 0..8.
alert_level_to_integer(<<"emergency">>) ->
    8;
alert_level_to_integer(<<"critical">>) ->
    7;
alert_level_to_integer(<<"alert">>) ->
    6;
alert_level_to_integer(<<"error">>) ->
    5;
alert_level_to_integer(<<"warning">>) ->
    4;
alert_level_to_integer(<<"notice">>) ->
    3;
alert_level_to_integer(<<"info">>) ->
    2;
alert_level_to_integer(<<"debug">>) ->
    1.
