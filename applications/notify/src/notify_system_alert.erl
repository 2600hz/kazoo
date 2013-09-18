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
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:system_alert_v(JObj),
    whapps_util:put_callid(JObj),
    lager:debug("creating system alert notice"),   
    Props = create_template_props(JObj),   
    {ok, TxtBody} = notify_util:render_template(undefined, ?DEFAULT_TEXT_TMPL, Props),    
    {ok, HTMLBody} = notify_util:render_template(undefined, ?DEFAULT_HTML_TMPL, Props),           
    Subject = wh_json:get_ne_value(<<"Subject">>, JObj),    
    To = whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>),
    build_and_send_email(TxtBody, HTMLBody, Subject, To, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object()) -> proplist().
create_template_props(Event) ->
    [{<<"request">>, notify_util:json_to_template_props(wh_json:delete_keys([<<"Details">>
                                                                                 ,<<"App-Version">>
                                                                                 ,<<"App-Name">>
                                                                                 ,<<"Event-Name">>
                                                                                 ,<<"Event-Category">>
                                                                                 ,<<"Server-ID">>
                                                                                 ,<<"Message">>
                                                                                 ,<<"Subject">>
                                                                            ], Event))}
     ,{<<"message">>, wh_json:get_binary_value(<<"Message">>, Event)}
     ,{<<"details">>, notify_util:json_to_template_props(wh_json:get_value(<<"Details">>, Event))}
     ,{<<"service">>, notify_util:get_service_props(wh_json:new(), ?MOD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To],
    ok;
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
    notify_util:send_email(From, To, Email),
    ok.
