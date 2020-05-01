%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% @author Karl Anderson <karl@2600hz.org>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_transaction).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_transaction_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_transaction_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_transaction_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".transaction">>).

%%------------------------------------------------------------------------------
%% @doc initialize the module
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = kapi_notifications:transaction_v(JObj),

    _ = kz_log:put_callid(JObj),

    lager:debug("creating transaction notice"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    {ok, Account} = notify_util:get_account_doc(JObj),

    Props = create_template_props(JObj, Account),

    {ok, TxtBody} = notify_util:render_template(undefined, ?DEFAULT_TEXT_TMPL, Props),
    {ok, HTMLBody} = notify_util:render_template(undefined, ?DEFAULT_HTML_TMPL, Props),

    To = kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>),

    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"transaction">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    notify_util:maybe_send_update(build_and_send_email(TxtBody, HTMLBody, Subject, To, Props)
                                 ,RespQ
                                 ,MsgId
                                 ).

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:object()) -> kz_term:proplist().
create_template_props(Event, Account) ->
    props:filter_empty([{<<"account">>, notify_util:json_to_template_props(Account)}
                       ,{<<"plan">>, notify_util:json_to_template_props(kz_json:get_value(<<"Service-Plan">>, Event))}
                       ,{<<"transaction">>, transaction_data(Event)}
                       ,{<<"service">>, notify_util:get_service_props(kz_json:new(), ?MOD_CONFIG_CAT)}
                       ]).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:ne_binary() | [kz_term:ne_binary(),...], kz_term:proplist()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),
    PlainTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>, <<"7BIT">>),
    HTMLTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>, <<"7BIT">>),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
            ,[{<<"From">>, From}
             ,{<<"To">>, To}
             ,{<<"Subject">>, Subject}
             ]
            ,ContentTypeParams
            ,[{<<"multipart">>, <<"alternative">>, [], []
              ,[{<<"text">>, <<"plain">>
                ,props:filter_undefined(
                   [{<<"Content-Type">>, iolist_to_binary([<<"text/plain">>, CharsetString])}
                   ,{<<"Content-Transfer-Encoding">>, PlainTransferEncoding}
                   ])
                ,[], iolist_to_binary(TxtBody)}
               ,{<<"text">>, <<"html">>
                ,props:filter_undefined(
                   [{<<"Content-Type">>, iolist_to_binary([<<"text/html">>, CharsetString])}
                   ,{<<"Content-Transfer-Encoding">>, HTMLTransferEncoding}
                   ])
                ,[], iolist_to_binary(HTMLBody)}
               ]
              }
             ]
            },
    notify_util:send_email(From, To, Email).

transaction_data(Event) ->
    Props = notify_util:json_to_template_props(Event),
    props:filter_undefined(
      [{<<"amount">>, get_transaction_amount(Props)}
      ,{<<"success">>, props:is_true(<<"success">>, Props)}
      ,{<<"response">>, props:get_value(<<"response">>, Props)}
      ,{<<"id">>, props:get_value(<<"id">>, Props)}
      ,{<<"add_ons">>, props:get_value(<<"add_ons">>, Props, [])}
      ,{<<"discounts">>, props:get_value(<<"discounts">>, Props, [])}
      ,{<<"address">>
       ,props:filter_undefined(
          [{<<"first_name">>, props:get_value([<<"billing_address">>, <<"first_name">>], Props)}
          ,{<<"last_name">>, props:get_value([<<"billing_address">>, <<"last_name">>], Props)}
          ,{<<"company">>, props:get_value([<<"billing_address">>, <<"company">>], Props)}
          ,{<<"street_address">>, props:get_value([<<"billing_address">>, <<"street_address">>], Props)}
          ,{<<"extended_address">>, props:get_value([<<"billing_address">>, <<"extended_address">>], Props)}
          ,{<<"locality">>, props:get_value([<<"billing_address">>, <<"locality">>], Props)}
          ,{<<"region">>, props:get_value([<<"billing_address">>, <<"region">>], Props)}
          ,{<<"postal_code">>, props:get_value([<<"billing_address">>, <<"postal_code">>], Props)}
          ,{<<"country_name">>, props:get_value([<<"billing_address">>, <<"country_name">>], Props)}
          ,{<<"phone">>, props:get_value([<<"billing_address">>, <<"phone">>], Props)}
          ,{<<"email">>, props:get_value([<<"billing_address">>, <<"email">>], Props)}
          ])
       }
      ,{<<"card_last_four">>, props:get_value(<<"card_last_four">>, Props)}
      ,{<<"tax_amount">>, props:get_value(<<"tax_amount">>, Props)}
      ,{<<"date">>, props:get_value(<<"timestamp">>, Props), Props}
      ,{<<"purchase_order">>, purchase_order(Props)}
      ,{<<"currency_code">>, props:get_value(<<"currency_code">>, Props)}
      ]
     ).

%% amount is expected to be in dollars
-spec get_transaction_amount('undefined' | kz_term:proplist()) -> kz_term:api_float().
get_transaction_amount('undefined') -> 'undefined';
get_transaction_amount(Props) ->
    case props:get_value(<<"amount">>, Props) of
        'undefined' -> 'undefined';
        Amount -> kz_term:to_float(Amount)
    end.

-spec purchase_order(kz_term:proplist()) -> binary().
purchase_order(Props) ->
    binary:replace(props:get_ne_binary_value(<<"purchase_order">>, Props, <<>>)
                  ,<<"_">>
                  ,<<" ">>
                  ,[global]
                  ).
