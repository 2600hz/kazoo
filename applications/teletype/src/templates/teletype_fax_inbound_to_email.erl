%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_inbound_to_email).

-export([init/0
         ,handle_inbound_fax/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_inbound_to_email">>).

-define(TEMPLATE_ID, <<"fax_inbound_to_email">>).

    %%  ,{<<"from_user">>, wnm_util:pretty_print(FromE164)}
    %%  ,{<<"from_realm">>, wh_json:get_value(<<"from_realm">>, DataJObj)}
    %%  ,{<<"to_user">>, wnm_util:pretty_print(ToE164)}
    %%  ,{<<"to_realm">>, wh_json:get_value(<<"to_realm">>, DataJObj)}
    %%  ,{<<"fax_id">>, wh_json:get_value(<<"fax_id">>, DataJObj)}
    %%  ,{<<"fax_media">>, wh_json:get_value(<<"fax_name">>, DataJObj)}
    %%  ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, DataJObj)}
    %%  | fax_values(wh_json:get_value(<<"fax_info">>, DataJObj, wh_json:new()))


-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"caller_id.number">>, <<"caller_id_number">>, <<"Caller ID Number">>, <<"Number of the caller">>)
            ,?MACRO_VALUE(<<"caller_id.name">>, <<"caller_id_name">>, <<"Caller ID Name">>, <<"Name of the caller">>)
            ,?MACRO_VALUE(<<"callee_id.number">>, <<"callee_id_number">>, <<"Callee ID Number">>, <<"Number of the callee">>)
            ,?MACRO_VALUE(<<"callee_id.name">>, <<"callee_id_name">>, <<"Callee ID Name">>, <<"Name of the callee">>)
            ,?MACRO_VALUE(<<"date_called.utc">>, <<"date_called_utc">>, <<"Date (UTC)">>, <<"When was the voicemail left (UTC)">>)
            ,?MACRO_VALUE(<<"date_called.local">>, <<"date_called_local">>, <<"Date">>, <<"When was the voicemail left (Local time)">>)
            ,?MACRO_VALUE(<<"from.user">>, <<"from_user">>, <<"From User">>, <<"SIP From Username">>)
           ]
          )).

-define(TEMPLATE_TEXT, <<"New Fax ({{fax.total_pages}} Pages)\n\nCaller ID: {{fax.caller_id_number}}\nCaller Name: {{fax.caller_id_name}}\n\nCalled To: {{fax.to_user}}   (Originally dialed number)\nCalled On: {{fax.date_called|date:\"l, F j, Y \\a\\t H:i\"}}\n\n\nFor help or questions about receiving faxes, please contact support at {{service.support_number}} or email {{service.support_email}}.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>New Fax ({{fax.total_pages}} Pages)</h3><table><tr><td>Caller ID</td><td>{{fax.caller_id_name}} ({{fax.caller_id_number}})</td></tr><tr><td>Callee ID</td><td>{{fax.to_user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{fax.date_called|date:\"l, F j, Y \\a\\t H:i\"}}</td></tr></table><p>For help or questions about receiving faxes, please contact {{service.support_number}} or email <a href=\"mailto:{{service.support_email}}\">Support</a></p><p style=\"font-size: 9px;color:#C0C0C0\">{{fax.call_id}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"New fax from {{fax.caller_id_name}} ({{fax.caller_id_number}})">>).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, ?TEMPLATE_MACROS, ?TEMPLATE_TEXT, ?TEMPLATE_HTML).

-spec handle_inbound_fax(wh_json:object(), wh_proplist()) -> 'ok'.
handle_inbound_fax(JObj, _Props) ->
    'true' = wapi_notifications:fax_inbound_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(wh_api:remove_defaults(JObj)),

    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    FaxId = wh_json:get_value(<<"fax_id">>, DataJObj),
    {'ok', FaxJObj} = kazoo_modb:open_doc(AccountId, FaxId),

    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),

    Macros = build_template_data(
               wh_json:set_values([{<<"account">>, wh_doc:public_fields(AccountJObj)}
                                   ,{<<"fax">>, wh_doc:public_fields(FaxJObj)}
                                  ]
                                  ,DataJObj
                                 )),

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, wh_json:get_value(<<"Account-ID">>, JObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),

    %% Send email
    teletype_util:send_email(?TEMPLATE_ID
                             ,DataJObj
                             ,props:get_value(<<"service">>, Macros)
                             ,Subject
                             ,RenderedTemplates
                            ).

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    [{<<"account">>, wh_json:to_proplist(wh_json:get_value(<<"account">>, DataJObj))}
     ,{<<"fax">>, build_fax_template_data(DataJObj)}
     ,{<<"service">>, teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT)}
     ,{<<"caller_id">>, caller_id_data(DataJObj)}
     ,{<<"callee_id">>, callee_id_data(DataJObj)}
     ,{<<"date_called">>, date_called_data(DataJObj)}
    ].

-spec caller_id_data(wh_json:object()) -> wh_proplist().
caller_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"name">>, wh_json:get_value(<<"caller_id_name">>, DataJObj)}
       ,{<<"number">>, wh_json:get_value(<<"caller_id_number">>, DataJObj)}
      ]).

-spec callee_id_data(wh_json:object()) -> wh_proplist().
callee_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"name">>, wh_json:get_value(<<"callee_id_name">>, DataJObj)}
       ,{<<"number">>, wh_json:get_value(<<"callee_id_number">>, DataJObj)}
      ]).

date_called_data(DataJObj) ->
    DateCalled = wh_json:get_integer_value(<<"fax_timestamp">>, DataJObj, wh_util:current_tstamp()),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),
    Timezone = wh_json:get_value([<<"fax">>, <<"rx_result">>, <<"timezone">>], DataJObj, <<"UTC">>),
    ClockTimezone = whapps_config:get(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    props:filter_undefined(
      [{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
       ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
      ]).

build_fax_template_data(DataJObj) ->
    ToE164 = wh_json:get_value(<<"to_user">>, DataJObj),
    FromE164 = wh_json:get_value(<<"from_user">>, DataJObj),

    [{<<"from_user">>, wnm_util:pretty_print(FromE164)}
     ,{<<"from_realm">>, wh_json:get_value(<<"from_realm">>, DataJObj)}
     ,{<<"to_user">>, wnm_util:pretty_print(ToE164)}
     ,{<<"to_realm">>, wh_json:get_value(<<"to_realm">>, DataJObj)}
     ,{<<"fax_id">>, wh_json:get_value(<<"fax_id">>, DataJObj)}
     ,{<<"fax_media">>, wh_json:get_value(<<"fax_name">>, DataJObj)}
     ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, DataJObj)}
     | fax_values(wh_json:get_value(<<"fax_info">>, DataJObj, wh_json:new()))
    ].

-spec fax_values(wh_json:object()) -> wh_proplist().
fax_values(DataJObj) ->
    [{K, V}
     || {<<"fax_", K/binary>>, V} <- wh_json:to_proplist(DataJObj)
    ].
