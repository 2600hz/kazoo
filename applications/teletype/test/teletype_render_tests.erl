%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_render_tests).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

-export([overwrite_t0/3]).

render_test_() ->
    [?_assertEqual(32, length(?DEFAULT_MODULES))
     %% ,test_rendering(teletype_account_zone_change)
     %% ,test_rendering(teletype_cnam_request)
     %% ,test_rendering(teletype_customer_update)
     %% ,test_rendering(teletype_denied_emergency_bridge)
    ,test_rendering(teletype_deregister)
     %% ,test_rendering(teletype_fax_inbound_error_to_email)
     %% ,test_rendering(teletype_fax_inbound_to_email)
     %% ,test_rendering(teletype_fax_outbound_error_to_email)
     %% ,test_rendering(teletype_fax_outbound_smtp_error_to_email)
     %% ,test_rendering(teletype_fax_outbound_to_email)
     %% ,test_rendering(teletype_first_occurrence)
    ,test_rendering(teletype_low_balance)
     %% ,test_rendering(teletype_missed_call)
    ,test_rendering(teletype_new_account)
    ,test_rendering(teletype_new_user)
     %% ,test_rendering(teletype_password_recovery)
     %% ,test_rendering(teletype_port_cancel)
     %% ,test_rendering(teletype_port_comment)
     %% ,test_rendering(teletype_port_pending)
     %% ,test_rendering(teletype_port_rejected)
     %% ,test_rendering(teletype_port_request)
     %% ,test_rendering(teletype_port_request_admin)
     %% ,test_rendering(teletype_port_scheduled)
     %% ,test_rendering(teletype_port_unconfirmed)
     %% ,test_rendering(teletype_ported)
    ,test_rendering(teletype_service_added)
    ,test_rendering(teletype_system_alert)
     %% ,test_rendering(teletype_topup)
     %% ,test_rendering(teletype_transaction)
     %% ,test_rendering(teletype_voicemail_full)
     %% ,test_rendering(teletype_voicemail_to_email)
     %% ,test_rendering(teletype_webhook_disabled)
    ].

test_rendering(Module) ->
    TemplateId = Module:id(),
    TemplateIdStr = binary_to_list(TemplateId),
    Fixture = "notif__" ++ TemplateIdStr ++ ".json",
    {ok,FixtureJObj} = kz_json:fixture(?APP, Fixture),
    DataJObj = kz_json:normalize(FixtureJObj),
    Macros = Module:macros(DataJObj),
    CTs = teletype_templates:master_content_types(TemplateId),
    [{"Render "++ TemplateIdStr ++" "++ binary_to_list(CT) ++" using "++ Fixture
     ,render(TemplateId, CT, Macros)
     }
     || CT <- CTs
    ].

render(TemplateId, CT, Macros) ->
    TmpModule = teletype_templates:renderer_name(TemplateId, CT),
    {ok, Template} = fetch_template(TemplateId, CT),
    {ok, Rendered} = kz_template:render(Template, TmpModule, Macros),
    %% Below is only when adding new tests
    %% overwrite_t0(TemplateId, CT, Rendered),
    %% Above is only when adding new tests
    [?_assertEqual(T0, Line)
     || {T0, Line} <- lists:zip(t0(TemplateId, CT), lines(iolist_to_binary(Rendered)))
    ].

t0(TemplateId, CT) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:lib_dir(?APP), "test", <<TemplateId/binary,".",Ext/binary>>]),
    ?LOG_DEBUG("reading t0 template ~s", [Path]),
    {ok, Bin} = file:read_file(Path),
    lines(Bin).

overwrite_t0(TemplateId, CT, Rendered) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:lib_dir(?APP), "test", <<TemplateId/binary,".",Ext/binary>>]),
    ok = file:write_file(Path, Rendered).

fetch_template(TemplateId, CT) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:priv_dir(?APP), "templates", <<TemplateId/binary,".",Ext/binary>>]),
    ?LOG_DEBUG("reading template ~s", [Path]),
    file:read_file(Path).

ct_to_ext(<<"text/plain">>) -> <<"text">>;
ct_to_ext(<<"text/html">>) -> <<"html">>.

lines(Bin) ->
    [kz_binary:strip(Line)
     || Line <- binary:split(Bin, <<$\n>>, [global])
    ].
