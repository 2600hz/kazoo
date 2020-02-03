%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_render_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/teletype.hrl").

-export([overwrite_t0/3]).
-export([manual_rendering/1, manual_rendering/2]).

-spec render_test_() -> any().
render_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_ReturnOfSetup) ->
             [?_assertEqual(37, length(?DEFAULT_MODULES))
              %% ,test_rendering(teletype_account_zone_change)
             ,test_rendering(teletype_bill_reminder)
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
              %% ,test_rendering(teletype_number_feature_manual_action)
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
             ]
     end
    }.

setup() ->
    ?LOG_DEBUG(":: Setting up Kazoo FixtureDB"),

    {ok, _} = application:ensure_all_started(kazoo_config),
    kazoo_fixturedb:start().

cleanup(LinkPid) ->
    _DataLink = erlang:exit(LinkPid, normal),
    Ref = monitor(process, LinkPid),
    receive
        {'DOWN', Ref, process, LinkPid, _Reason} ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.

test_rendering(Module) ->
    #{id_str := TemplateIdStr
     ,id := TemplateId
     ,cts := CTs
     ,macros := Macros
     ,fixture_file := Fixture
     } = call_template(Module),
    [{"Render "++ TemplateIdStr ++" "++ binary_to_list(CT) ++" using "++ Fixture
     ,render(TemplateId, CT, Macros)
     }
     || CT <- CTs
    ].

render(TemplateId, CT, Macros) ->
    {'ok', Rendered} = render_t0(TemplateId, CT, Macros),
    [?_assert(
        lists:all(fun({T0, Line}) -> T0 =:= Line end
                 ,lists:zip(t0(TemplateId, CT), lines(iolist_to_binary(Rendered)))
                 )
       )
    ].

render_t0(TemplateId, CT, Macros) ->
    TmpModule = teletype_templates:renderer_name(TemplateId, CT),
    {ok, Template} = fetch_template(TemplateId, CT),
    kz_template:render(Template, TmpModule, Macros).

-spec manual_rendering(atom()) -> 'ok'.
manual_rendering(Module) ->
    manual_rendering(Module, 'false').

-spec manual_rendering(atom(), boolean()) -> 'ok'.
manual_rendering(Module, ShouldOverwrite) ->
    #{cts := CTs
     ,id :=TemplateId
     ,macros := Macros
     ,fixture_file := Fixture
     } = Map = call_template(Module),
    ?DEV_LOG("Template ~s uses fixture ~s, macros are:~n~p~n", [TemplateId, Fixture, Macros]),
    _ = [do_maunal_render(ShouldOverwrite, CT, Map) || CT <- CTs],
    ok.

do_maunal_render(ShouldOverwrite, CT, #{id_str := TemplateIdStr
                                       ,id := TemplateId
                                       ,macros := Macros
                                       }) ->
    io:format(user, "Rendering ~s for ~s: ", [TemplateIdStr, CT]),
    case render_t0(TemplateId, CT, Macros) of
        {'ok', Rendered} when ShouldOverwrite ->
            io:format(user, "rendered successfully, writing rendered template...~n", []),
            overwrite_t0(TemplateId, CT, Rendered);
        {'ok', _} ->
            io:format(user, "rendered successfully~n", []);
        {'error', _Error} ->
            io:format(user, "rendering failed: ~p~n", [_Error])
    end.

call_template(Module) ->
    TemplateId = Module:id(),
    TemplateIdStr = binary_to_list(TemplateId),
    Fixture = "fixtures-api/notifications/" ++ TemplateIdStr ++ ".json",
    {ok, FixtureJObj} = kz_json:fixture(kazoo_amqp, Fixture),
    DataJObj = kz_json:normalize(FixtureJObj),
    Macros = Module:macros(DataJObj),
    RealyMacrosThisTime = update_system_macros(Macros),
    CTs = teletype_templates:master_content_types(TemplateId),
    #{id_str => TemplateIdStr
     ,id => TemplateId
     ,cts => CTs
     ,macros => RealyMacrosThisTime
     ,fixture_file => Fixture
     }.

update_system_macros(Macros) ->
    props:set_value(<<"system">>, fake_system(), Macros).

fake_system() ->
    [{<<"hostname">>, <<"localhost.local">>}
    ,{<<"encoded_hostname">>, <<"_LBdE1TNqHSBgfhxgWW_7Q">>}
    ,{<<"node">>, <<"greate_test_env@neverland">>}
    ,{<<"encoded_node">>, <<"6rdR7MIUnpFLhBvGwVZO1g">>}
    ].

t0(TemplateId, CT) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:lib_dir(?APP), "test/rendered-templates/", <<TemplateId/binary,".",Ext/binary>>]),
    %% ?LOG_DEBUG("reading t0 template ~s", [Path]),
    {ok, Bin} = file:read_file(Path),
    lines(Bin).

-spec overwrite_t0(kz_term:ne_binary(), kz_term:ne_binary(), binary()) -> 'ok'.
overwrite_t0(TemplateId, CT, Rendered) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:lib_dir(?APP), "test/rendered-templates/", <<TemplateId/binary,".",Ext/binary>>]),
    ok = file:write_file(Path, Rendered).

fetch_template(TemplateId, CT) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:priv_dir(?APP), "templates", <<TemplateId/binary,".",Ext/binary>>]),
    %% ?LOG_DEBUG("reading template ~s", [Path]),
    file:read_file(Path).

ct_to_ext(<<"text/plain">>) -> <<"text">>;
ct_to_ext(<<"text/html">>) -> <<"html">>.

lines(Bin) ->
    [kz_binary:strip(Line)
     || Line <- binary:split(Bin, <<$\n>>, [global])
    ].
