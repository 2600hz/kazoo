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
    [test_rendering(teletype_deregister)
     %% ,test_rendering(teletype_fax_inbound_error_to_email)
    ,test_rendering(teletype_low_balance)
    ,test_rendering(teletype_new_account)
    ,test_rendering(teletype_new_user)
    ,test_rendering(teletype_service_added)
     %% ,test_rendering(teletype_system_alert)
     %% ,test_rendering(teletype_voicemail_to_email)
    ].

test_rendering(Module) ->
    TemplateId = Module:id(),
    TemplateIdStr = binary_to_list(TemplateId),
    Fixture = "notif__" ++ TemplateIdStr ++ ".json",
    DataJObj = kz_json:normalize(teletype_util:fixture(Fixture)),
    ?LOG_DEBUG(">>> normalized ~s", [kz_json:encode(DataJObj)]),
    Macros = Module:macros(DataJObj),
    ?LOG_DEBUG(">>> macros ~p", [Macros]),
    CTs = teletype_templates:master_content_types(TemplateId),
    ?LOG_DEBUG(">>> CTs ~p", [CTs]),
    [{"Render "++ TemplateIdStr ++" "++ binary_to_list(CT) ++" using "++ Fixture
     ,render(TemplateId, CT, Macros)
     }
     || CT <- CTs
    ].

render(TemplateId, CT, Macros) ->
    TmpModule = teletype_templates:renderer_name(TemplateId, CT),
    ?LOG_DEBUG(">>> TmpModule ~p", [TmpModule]),
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
