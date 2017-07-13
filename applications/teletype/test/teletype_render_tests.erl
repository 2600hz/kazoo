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

render_preview_test_() ->
    [test_rendering("deregister")
    ,test_rendering("service_added")
    ].

test_rendering(TemplateIdStr) ->
    TemplateId = list_to_binary(TemplateIdStr),
    Module = list_to_atom("teletype_" ++ TemplateIdStr),
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
    {ok, Template} = preview_template(TemplateId, CT),
    {ok, Rendered} = kz_template:render(Template, TmpModule, Macros),
    %% Below is only when adding new tests
    %% overwrite_t0(TemplateId, CT, Rendered),
    %% Above is only when adding new tests
    ?_assertEqual(t0(TemplateId, CT), lines(iolist_to_binary(Rendered))).

t0(TemplateId, CT) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:lib_dir(?APP), <<TemplateId/binary,".",Ext/binary>>]),
    ?LOG_DEBUG("reading t0 template ~s", [Path]),
    {ok, Bin} = file:read_file(Path),
    lines(Bin).

overwrite_t0(TemplateId, CT, Rendered) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:lib_dir(?APP), <<TemplateId/binary,".",Ext/binary>>]),
    ok = file:write_file(Path, Rendered).

preview_template(TemplateId, CT) ->
    Ext = ct_to_ext(CT),
    Path = filename:join([code:priv_dir(?APP), "templates", <<TemplateId/binary,".",Ext/binary>>]),
    ?LOG_DEBUG("reading preview template ~s", [Path]),
    file:read_file(Path).

ct_to_ext(<<"text/plain">>) -> <<"text">>;
ct_to_ext(<<"text/html">>) -> <<"html">>.

lines(Bin) ->
    binary:split(Bin, <<$\n>>, [global]).
