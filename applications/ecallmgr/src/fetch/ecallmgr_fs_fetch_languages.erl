%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_languages).

%% API
-export([init/0]).

-export([language_req/1]).

-include("ecallmgr.hrl").

-import(ecallmgr_fs_xml,
        [section_el/3
        ,xml_attrib/2
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Initializes the bindings
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"fetch.languages.*.language_req">>, ?MODULE, 'language_req'),
    'ok'.

-spec language_req(map()) -> fs_sendmsg_ret().
language_req(#{fetch_id := Id, payload := JObj} = Ctx) ->
    kz_log:put_callid(Id),
    {ok, Xml} = language_resp_xml(JObj),
    freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(Xml)}).

-spec language_resp_xml(kz_json:object()) -> {'ok', iolist()}.
language_resp_xml(JObj) ->
    BaseDir = <<"$${sounds_dir}">>,
    Voice = <<"callie">>,
    Lang = kz_json:get_ne_binary_value(<<"lang">>, JObj, <<"en-us">>),
    LangParts = binary:split(Lang, <<"-">>),
    UrlParts = lists:flatten([BaseDir
                             ,LangParts
                             ,Voice
                             ]),
    Props = [{<<"name">>, Lang}
            ,{<<"say-module">>, lists:nth(1, LangParts)}
            ,{<<"sound-prefix">>, kz_binary:join(UrlParts, <<"/">>)}
            ],
    LanguageEl = language_el(Props, phrases_el([])),
    SectionEl = section_el(<<"languages">>, <<"Languages Response">>, LanguageEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

language_el(Props, PhrasesEl) ->
    #xmlElement{name='language'
               ,attributes=[xml_attrib(K, V)
                            || {K, V} <- props:unique(
                                           props:filter_undefined(Props)
                                          )
                           ]
               ,content=[PhrasesEl]
               }.

phrases_el(MacrosEl) ->
    #xmlElement{name='phrases'
               ,content=[macros_el(MacrosEl)]
               }.

macros_el(MacrosEl) ->
    #xmlElement{name='macros'
               ,content=MacrosEl
               }.
