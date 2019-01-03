%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc XML-formatter for FreeSWITCH XML responses
%%% Copy of xmerl/src/xmerl_xml.erl
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(fs_xml).

-export(['#xml-inheritance#'/0
        ]).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1
        ]).

-include_lib("xmerl/include/xmerl.hrl").

-spec '#xml-inheritance#'() -> [].
'#xml-inheritance#'() ->
    [].

%% The '#text#' function is called for every text segment.

-spec '#text#'(any()) -> iolist().
'#text#'(Text) ->
    xmerl_lib:export_text(Text).

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), list(), any()) -> iolist().
'#root#'(Data, _Attrs, [], _E) ->
    ["<document type=\"freeswitch/xml\">", Data, "</document>"].

%% The '#element#' function is the default handler for XML elements.
-spec '#element#'(any(), any(), any(), any(), any()) -> iolist().
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    xmerl_lib:markup(Tag, Attrs, Data).
