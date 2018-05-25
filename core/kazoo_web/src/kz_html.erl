%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc HML helper functions for Kazoo
%%% @author Mark Magnusson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_html).

-export([escape/1]).
-export([make_paraghraph/1
        ,is_block_element/1
        ,export_content/1, export_content/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type tag() :: atom().
-type exporty() :: {tag(), kz_types:xml_attribs(), [exporty()]} |
                   {tag(), exporty()} |
                   tag() |
                   list() |
                   string() |
                   binary() |
                   iolist() |
                   #xmlText{} |
                   #xmlElement{}.
-type exporties() :: [exporty()].
-type exported() :: binary().

-export_type([tag/0
             ,exporty/0
             ,exporties/0
             ,exported/0
             ]).

%%------------------------------------------------------------------------------
%% @doc Escapes an HTML string.
%% @end
%%------------------------------------------------------------------------------
-spec escape(binary()) -> binary().
escape(Source) ->
    escape(Source, <<>>).

-spec escape(binary(), binary()) -> binary().
escape(<<>>, Acc) ->
    Acc;

escape(<<$<, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, "&lt;">>);

escape(<<$>, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, "&gt;">>);

escape(<<$&, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, "&amp;">>);

escape(<<H, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, H>>).

%% @equiv export_content(ErlHtml, 'xmerl_html')
-spec export_content(exporty() | exporties()) -> exported().
export_content(ErlHtml) ->
    export_content(ErlHtml, 'xmerl_html').

%%------------------------------------------------------------------------------
%% @doc Export simple content of elements to HTML.
%%
%% This functions calls {@link xmerl:export_simple_content/2}. `CallBack' can be
%% default xmerl's callback `xmerl_html' or `xmel_xml' or your own module name.
%% @end
%%------------------------------------------------------------------------------
-spec export_content(exporty() | exporties(), atom()) -> exported().
export_content([], _) ->
    <<>>;
export_content([Char | _]=String, Context) when is_integer(Char) ->
    export_content([String], Context); %% xmerl won't accept string content
export_content([_|_]=ErlHtmls, Callback) ->
    iolist_to_binary(xmerl:export_simple_content(ErlHtmls, Callback));
export_content(ErlHtml, Callback) ->
    export_content([ErlHtml], Callback).

%%------------------------------------------------------------------------------
%% @doc Put loose in-line elements in a HTML paragraph.
%% @end
%%------------------------------------------------------------------------------
-spec make_paraghraph(kz_types:xml_els()) -> exporties().
make_paraghraph(Els) ->
    make_paraghraph(Els, [], []).

-spec make_paraghraph(kz_types:xml_els(), exporties(), exporties()) -> exporties().
make_paraghraph([El=#xmlElement{name = Name}|Els], InlineEls, BlockEls) ->
    case is_block_element(Name) of
        'true' -> par_flush(Els, InlineEls, [El | BlockEls]);
        'false' -> make_paraghraph(Els, [El | InlineEls], BlockEls)
    end;
make_paraghraph([El=#xmlText{value = Value}|Els], InlineEls, BlockEls) ->
    case kz_term:is_only_whitespace(Value) of
        'true' -> make_paraghraph(Els, InlineEls, BlockEls);
        'false' ->
            Normalized = El#xmlText{value = [C || C <- Value, C =/= $\n, C =/= $\r]},
            make_paraghraph(Els, [Normalized | InlineEls], BlockEls)
    end;
make_paraghraph([], [], BlockEls) ->
    lists:reverse(BlockEls);
make_paraghraph([], InlineEls, BlockEls) ->
    lists:reverse([{'p', lists:reverse(InlineEls)} | BlockEls]).

-spec par_flush(kz_types:xml_els(), exporties(), exporties()) -> exporties().
par_flush(Els, [], BlockEls) ->
    make_paraghraph(Els, [], BlockEls);
par_flush(Els, InlineEls, BlockEls) ->
    make_paraghraph(Els, [], BlockEls ++ [{'p', InlineEls}]).

%%------------------------------------------------------------------------------
%% @doc Check if the HTML tag is in-line or block element.
%% @end
%%------------------------------------------------------------------------------
-spec is_block_element(atom()) -> boolean().
is_block_element('article') -> 'true';
is_block_element('aside') -> 'true';
is_block_element('canvas') -> 'true';
is_block_element('dd') -> 'true';
is_block_element('div') -> 'true';
is_block_element('dl') -> 'true';
is_block_element('dt') -> 'true';
is_block_element('fieldset') -> 'true';
is_block_element('figcaption') -> 'true';
is_block_element('figure') -> 'true';
is_block_element('footer') -> 'true';
is_block_element('form') -> 'true';
is_block_element('h1') -> 'true';
is_block_element('h2') -> 'true';
is_block_element('h3') -> 'true';
is_block_element('h4') -> 'true';
is_block_element('h5') -> 'true';
is_block_element('h6') -> 'true';
is_block_element('header') -> 'true';
is_block_element('hgroup') -> 'true';
is_block_element('hr') -> 'true';
is_block_element('li') -> 'true';
is_block_element('main') -> 'true';
is_block_element('nav') -> 'true';
is_block_element('noscript') -> 'true';
is_block_element('ol') -> 'true';
is_block_element('output') -> 'true';
is_block_element('p') -> 'true';
is_block_element('pre') -> 'true';
is_block_element('section') -> 'true';
is_block_element('table') -> 'true';
is_block_element('tfoot') -> 'true';
is_block_element('ul') -> 'true';
is_block_element('video') -> 'true';
is_block_element(_) -> 'false'.
