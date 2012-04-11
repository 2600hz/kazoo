%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
%% @author Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% @copyright 2010 Erlang Solutions Ltd 
%% @end
%% =====================================================================

%% Description  : Callback module for exporting XML to Markdown.

-module(edown_xmerl).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, find_attribute/2, export_text/1]).

-include_lib("xmerl/include/xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    to_string(Text).

to_string(S) ->
    binary_to_list(iolist_to_binary([S])).

strip(Str) -> lstrip(rstrip(Str)).
lstrip(Str) -> re:replace(Str,"^\\s","",[]).
rstrip(Str) -> re:replace(Str, "\\s\$", []).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, Attrs, [], _E) -> 
    case find_attribute(header, Attrs) of
	{value, Hdr} ->
	    [lists:flatten(io_lib:fwrite("HEADER: ~p~n", [Hdr])), Data];
	false ->
	    Data
    end.

%% Note that SGML does not have the <Tag/> empty-element form.
%% Furthermore, for some element types, the end tag may be forbidden -
%% this can be handled by extending this module - see xmerl_otpsgml.erl
%% for an example. (By default, we always generate the end tag, to make
%% sure that the scope of a markup is not extended by mistake.)

'#element#'('pre_pre', Data, Attrs, Parents, E) ->
    case re:run(Data, "<a href=", []) of
	{match, _} ->
	    '#element#'(pre, Data, Attrs, Parents, E);
	nomatch ->
	    '#element#'(pre, escape_pre(Data), Attrs, Parents, E)
    end;
'#element#'('esc_tt', Data, Attrs, Parents, E) ->
    case within_html(Parents) of
	true ->
	    '#element#'(tt, escape_pre(Data), Attrs, Parents, E);
	false ->
	    '#element#'(tt, Data, Attrs, Parents, E)
    end;
'#element#'('pre', Data, Attrs, Parents, E) ->
    xmerl_html:'#element#'('pre', Data, Attrs, Parents, E);
'#element#'('div', Data, _, _Parents, _E) ->
    %% special case - we use 'div' to enforce html encoding
    Data;
'#element#'('pretext', Data, _Attrs, _Parents, _E) ->
    Data;
'#element#'(a, Data, Attrs, Parents, E) ->
    case edown_lib:redirect_uri(E) of
	false ->
	    elem(a, Data, Attrs, Parents, E);
	#xmlElement{attributes = Attrs1, parents = Parents1} = E1 ->
	    elem(a, Data, Attrs1, Parents1, E1)
    end;
'#element#'(Tag, Data, Attrs, Parents, E) ->
    elem(Tag, Data, Attrs, Parents, E).

%% the alias_for/5 function is used to force HTML rendering of things we
%% know are likely to cause problems with Markdown.
elem(local_defs, Data, Attrs, Parents, E) ->
    alias_for(ul, Data, Attrs, Parents, E);
elem(localdef, Data, Attrs, Parents, E) ->
    alias_for(li, Data, Attrs, Parents, E);
elem(Tag, Data, Attrs, Parents, E) ->
    case needs_html(Tag, Attrs) orelse within_html(Parents) of
	true ->
	    html_elem(Tag, Data, Attrs, Parents, E);
	false ->
	    md_elem(Tag, Data, Attrs, Parents, E)
    end.

escape_pre(Data) ->
    re:replace(re:replace(Data, "<", "\\&lt;", [global]), ">", "\\&gt;", [global]).

alias_for(Tag, Data, Attrs, Parents, E) ->
    xmerl_html:'#element#'(Tag, Data, Attrs, Parents, E#xmlElement{name = Tag}).

html_elem(Tag, Data, Attrs, Parents, E) ->
    HTML = fun() ->
		   xmerl_html:'#element#'(Tag, Data, Attrs, Parents, E)
	   end,
    case within_html(Parents) of
	true ->
	    HTML();
	false ->
	    ["\n\n", HTML(), "\n\n"]
    end.

md_elem(a, Data, Attrs, _Parents, _E) ->
    %% io:fwrite("A TAG = ~p~nPs = ~p~n", [_E, _Parents]),
    case lists:keyfind(href, #xmlAttribute.name, Attrs) of
	#xmlAttribute{value = HRef}  ->
	    ["[", Data, "](", HRef, ")"];
	false ->
	    case lists:keyfind(name, #xmlAttribute.name, Attrs) of
		#xmlAttribute{} ->
		    [ %%"\n",
		     xmerl_lib:start_tag(a,Attrs),
		     Data,
		     xmerl_lib:end_tag(a)
		      %%"\n"]
		      ]
	    end
    end;
md_elem(img, _Data, Attrs, _Parents, _E) ->
    #xmlAttribute{value = Src} = lists:keyfind(src,#xmlAttribute.name,Attrs),
    #xmlAttribute{value = Alt} = lists:keyfind(alt,#xmlAttribute.name,Attrs),
    ["![", Alt, "](", Src, ")"];
md_elem(li, Data, _Attrs, [{ul,_}|_], _E) ->
    ["* ", strip(Data), "\n"];
md_elem(li, Data, _Attrs, [{ol,_}|_], _E) ->
    ["1. ", strip(Data), "\n"];
md_elem(Tag, Data, Attrs, Parents, E) ->
    case Tag of
	title ->
	    %% io:fwrite("TITLE = |~s|~n", [Data]),
	    Str = lists:flatten(Data),
	    [Str, "\n", [$= || _ <- to_string(Str)], "\n"];
	html  -> Data;
	body  -> Data;
	'div' -> Data;
	ul    -> Data;
	ol    -> Data;
	p     -> ["\n\n", Data];
	b     -> ["__", no_nl(Data), "__"];
	em    -> ["_", no_nl(Data), "_"];
	i     -> ["_", no_nl(Data), "_"];
	tt    -> ["`", no_nl(Data), "`"];
	code  -> ["`", no_nl(Data), "`"];
	dl    -> Data;
	dt    -> html_elem(dt, Data, Attrs, Parents, E);
	dd    -> html_elem(dd, Data, Attrs, Parents, E);
	h1 -> ["\n\n#", no_nl(Data), "#\n"];
	h2 -> ["\n\n##", no_nl(Data), "##\n"];
	h3 -> ["\n\n###", no_nl(Data), "##\n"];
	h4 -> ["\n\n####", no_nl(Data), "##\n"];
	hr -> "---------\n";
	head -> [];
	_ ->
		    ["\n",
		     xmerl_lib:start_tag(Tag,Attrs),
		     Data,
		     xmerl_lib:end_tag(Tag),
		    "\n"]
    end.

within_html(Tags) ->
    lists:any(fun({pre,_}) -> true;
		 ({pre_pre,_}) -> true;
		 ({T,_}) -> needs_html(T)
	      end, Tags).

needs_html(T) ->
    needs_html(T, []).

needs_html(T, _Attrs) ->
    lists:member(T, [table,'div',dd,dt,local_defs,localdef]).

no_nl(S) ->
    string:strip([C || C <- to_string(S),
		       C =/= $\n], both).

%% attr(#xmlAttribute{name = N, value = V}) ->
%%     "(" ++ atom_to_list(N) ++ "=" ++ [a_val(V)] ++ ")".

%% a_val(V) when is_atom(V) ->
%%     atom_to_list(V);
%% a_val(V) when is_integer(V) ->
%%     integer_to_list(V);
%% a_val(V) ->
%%     V.




