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

%% @doc Markdown for EDoc - common support functions
%% @end

-module(edown_lib).

-export([export/1, redirect_uri/1, get_attrval/2]).

-include_lib("xmerl/include/xmerl.hrl").


export(Data) ->
    xmerl:export_simple_content(Data, edown_xmerl).


redirect_uri(#xmlElement{} = E) ->
    redirect_uri(get_attrval(href, E), get_attrval(name, E), E);
redirect_uri(_E) ->
    false.

redirect_uri("http://www.erlang.org" ++ _ = URI, _, E) ->
    %% abusing the filename API a little - but whatever works...
    case filename:split(URI) of
	[_,"www.erlang.org","doc","man",_,"doc",Mod] ->
	    NewURI = "http://www.erlang.org/doc/man/" ++ Mod,
	    replace_uri(NewURI, E);
	_Split ->
	    false
    end;
redirect_uri("/" ++ _  = URI, "//" ++ _, E) ->
    case lists:prefix(otp_root(), URI) of
	true ->
	    case lists:reverse(filename:split(URI)) of
		[Mod, "doc", _App | _] ->
		    NewURI = "http://www.erlang.org/doc/man/" ++ Mod,
		    replace_uri(NewURI, E);
		_ ->
		    false
	    end;
	false ->
	    false
    end;
redirect_uri("", _, _) ->
    false;
redirect_uri(Href, _Name, E) ->
    case lists:member("/", Href) of
	false ->
	    [_|_] = URI = get_attrval(href, E),
	    NewURI = re:replace(URI,".html",".md",[{return,list}]),
	    replace_uri(NewURI, E);
	true ->
	    false
    end.

replace_uri(URI, #xmlElement{attributes = As} = E) ->
    #xmlAttribute{} = A = lists:keyfind(href, #xmlAttribute.name, As),
    As1 = lists:keyreplace(href, #xmlAttribute.name, As,
			   A#xmlAttribute{value = URI}),
    E#xmlElement{attributes = As1}.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].


otp_root() ->
    {ok, [[Root]]} = init:get_argument(root),
    Root.
