%%%-------------------------------------------------------------------
%%% File    : openid_utils.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 18 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(openid_utils).

-export([get_tags/2, get_tags/4]).
-export([url_encode/1, url_decode/1]).
-export([uri_encode/1, uri_decode/1]).
-export([normalize_id/1, normalize_http/1]).

-include("openid.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").

get_tags(Content, Tag) ->
    find_tags(Content, {[], Tag, none, none}).

get_tags(Content, Tag, AttrName, AttrVal) ->
    find_tags(Content, {[], Tag, string:to_lower(AttrName), string:to_lower(AttrVal)}).

find_tags("</head>" ++ _Rest, {Buffer,_,_,_}) -> lists:reverse(Buffer);
find_tags("", {Buffer,_,_,_}) -> lists:reverse(Buffer);
find_tags("<" ++ Rest, {_,Tag,_,_}=State) -> read_tag(Rest, Tag, State);
find_tags([_|Rest], State) -> find_tags(Rest, State).

read_tag([$\s|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([$\r|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([$\n|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([$\t|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([], _, State) -> find_tags("", State);
read_tag(Rest, [], State) -> get_tag_content(Rest, State);
read_tag([C1|Rest], [C2|TagRest]=Tag, State) -> 
    case string:to_lower(C1) == string:to_lower(C2) of
        true -> read_tag(Rest, TagRest, State);
        false-> read_tag(Rest, Tag, State)
    end;
read_tag(Rest, _, State) -> skip_tag(Rest, State).

skip_tag([$>|Rest], State) -> find_tags(Rest, State);
skip_tag("", State) -> find_tags("", State);
skip_tag([_|Rest], State) -> skip_tag(Rest, State).

check_attrs(PropList, Tail, {Buffer,Tag,none,none}) ->
    find_tags(Tail, {[PropList|Buffer],Tag,none,none});
check_attrs(PropList, Tail, {_,_,Key,Val}=State) ->
    case ?GVD(Key, PropList, none) of
        none -> find_tags(Tail, State);
        IVal -> check_val(string:to_lower(IVal), Val, PropList, Tail, State)
    end.

get_tag_content(Rest, State) ->
    {Content, Tail} = get_raw_content(Rest, []),
    case re:run(string:to_lower(Content),
                "([a-z0-9-]+)\s*=\s*[\"'](.*?)[\"']", % "
                [{capture, all_but_first, list}, global]) of
        {match, Bits} -> check_attrs([{string:to_lower(K),V} || [K,V] <- Bits], Tail, State);
        _ -> find_tags(Tail, State)
    end.

get_raw_content(">" ++ Tail, Content) -> {lists:reverse(Content), Tail};
get_raw_content([Char|Rest], Bits) -> get_raw_content(Rest, [Char|Bits]).

check_val(V, V, PropList, Tail, {Buffer,Tag,Key,Val})->
    find_tags(Tail, {[PropList|Buffer],Tag,Key,Val});
check_val(_, _, _, Tail, State) ->
    find_tags(Tail, State).

normalize_id(Identifier) ->
    Max = 1000000000,
    Scheme = lists:sublist(Identifier, 8),
    case string:to_lower(Scheme) of
	"xri://" ++ _ ->
	    lists:sublist(Identifier, 7, Max);
	"http://" ++ _ ->
	    Components = lists:sublist(Identifier, 8, Max),
	    normalize_http("http://" ++ Components);
	"https://" ++ _ ->
	    Components = lists:sublist(Identifier, 9, Max),
	    normalize_http("https://" ++ Components);
	[H|_] ->
	    case lists:member(H, ?XRI_GCTX_SYMBOLS) of
		true -> Identifier;
		false -> normalize_http("http://" ++ Identifier)
	    end
    end.

normalize_http(URL) ->
    #url{host=Host, port=Port, username=Username, password=Password, path=Path, protocol=Protocol} = ibrowse_lib:parse_url(URL),
    NewProtocol = atom_to_list(Protocol) ++ "://",
    NewCreds = case {Username, Password} of
		   {undefined, undefined} -> "";
		   {Username, ""} -> Username ++ "@";
		   {Username, Password} -> Username ++ ":" ++ Password ++ "@"
	       end,
    NewHost = normalize_host(Host),
    NewPort = case {Protocol, Port} of
		  {http, 80} -> "";
		  {https, 443} -> "";
		  _ -> ":" ++ integer_to_list(Port)
	      end,
    NewPath = normalize_path(Path),
    NewProtocol ++ NewCreds ++ NewHost ++ NewPort ++ [$/|NewPath].

normalize_host(Host) when is_list(Host) ->
    [ normalize_host(C) || C <- Host ];
normalize_host(C) when is_integer(C) andalso (C >= $A) andalso (C =< $Z) ->
    C + 32;
normalize_host(C) when is_integer(C) -> C.

normalize_path([]) -> "";
normalize_path(Path) when is_list(Path) ->
    FragFreePath = remove_path_fragment(Path),
    {BarePath, QueryString} = lists:splitwith(fun(X) -> X =/= $? end, FragFreePath),
    FinalSlash = hd(lists:reverse(BarePath)) =:= $/,
    NewQueryString = normalise_querystring(QueryString),
    Segments = string:tokens(BarePath, "/"),
    DotFreeSegments = remove_dot_segments(Segments),
    PESegments = pe_normalise_segments(DotFreeSegments),
    NewPath = string:join(PESegments, "/"),
    case NewPath of
	[] -> "" ++ NewQueryString;
	NewPath when FinalSlash -> NewPath ++ "/" ++ NewQueryString;
	NewPath -> NewPath ++ NewQueryString
    end.
    
remove_path_fragment(Path) -> remove_path_fragment(Path, []).
remove_path_fragment([$#|_], SoFar) -> lists:reverse(SoFar);
remove_path_fragment([], SoFar) -> lists:reverse(SoFar);
remove_path_fragment([H|T], SoFar) -> remove_path_fragment(T, [H|SoFar]).

remove_dot_segments(Segments) ->
    remove_dot_segments([], Segments).

remove_dot_segments(Path, []) ->
    lists:reverse(Path);
remove_dot_segments([_|Path], [".."|Rest]) ->
    remove_dot_segments(Path, Rest);
remove_dot_segments(Path, ["."|Rest]) ->
    remove_dot_segments(Path, Rest);
remove_dot_segments(Path, [Seg|Rest]) ->
    remove_dot_segments([Seg|Path], Rest).

pe_normalise_segments(Segments) when is_list(Segments) ->
    [ pe_normalise_segment(Segment) || Segment <- Segments ].
pe_normalise_segment(Segment) ->
    RemovePE = uri_decode(Segment),
    openid_utils:uri_encode(RemovePE).

normalise_querystring("") -> "";
normalise_querystring([$?|QS]) ->
    Params = string:tokens(QS, "&"),
    NewParams = lists:foldr(
		  fun(Param, Acc) ->
			  NewParam = case lists:splitwith(fun(X) -> X =/= $= end, Param) of
					 {Key, ""} -> uri_encode(uri_decode(Key));
					 {Key, [$=|Value]} ->
					     NewKey = uri_encode(uri_decode(Key)),
					     NewValue = uri_encode(uri_decode(Value)),
					     NewKey ++ "=" ++ NewValue
				     end,
			  [NewParam|Acc]
		  end, [], Params),
    NewQS = string:join(NewParams, "&"),
    "?" ++ NewQS.

%% Sourced with permission from Tim's repo at
%% http://github.com/tim/erlang-percent-encoding/blob/master/src/percent.erl

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

%%
%% Percent encoding/decoding as defined by the application/x-www-form-urlencoded
%% content type (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1).
%%

url_encode(Str) when is_list(Str) ->
  url_encode(lists:reverse(Str, []), []).

url_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $. ->
  url_encode(T, [X | Acc]);
url_encode([32 | T], Acc) ->
  url_encode(T, [$+ | Acc]);
url_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr_encode(X bsr 4), hexchr_encode(X band 16#0f) | Acc],
  url_encode(T, NewAcc);
url_encode([], Acc) ->
  Acc.

url_decode(Str) when is_list(Str) ->
  url_decode(Str, []).

url_decode([$+ | T], Acc) ->
  url_decode(T, [32 | Acc]);
url_decode([$%, A, B | T], Acc) ->
  Char = (hexchr_decode(A) bsl 4) + hexchr_decode(B),
  url_decode(T, [Char | Acc]);
url_decode([X | T], Acc) ->
  url_decode(T, [X | Acc]);
url_decode([], Acc) ->
  lists:reverse(Acc, []).

%%
%% Percent encoding/decoding as defined by RFC 3986 (http://tools.ietf.org/html/rfc3986).
%%

uri_encode(Str) when is_list(Str) ->
  uri_encode(lists:reverse(Str, []), []).

uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr_encode(X bsr 4), hexchr_encode(X band 16#0f) | Acc],
  uri_encode(T, NewAcc);
uri_encode([], Acc) ->
  Acc.

uri_decode(Str) when is_list(Str) ->
  uri_decode(Str, []).

uri_decode([$%, A, B | T], Acc) ->
  Char = (hexchr_decode(A) bsl 4) + hexchr_decode(B),
  uri_decode(T, [Char | Acc]);
uri_decode([X | T], Acc) ->
  uri_decode(T, [X | Acc]);
uri_decode([], Acc) ->
  lists:reverse(Acc, []).

%%
%% Helper functions.
%%

-compile({inline, [{hexchr_encode, 1}, {hexchr_decode, 1}]}).

hexchr_encode(N) when N >= 10 andalso N < 16 ->
  N + $A - 10;
hexchr_encode(N) when N >= 0 andalso N < 10 ->
  N + $0.

hexchr_decode(C) when C >= $a andalso C =< $f ->
  C - $a + 10;
hexchr_decode(C) when C >= $A andalso C =< $F ->
  C - $A + 10;
hexchr_decode(C) when C >= $0 andalso C =< $9 ->
  C - $0.
