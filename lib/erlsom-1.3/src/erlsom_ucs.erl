%%% -*- Erlang -*-
%%%-------------------------------------------------------------------
%%% Author: Lon Willett <Lon.Willett@sse.ie>
%%%
%%% Description: Some minimal support for encoding, decoding, and
%%% manipulating strings of ISO-10646 characters (i.e. Unicode).
%%%-------------------------------------------------------------------


%% NOTICE: This is just an excerpt of the original ucs application

%% This is a copy from xmerl_ucs, but it has
%% been modified to handle the case that a block of data ends in the middle 
%% of a group of bytes that make up 1 character. In such a case the 
%% bytes that belong to the incomplete character are passed back, so that
%% they can be put in front of the next block of data.

%% the function 'to_utf8' is an exact copy.

-module(erlsom_ucs).
-vsn('0.3').
-author('Lon.Willett@sse.ie').
-modified_by('johan.blom@mobilearts.se').
-modified_by('w.a.de.jong@gmail.com').
-compile([verbose,report_warnings,warn_unused_vars]).


-export([to_utf8/1, from_utf8/1]).
-export([decode_utf8/1]).
-export([char_to_utf8/1]).

-export([from_utf16be/1, from_utf16le/1]).

%% TODO: relpace this by something a bit more efficient
decode_utf8(Utf8) ->
  case from_utf8(Utf8) of
    {String, []} -> String;
    _ -> error
  end.


%% from_utf8([Byte]) -> {[UnicodeChar], Tail}
%% Decode UTF-8 encoded character-strings. 
%%
%% Modification (WdJ): Added an output parameter (Tail): 
%% If the string ends in the middle of a character, the bytes 
%% of that incomplete character are returned (if not, the new 
%% parameter has value []). 
%% The goal is to allow parsing of data in arbitrary blocks.

from_utf8(Bin) when is_binary(Bin) -> 
  from_utf8(binary_to_list(Bin));

from_utf8(List) -> 
    case expand_utf8(List) of
	{Result, Rest, 0} -> 
          %% case Rest of
            %% [] -> ok;
            %% _ -> io:format("Rest: ~p~n", [Rest])
          %% end,
          {Result, Rest};
	{_Res,_Rest, _NumBadChar} ->
	    exit({ucs,{bad_utf8_character_code}})
    end.

%% expand_utf8([Byte]) -> {[UnicodeChar], Tail, NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%% Modification (WdJ): Added an output parameter (Tail): 
%% If the string ends in the middle of a character, the bytes 
%% of that incomplete character are returned (if not, the new 
%% parameter has value []). 
%% The goal is to allow parsing of data in arbitrary blocks.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".
expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
					 C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when 16#80 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1], Acc, Bad) when C1 band 16#E0 =:= 16#C0 ->
  {lists:reverse(Acc), [C1], Bad};
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
					    C2 band 16#C0 =:= 16#80,
					    C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when 16#800 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1], Acc, Bad) when C1 band 16#F0 =:= 16#E0 ->
  {lists:reverse(Acc), [C1], Bad};
expand_utf8_1([C1,C2], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
				      C2 band 16#C0 =:= 16#80 ->
  {lists:reverse(Acc), [C1, C2], Bad};
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
					       C2 band 16#C0 =:= 16#80,
					       C3 band 16#C0 =:= 16#80,
					       C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when 16#10000 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1], Acc, Bad) when C1 band 16#F8 =:= 16#F0 ->
  {lists:reverse(Acc), [C1], Bad};
expand_utf8_1([C1,C2], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
			              C2 band 16#C0 =:= 16#80 ->
  {lists:reverse(Acc), [C1, C2], Bad};
expand_utf8_1([C1,C2,C3], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
					       C2 band 16#C0 =:= 16#80,
					       C3 band 16#C0 =:= 16#80 ->
  {lists:reverse(Acc), [C1, C2, C3], Bad};
expand_utf8_1([_Bad|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc), [], Bad}.

%% from_utf16be(List) -> {[UnicodeChar], Tail, NumberOfBadBytes}
%%  Expand UTF16 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%% Modification (WdJ): Added an output parameter (Tail): 
%% If the string ends in the middle of a character, the bytes 
%% of that incomplete character are returned (if not, the new 
%% parameter has value <<>>). 
%% The goal is to allow parsing of data in arbitrary blocks.
%% Also: changed to work on lists in stead of binaries.
from_utf16be(Bin) when is_binary(Bin) -> from_utf16be(binary_to_list(Bin),[]);
from_utf16be(List) -> from_utf16be(List,[]).

from_utf16be([_Byte] = Rest, Acc) ->
  {lists:reverse(Acc), Rest};
%% from_utf16be(<<Ch:16/big-unsigned-integer, Rest/binary>>, Acc)
  %% when Ch < 16#D800; Ch > 16#DFFF ->
    %% if Ch < 16#FFFE -> from_utf16be(Rest,[Ch|Acc]) end;
from_utf16be([Byte1, Byte2 | Rest], Acc)
  when Byte1 < 16#D8; Byte1 > 16#DF ->
    Ch = Byte1 * 256 + Byte2,
    if Ch < 16#FFFE -> from_utf16be(Rest,[Ch|Acc]) end;
%% from_utf16be(<<Hi:16/big-unsigned-integer, Lo:16/big-unsigned-integer,
	       %% Rest/binary>>, Acc)
  %% when Hi >= 16#D800, Hi < 16#DC00, Lo >= 16#DC00, Lo =< 16#DFFF ->
from_utf16be([Hi1, Hi2, Lo1, Lo2 | Rest], Acc)
  when Hi1 >= 16#D8, Hi1 < 16#DC, Lo1 >= 16#DC, Lo1 < 16#E0 ->
    %% Surrogate pair
    Hi = Hi1 * 256 + Hi2,
    Lo = Lo1 * 256 + Lo2,
    Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    from_utf16be(Rest, [Ch|Acc]);
from_utf16be([Hi1, _Hi2] = Rest, Acc)
  when Hi1 >= 16#D8, Hi1 < 16#DC ->
    %% Surrogate pair, incomplete
    {lists:reverse(Acc), Rest};
from_utf16be([Hi1, _Hi2, _Byte] = Rest, Acc)
  when Hi1 >= 16#D8, Hi1 < 16#DC ->
    %% Surrogate pair, incomplete
    {lists:reverse(Acc), Rest};
from_utf16be([],Acc) ->
    {lists:reverse(Acc), []};
from_utf16be(_List,_Acc) ->
    {error,not_utf16be}.

from_utf16le(Bin) when is_binary(Bin) -> from_utf16le(binary_to_list(Bin),[]);
from_utf16le(List) -> from_utf16le(List,[]).

from_utf16le([_Byte] = Rest, Acc) ->
  {lists:reverse(Acc), Rest};
%% from_utf16le(<<Ch:16/little-unsigned-integer, Rest/binary>>, Acc)
  %% when Ch < 16#D800; Ch > 16#DFFF ->
    %% if Ch < 16#FFFE -> from_utf16le(Rest, [Ch|Acc]) end;
from_utf16le([Byte1, Byte2 | Rest], Acc)
  when Byte2 < 16#D8; Byte2 > 16#DF ->
    Ch = Byte2 * 256 + Byte1,
    if Ch < 16#FFFE -> from_utf16le(Rest,[Ch|Acc]) end;
%% from_utf16le(<<Hi:16/little-unsigned-integer, Lo:16/little-unsigned-integer,
	       %% Rest/binary>>, Acc)
  %% when Hi >= 16#D800, Hi < 16#DC00, Lo >= 16#DC00, Lo =< 16#DFFF ->
    %% %% Surrogate pair
    %% Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    %% from_utf16le(Rest, [Ch|Acc]);
from_utf16le([Hi1, Hi2, Lo1, Lo2 | Rest], Acc)
  when Hi2 >= 16#D8, Hi2 < 16#DC, Lo2 >= 16#DC, Lo2 < 16#E0 ->
    %% Surrogate pair
    Hi = Hi2 * 256 + Hi1,
    Lo = Lo2 * 256 + Lo1,
    Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    from_utf16le(Rest, [Ch|Acc]);
%% from_utf16le(<<Hi:16/little-unsigned-integer>> = Rest, Acc)
  %% when Hi >= 16#D800, Hi < 16#DC00 ->
    %% %% Surrogate pair, incomplete
    %% {lists:reverse(Acc), Rest};
from_utf16le([_Hi1, Hi2] = Rest, Acc)
  when Hi2 >= 16#D8, Hi2 < 16#DC ->
    %% Surrogate pair, incomplete
    {lists:reverse(Acc), Rest};
%% from_utf16le(<<Hi:16/little-unsigned-integer, _Byte>> = Rest, Acc)
  %% when Hi >= 16#D800, Hi < 16#DC00 ->
    %% %% Surrogate pair, incomplete
    %% {lists:reverse(Acc), Rest};
from_utf16le([_Hi1, Hi2, _Byte] = Rest, Acc)
  when Hi2 >= 16#D8, Hi2 < 16#DC ->
    %% Surrogate pair, incomplete
    {lists:reverse(Acc), Rest};
from_utf16le([],Acc) ->
    {lists:reverse(Acc), []};
from_utf16le(_Bin,_Acc) ->
    {error,not_utf16le}.

%%% UTF-8 encoding and decoding
%% TODO: isn't this very inefficient? Building all these lists?
to_utf8(List) when is_list(List) -> lists:flatmap(fun to_utf8/1, List);
to_utf8(Ch) -> char_to_utf8_list(Ch).

%% TODO: this is probably not the best way to do this.
char_to_utf8(Char) ->
  list_to_binary(char_to_utf8_list(Char)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UTF-8 support
%%% Possible errors encoding UTF-8:
%%%	- Non-character values (something other than 0 .. 2^31-1).
%%%	- Surrogate pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
%%% Possible errors decoding UTF-8:
%%%	- 10xxxxxx or 1111111x as initial byte.
%%%	- Insufficient number of 10xxxxxx octets following an initial octet of
%%%	multi-octet sequence.
%%% 	- Non-canonical encoding used.
%%%	- Surrogate-pair code encoded as UTF-8.
%%%	- 16#FFFE or 16#FFFF character in string.
char_to_utf8_list(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch < 128 ->
	    %% 0yyyyyyy
	    [Ch];
       Ch < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    [16#C0 + (Ch bsr 6),
	     128+(Ch band 16#3F)];
       Ch < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
		    [16#E0 + (Ch bsr 12),
		     128+((Ch bsr 6) band 16#3F),
		     128+(Ch band 16#3F)]
	    end;
       Ch < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    [16#F0+(Ch bsr 18),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#F8+(Ch bsr 24),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#FC+(Ch bsr 30),
	     128+((Ch bsr 24) band 16#3F),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)]
    end.

