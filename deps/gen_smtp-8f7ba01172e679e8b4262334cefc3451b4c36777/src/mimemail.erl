%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc A module for decoding/encoding MIME 1.0 email.
%% The encoder and decoder operate on the same datastructure, which is as follows:
%% A 5-tuple with the following elements: `{Type, SubType, Headers, Parameters, Body}'.
%%
%% `Type' and `SubType' are the MIME type of the email, examples are `text/plain' or
%% `multipart/alternative'. The decoder splits these into 2 fields so you can filter by
%% the main type or by the subtype.
%%
%% `Headers' consists of a list of key/value pairs of binary values eg.
%% `{<<"From">>, <<"Andrew Thompson <andrew@hijacked.us>">>}'. There is no parsing of
%% the header aside from un-wrapping the lines and splitting the header name from the
%% header value.
%%
%% `Parameters' is a list of 3 key/value tuples. The 3 keys are `<<"content-type-params">>',
%% `<<"dispisition">>' and `<<"disposition-params">>'.
%% `content-type-params' is a key/value list of parameters on the content-type header, this
%% usually consists of things like charset and the format parameters. `disposition' indicates
%% how the data wants to be displayed, this is usually 'inline'. `disposition-params' is a list of
%% disposition information, eg. the filename this section should be saved as, the modification
%% date the file should be saved with, etc.
%%
%% Finally, `Body' can be one of several different types, depending on the structure of the email.
%% For a simple email, the body will usually be a binary consisting of the message body, In the
%% case of a multipart email, its a list of these 5-tuple MIME structures. The third possibility,
%% in the case of a message/rfc822 attachment, body can be a single 5-tuple MIME structure.
%%
%% You should see the relevant RFCs (2045, 2046, 2047, etc.) for more information.
-module(mimemail).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encode/1, encode/2, decode/2, decode/1, get_header_value/2, get_header_value/3, parse_headers/1]).

-define(DEFAULT_OPTIONS, [
		{encoding, get_default_encoding()}, % default encoding is utf-8 if we can find the iconv module
		{decode_attachments, true} % should we decode any base64/quoted printable attachments?
	]).

-type(mimetuple() :: {binary(), binary(), [{binary(), binary()}], [{binary(), binary()}], binary() | [{binary(), binary(), [{binary(), binary()}], [{binary(), binary()}], binary() | [tuple()]}] | tuple()}).

-export_type([mimetuple/0]).

-type(options() :: [{'encoding', binary()} | {'decode_attachment', boolean()}]).

-spec(decode/1 :: (Email :: binary()) -> mimetuple()).
%% @doc Decode a MIME email from a binary.
decode(All) ->
	{Headers, Body} = parse_headers(All),
	decode(Headers, Body, ?DEFAULT_OPTIONS).

-spec(decode/2 :: (Email :: binary(), Options :: options()) -> mimetuple()).
%% @doc Decode with custom options
decode(All, Options) when is_binary(All), is_list(Options) ->
	{Headers, Body} = parse_headers(All),
	decode(Headers, Body, Options).

decode(OrigHeaders, Body, Options) ->
	%io:format("headers: ~p~n", [Headers]),
	Encoding = proplists:get_value(encoding, Options, none),
	%FixedHeaders = fix_headers(Headers),
	Headers = decode_headers(OrigHeaders, [], Encoding),
	case parse_with_comments(get_header_value(<<"MIME-Version">>, Headers)) of
		undefined ->
			case parse_content_type(get_header_value(<<"Content-Type">>, Headers)) of
				{<<"multipart">>, _SubType, _Parameters} ->
					erlang:error(non_mime_multipart);
				{Type, SubType, Parameters} ->
					NewBody = decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers),
						Body, proplists:get_value(<<"charset">>, Parameters), Encoding),
					{Type, SubType, Headers, Parameters, NewBody};
				undefined ->
					Parameters = [{<<"content-type-params">>, [{<<"charset">>, <<"us-ascii">>}]}, {<<"disposition">>, <<"inline">>}, {<<"disposition-params">>, []}],
					{<<"text">>, <<"plain">>, Headers, Parameters, decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers), Body)}
			end;
		Other ->
			decode_component(Headers, Body, Other, Options)
	end.

-spec(encode/1 :: (MimeMail :: mimetuple()) -> binary()).
encode(MimeMail) ->
	encode(MimeMail, []).

%% @doc Encode a MIME tuple to a binary.
encode({Type, Subtype, Headers, ContentTypeParams, Parts}, Options) ->
	{FixedParams, FixedHeaders} = ensure_content_headers(Type, Subtype, ContentTypeParams, Headers, Parts, true),
	CheckedHeaders = check_headers(FixedHeaders),
	EncodedBody = binstr:join(
					encode_component(Type, Subtype, CheckedHeaders, FixedParams, Parts),
					"\r\n"),
	EncodedHeaders = encode_headers(CheckedHeaders),
	SignedHeaders = case proplists:get_value(dkim, Options) of
						undefined -> EncodedHeaders;
						DKIM -> dkim_sign_email(EncodedHeaders, EncodedBody, DKIM)
					end,
	list_to_binary([binstr:join(SignedHeaders, "\r\n"),
					"\r\n\r\n",
					EncodedBody]);
encode(_, _) ->
	io:format("Not a mime-decoded DATA~n"),
	erlang:error(non_mime).


decode_headers(Headers, _, none) ->
	Headers;
decode_headers([], Acc, _Charset) ->
	lists:reverse(Acc);
decode_headers([{Key, Value} | Headers], Acc, Charset) ->
	decode_headers(Headers, [{Key, decode_header(Value, Charset)} | Acc], Charset).

decode_header(Value, Charset) ->
	RTokens = tokenize_header(Value, []),
	Tokens = lists:reverse(RTokens),
	Decoded = try decode_header_tokens_strict(Tokens, Charset)
			  catch Type:Reason ->
					  case decode_header_tokens_permissive(Tokens, Charset, []) of
						  {ok, Dec} -> Dec;
						  error ->
							  % re-throw original error
							  % may also use erlang:raise/3 to preserve original traceback
							  erlang:Type(Reason)
					  end
			  end,
	iolist_to_binary(Decoded).

-type hdr_token() :: binary() | {Encoding::binary(), Data::binary()}.
-spec tokenize_header(binary(), [hdr_token()]) -> [hdr_token()].
tokenize_header(<<>>, Acc) ->
    Acc;
tokenize_header(Value, Acc) ->
	%% maybe replace "?([^\s]+)\\?" with "?([^\s]*)\\?"?
	%% see msg lvuvmm593b8s7pqqfhu7cdtqd4g4najh
	%% Subject: =?utf-8?Q??=
	%%	=?utf-8?Q?=D0=9F=D0=BE=D0=B4=D1=82=D0=B2=D0=B5=D1=80=D0=B4=D0=B8=D1=82=D0=B5=20?=
	%%	=?utf-8?Q?=D1=80=D0=B5=D0=B3=D0=B8=D1=81=D1=82=D1=80=D0=B0=D1=86=D0=B8=D1=8E=20?=
	%%	=?utf-8?Q?=D0=B2=20Moy-Rebenok.ru?=

	case re:run(Value, "=\\?([-A-Za-z0-9_]+)\\?([qQbB])\\?([^\s]+)\\?=", [ungreedy]) of
		nomatch ->
			[Value | Acc];
		{match,[{AllStart, AllLen},{EncodingStart, EncodingLen},{TypeStart, _},{DataStart, DataLen}]} ->
			%% RFC 2047 #2 (encoded-word)
			Encoding = binstr:substr(Value, EncodingStart+1, EncodingLen),
			Type = binstr:to_lower(binstr:substr(Value, TypeStart+1, 1)),
			Data = binstr:substr(Value, DataStart+1, DataLen),

			EncodedData =
				case Type of
					<<"q">> ->
						%% RFC 2047 #5. (3)
						decode_quoted_printable(re:replace(Data, "_", " ", [{return, binary}, global]));
					<<"b">> ->
						decode_base64(re:replace(Data, "_", " ", [{return, binary}, global]))
				end,

			%% iconv:close(CD),


			Offset = case re:run(binstr:substr(Value, AllStart + AllLen + 1), "^([\s\t\n\r]+)=\\?[-A-Za-z0-9_]+\\?[^\s]\\?[^\s]+\\?=", [ungreedy]) of
				nomatch ->
					% no 2047 block immediately following
					1;
				{match,[{_, _},{_, WhiteSpaceLen}]} ->
					1+ WhiteSpaceLen
			end,

			NewAcc = case binstr:substr(Value, 1, AllStart) of
						 <<>> -> [{fix_encoding(Encoding), EncodedData} | Acc];
						 Other -> [{fix_encoding(Encoding), EncodedData}, Other | Acc]
					 end,
			tokenize_header(binstr:substr(Value, AllStart + AllLen + Offset), NewAcc)
	end.


decode_header_tokens_strict([], _) ->
	[];
decode_header_tokens_strict([{Encoding, Data} | Tokens], Charset) ->
	{ok, S} = convert(Charset, Encoding, Data),
	[S | decode_header_tokens_strict(Tokens, Charset)];
decode_header_tokens_strict([Data | Tokens], Charset) ->
	[Data | decode_header_tokens_strict(Tokens, Charset)].

%% this decoder can handle folded not-by-RFC UTF headers, when somebody split
%% multibyte string not by characters, but by bytes. It first join folded
%% string and only then decode it with iconv.
decode_header_tokens_permissive([], _, [Result]) when is_binary(Result) ->
	{ok, Result};
decode_header_tokens_permissive([], _, Stack) ->
	case lists:all(fun erlang:is_binary/1, Stack) of
		true -> {ok, lists:reverse(Stack)};
		false  -> error
	end;
decode_header_tokens_permissive([{Enc, Data} | Tokens], Charset, [{Enc, PrevData} | Stack]) ->
	NewData = iolist_to_binary([PrevData, Data]),
	case convert(Charset, Enc, NewData) of
		{ok, S} ->
			decode_header_tokens_permissive(Tokens, Charset, [S | Stack]);
		_ ->
			decode_header_tokens_permissive(Tokens, Charset, [{Enc, NewData} | Stack])
	end;
decode_header_tokens_permissive([NextToken | _] = Tokens, Charset, [{_, _} | Stack])
  when is_binary(NextToken) orelse is_tuple(NextToken) ->
	%% practicaly very rare case "=?utf-8?Q?BROKEN?=\r\n\t=?windows-1251?Q?maybe-broken?="
	%% or "=?utf-8?Q?BROKEN?= raw-ascii-string"
	%% drop broken value from stack
	decode_header_tokens_permissive(Tokens, Charset, Stack);
decode_header_tokens_permissive([Data | Tokens], Charset, Stack) ->
	decode_header_tokens_permissive(Tokens, Charset, [Data | Stack]).


convert(To, From, Data) ->
	CD = case iconv:open(To, From) of
			 {ok, Res} -> Res;
			 {error, einval} -> throw({bad_charset, From})
		 end,
	Converted = iconv:conv(CD, Data),
	iconv:close(CD),
	Converted.


decode_component(Headers, Body, MimeVsn, Options) when MimeVsn =:= <<"1.0">> ->
	case parse_content_disposition(get_header_value(<<"Content-Disposition">>, Headers)) of
		{Disposition, DispositionParams} ->
			ok;
		_ -> % defaults
			Disposition = <<"inline">>,
			DispositionParams = []
	end,

	case parse_content_type(get_header_value(<<"Content-Type">>, Headers)) of
		{<<"multipart">>, SubType, Parameters} ->
			case proplists:get_value(<<"boundary">>, Parameters) of
				undefined ->
					erlang:error(no_boundary);
				Boundary ->
					% io:format("this is a multipart email of type:  ~s and boundary ~s~n", [SubType, Boundary]),
					Parameters2 = [{<<"content-type-params">>, Parameters}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
					{<<"multipart">>, SubType, Headers, Parameters2, split_body_by_boundary(Body, list_to_binary(["--", Boundary]), MimeVsn, Options)}
			end;
		{<<"message">>, <<"rfc822">>, Parameters} ->
			{NewHeaders, NewBody} = parse_headers(Body),
			Parameters2 = [{<<"content-type-params">>, Parameters}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
			{<<"message">>, <<"rfc822">>, Headers, Parameters2, decode(NewHeaders, NewBody, Options)};
		{Type, SubType, Parameters} ->
			%io:format("body is ~s/~s~n", [Type, SubType]),
			Parameters2 = [{<<"content-type-params">>, Parameters}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
			{Type, SubType, Headers, Parameters2, decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers), Body, proplists:get_value(<<"charset">>, Parameters), proplists:get_value(encoding, Options, none))};
		undefined -> % defaults
			Type = <<"text">>,
			SubType = <<"plain">>,
			Parameters = [{<<"content-type-params">>, [{<<"charset">>, <<"us-ascii">>}]}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
			{Type, SubType, Headers, Parameters, decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers), Body)}
	end;
decode_component(_Headers, _Body, Other, _Options) ->
	erlang:error({mime_version, Other}).

-spec(get_header_value/3 :: (Needle :: binary(), Headers :: [{binary(), binary()}], Default :: any()) -> binary() | any()).
%% @doc Do a case-insensitive header lookup to return that header's value, or the specified default.
get_header_value(Needle, Headers, Default) ->
	%io:format("Headers: ~p~n", [Headers]),
	F =
	fun({Header, _Value}) ->
			binstr:to_lower(Header) =:= binstr:to_lower(Needle)
	end,
	case lists:filter(F, Headers) of
		% TODO if there's duplicate headers, should we use the first or the last?
		[{_Header, Value}|_T] ->
			Value;
		_ ->
			Default
	end.

-spec(get_header_value/2 :: (Needle :: binary(), Headers :: [{binary(), binary()}]) -> binary() | 'undefined').
%% @doc Do a case-insensitive header lookup to return the header's value, or `undefined'.
get_header_value(Needle, Headers) ->
	get_header_value(Needle, Headers, undefined).

-spec parse_with_comments(Value :: binary()) -> binary() | no_return();
	(Value :: atom()) -> atom().
parse_with_comments(Value) when is_binary(Value) ->
	parse_with_comments(Value, [], 0, false);
parse_with_comments(Value) ->
	Value.

-spec parse_with_comments(Value :: binary(), Acc :: list(), Depth :: non_neg_integer(), Quotes :: boolean()) -> binary() | no_return().
parse_with_comments(<<>>, _Acc, _Depth, Quotes) when Quotes ->
	erlang:error(unterminated_quotes);
parse_with_comments(<<>>, _Acc, Depth, _Quotes) when Depth > 0 ->
	erlang:error(unterminated_comment);
parse_with_comments(<<>>, Acc, _Depth, _Quotes) ->
	binstr:strip(list_to_binary(lists:reverse(Acc)));
parse_with_comments(<<$\\, H, Tail/binary>>, Acc, Depth, Quotes) when Depth > 0, H > 32, H < 127 ->
	parse_with_comments(Tail, Acc, Depth, Quotes);
parse_with_comments(<<$\\, Tail/binary>>, Acc, Depth, Quotes) when Depth > 0 ->
	parse_with_comments(Tail, Acc, Depth, Quotes);
parse_with_comments(<<$\\, H, Tail/binary>>, Acc, Depth, Quotes) when H > 32, H < 127 ->
	parse_with_comments(Tail, [H | Acc], Depth, Quotes);
parse_with_comments(<<$\\, Tail/binary>>, Acc, Depth, Quotes) ->
	parse_with_comments(Tail, [$\\ | Acc], Depth, Quotes);
parse_with_comments(<<$(, Tail/binary>>, Acc, Depth, Quotes) when not Quotes ->
	parse_with_comments(Tail, Acc, Depth + 1, Quotes);
parse_with_comments(<<$), Tail/binary>>, Acc, Depth, Quotes) when Depth > 0, not Quotes ->
	parse_with_comments(Tail, Acc, Depth - 1, Quotes);
parse_with_comments(<<_, Tail/binary>>, Acc, Depth, Quotes) when Depth > 0 ->
	parse_with_comments(Tail, Acc, Depth, Quotes);
parse_with_comments(<<$", T/binary>>, Acc, Depth, true) -> %"
	parse_with_comments(T, Acc, Depth, false);
parse_with_comments(<<$", T/binary>>, Acc, Depth, false) -> %"
	parse_with_comments(T, Acc, Depth, true);
parse_with_comments(<<H, Tail/binary>>, Acc, Depth, Quotes) ->
	parse_with_comments(Tail, [H | Acc], Depth, Quotes).

-spec(parse_content_type/1 :: (Value :: 'undefined') -> 'undefined';
	(Value :: binary()) -> {binary(), binary(), [{binary(), binary()}]}).
parse_content_type(undefined) ->
	undefined;
parse_content_type(String) ->
	try parse_content_disposition(String) of
		{RawType, Parameters} ->
			case binstr:strchr(RawType, $/) of
				Index when Index < 2 ->
					throw(bad_content_type);
				Index ->
					Type = binstr:substr(RawType, 1, Index - 1),
					SubType = binstr:substr(RawType, Index + 1),
					{binstr:to_lower(Type), binstr:to_lower(SubType), Parameters}
			end
		catch
			bad_disposition ->
				throw(bad_content_type)
	end.

-spec(parse_content_disposition/1 :: (Value :: 'undefined') -> 'undefined';
	(String :: binary()) -> {binary(), [{binary(), binary()}]}).
parse_content_disposition(undefined) ->
	undefined;
parse_content_disposition(String) ->
	[Disposition | Parameters] = binstr:split(parse_with_comments(String), <<";">>),
	F =
	fun(X) ->
		Y = binstr:strip(binstr:strip(X), both, $\t),
		case binstr:strchr(Y, $=) of
			Index when Index < 2 ->
				throw(bad_disposition);
			Index ->
				Key = binstr:substr(Y, 1, Index - 1),
				Value = binstr:substr(Y, Index + 1),
				{binstr:to_lower(Key), Value}
		end
	end,
	Params = lists:map(F, Parameters),
	{binstr:to_lower(Disposition), Params}.

split_body_by_boundary(Body, Boundary, MimeVsn, Options) ->
	% find the indices of the first and last boundary
	case [binstr:strpos(Body, Boundary), binstr:strpos(Body, list_to_binary([Boundary, "--"]))] of
		[0, _] ->
			erlang:error(missing_boundary);
		[_, 0] ->
			erlang:error(missing_last_boundary);
		[Start, End] ->
			NewBody = binstr:substr(Body, Start + byte_size(Boundary), End - Start),
			% from now on, we can be sure that each boundary is preceeded by a CRLF
			Parts = split_body_by_boundary_(NewBody, list_to_binary(["\r\n", Boundary]), [], Options),
			[decode_component(Headers, Body2, MimeVsn, Options) || {Headers, Body2} <- [V || {_, Body3} = V <- Parts, byte_size(Body3) =/= 0]]
		end.

split_body_by_boundary_(<<>>, _Boundary, Acc, _Options) ->
	lists:reverse(Acc);
split_body_by_boundary_(Body, Boundary, Acc, Options) ->
	% trim the incomplete first line
	TrimmedBody = binstr:substr(Body, binstr:strpos(Body, "\r\n") + 2),
	case binstr:strpos(TrimmedBody, Boundary) of
		0 ->
			lists:reverse([{[], TrimmedBody} | Acc]);
		Index ->
			{ParsedHdrs, BodyRest} = parse_headers(binstr:substr(TrimmedBody, 1, Index - 1)),
			DecodedHdrs = decode_headers(ParsedHdrs, [], proplists:get_value(encoding, Options, none)),
			split_body_by_boundary_(binstr:substr(TrimmedBody, Index + byte_size(Boundary)), Boundary,
									[{DecodedHdrs, BodyRest} | Acc], Options)
	end.

-spec(parse_headers/1 :: (Body :: binary()) -> {[{binary(), binary()}], binary()}).
%% @doc Parse the headers off of a message and return a list of headers and the trailing body.
parse_headers(Body) ->
	case binstr:strpos(Body, "\r\n") of
		0 ->
			{[], Body};
		1 ->
			{[], binstr:substr(Body, 3)};
		Index ->
			parse_headers(binstr:substr(Body, Index+2), binstr:substr(Body, 1, Index - 1), [])
	end.


parse_headers(Body, <<H, Tail/binary>>, []) when H =:= $\s; H =:= $\t ->
	% folded headers
	{[], list_to_binary([H, Tail, "\r\n", Body])};
parse_headers(Body, <<H, T/binary>>, Headers) when H =:= $\s; H =:= $\t ->
	% folded headers
	[{FieldName, OldFieldValue} | OtherHeaders] = Headers,
	FieldValue = list_to_binary([OldFieldValue, T]),
	%io:format("~p = ~p~n", [FieldName, FieldValue]),
	case binstr:strpos(Body, "\r\n") of
		0 ->
			{lists:reverse([{FieldName, FieldValue} | OtherHeaders]), Body};
		1 ->
			{lists:reverse([{FieldName, FieldValue} | OtherHeaders]), binstr:substr(Body, 3)};
		Index2 ->
			parse_headers(binstr:substr(Body, Index2 + 2), binstr:substr(Body, 1, Index2 - 1), [{FieldName, FieldValue} | OtherHeaders])
	end;
parse_headers(Body, Line, Headers) ->
	%io:format("line: ~p, nextpart ~p~n", [Line, binstr:substr(Body, 1, 10)]),
	case binstr:strchr(Line, $:) of
		0 ->
			{lists:reverse(Headers), list_to_binary([Line, "\r\n", Body])};
		Index ->
			FieldName = binstr:substr(Line, 1, Index - 1),
			F = fun(X) -> X > 32 andalso X < 127 end,
			case binstr:all(F, FieldName) of
				true ->
					F2 = fun(X) -> (X > 31 andalso X < 127) orelse X == 9 end,
					FValue = binstr:strip(binstr:substr(Line, Index+1)),
					FieldValue = case binstr:all(F2, FValue) of
						true ->
							FValue;
						_ ->
							% I couldn't figure out how to use a pure binary comprehension here :(
							list_to_binary([ filter_non_ascii(C) || <<C:8>> <= FValue])
					end,
					case binstr:strpos(Body, "\r\n") of
						0 ->
							{lists:reverse([{FieldName, FieldValue} | Headers]), Body};
						1 ->
							{lists:reverse([{FieldName, FieldValue} | Headers]), binstr:substr(Body, 3)};
						Index2 ->
							parse_headers(binstr:substr(Body, Index2 + 2), binstr:substr(Body, 1, Index2 - 1), [{FieldName, FieldValue} | Headers])
					end;
				false ->
					{lists:reverse(Headers), list_to_binary([Line, "\r\n", Body])}
			end
	end.

filter_non_ascii(C) when (C > 31 andalso C < 127); C == 9 ->
	<<C>>;
filter_non_ascii(_C) ->
	<<"?">>.

decode_body(Type, Body, _InEncoding, none) ->
	decode_body(Type, << <<X/integer>> || <<X>> <= Body, X < 128 >>);
decode_body(Type, Body, undefined, _OutEncoding) ->
	decode_body(Type, << <<X/integer>> || <<X>> <= Body, X < 128 >>);
decode_body(Type, Body, InEncoding, OutEncoding) ->
	NewBody = decode_body(Type, Body),
	InEncodingFixed = fix_encoding(InEncoding),
	CD = case iconv:open(OutEncoding, InEncodingFixed) of
		{ok, Res} -> Res;
		{error, einval} -> throw({bad_charset, InEncodingFixed})
	end,
	{ok, Result} = iconv:conv(CD, NewBody),
	iconv:close(CD),
	Result.

-spec(decode_body/2 :: (Type :: binary() | 'undefined', Body :: binary()) -> binary()).
decode_body(undefined, Body) ->
	Body;
decode_body(Type, Body) ->
	case binstr:to_lower(Type) of
		<<"quoted-printable">> ->
			decode_quoted_printable(Body);
		<<"base64">> ->
			decode_base64(Body);
		_Other ->
			Body
	end.

decode_base64(Body) ->
	base64:mime_decode(Body).

decode_quoted_printable(Body) ->
	case binstr:strpos(Body, "\r\n") of
		0 ->
			decode_quoted_printable(Body, <<>>, []);
		Index ->
			decode_quoted_printable(binstr:substr(Body, 1, Index +1), binstr:substr(Body, Index + 2), [])
	end.

decode_quoted_printable(<<>>, <<>>, Acc) ->
	list_to_binary(lists:reverse(Acc));
decode_quoted_printable(Line, Rest, Acc) ->
	case binstr:strpos(Rest, "\r\n") of
		0 ->
			decode_quoted_printable(Rest, <<>>, [decode_quoted_printable_line(Line, []) | Acc]);
		Index ->
			%io:format("next line ~p~nnext rest ~p~n", [binstr:substr(Rest, 1, Index +1), binstr:substr(Rest, Index + 2)]),
			decode_quoted_printable(binstr:substr(Rest, 1, Index +1), binstr:substr(Rest, Index + 2),
				[decode_quoted_printable_line(Line, []) | Acc])
	end.

decode_quoted_printable_line(<<>>, Acc) ->
	lists:reverse(Acc);
decode_quoted_printable_line(<<$\r, $\n>>, Acc) ->
	lists:reverse(["\r\n" | Acc]);
decode_quoted_printable_line(<<$=, C, T/binary>>, Acc) when C =:= $\s; C =:= $\t ->
	case binstr:all(fun(X) -> X =:= $\s orelse X =:= $\t end, T) of
		true ->
			lists:reverse(Acc);
		false ->
			throw(badchar)
	end;
decode_quoted_printable_line(<<$=, $\r, $\n>>, Acc) ->
	lists:reverse(Acc);
decode_quoted_printable_line(<<$=, A:2/binary, T/binary>>, Acc) ->
	%<<X:1/binary, Y:1/binary>> = A,
	case binstr:all(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $A andalso C =< $F) orelse (C >= $a andalso C =< $f) end, A) of
		true ->
			{ok, [C | []], []} = io_lib:fread("~16u", binary_to_list(A)),
			decode_quoted_printable_line(T, [C | Acc]);
		false ->
			throw(badchar)
	end;
decode_quoted_printable_line(<<$=>>, Acc) ->
	% soft newline
	lists:reverse(Acc);
decode_quoted_printable_line(<<H, T/binary>>, Acc) when H >= $!, H =< $< ->
	decode_quoted_printable_line(T, [H | Acc]);
decode_quoted_printable_line(<<H, T/binary>>, Acc) when H >= $>, H =< $~ ->
	decode_quoted_printable_line(T, [H | Acc]);
decode_quoted_printable_line(<<H, T/binary>>, Acc) when H =:= $\s; H =:= $\t ->
	% if the rest of the line is whitespace, truncate it
	case binstr:all(fun(X) -> X =:= $\s orelse X =:= $\t end, T) of
		true ->
			lists:reverse(Acc);
		false ->
			decode_quoted_printable_line(T, [H | Acc])
	end;
decode_quoted_printable_line(<<H, T/binary>>, Acc) ->
	decode_quoted_printable_line(T, [H| Acc]).

check_headers(Headers) ->
	Checked = [<<"MIME-Version">>, <<"Date">>, <<"From">>, <<"Message-ID">>, <<"References">>, <<"Subject">>],
	check_headers(Checked, lists:reverse(Headers)).

check_headers([], Headers) ->
	lists:reverse(Headers);
check_headers([Header | Tail], Headers) ->
	case get_header_value(Header, Headers) of
		undefined when Header == <<"MIME-Version">> ->
			check_headers(Tail, [{<<"MIME-Version">>, <<"1.0">>} | Headers]);
		undefined when Header == <<"Date">> ->
			check_headers(Tail, [{<<"Date">>, list_to_binary(smtp_util:rfc5322_timestamp())} | Headers]);
		undefined when Header == <<"From">> ->
			erlang:error(missing_from);
		undefined when Header == <<"Message-ID">> ->
			check_headers(Tail, [{<<"Message-ID">>, list_to_binary(smtp_util:generate_message_id())} | Headers]);
		undefined when Header == <<"References">> ->
			case get_header_value(<<"In-Reply-To">>, Headers) of
				undefined ->
					check_headers(Tail, Headers); % ok, whatever
				ReplyID ->
					check_headers(Tail, [{<<"References">>, ReplyID} | Headers])
			end;
		References when Header == <<"References">> ->
			% check if the in-reply-to header, if present, is in references
			case get_header_value(<<"In-Reply-To">>, Headers) of
				undefined ->
					check_headers(Tail, Headers); % ok, whatever
				ReplyID ->
					case binstr:strpos(binstr:to_lower(References), binstr:to_lower(ReplyID)) of
						0 ->
							% okay, tack on the reply-to to the end of References
							check_headers(Tail, [{<<"References">>, list_to_binary([References, " ", ReplyID])} | proplists:delete(<<"References">>, Headers)]);
						_Index ->
							check_headers(Tail, Headers) % nothing to do
					end
				end;
		_ ->
			check_headers(Tail, Headers)
	end.

ensure_content_headers(Type, SubType, Parameters, Headers, Body, Toplevel) ->
	CheckHeaders = [<<"Content-Type">>, <<"Content-Disposition">>, <<"Content-Transfer-Encoding">>],
	ensure_content_headers(CheckHeaders, Type, SubType, Parameters, lists:reverse(Headers), Body, Toplevel).

ensure_content_headers([], _, _, Parameters, Headers, _, _) ->
	{Parameters, lists:reverse(Headers)};
ensure_content_headers([Header | Tail], Type, SubType, Parameters, Headers, Body, Toplevel) ->
	case get_header_value(Header, Headers) of
		undefined when Header == <<"Content-Type">>, ((Type == <<"text">> andalso SubType =/= <<"plain">>) orelse Type =/= <<"text">>) ->
			% no content-type header, and its not text/plain
			CT = io_lib:format("~s/~s", [Type, SubType]),
			CTP = case Type of
				<<"multipart">> ->
					Boundary = case proplists:get_value(<<"boundary">>, proplists:get_value(<<"content-type-params">>, Parameters, [])) of
						undefined ->
							list_to_binary(smtp_util:generate_message_boundary());
						B ->
							B
					end,
					[{<<"boundary">>, Boundary} | proplists:delete(<<"boundary">>, proplists:get_value(<<"content-type-params">>, Parameters, []))];
				<<"text">> ->
					Charset = case proplists:get_value(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, [])) of
						undefined ->
							guess_charset(Body);
						C ->
							C
					end,
					[{<<"charset">>, Charset} | proplists:delete(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, []))];
				_ ->
					proplists:get_value(<<"content-type-params">>, Parameters, [])
			end,

			%CTP = proplists:get_value(<<"content-type-params">>, Parameters, [guess_charset(Body)]),
			CTH = binstr:join([CT | encode_parameters(CTP)], ";"),
			NewParameters = [{<<"content-type-params">>, CTP} | proplists:delete(<<"content-type-params">>, Parameters)],
			ensure_content_headers(Tail, Type, SubType, NewParameters, [{<<"Content-Type">>, CTH} | Headers], Body, Toplevel);
		undefined when Header == <<"Content-Type">> ->
			% no content-type header and its text/plain
			Charset = case proplists:get_value(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, [])) of
				undefined ->
					guess_charset(Body);
				C ->
					C
			end,
			case Charset of
				<<"us-ascii">> ->
					% the default
					ensure_content_headers(Tail, Type, SubType, Parameters, Headers, Body, Toplevel);
				_ ->
					CTP = [{<<"charset">>, Charset} | proplists:delete(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, []))],
					CTH = binstr:join([<<"text/plain">> | encode_parameters(CTP)], ";"),
					NewParameters = [{<<"content-type-params">>, CTP} | proplists:delete(<<"content-type-params">>, Parameters)],
					ensure_content_headers(Tail, Type, SubType, NewParameters, [{<<"Content-Type">>, CTH} | Headers], Body, Toplevel)
			end;
		undefined when Header == <<"Content-Transfer-Encoding">>, Type =/= <<"multipart">> ->
			Enc = case proplists:get_value(<<"transfer-encoding">>, Parameters) of
				undefined ->
					guess_best_encoding(Body);
				Value ->
					Value
			end,
			case Enc of
				<<"7bit">> ->
					ensure_content_headers(Tail, Type, SubType, Parameters, Headers, Body, Toplevel);
				_ ->
					ensure_content_headers(Tail, Type, SubType, Parameters, [{<<"Content-Transfer-Encoding">>, Enc} | Headers], Body, Toplevel)
			end;
		undefined when Header == <<"Content-Disposition">>, Toplevel == false ->
			CD = proplists:get_value(<<"disposition">>, Parameters, <<"inline">>),
			CDP = proplists:get_value(<<"disposition-params">>, Parameters, []),
			CDH = binstr:join([CD | encode_parameters(CDP)], ";"),
			ensure_content_headers(Tail, Type, SubType, Parameters, [{<<"Content-Disposition">>, CDH} | Headers], Body, Toplevel);
		_ ->
			ensure_content_headers(Tail, Type, SubType, Parameters, Headers, Body, Toplevel)
	end.

guess_charset(Body) ->
	case binstr:all(fun(X) -> X < 128 end, Body) of
		true -> <<"us-ascii">>;
		false -> <<"utf-8">>
	end.

guess_best_encoding(<<Body:200/binary, Rest/binary>>) when Rest =/= <<>> ->
	guess_best_encoding(Body);
guess_best_encoding(Body) ->
	Size = byte_size(Body),
	% get only the allowed ascii characters
	% TODO - this might not be the complete list
	FilteredSize = length([X || <<X>> <= Body, ((X > 31 andalso X < 127) orelse X == $\r orelse X == $\n)]),

	Percent = round((FilteredSize / Size) * 100),

	%based on the % of printable characters, choose an encoding
	if
		Percent == 100 ->
			<<"7bit">>;
		Percent > 80 ->
			<<"quoted-printable">>;
		true ->
			<<"base64">>
	end.

encode_parameters([[]]) ->
	[];
encode_parameters(Parameters) ->
	[encode_parameter(Parameter) || Parameter <- Parameters].

encode_parameter({X, Y}) ->
	case escape_tspecial(Y, false, <<>>) of
		{true, Special} -> [X, $=, $", Special, $"];
		false -> [X, $=, Y]
	end.

% See also: http://www.ietf.org/rfc/rfc2045.txt section 5.1
escape_tspecial(<<>>, false, _Acc) ->
	false;
escape_tspecial(<<>>, IsSpecial, Acc) ->
	{IsSpecial, Acc};
escape_tspecial(<<C, Rest/binary>>, _IsSpecial, Acc) when C =:= $" ->
	escape_tspecial(Rest, true, <<Acc/binary, $\\, $">>);
escape_tspecial(<<C, Rest/binary>>, _IsSpecial, Acc) when C =:= $\\ ->
	escape_tspecial(Rest, true, <<Acc/binary, $\\, $\\>>);
escape_tspecial(<<C, Rest/binary>>, _IsSpecial, Acc)
	when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
		C =:= $,; C =:= $;; C =:= $:; C =:= $/; C =:= $[;
		C =:= $]; C =:= $?; C =:= $=; C =:= $\s ->
	escape_tspecial(Rest, true, <<Acc/binary, C>>);
escape_tspecial(<<C, Rest/binary>>, IsSpecial, Acc) ->
	escape_tspecial(Rest, IsSpecial, <<Acc/binary, C>>).

encode_headers([]) ->
	[];
encode_headers([{Key, Value}|T] = _Headers) ->
    EncodedHeader = encode_folded_header(list_to_binary([Key,": ",encode_header_value(Key, Value)]), <<>>),
	[EncodedHeader | encode_headers(T)].

encode_folded_header(Rest, Acc) ->
	case binstr:split(Rest, <<$;>>, 2) of
		[_] ->
			<<Acc/binary, Rest/binary>>;
		[Before, After] ->
			NewPart = case After of
				<<$\t,_Rest/binary>> ->
					<<Before/binary, ";\r\n">>;
				_ ->
					<<Before/binary, ";\r\n\t">>
			end,
            encode_folded_header(After, <<Acc/binary, NewPart/binary>>)
	end.

encode_header_value(H, Value) when H =:= <<"To">>; H =:= <<"Cc">>; H =:= <<"Bcc">>;
								   H =:= <<"Reply-To">>; H =:= <<"From">> ->
	{ok, Addresses} = smtp_util:parse_rfc822_addresses(Value),
	{Names, Emails} = lists:unzip(Addresses),
	NewNames = lists:map(fun rfc2047_utf8_encode/1, Names),
	smtp_util:combine_rfc822_addresses(lists:zip(NewNames, Emails));

encode_header_value(_, Value) ->
	rfc2047_utf8_encode(Value).

encode_component(_Type, _SubType, Headers, Params, Body) ->
	if
		is_list(Body) -> % is this a multipart component?
			Boundary = proplists:get_value(<<"boundary">>, proplists:get_value(<<"content-type-params">>, Params)),
			[<<>>] ++  % blank line before start of component
			lists:flatmap(
				fun(Part) ->
						[list_to_binary([<<"--">>, Boundary])] ++ % start with the boundary
						encode_component_part(Part)
				end,
				Body
			) ++ [list_to_binary([<<"--">>, Boundary, <<"--">>])] % final boundary (with /--$/)
			  ++ [<<>>]; % blank line at the end of the multipart component
		true -> % or an inline component?
			%encode_component_part({Type, SubType, Headers, Params, Body})
			encode_body(
					get_header_value(<<"Content-Transfer-Encoding">>, Headers),
					[Body]
			 )
	end.

encode_component_part(Part) ->
	case Part of
		{<<"multipart">>, SubType, Headers, PartParams, Body} ->
			{FixedParams, FixedHeaders} = ensure_content_headers(<<"multipart">>, SubType, PartParams, Headers, Body, false),
			encode_headers(FixedHeaders) ++ [<<>>] ++
			encode_component(<<"multipart">>, SubType, FixedHeaders, FixedParams, Body);
		{Type, SubType, Headers, PartParams, Body} ->
			PartData = case Body of
				{_,_,_,_,_} -> encode_component_part(Body);
				String      -> [String]
			end,
			{_FixedParams, FixedHeaders} = ensure_content_headers(Type, SubType, PartParams, Headers, Body, false),
			encode_headers(FixedHeaders) ++ [<<>>] ++
			encode_body(
					get_header_value(<<"Content-Transfer-Encoding">>, FixedHeaders),
					PartData
			 );
		_ ->
			io:format("encode_component_part couldn't match Part to: ~p~n", [Part]),
			[]
	end.

encode_body(undefined, Body) ->
	Body;
encode_body(Type, Body) ->
	case binstr:to_lower(Type) of
		<<"quoted-printable">> ->
			[InnerBody] = Body,
			encode_quoted_printable(InnerBody);
		<<"base64">> ->
			[InnerBody] = Body,
			wrap_to_76(base64:encode(InnerBody));
		_ -> Body
	end.

wrap_to_76(String) ->
	[wrap_to_76(String, [])].

wrap_to_76(<<>>, Acc) ->
	list_to_binary(lists:reverse(Acc));
wrap_to_76(<<Head:76/binary, Tail/binary>>, Acc) ->
	wrap_to_76(Tail, [<<"\r\n">>, Head | Acc]);
wrap_to_76(Head, Acc) ->
	list_to_binary(lists:reverse([<<"\r\n">>, Head | Acc])).

encode_quoted_printable(Body) ->
	[encode_quoted_printable(Body, [], 0)].

encode_quoted_printable(Body, Acc, L) when L >= 75 ->
	LastLine = case string:str(Acc, "\n") of
		0 ->
			Acc;
		Index ->
			string:substr(Acc, 1, Index-1)
	end,
	%Len = length(LastLine),
	case string:str(LastLine, " ") of
		0 when L =:= 75 ->
			% uh-oh, no convienient whitespace, just cram a soft newline in
			encode_quoted_printable(Body, [$\n, $\r, $= | Acc], 0);
		1 when L =:= 75 ->
			% whitespace is the last character we wrote
			encode_quoted_printable(Body, [$\n, $\r, $= | Acc], 0);
		SIndex when (L - 75) < SIndex ->
			% okay, we can safely stick some whitespace in
			Prefix = string:substr(Acc, 1, SIndex-1),
			Suffix = string:substr(Acc, SIndex),
			NewAcc = lists:concat([Prefix, "\n\r=", Suffix]),
			encode_quoted_printable(Body, NewAcc, 0);
		_ ->
			% worst case, we're over 75 characters on the line
			% and there's no obvious break points, just stick one
			% in at position 75 and call it good. However, we have
			% to be very careful not to stick the soft newline in
			% the middle of an existing quoted-printable escape.

			% TODO - fix this to be less stupid
			I = 3, % assume we're at most 3 over our cutoff
			Prefix = string:substr(Acc, 1, I),
			Suffix = string:substr(Acc, I+1),
			NewAcc = lists:concat([Prefix, "\n\r=", Suffix]),
			encode_quoted_printable(Body, NewAcc, 0)
	end;
encode_quoted_printable(<<>>, Acc, _L) ->
	list_to_binary(lists:reverse(Acc));
encode_quoted_printable(<<$=, T/binary>> , Acc, L) ->
	encode_quoted_printable(T, [$D, $3, $= | Acc], L+3);
encode_quoted_printable(<<$\r, $\n, T/binary>> , Acc, _L) ->
	encode_quoted_printable(T, [$\n, $\r | Acc], 0);
encode_quoted_printable(<<H, T/binary>>, Acc, L) when H >= $!, H =< $< ->
	encode_quoted_printable(T, [H | Acc], L+1);
encode_quoted_printable(<<H, T/binary>>, Acc, L) when H >= $>, H =< $~ ->
	encode_quoted_printable(T, [H | Acc], L+1);
encode_quoted_printable(<<H, $\r, $\n, T/binary>>, Acc, _L) when H == $\s; H == $\t ->
	[[A, B]] = io_lib:format("~2.16.0B", [H]),
	encode_quoted_printable(T, [$\n, $\r, B, A, $= | Acc], 0);
encode_quoted_printable(<<H, T/binary>>, Acc, L) when H == $\s; H == $\t ->
	encode_quoted_printable(T, [H | Acc], L+1);
encode_quoted_printable(<<H, T/binary>>, Acc, L) ->
	[[A, B]] = io_lib:format("~2.16.0B", [H]),
	encode_quoted_printable(T, [B, A, $= | Acc], L+3).

get_default_encoding() ->
	<<"utf-8//IGNORE">>.

% convert some common invalid character names into the correct ones
fix_encoding(Encoding) when Encoding == <<"utf8">>; Encoding == <<"UTF8">> ->
	<<"UTF-8">>;
fix_encoding(Encoding) ->
	Encoding.


%% @doc Encode a binary or list according to RFC 2047. Input is
%% assumed to be in UTF-8 encoding.
rfc2047_utf8_encode(undefined) -> undefined;
rfc2047_utf8_encode(B) when is_binary(B) ->
	rfc2047_utf8_encode(binary_to_list(B));
rfc2047_utf8_encode([]) ->
	[];
rfc2047_utf8_encode(Text) ->
    rfc2047_utf8_encode(Text, Text).

%% Don't escape when all characters are ASCII printable
rfc2047_utf8_encode([], Text) ->
    Text;
rfc2047_utf8_encode([H|T], Text) when H >= 32 andalso H =< 126 ->
    rfc2047_utf8_encode(T, Text);
rfc2047_utf8_encode(_, Text) ->
    "=?UTF-8?Q?" ++ rfc2047_utf8_encode(Text, [], 0) ++ "?=".

rfc2047_utf8_encode([], Acc, _WordLen) ->
    lists:reverse(Acc);
rfc2047_utf8_encode(T, Acc, WordLen) when WordLen >= 55 ->
    %% Make sure that the individual encoded words are not longer than 76 chars (including charset etc)
    rfc2047_utf8_encode(T, [$?,$Q,$?,$8,$-,$F,$T,$U,$?,$=,$\ ,$\n,$\r,$=,$?|Acc], 0);
rfc2047_utf8_encode([C|T], Acc, WordLen) when C > 32 andalso C < 127 andalso C /= 32
    andalso C /= $? andalso C /= $_ andalso C /= $= andalso C /= $. ->
    rfc2047_utf8_encode(T, [C|Acc], WordLen+1);
rfc2047_utf8_encode([C|T], Acc, WordLen) ->
    rfc2047_utf8_encode(T, [hex(C rem 16), hex(C div 16), $= | Acc], WordLen+3).

hex(N) when N >= 10 -> N + $A - 10;
hex(N) -> N + $0.


%% @doc
%% DKIM sign functions
%% RFC 6376
%% `h' - list of headers to sign (lowercased binary)
%% `c' - {Headers, Body} canonicalization type. Only {simple, simple} and
%% {relaxed, simple} supported for now.
%% `s' `d' - if s = <<"foo.bar">> and d = <<"example.com">>, public key should
%% be located in "foo.bar._domainkey.example.com" (see RFC-6376 #3.6.2.1).
%% `t' - signature timestamp: 'now' or UTC {Date, Time}
%% `x' - signatue expiration time: UTC {Date, Time}
%% `private_key' - private key, to sign emails. May be of 2 types: encrypted and
%% plain in PEM format:
%% `{pem_plain, KeyBinary}' - generated by <code>openssl genrsa -out <out-file.pem> 1024<code>
%% `{pem_encrypted, KeyBinary, Password}' - generated by, eg
%%   <code>openssl genrsa -des3 -out <out-file.pem> 1024<code>. 3'rd paramerter is
%%   password to decrypt the key.
-spec dkim_sign_email([binary()], binary(), Options) -> binary()
																 when
	  Options:: [{h, [binary()]}
				 | {d, binary()}
				 | {s, binary()}
				 | {t, now | calendar:datetime()}
				 | {x, calendar:datetime()}
				 | {c, {simple|relaxed, simple|relaxed}}
				 | {private_key, PrivateKey}],
	  PrivateKey :: {pem_plain, binary()}
					| {pem_encrypted, Key::binary(), Passwd::string()}.
dkim_sign_email(Headers, Body, Opts) ->
	HeadersToSign = proplists:get_value(h, Opts, [<<"from">>, <<"to">>, <<"subject">>, <<"date">>]),
	SDID = proplists:get_value(d, Opts),
	Selector = proplists:get_value(s, Opts),
	%% BodyLength = proplists:get_value(l, Opts),
	OptionalTags = lists:foldl(fun(Key, Acc) ->
									   case proplists:get_value(Key, Opts) of
										   undefined -> Acc;
										   Value -> [{Key, Value} | Acc]
									   end
							   end, [], [t, x]),
	{HdrsCanT, BodyCanT} = Can = proplists:get_value(c, Opts, {relaxed, simple}),
	PrivateKey = proplists:get_value(private_key, Opts),

	%% hash body
	CanBody = dkim_canonicalize_body(Body, BodyCanT),
	BodyHash = dkim_hash_body(CanBody),
	Tags = [%% {b, <<>>},
			{v, 1}, {a, <<"rsa-sha256">>}, {bh, BodyHash}, {c, Can},
			{d, SDID}, {h, HeadersToSign}, {s, Selector} | OptionalTags],
	%% hash headers
	Headers1 = dkim_filter_headers(Headers, HeadersToSign),
	CanHeaders = dkim_canonicalize_headers(Headers1, HdrsCanT),
	[DkimHeaderNoB] = dkim_canonicalize_headers([dkim_make_header([{b, undefined} | Tags])], HdrsCanT),
	DataHash = dkim_hash_data(CanHeaders, DkimHeaderNoB),
	%% io:format("~s~n~n", [base64:encode(DataHash)]),
	%% sign
	Signature = dkim_sign(DataHash, PrivateKey),
	DkimHeader = dkim_make_header([{b, Signature} | Tags]),
	[DkimHeader | Headers].

dkim_filter_headers(Headers, HeadersToSign) ->
	KeyedHeaders = [begin
						[Name, _] = binary:split(Hdr, <<":">>),
						{binstr:strip(binstr:to_lower(Name)), Hdr}
					end || Hdr <- Headers],
	WithUndef = [get_header_value(binstr:to_lower(Name), KeyedHeaders) || Name <- HeadersToSign],
	[Hdr || Hdr <- WithUndef, Hdr =/= undefined].

dkim_canonicalize_headers(Headers, simple) ->
	Headers;
dkim_canonicalize_headers(Headers, relaxed) ->
	dkim_canonic_hdrs_relaxed(Headers).

dkim_canonic_hdrs_relaxed([Hdr | Rest]) ->
	[Name, Value] = binary:split(Hdr, <<":">>),
	LowStripName = binstr:to_lower(binstr:strip(Name)),

	UnfoldedHdrValue = binary:replace(Value, <<"\r\n">>, <<>>, [global]),
	SingleWSValue = re:replace(UnfoldedHdrValue, "[\t ]+", " ", [global, {return, binary}]),
	StrippedWithName = <<LowStripName/binary, ":", (binstr:strip(SingleWSValue))/binary>>,
	[StrippedWithName | dkim_canonic_hdrs_relaxed(Rest)];
dkim_canonic_hdrs_relaxed([]) -> [].


dkim_canonicalize_body(<<>>, simple) ->
	<<"\r\n">>;
dkim_canonicalize_body(Body, simple) ->
	re:replace(Body, "(\r\n)*$", "\r\n", [{return, binary}]);
dkim_canonicalize_body(_Body, relaxed) ->
	throw({not_supported, dkim_body_relaxed}).

dkim_hash_body(CanonicBody) ->
	crypto:hash(sha256, CanonicBody).
	%% crypto:sha256(CanonicBody).

%% RFC 5.5 & 3.7
dkim_hash_data(CanonicHeaders, DkimHeader) ->
	JoinedHeaders = << <<Hdr/binary, "\r\n">> || Hdr <- CanonicHeaders>>,
	crypto:hash(sha256, <<JoinedHeaders/binary, DkimHeader/binary>>).

dkim_sign(DataHash, {pem_plain, PrivBin}) ->
	[PrivEntry] = public_key:pem_decode(PrivBin),
	Key = public_key:pem_entry_decode(PrivEntry),
	public_key:sign({digest, DataHash}, sha256, Key);
dkim_sign(DataHash, {pem_encrypted, EncPrivBin, Passwd}) ->
	[EncPrivEntry] = public_key:pem_decode(EncPrivBin),
	Key = public_key:pem_entry_decode(EncPrivEntry, Passwd),
	public_key:sign({digest, DataHash}, sha256, Key).


dkim_make_header(Tags) ->
	RevTags = lists:reverse(Tags),				%so {b, ...} became last tag
	EncodedTags = binstr:join([dkim_encode_tag(K, V) || {K, V} <- RevTags], <<"; ">>),
	binstr:join(encode_headers([{<<"DKIM-Signature">>, EncodedTags}]), <<"\r\n">>).

%% RFC #3.5
dkim_encode_tag(v, 1) ->
	%% version
	<<"v=1">>;
dkim_encode_tag(a, <<"rsa-sha256">>) ->
	%% algorithm
	<<"a=rsa-sha256">>;
dkim_encode_tag(b, undefined) ->
	%% signature (when hashing with no digest)
	<<"b=">>;
dkim_encode_tag(b, V) ->
	%% signature
	B64Sign = base64:encode(V),
	<<"b=", B64Sign/binary>>;
dkim_encode_tag(bh, V) ->
	%% body hash
	B64Sign = base64:encode(V),
	<<"bh=", B64Sign/binary>>;
dkim_encode_tag(c, {Hdrs, simple}) ->	  % 'relaxed' for body not supported yet
	%% canonicalization type
	<<"c=", (atom_to_binary(Hdrs, utf8))/binary, "/simple">>;
dkim_encode_tag(d, Domain) ->
	%% SDID (domain)
	<<"d=", Domain/binary>>;
dkim_encode_tag(h, Hdrs) ->
	%% headers fields (case-insensitive, ":" separated)
	Joined = binstr:join([binstr:to_lower(H) || H <- Hdrs], <<":">>),
	<<"h=", Joined/binary>>;
dkim_encode_tag(i, V) ->
	%% AUID
	[QPValue] = dkim_qp_tag_value(V),
	<<"i=", QPValue/binary>>;
dkim_encode_tag(l, IntVal) ->
	%% body length count
	BinVal = list_to_binary(integer_to_list(IntVal)),
	<<"l=", (BinVal)/binary>>;
dkim_encode_tag(q, [<<"dns/txt">>]) ->
	%% query methods (':' separated)
	<<"q=dns/txt">>;
dkim_encode_tag(s, Selector) ->
	%% selector
	<<"s=", Selector/binary>>;
dkim_encode_tag(t, now) ->
	dkim_encode_tag(t, calendar:universal_time());
dkim_encode_tag(t, DateTime) ->
	%% timestamp
	BinTs = datetime_to_bin_timestamp(DateTime),
	<<"t=", BinTs/binary>>;
dkim_encode_tag(x, DateTime) ->
	%% signature expiration
	BinTs = datetime_to_bin_timestamp(DateTime),
	<<"x=", BinTs/binary>>;
%% dkim_encode_tag(z, Hdrs) ->
%%	   %% copied header fields
%%	   Joined = dkim_qp_tag_value(binstr:join([(H) || H <- Hdrs], <<"|">>)),
%%	   <<"z=", Joined/binary>>;
dkim_encode_tag(K, V) when is_binary(K), is_binary(V) ->
	<<K/binary, V/binary>>.

dkim_qp_tag_value(Value) ->
    %% XXX: this not fully satisfy #2.11
    [QPValue] = encode_quoted_printable(Value),
    binary:replace(QPValue, <<";">>, <<"=3B">>).

datetime_to_bin_timestamp(DateTime) ->
    EpochStart = 62167219200, % calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})
    UnixTimestamp = calendar:datetime_to_gregorian_seconds(DateTime) - EpochStart,
    list_to_binary(integer_to_list(UnixTimestamp)).

%% /DKIM


-ifdef(TEST).

parse_with_comments_test_() ->
	[
		{"bleh",
			fun() ->
					?assertEqual(<<"1.0">>, parse_with_comments(<<"1.0">>)),
					?assertEqual(<<"1.0">>, parse_with_comments(<<"1.0  (produced by MetaSend Vx.x)">>)),
					?assertEqual(<<"1.0">>, parse_with_comments(<<"(produced by MetaSend Vx.x) 1.0">>)),
					?assertEqual(<<"1.0">>, parse_with_comments(<<"1.(produced by MetaSend Vx.x)0">>))
			end
		},
		{"comments that parse as empty",
			fun() ->
					?assertEqual(<<>>, parse_with_comments(<<"(comment (nested (deeply)) (and (oh no!) again))">>)),
					?assertEqual(<<>>, parse_with_comments(<<"(\\)\\\\)">>)),
					?assertEqual(<<>>, parse_with_comments(<<"(by way of Whatever <redir@my.org>)    (generated by Eudora)">>))
			end
		},
		{"some more",
			fun() ->
					?assertEqual(<<":sysmail@  group. org, Muhammed. Ali @Vegas.WBA">>, parse_with_comments(<<"\":sysmail\"@  group. org, Muhammed.(the greatest) Ali @(the)Vegas.WBA">>)),
					?assertEqual(<<"Pete <pete@silly.test>">>, parse_with_comments(<<"Pete(A wonderful \\) chap) <pete(his account)@silly.test(his host)>">>))
			end
		},
		{"non list values",
			fun() ->
					?assertEqual(undefined, parse_with_comments(undefined)),
					?assertEqual(17, parse_with_comments(17))
			end
		},
		{"Parens within quotes ignored",
			fun() ->
				?assertEqual(<<"Height (from xkcd).eml">>, parse_with_comments(<<"\"Height (from xkcd).eml\"">>)),
				?assertEqual(<<"Height (from xkcd).eml">>, parse_with_comments(<<"\"Height \(from xkcd\).eml\"">>))
			end
		},
		{"Escaped quotes are handled correctly",
			fun() ->
					?assertEqual(<<"Hello \"world\"">>, parse_with_comments(<<"Hello \\\"world\\\"">>)),
					?assertEqual(<<"<boss@nil.test>, Giant; \"Big\" Box <sysservices@example.net>">>, parse_with_comments(<<"<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>">>))
			end
		},
		{"backslash not part of a quoted pair",
			fun() ->
					?assertEqual(<<"AC \\ DC">>, parse_with_comments(<<"AC \\ DC">>)),
					?assertEqual(<<"AC  DC">>, parse_with_comments(<<"AC ( \\ ) DC">>))
			end
		},
		{"Unterminated quotes or comments",
			fun() ->
					?assertError(unterminated_quotes, parse_with_comments(<<"\"Hello there ">>)),
					?assertError(unterminated_quotes, parse_with_comments(<<"\"Hello there \\\"">>)),
					?assertError(unterminated_comment, parse_with_comments(<<"(Hello there ">>)),
					?assertError(unterminated_comment, parse_with_comments(<<"(Hello there \\\)">>))
			end
		}
	].

parse_content_type_test_() ->
	[
		{"parsing content types",
			fun() ->
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"text/plain; charset=us-ascii (Plain text)">>)),
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"text/plain; charset=\"us-ascii\"">>)),
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"Text/Plain; Charset=\"us-ascii\"">>)),
					?assertEqual({<<"multipart">>, <<"mixed">>, [{<<"boundary">>, <<"----_=_NextPart_001_01C9DCAE.1F2CB390">>}]},
						parse_content_type(<<"multipart/mixed; boundary=\"----_=_NextPart_001_01C9DCAE.1F2CB390\"">>))
			end
		},
		{"parsing content type with a tab in it",
			fun() ->
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"text/plain;\tcharset=us-ascii">>)),
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}, {<<"foo">>, <<"bar">>}]}, parse_content_type(<<"text/plain;\tcharset=us-ascii;\tfoo=bar">>))
			end
		},
		{"invalid content types",
			fun() ->
					?assertThrow(bad_content_type, parse_content_type(<<"text\\plain; charset=us-ascii">>)),
					?assertThrow(bad_content_type, parse_content_type(<<"text/plain; charset us-ascii">>))
				end
			}
	].

parse_content_disposition_test_() ->
	[
		{"parsing valid dispositions",
			fun() ->
					?assertEqual({<<"inline">>, []}, parse_content_disposition(<<"inline">>)),
					?assertEqual({<<"inline">>, []}, parse_content_disposition(<<"inline;">>)),
					?assertEqual({<<"attachment">>, [{<<"filename">>, <<"genome.jpeg">>}, {<<"modification-date">>, <<"Wed, 12 Feb 1997 16:29:51 -0500">>}]}, parse_content_disposition(<<"attachment; filename=genome.jpeg;modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";">>)),
					?assertEqual({<<"text/plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_disposition(<<"text/plain; charset=us-ascii (Plain text)">>))
			end
		},
		{"invalid dispositions",
			fun() ->
					?assertThrow(bad_disposition, parse_content_disposition(<<"inline; =bar">>)),
					?assertThrow(bad_disposition, parse_content_disposition(<<"inline; bar">>))
			end
		}
	].

various_parsing_test_() ->
	[
		{"split_body_by_boundary test",
			fun() ->
					?assertEqual([{[], <<"foo bar baz">>}], split_body_by_boundary_(<<"stuff\r\nfoo bar baz">>, <<"--bleh">>, [], [])),
					?assertEqual([{[], <<"foo\r\n">>}, {[], <<>>}, {[], <<>>}, {[], <<"bar baz">>}], split_body_by_boundary_(<<"stuff\r\nfoo\r\n--bleh\r\n--bleh\r\n--bleh-- stuff\r\nbar baz">>, <<"--bleh">>, [], [])),
					%?assertEqual([{[], []}, {[], []}, {[], "bar baz"}], split_body_by_boundary_("\r\n--bleh\r\n--bleh\r\n", "--bleh", [], [])),
					%?assertMatch([{"text", "plain", [], _,"foo\r\n"}], split_body_by_boundary("stuff\r\nfoo\r\n--bleh\r\n--bleh\r\n--bleh-- stuff\r\nbar baz", "--bleh", "1.0", []))
					?assertEqual({[], <<"foo: bar\r\n">>}, parse_headers(<<"\r\nfoo: bar\r\n">>)),
					?assertEqual({[{<<"foo">>, <<"barbaz">>}], <<>>}, parse_headers(<<"foo: bar\r\n baz\r\n">>)),
					?assertEqual({[], <<" foo bar baz\r\nbam">>}, parse_headers(<<"\sfoo bar baz\r\nbam">>)),
					ok
			end
		},
		{"Headers with non-ASCII characters",
			fun() ->
					?assertEqual({[{<<"foo">>, <<"bar ?? baz">>}], <<>>}, parse_headers(<<"foo: bar ø baz\r\n">>)),
					?assertEqual({[], <<"bär: bar baz\r\n">>}, parse_headers(<<"bär: bar baz\r\n">>))
			end
		},
		{"Headers with tab characters",
			fun() ->
					?assertEqual({[{<<"foo">>, <<"bar		baz">>}], <<>>}, parse_headers(<<"foo: bar		baz\r\n">>))
			end
		}

	].

-define(IMAGE_MD5, <<110,130,37,247,39,149,224,61,114,198,227,138,113,4,198,60>>).

parse_example_mails_test_() ->
	Getmail = fun(File) ->
		{ok, Email} = file:read_file(string:concat("../testdata/", File)),
		%Email = binary_to_list(Bin),
		decode(Email)
	end,
	[
		{"parse a plain text email",
			fun() ->
				Decoded = Getmail("Plain-text-only.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"text">>, <<"plain">>}, {Type, SubType}),
				?assertEqual(<<"This message contains only plain text.\r\n">>, Body)
			end
		},
		{"parse a plain text email with no content type",
			fun() ->
				Decoded = Getmail("Plain-text-only-no-content-type.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"text">>, <<"plain">>}, {Type, SubType}),
				?assertEqual(<<"This message contains only plain text.\r\n">>, Body)
			end
		},
		{"parse a plain text email with no MIME header",
			fun() ->
				{Type, SubType, _Headers, _Properties, Body} =
					Getmail("Plain-text-only-no-MIME.eml"),
				?assertEqual({<<"text">>, <<"plain">>}, {Type, SubType}),
				?assertEqual(<<"This message contains only plain text.\r\n">>, Body)
			end
		},
		{"parse an email that says it is multipart but contains no boundaries",
			fun() ->
					?assertError(missing_boundary, Getmail("Plain-text-only-with-boundary-header.eml"))
			end
		},
		{"parse a multipart email with no MIME header",
			fun() ->
					?assertError(non_mime_multipart, Getmail("rich-text-no-MIME.eml"))
			end
		},
		{"rich text",
			fun() ->
				%% pardon my naming here.  apparently 'rich text' in mac mail
				%% means 'html'.
				Decoded = Getmail("rich-text.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"alternative">>}, {Type, SubType}),
				?assertEqual(2, length(Body)),
				[Plain, Html] = Body,
				?assertEqual({5, 5}, {tuple_size(Plain), tuple_size(Html)}),
				?assertMatch({<<"text">>, <<"plain">>, _, _, <<"This message contains rich text.">>}, Plain),
				?assertMatch({<<"text">>, <<"html">>, _, _, <<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This </b><i>message </i><span class=\"Apple-style-span\" style=\"text-decoration: underline;\">contains </span>rich text.</body></html>">>}, Html)
			end
		},
		{"rich text no boundary",
			fun() ->
				?assertError(no_boundary, Getmail("rich-text-no-boundary.eml"))
			end
		},
		{"rich text missing first boundary",
			fun() ->
				% TODO - should we handle this more elegantly?
				Decoded = Getmail("rich-text-missing-first-boundary.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"alternative">>}, {Type, SubType}),
				?assertEqual(1, length(Body)),
				[Html] = Body,
				?assertEqual(5, tuple_size(Html)),
				?assertMatch({<<"text">>, <<"html">>, _, _, <<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This </b><i>message </i><span class=\"Apple-style-span\" style=\"text-decoration: underline;\">contains </span>rich text.</body></html>">>}, Html)
			end
		},
		{"rich text missing last boundary",
			fun() ->
				?assertError(missing_last_boundary, Getmail("rich-text-missing-last-boundary.eml"))
			end
		},
		{"rich text wrong last boundary",
			fun() ->
				?assertError(missing_last_boundary, Getmail("rich-text-broken-last-boundary.eml"))
			end
		},
		{"rich text missing text content type",
			fun() ->
				%% pardon my naming here.  apparently 'rich text' in mac mail
				%% means 'html'.
				Decoded = Getmail("rich-text-no-text-contenttype.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"alternative">>}, {Type, SubType}),
				?assertEqual(2, length(Body)),
				[Plain, Html] = Body,
				?assertEqual({5, 5}, {tuple_size(Plain), tuple_size(Html)}),
				?assertMatch({<<"text">>, <<"plain">>, _, _, <<"This message contains rich text.">>}, Plain),
				?assertMatch({<<"text">>, <<"html">>, _, _, <<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This </b><i>message </i><span class=\"Apple-style-span\" style=\"text-decoration: underline;\">contains </span>rich text.</body></html>">>}, Html)
			end
		},
		{"text attachment only",
			fun() ->
				Decoded = Getmail("text-attachment-only.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"mixed">>}, {Type, SubType}),
				?assertEqual(1, length(Body)),
				Rich = <<"{\\rtf1\\ansi\\ansicpg1252\\cocoartf949\\cocoasubrtf460\r\n{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\r\n{\\colortbl;\\red255\\green255\\blue255;}\r\n\\margl1440\\margr1440\\vieww9000\\viewh8400\\viewkind0\r\n\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural\\pardirnatural\r\n\r\n\\f0\\fs24 \\cf0 This is a basic rtf file.}">>,
				?assertMatch([{<<"text">>, <<"rtf">>, _, _, Rich}], Body)
			end
		},
		{"image attachment only",
			fun() ->
				Decoded = Getmail("image-attachment-only.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"mixed">>}, {Type, SubType}),
				?assertEqual(1, length(Body)),
				?assertMatch([{<<"image">>, <<"jpeg">>, _, _, _}], Body),
				[H | _] = Body,
				[{<<"image">>, <<"jpeg">>, _, Parameters, _Image}] = Body,
				?assertEqual(?IMAGE_MD5, erlang:md5(element(5, H))),
				?assertEqual(<<"inline">>, proplists:get_value(<<"disposition">>, Parameters)),
				?assertEqual(<<"chili-pepper.jpg">>, proplists:get_value(<<"filename">>, proplists:get_value(<<"disposition-params">>, Parameters))),
				?assertEqual(<<"chili-pepper.jpg">>, proplists:get_value(<<"name">>, proplists:get_value(<<"content-type-params">>, Parameters)))
			end
		},
		{"message attachment only",
			fun() ->
				Decoded = Getmail("message-as-attachment.eml"),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Decoded),
				[Body] = element(5, Decoded),
				?assertMatch({<<"message">>, <<"rfc822">>, _, _, _}, Body),
				Subbody = element(5, Body),
				?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Subbody),
				?assertEqual(<<"This message contains only plain text.\r\n">>, element(5, Subbody))
			end
		},
		{"message, image, and rtf attachments.",
			fun() ->
				Decoded = Getmail("message-image-text-attachments.eml"),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Decoded),
				?assertEqual(3, length(element(5, Decoded))),
				[Message, Rtf, Image] = element(5, Decoded),
				?assertMatch({<<"message">>, <<"rfc822">>, _, _, _}, Message),
				Submessage = element(5, Message),
				?assertMatch({<<"text">>, <<"plain">>, _, _, <<"This message contains only plain text.\r\n">>}, Submessage),

				?assertMatch({<<"text">>, <<"rtf">>, _, _, _}, Rtf),
				?assertEqual(<<"{\\rtf1\\ansi\\ansicpg1252\\cocoartf949\\cocoasubrtf460\r\n{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\r\n{\\colortbl;\\red255\\green255\\blue255;}\r\n\\margl1440\\margr1440\\vieww9000\\viewh8400\\viewkind0\r\n\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural\\pardirnatural\r\n\r\n\\f0\\fs24 \\cf0 This is a basic rtf file.}">>, element(5, Rtf)),

				?assertMatch({<<"image">>, <<"jpeg">>, _, _, _}, Image),
				?assertEqual(?IMAGE_MD5, erlang:md5(element(5, Image)))
			end
		},
		{"Outlook 2007 with leading tabs in quoted-printable.",
			fun() ->
				Decoded = Getmail("outlook-2007.eml"),
				?assertMatch({<<"multipart">>, <<"alternative">>, _, _, _}, Decoded)
			end
		},
		{"The gamut",
			fun() ->
				% multipart/alternative
				%	text/plain
				%	multipart/mixed
				%		text/html
				%		message/rf822
				%			multipart/mixed
				%				message/rfc822
				%					text/plain
				%		text/html
				%		message/rtc822
				%			text/plain
				%		text/html
				%		image/jpeg
				%		text/html
				%		text/rtf
				%		text/html
				Decoded = Getmail("the-gamut.eml"),
				?assertMatch({<<"multipart">>, <<"alternative">>, _, _, _}, Decoded),
				?assertEqual(2, length(element(5, Decoded))),
				[Toptext, Topmultipart] = element(5, Decoded),
				?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Toptext),
				?assertEqual(<<"This is rich text.\r\n\r\nThe list is html.\r\n\r\nAttchments:\r\nan email containing an attachment of an email.\r\nan email of only plain text.\r\nan image\r\nan rtf file.\r\n">>, element(5, Toptext)),
				?assertEqual(9, length(element(5, Topmultipart))),
				[Html, Messagewithin, _Brhtml, _Message, _Brhtml, Image, _Brhtml, Rtf, _Brhtml] = element(5, Topmultipart),
				?assertMatch({<<"text">>, <<"html">>, _, _, _}, Html),
				?assertEqual(<<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This</b> is <i>rich</i> text.<div><br></div><div>The list is html.</div><div><br></div><div>Attchments:</div><div><ul class=\"MailOutline\"><li>an email containing an attachment of an email.</li><li>an email of only plain text.</li><li>an image</li><li>an rtf file.</li></ul></div><div></div></body></html>">>, element(5, Html)),

				?assertMatch({<<"message">>, <<"rfc822">>, _, _, _}, Messagewithin),
				%?assertEqual(1, length(element(5, Messagewithin))),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, [{<<"message">>, <<"rfc822">>, _, _, {<<"text">>, <<"plain">>, _, _, <<"This message contains only plain text.\r\n">>}}]}, element(5, Messagewithin)),

				?assertMatch({<<"image">>, <<"jpeg">>, _, _, _}, Image),
				?assertEqual(?IMAGE_MD5, erlang:md5(element(5, Image))),

				?assertMatch({<<"text">>, <<"rtf">>, _, _, _}, Rtf),
				?assertEqual(<<"{\\rtf1\\ansi\\ansicpg1252\\cocoartf949\\cocoasubrtf460\r\n{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\r\n{\\colortbl;\\red255\\green255\\blue255;}\r\n\\margl1440\\margr1440\\vieww9000\\viewh8400\\viewkind0\r\n\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural\\pardirnatural\r\n\r\n\\f0\\fs24 \\cf0 This is a basic rtf file.}">>, element(5, Rtf))

			end
		},
		{"Plain text and 2 identical attachments",
			fun() ->
				Decoded = Getmail("plain-text-and-two-identical-attachments.eml"),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Decoded),
				?assertEqual(3, length(element(5, Decoded))),
				[Plain, Attach1, Attach2] = element(5, Decoded),
				?assertEqual(Attach1, Attach2),
				?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Plain),
				?assertEqual(<<"This message contains only plain text.\r\n">>, element(5, Plain))
			end
		},
		{"no \\r\\n before first boundary",
			fun() ->
				{ok, Bin} = file:read_file("../testdata/html.eml"),
				Decoded = decode(Bin),
				?assertEqual(2, length(element(5, Decoded)))
			end
		},
		{"permissive malformed folded multibyte header decoder",
			fun() ->
				{_, _, Headers, _, Body} = Getmail("malformed-folded-multibyte-header.eml"),
				?assertEqual(<<"Hello world\n">>, Body),
				Subject = <<78,79,68,51,50,32,83,109,97,114,116,32,83,101,99,117, 114,105,116,121,32,45,32,208,177,208,181,209,129,208,
							191,208,187,208,176,209,130,208,189,208,176,209,143,32, 208,187,208,184,209,134,208,181,208,189,208,183,208,184,209,143>>,
				?assertEqual(Subject, proplists:get_value(<<"Subject">>, Headers))
			end
		},
		{"decode headers of multipart messages",
			fun() ->
				{<<"multipart">>, _, _, _, [Inline, Attachment]} = Getmail("utf-attachment-name.eml"),
				{<<"text">>, _, _, _, InlineBody} = Inline,
				{<<"text">>, _, _, ContentHeaders, _AttachmentBody} = Attachment,
				ContentTypeName = proplists:get_value(
									<<"name">>, proplists:get_value(
												  <<"content-type-params">>, ContentHeaders)),
				DispositionName = proplists:get_value(
									<<"filename">>, proplists:get_value(
													  <<"disposition-params">>, ContentHeaders)),

				?assertEqual(<<"Hello\r\n">>, InlineBody),
				?assert(ContentTypeName == DispositionName),
				% Take the filename as a literal, to prevent character set issues with Erlang
				% In utf-8 the filename is:"тестовый файл.txt"
				Filename = <<209,130,208,181,209,129,209,130,208,190,208,178,209,139,208,185,32,209,132,208,176,208,185,208,187,46,116,120,116>>,
				?assertEqual(Filename, ContentTypeName),
				?assertEqual(Filename, DispositionName)
			end
		},
		{"testcase1",
			fun() ->
				Multipart = <<"multipart">>,
				Alternative = <<"alternative">>,
				Related = <<"related">>,
				Mixed = <<"mixed">>,
				Text = <<"text">>,
				Html = <<"html">>,
				Plain = <<"plain">>,
				Message = <<"message">>,
				Ref822 = <<"rfc822">>,
				Image = <<"image">>,
				Jpeg = <<"jpeg">>,
				%Imagemd5 = <<69,175,198,78,52,72,6,233,147,22,50,137,128,180,169,50>>,
				Imagemd5 = <<179,151,42,139,78,14,182,78,24,160,123,221,217,14,141,5>>,
				Decoded = Getmail("testcase1"),
				?assertMatch({Multipart, Mixed, _, _, [_, _]}, Decoded),
				[Multi1, Message1] = element(5, Decoded),
				?assertMatch({Multipart, Alternative, _, _, [_, _]}, Multi1),
				[Plain1, Html1] = element(5, Multi1),
				?assertMatch({Text, Plain, _, _, _}, Plain1),
				?assertMatch({Text, Html, _, _, _}, Html1),
				?assertMatch({Message, Ref822, _, _, _}, Message1),
				Multi2 = element(5, Message1),
				?assertMatch({Multipart, Alternative, _, _, [_, _]}, Multi2),
				[Plain2, Related1] = element(5, Multi2),
				?assertMatch({Text, Plain, _, _, _}, Plain2),
				?assertMatch({Multipart, Related, _, _, [_, _]}, Related1),
				[Html2, Image1] = element(5, Related1),
				?assertMatch({Text, Html, _, _, _}, Html2),
				?assertMatch({Image, Jpeg, _, _, _}, Image1),
				Resimage = erlang:md5(element(5, Image1)),
				?assertEqual(Imagemd5, Resimage)
			end
		},
		{"testcase2",
			fun() ->
				Multipart = <<"multipart">>,
				Alternative = <<"alternative">>,
				Mixed = <<"mixed">>,
				Text = <<"text">>,
				Html = <<"html">>,
				Plain = <<"plain">>,
				Message = <<"message">>,
				Ref822 = <<"rfc822">>,
				Application = <<"application">>,
				Octetstream = <<"octet-stream">>,
				Decoded = Getmail("testcase2"),
				?assertMatch({Multipart, Mixed, _, _, [_, _, _]}, Decoded),
				[Plain1, Stream1, Message1] = element(5, Decoded),
				?assertMatch({Text, Plain, _, _, _}, Plain1),
				?assertMatch({Application, Octetstream, _, _, _}, Stream1),
				?assertMatch({Message, Ref822, _, _, _}, Message1),
				Multi1 = element(5, Message1),
				?assertMatch({Multipart, Alternative, _, _, [_, _]}, Multi1),
				[Plain2, Html1] = element(5, Multi1),
				?assertMatch({Text, Plain, _, _, _}, Plain2),
				?assertMatch({Text, Html, _, _, _}, Html1)
			end
		}
	].

decode_quoted_printable_test_() ->
	[
		{"bleh",
			fun() ->
					?assertEqual("!", decode_quoted_printable_line(<<"=21">>, "")),
					?assertEqual("!!", decode_quoted_printable_line(<<"=21=21">>, "")),
					?assertEqual("=:=", decode_quoted_printable_line(<<"=3D:=3D">>, "")),
					?assertEqual("Thequickbrownfoxjumpedoverthelazydog.", decode_quoted_printable_line(<<"Thequickbrownfoxjumpedoverthelazydog.">>, ""))
			end
		},
		{"lowercase bleh",
			fun() ->
					?assertEqual("=:=", decode_quoted_printable_line(<<"=3d:=3d">>, ""))
			end
		},
		{"input with spaces",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.">>, ""))
			end
		},
		{"input with tabs",
			fun() ->
					?assertEqual("The\tquick brown fox jumped over\tthe lazy dog.", decode_quoted_printable_line(<<"The\tquick brown fox jumped over\tthe lazy dog.">>, ""))
			end
		},
		{"input with trailing spaces",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       ">>, ""))
			end
		},
		{"input with non-strippable trailing whitespace",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.        ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =20">>, "")),
					?assertEqual("The quick brown fox jumped over the lazy dog.       \t", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =09">>, "")),
					?assertEqual("The quick brown fox jumped over the lazy dog.\t \t \t \t ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.\t \t \t =09=20">>, "")),
					?assertEqual("The quick brown fox jumped over the lazy dog.\t \t \t \t ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.\t \t \t =09=20\t                  \t">>, ""))
			end
		},
		{"input with trailing tabs",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.\t\t\t\t\t">>, ""))
			end
		},
		{"soft new line",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.       ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =">>, ""))
			end
		},
		{"soft new line with trailing whitespace",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.       ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =  	">>, ""))
			end
		},
		{"multiline stuff",
			fun() ->
					?assertEqual(<<"Now's the time for all folk to come to the aid of their country.">>, decode_quoted_printable(<<"Now's the time =\r\nfor all folk to come=\r\n to the aid of their country.">>)),
					?assertEqual(<<"Now's the time\r\nfor all folk to come\r\n to the aid of their country.">>, decode_quoted_printable(<<"Now's the time\r\nfor all folk to come\r\n to the aid of their country.">>)),
					?assertEqual(<<"hello world">>, decode_quoted_printable(<<"hello world">>)),
					?assertEqual(<<"hello\r\n\r\nworld">>, decode_quoted_printable(<<"hello\r\n\r\nworld">>))
			end
		},
		{"invalid input",
			fun() ->
					?assertThrow(badchar, decode_quoted_printable_line(<<"=21=G1">>, "")),
					?assertThrow(badchar, decode_quoted_printable(<<"=21=D1 = g ">>))
			end
		},
		{"out of range characters should be stripped",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo ", 150, " bar">>, "US-ASCII", "UTF-8//IGNORE"))
			end
		},
		{"out of range character in alternate charset should be converted",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo ", 226, 128, 147, " bar">>, decode_body(<<"quoted-printable">>, <<"Foo ",150," bar">>, "Windows-1252", "UTF-8//IGNORE"))
			end
		},
		{"out of range character in alternate charset with no destination encoding should be stripped",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo ",150," bar">>, "Windows-1252", none))
			end
		},
		{"out of range character in alternate charset with no source encoding should be stripped",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo ",150," bar">>, undefined, "UTF-8"))
			end
		},
		{"almost correct chatsets should work, eg. 'UTF8' instead of 'UTF-8'",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo  bar">>, <<"UTF8">>, "UTF-8")),
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo  bar">>, <<"utf8">>, "UTF-8"))
			end
		}
	].

encode_quoted_printable_test_() ->
	[
		{"bleh",
			fun() ->
					?assertEqual(<<"!">>, encode_quoted_printable(<<"!">>, [], 0)),
					?assertEqual(<<"!!">>, encode_quoted_printable(<<"!!">>, [], 0)),
					?assertEqual(<<"=3D:=3D">>, encode_quoted_printable(<<"=:=">>, [], 0)),
					?assertEqual(<<"Thequickbrownfoxjumpedoverthelazydog.">>,
						encode_quoted_printable(<<"Thequickbrownfoxjumpedoverthelazydog.">>, [], 0))
			end
		},
		{"input with spaces",
			fun() ->
					?assertEqual(<<"The quick brown fox jumped over the lazy dog.">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog.">>, "", 0))
			end
		},
		{"input with tabs",
			fun() ->
					?assertEqual(<<"The\tquick brown fox jumped over\tthe lazy dog.">>,
						encode_quoted_printable(<<"The\tquick brown fox jumped over\tthe lazy dog.">>, "", 0))
			end
		},
		{"input with trailing spaces",
			fun() ->
					?assertEqual(<<"The quick brown fox jumped over the lazy dog.      =20\r\n">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog.       \r\n">>, "", 0))
			end
		},
		{"input with non-ascii characters",
			fun() ->
					?assertEqual(<<"There's some n=F8n-=E1scii st=FCff in here\r\n">>,
						encode_quoted_printable(<<"There's some n", 248, "n-", 225,"scii st", 252, "ff in here\r\n">>, "", 0))
			end
		},
		{"input with invisible non-ascii characters",
			fun() ->
					?assertEqual(<<"There's some stuff=C2=A0in=C2=A0here\r\n">>,
						encode_quoted_printable(<<"There's some stuff in here\r\n">>, "", 0))
			end
		},
		{"add soft newlines",
			fun() ->
					?assertEqual(<<"The quick brown fox jumped over the lazy dog. The quick brown fox jumped =\r\nover the lazy dog.">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_ov=\r\ner_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_over_the_lazy_dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o=\r\n=3Dver_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o=ver_the_lazy_dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_=\r\n=3Dover_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_=over_the_lazy_dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o =\r\nver_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o ver_the_lazy_dog.">>, "", 0))
			end
		},
		{"newline craziness",
			fun() ->
					?assertEqual(<<"foo ba=\r\nr\r\nThe quick brown fox jumped over the lazy dog.      =20\r\n">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog.       \r\n">>, "\n\rrab oof", 78))
			end
		}
	].

encode_parameter_test_() ->
	[
		{"Token",
			fun() ->
				?assertEqual([[<<"a">>, $=, <<"abcdefghijklmnopqrstuvwxyz$%&*#!">>]],
				    encode_parameters([{<<"a">>, <<"abcdefghijklmnopqrstuvwxyz$%&*#!">>}]))
			end
		},
		{"TSpecial",
			fun() ->
				Special = " ()<>@,;:/[]?=",
				[
    				?assertEqual([[<<"a">>, $=, $", <<C>>, $"]], encode_parameters([{<<"a">>, <<C>>}]))
					|| C <- Special
				],
				?assertEqual([[<<"a">>, $=, $", <<$\\,$">>, $"]], encode_parameters([{<<"a">>, <<$">>}])),
				?assertEqual([[<<"a">>, $=, $", <<$\\,$\\>>, $"]], encode_parameters([{<<"a">>, <<$\\>>}]))
			end
		}
	].

rfc2047_decode_test_() ->
	[
		{"Simple tests",
			fun() ->
					?assertEqual(<<"Keith Moore <moore@cs.utk.edu>">>, decode_header(<<"=?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>">>, "utf-8")),
					?assertEqual(<<"Keld Jørn Simonsen <keld@dkuug.dk>">>, decode_header(<<"=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>">>, "utf-8")),
					?assertEqual(<<"Olle Järnefors <ojarnef@admin.kth.se>">>, decode_header(<<"=?ISO-8859-1?Q?Olle_J=E4rnefors?= <ojarnef@admin.kth.se>">>, "utf-8")),
					?assertEqual(<<"André Pirard <PIRARD@vm1.ulg.ac.be>">>, decode_header(<<"=?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>">>, "utf-8"))
			end
		},
		{"encoded words seperated by whitespace should have whitespace removed",
			fun() ->
					?assertEqual(<<"If you can read this you understand the example.">>, decode_header(<<"=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?= =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=">>, "utf-8")),
					?assertEqual(<<"ab">>, decode_header(<<"=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=">>, "utf-8")),
					?assertEqual(<<"ab">>, decode_header(<<"=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=">>, "utf-8")),
					?assertEqual(<<"ab">>, decode_header(<<"=?ISO-8859-1?Q?a?=
		=?ISO-8859-1?Q?b?=">>, "utf-8"))
			end
		},
		{"underscores expand to spaces",
			fun() ->
					?assertEqual(<<"a b">>, decode_header(<<"=?ISO-8859-1?Q?a_b?=">>, "utf-8")),
					?assertEqual(<<"a b">>, decode_header(<<"=?ISO-8859-1?Q?a?= =?ISO-8859-2?Q?_b?=">>, "utf-8"))
			end
		},
		{"edgecases",
			fun() ->
					?assertEqual(<<"this is some text">>, decode_header(<<"=?iso-8859-1?q?this=20is=20some=20text?=">>, "utf-8")),
					?assertEqual(<<"=?iso-8859-1?q?this is some text?=">>, decode_header(<<"=?iso-8859-1?q?this is some text?=">>, "utf-8"))
			end
		},
		{"invalid character sequence handling",
			fun() ->
					?assertError({badmatch, {error, eilseq}}, decode_header(<<"=?us-ascii?B?dGhpcyBjb250YWlucyBhIGNvcHlyaWdodCCpIHN5bWJvbA==?=">>, "utf-8")),
					?assertEqual(<<"this contains a copyright  symbol">>, decode_header(<<"=?us-ascii?B?dGhpcyBjb250YWlucyBhIGNvcHlyaWdodCCpIHN5bWJvbA==?=">>, "utf-8//IGNORE")),
					?assertEqual(<<"this contains a copyright © symbol">>, decode_header(<<"=?iso-8859-1?B?dGhpcyBjb250YWlucyBhIGNvcHlyaWdodCCpIHN5bWJvbA==?=">>, "utf-8//IGNORE"))
			end
		},
		{"multiple unicode email addresses",
			fun() ->
					?assertEqual(<<"Jacek Złydach <jacek.zlydach@erlang-solutions.com>, chak de planet óóóó <jz@erlang-solutions.com>, Jacek Złydach <jacek.zlydach@erlang-solutions.com>, chak de planet óóóó <jz@erlang-solutions.com>">>, decode_header(<<"=?UTF-8?B?SmFjZWsgWsWCeWRhY2g=?= <jacek.zlydach@erlang-solutions.com>, =?UTF-8?B?Y2hhayBkZSBwbGFuZXQgw7PDs8Ozw7M=?= <jz@erlang-solutions.com>, =?UTF-8?B?SmFjZWsgWsWCeWRhY2g=?= <jacek.zlydach@erlang-solutions.com>, =?UTF-8?B?Y2hhayBkZSBwbGFuZXQgw7PDs8Ozw7M=?= <jz@erlang-solutions.com>">>, "utf-8"))
			end
		},
		{"decode something I encoded myself",
			fun() ->
				A = <<"Jacek Złydach <jacek.zlydach@erlang-solutions.com>">>,
				?assertEqual(A, decode_header(list_to_binary(rfc2047_utf8_encode(A)), "utf-8"))
			end
		}
	].

encoding_test_() ->
	[
		{"Simple email",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>},
							{<<"Message-ID">>, <<"<abcd@example.com>">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Date">>, <<"Sun, 01 Nov 2009 14:44:47 +0200">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = <<"From: me@example.com\r\nTo: you@example.com\r\nSubject: This is a test\r\nMessage-ID: <abcd@example.com>\r\nMIME-Version: 1.0\r\nDate: Sun, 01 Nov 2009 14:44:47 +0200\r\n\r\nThis is a plain message">>,
					?assertEqual(Result, encode(Email))
			end
		},
		{"Email with UTF-8 characters",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"Subject">>, <<"Fræderik Hølljen">>},
							{<<"From">>, <<"Fræderik Hølljen <me@example.com>">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Message-ID">>, <<"<abcd@example.com>">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Date">>, <<"Sun, 01 Nov 2009 14:44:47 +0200">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = <<"Subject: =?UTF-8?Q?Fr=C3=A6derik=20H=C3=B8lljen?=\r\nFrom: =?UTF-8?Q?Fr=C3=A6derik=20H=C3=B8lljen?= <me@example.com>\r\nTo: you@example.com\r\nMessage-ID: <abcd@example.com>\r\nMIME-Version: 1.0\r\nDate: Sun, 01 Nov 2009 14:44:47 +0200\r\n\r\nThis is a plain message">>,
					?assertEqual(Result, encode(Email))
			end
		},
		{"multipart/alternative email",
			fun() ->
					Email = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Content-Type">>,
								<<"multipart/alternative; boundary=wtf-123234234">>}],
						[{<<"content-type-params">>,
								[{<<"boundary">>, <<"wtf-123234234">>}]},
							{<<"disposition">>,<<"inline">>},
							{<<"disposition-params">>,[]}],
						[{<<"text">>,<<"plain">>,
								[{<<"Content-Type">>,
										<<"text/plain;charset=US-ASCII;format=flowed">>},
									{<<"Content-Transfer-Encoding">>,<<"7bit">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>},
											{<<"format">>,<<"flowed">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"This message contains rich text.">>},
							{<<"text">>,<<"html">>,
								[{<<"Content-Type">>,<<"text/html;charset=US-ASCII">>},
									{<<"Content-Transfer-Encoding">>,<<"7bit">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"<html><body>This message also contains HTML</body></html>">>}]},
					Result = decode(encode(Email)),
					?assertMatch({<<"multipart">>, <<"alternative">>, _, _, [{<<"text">>,
									<<"plain">>, _, _, _}, {<<"text">>, <<"html">>, _, _, _}]},
						Result)
			end
		},
		{"multipart/alternative email with encoding",
			fun() ->
					Email = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Content-Type">>,
								<<"multipart/alternative; boundary=wtf-123234234">>}],
						[{<<"content-type-params">>,
								[{<<"boundary">>, <<"wtf-123234234">>}]},
							{<<"disposition">>,<<"inline">>},
							{<<"disposition-params">>,[]}],
						[{<<"text">>,<<"plain">>,
								[{<<"Content-Type">>,
										<<"text/plain;charset=US-ASCII;format=flowed">>},
									{<<"Content-Transfer-Encoding">>,<<"quoted-printable">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>},
											{<<"format">>,<<"flowed">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"This message contains rich text.\r\n",
								"and is =quoted printable= encoded!">>},
							{<<"text">>,<<"html">>,
								[{<<"Content-Type">>,<<"text/html;charset=US-ASCII">>},
									{<<"Content-Transfer-Encoding">>,<<"base64">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"<html><body>This message also contains",
								"HTML and is base64",
								"encoded\r\n\r\n</body></html>">>}]},
					Result = decode(encode(Email)),
					?assertMatch({<<"multipart">>, <<"alternative">>, _, _, [{<<"text">>,
									<<"plain">>, _, _, <<"This message contains rich text.\r\n",
									"and is =quoted printable= encoded!">>},
								{<<"text">>, <<"html">>, _, _,
									<<"<html><body>This message also contains",
									"HTML and is base64",
									"encoded\r\n\r\n</body></html>">>}]},
						Result)
			end
		},
		{"Missing headers should be added",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertNot(undefined == proplists:get_value(<<"Message-ID">>, element(3, Result))),
					?assertNot(undefined == proplists:get_value(<<"Date">>, element(3, Result))),
					?assertEqual(undefined, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Reference header should be added in presence of In-Reply-To",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"In-Reply-To">>, <<"<abcd@example.com>">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertEqual(<<"<abcd@example.com>">>, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Reference header should be appended to in presence of In-Reply-To, if appropiate",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"In-Reply-To">>, <<"<abcd@example.com>">>},
							{<<"References">>, <<"<wxyz@example.com>">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertEqual(<<"<wxyz@example.com> <abcd@example.com>">>, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Reference header should NOT be appended to in presence of In-Reply-To, if already present",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"In-Reply-To">>, <<"<abcd@example.com>">>},
							{<<"References">>, <<"<wxyz@example.com> <abcd@example.com>">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertEqual(<<"<wxyz@example.com> <abcd@example.com>">>, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Content-Transfer-Encoding header should be added if missing and appropriate",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a plain message with some non-ascii characters øÿ\r\nso there">>},
					Encoded = encode(Email),
					Result = decode(Encoded),
					?assertEqual(<<"quoted-printable">>, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result))),
					Email2 = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a plain message with no non-ascii characters">>},
					Encoded2 = encode(Email2),
					Result2 = decode(Encoded2),
					?assertEqual(undefined, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result2))),
					Email3 = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"transfer-encoding">>, <<"base64">>}],
						<<"This is a plain message with no non-ascii characters">>},
					Encoded3 = encode(Email3),
					Result3 = decode(Encoded3),
					?assertEqual(<<"base64">>, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result3)))
			end
		},
		{"Content-Type header should be added if missing and appropriate",
			fun() ->
					Email = {<<"text">>, <<"html">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a HTML message with some non-ascii characters øÿ\r\nso there">>},
					Encoded = encode(Email),
					Result = decode(Encoded),
					?assertEqual(<<"quoted-printable">>, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result))),
					?assertMatch(<<"text/html;charset=utf-8">>, proplists:get_value(<<"Content-Type">>, element(3, Result))),
					Email2 = {<<"text">>, <<"html">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a HTML message with no non-ascii characters\r\nso there">>},
					Encoded2 = encode(Email2),
					Result2 = decode(Encoded2),
					?assertMatch(<<"text/html;charset=us-ascii">>, proplists:get_value(<<"Content-Type">>, element(3, Result2))),
					Email3 = {<<"text">>, <<"html">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a text message with some invisible non-ascii characters\r\nso there">>},
					Encoded3 = encode(Email3),
					Result3 = decode(Encoded3),
					?assertMatch(<<"text/html;charset=utf-8">>, proplists:get_value(<<"Content-Type">>, element(3, Result3)))
			end
		},
		{"Content-Type header should be added for subparts too, if missing and appropriate",
			fun() ->
					Email4 = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>, <<"plain">>, [], [], <<"This is a multipart message with some invisible non-ascii characters\r\nso there">>}]},
					Encoded4 = encode(Email4),
					Result4 = decode(Encoded4),
					?assertMatch(<<"text/plain;charset=utf-8">>, proplists:get_value(<<"Content-Type">>, element(3, lists:nth(1,element(5, Result4)))))
			end
		},
		{"Content-Type header should be not added for subparts if they're text/plain us-ascii",
			fun() ->
					Email4 = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>, <<"plain">>, [], [], <<"This is a multipart message with no non-ascii characters\r\nso there">>}]},
					Encoded4 = encode(Email4),
					Result4 = decode(Encoded4),
					?assertMatch(undefined, proplists:get_value(<<"Content-Type">>, element(3, lists:nth(1,element(5, Result4)))))
			end
		},
		{"Content-Type header should be added for subparts if they're text/html us-ascii",
			fun() ->
					Email4 = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>, <<"html">>, [], [], <<"This is a multipart message with no non-ascii characters\r\nso there">>}]},
					Encoded4 = encode(Email4),
					Result4 = decode(Encoded4),
					?assertMatch(<<"text/html;charset=us-ascii">>, proplists:get_value(<<"Content-Type">>, element(3, lists:nth(1,element(5, Result4)))))
			end
		},
		{"A boundary should be generated if applicable",
			fun() ->
					Email = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>,<<"plain">>,
								[],
								[],
								<<"This message contains rich text.\r\n",
								"and is =quoted printable= encoded!">>},
							{<<"text">>,<<"html">>,
								[],
								[],
								<<"<html><body>This message also contains",
								"HTML and is base64",
								"encoded\r\n\r\n</body></html>">>}]},
					Encoded = encode(Email),
					Result = decode(Encoded),
					Boundary = proplists:get_value(<<"boundary">>, proplists:get_value(<<"content-type-params">>, element(4, Result))),
					?assert(is_binary(Boundary)),
					% ensure we don't add the header multiple times
					?assertEqual(1, length(proplists:get_all_values(<<"Content-Type">>, element(3, Result)))),
					% headers should be appended, not prepended
					?assertMatch({<<"From">>, _}, lists:nth(1, element(3, Result))),
					ok
			end
		}
	].

roundtrip_test_() ->
	[
		{"roundtrip test for the gamut",
			fun() ->
					{ok, Email} = file:read_file("../testdata/the-gamut.eml"),
					Decoded = decode(Email),
					_Encoded = encode(Decoded),
					%{ok, F1} = file:open("f1", [write]),
					%{ok, F2} = file:open("f2", [write]),
					%file:write(F1, Email),
					%file:write(F2, Encoded),
					%file:close(F1),
					%file:close(F2),
					?assertEqual(Email, Email)
			end
		},
		{"round trip plain text only email",
			fun() ->
					{ok, Email} = file:read_file("../testdata/Plain-text-only.eml"),
					Decoded = decode(Email),
					_Encoded = encode(Decoded),
					%{ok, F1} = file:open("f1", [write]),
					%{ok, F2} = file:open("f2", [write]),
					%file:write(F1, Email),
					%file:write(F2, Encoded),
					%file:close(F1),
					%file:close(F2),
					?assertEqual(Email, Email)
			end
		},
		{"round trip quoted-printable email",
			fun() ->
					{ok, Email} = file:read_file("../testdata/testcase1"),
					Decoded = decode(Email),
					_Encoded = encode(Decoded),
					%{ok, F1} = file:open("f1", [write]),
					%{ok, F2} = file:open("f2", [write]),
					%file:write(F1, Email),
					%file:write(F2, Encoded),
					%file:close(F1),
					%file:close(F2),
					?assertEqual(Email, Email)
					%ok
			end
		}
	].

dkim_canonicalization_test_() ->
	%% * canonicalization from #3.4.5
    Hdrs = [<<"A : X\r\n">>,
            <<"B : Y\t\r\n\tZ  \r\n">>],
    Body = <<" C \r\nD \t E\r\n\r\n\r\n">>,
	[{"Simple body canonicalization",
      fun() ->
              ?assertEqual(<<" C \r\nD \t E\r\n">>, dkim_canonicalize_body(Body, simple)),
              ?assertEqual(<<"\r\n">>, dkim_canonicalize_body(<<>>, simple)),
              ?assertEqual(<<"\r\n">>, dkim_canonicalize_body(<<"\r\n\r\n\r\n">>, simple)),
              ?assertEqual(<<"A\r\n\r\nB\r\n">>, dkim_canonicalize_body(<<"A\r\n\r\nB\r\n\r\n">>, simple))
      end},
	{"Simple headers canonicalization",
	fun() ->
			?assertEqual([<<"A : X\r\n">>,
						  <<"B : Y\t\r\n\tZ  \r\n">>],
						 dkim_canonicalize_headers(Hdrs, simple))
	end},
	{"Relaxed headers canonicalization",
	 fun() ->
			 ?assertEqual([<<"a:X">>,	  % \r\n's are stripped by current impl.
						   <<"b:Y Z">>],
						  dkim_canonicalize_headers(Hdrs, relaxed))
	 end}].

dkim_sign_test_() ->
	%% * sign using testdata/dkim*.pem
	{ok, PrivKey} = file:read_file("../testdata/dkim-rsa-private.pem"),
	[{"Sign simple",
	  fun() ->
			  Email = {<<"text">>, <<"plain">>,
					   [{<<"From">>, <<"me@example.com">>},
						{<<"Subject">>, <<"Hello world!">>},
						{<<"Date">>, <<"Thu, 28 Nov 2013 04:15:44 +0400">>},
						{<<"Message-ID">>, <<"the-id">>},
						{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}],
					   [],
					   <<"123">>},
			  Options = [{dkim, [{s, <<"foo.bar">>},
								 {d, <<"example.com">>},
								 {c, {simple, simple}},
								 {t, {{2014, 2, 4}, {23, 15, 00}}},
								 {x, {{2114, 2, 4}, {23, 15, 00}}},
								 {private_key, {pem_plain, PrivKey}}]}],

			  Enc = encode(Email, Options),
			  %% This `Enc' value can be verified, for example, by Python script
			  %% https://launchpad.net/dkimpy like:
			  %% >>> pubkey = ''.join(open("../testdata/dkim-rsa-public.pem").read().splitlines()[1:-1])
			  %% >>> dns_mock = lambda *args: 'v=DKIM1; g=*; k=rsa; p=' + pubkey
			  %% >>> import dkim
			  %% >>> d = dkim.DKIM(mime_message) % pass `Enc' value as 1'st argument
			  %% >>> d.verify(dnsfunc=dns_mock)
			  %% True
			  {_, _, [{DkimHdrName, DkimHdrVal} | _], _, _} = decode(Enc),
			  ?assertEqual(<<"DKIM-Signature">>, DkimHdrName),
			  ?assertEqual(<<"t=1391555700; x=4547229300; s=foo.bar; h=from:to:subject:date; d=example.com; c=simple/simple; "
							 "bh=Afm/S7SaxS19en1h955RwsupTF914DQUPqYU8Nh7kpw=; a=rsa-sha256; v=1; "
							 "b=Mtja7WpVvtOFT8rfzOS/2fRZ492jrgsHgD5YUl5zmPQ/NEEMjVhVX0JCkfZxWpxiKe"
							 "qwl7nTJy3xecdg12feGT1rGC+rV0vAX8LVc+AJ4T4A50hE8L4hpJ1Tv5rt2O2t0Xu1Wx"
							 "yH6Cmrhhh56istjL+ba+U1EHhV7uZXGpWXGa4=">>, DkimHdrVal)
	  end},
	 {"Sign relaxed headers, simple body",
	  fun() ->
			  Email = {<<"text">>, <<"plain">>,
					   [{<<"From">>, <<"me@example.com">>},
						{<<"Subject">>, <<"Hello world!">>},
						{<<"Date">>, <<"Thu, 28 Nov 2013 04:15:44 +0400">>},
						{<<"Message-ID">>, <<"the-id-relaxed">>},
						{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}],
					   [],
					   <<"123">>},
			  Options = [{dkim, [{s, <<"foo.bar">>},
								 {d, <<"example.com">>},
								 {c, {relaxed, simple}},
								 {private_key, {pem_plain, PrivKey}}]}],

			  Enc = encode(Email, Options),
			  file:write_file("/home/seriy/relaxed-signed.eml", Enc),
			  {_, _, [{DkimHdrName, DkimHdrVal} | _], _, _} = decode(Enc),
			  %% io:format(user, "~p", [DkimHdrVal]),
			  ?assertEqual(<<"DKIM-Signature">>, DkimHdrName),
			  ?assertEqual(
				 <<"s=foo.bar; h=from:to:subject:date; d=example.com; c=relaxed/simple; "
				   "bh=Afm/S7SaxS19en1h955RwsupTF914DQUPqYU8Nh7kpw=; a=rsa-sha256; v=1; "
				   "b=dXxKq6A7m4A3AoS90feuLP+IxOyXFTPIibja52E2JCAyOsxvIGlI51xR1LvmEaelv9"
				   "jJTH9iGyAC7RzTKxrWV1QXayvr05bsTy3vDw7P4vfZ1gmspuP/3Icw+J8KEn+p6+CRrf"
				   "T97QadH42PT6XmO2v01q5nhMgNE4yQyf9DBJs=">>, DkimHdrVal)
	  end}].

-endif.
