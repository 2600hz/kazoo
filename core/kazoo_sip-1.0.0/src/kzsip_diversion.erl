%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% Diversion SIP header manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzsip_diversion).

-export([to_binary/1
         ,from_binary/1
        ]).

-export([reason/1
         ,counter/1
         ,limit/1
         ,privacy/1
         ,screen/1
         ,extensions/1
         ,address/1
        ]).

-define(PARAM_REASON, <<"reason">>).
-define(PARAM_COUNTER, <<"counter">>).
-define(PARAM_LIMIT, <<"limit">>).
-define(PARAM_PRIVACY, <<"privacy">>).
-define(PARAM_SCREEN, <<"screen">>).
-define(PARAM_EXTENSION, <<"extension">>).
-define(PARAM_ADDRESS, <<"address">>).

-define(SOLO_EXTENSION, <<"_solo_">>).

-include("kazoo_sip.hrl").

-type diversion() :: wh_json:object().
-export_type([diversion/0]).

-spec reason(diversion()) -> api_binary().
-spec counter(diversion()) -> non_neg_integer().
-spec limit(diversion()) -> api_integer().
-spec privacy(diversion()) -> api_binary().
-spec screen(diversion()) -> api_binary().
-spec extensions(diversion()) -> api_list().
-spec address(diversion()) -> api_binary().

address(JObj) ->
    wh_json:get_ne_binary_value(?PARAM_ADDRESS, JObj).
reason(JObj) ->
    wh_json:get_ne_binary_value(?PARAM_REASON, JObj).
counter(JObj) ->
    wh_json:get_integer_value(?PARAM_COUNTER, JObj, 0).
limit(JObj) ->
    wh_json:get_integer_value(?PARAM_LIMIT, JObj).
privacy(JObj) ->
    wh_json:get_ne_binary_value(?PARAM_PRIVACY, JObj).
screen(JObj) ->
    wh_json:get_ne_binary_value(?PARAM_SCREEN, JObj).
extensions(JObj) ->
    case wh_json:get_ne_value(?PARAM_EXTENSION, JObj) of
        'undefined' -> 'undefined';
        Extensions ->
           lists:foldl(fun extensions_fold/2, [], wh_json:to_proplist(Extensions))
    end.

-spec extensions_fold({ne_binary(), ne_binary()}, wh_proplist()) ->
                             wh_proplist().
extensions_fold({K, ?SOLO_EXTENSION}, Acc) ->
    [K | Acc];
extensions_fold({_K, _V}=Extention, Acc) ->
    [Extention | Acc].

-spec from_binary(ne_binary()) -> wh_json:object().
from_binary(<<"Diversion:", Header/binary>>) ->
    from_binary(Header);
from_binary(Header) ->
    case parse_name_addr_header(Header) of
        {<<>>, Name} ->
            wh_json:from_list([{?PARAM_ADDRESS, Name}]);
        {Rest, Name} ->
            parse_params(Rest, wh_json:set_value(?PARAM_ADDRESS, Name, wh_json:new()))
    end.

-spec parse_name_addr_header(ne_binary()) -> {binary(), ne_binary()}.
parse_name_addr_header(Header) ->
    Start = lists:foldl(fun name_addr_start/2, Header, [$\n, $\s]),
    parse_name_addr({Start, []}).

-spec parse_name_addr({binary(), list()}) -> {binary(), ne_binary()}.
parse_name_addr({<<"<", Header/binary>>, Acc}) ->
    parse_name_addr(parse_name_addr_angle(Header, [$< | Acc]));
parse_name_addr({<<"\"", Header/binary>>, Acc}) ->
    parse_name_addr(parse_name_addr_double_quote(Header, [$" | Acc]));
parse_name_addr({<<"'", Header/binary>>, Acc}) ->
    parse_name_addr(parse_name_addr_single_quote(Header, [$" | Acc]));
parse_name_addr({<<"\n", Header/binary>>, Acc}) ->
    {Header, list_to_binary(lists:reverse(Acc))};
parse_name_addr({<<";", Header/binary>>, Acc}) ->
    {Header, list_to_binary(lists:reverse(Acc))};
parse_name_addr({<<>>, Acc}) ->
    {<<>>, list_to_binary(lists:reverse(Acc))};
parse_name_addr({<<C, Header/binary>>, Acc}) ->
    parse_name_addr({Header, [C | Acc]}).

-spec parse_name_addr_angle(ne_binary(), iolist()) ->
                                   {binary(), iolist()}.
parse_name_addr_angle(<<">", Header/binary>>, Acc) ->
    {wh_util:strip_binary(Header), [$> | Acc]};
parse_name_addr_angle(<<C, Header/binary>>, Acc) ->
    parse_name_addr_angle(Header, [C | Acc]).

-spec parse_name_addr_double_quote(ne_binary(), iolist()) ->
                                          {binary(), iolist()}.
parse_name_addr_double_quote(<<"\"", Header/binary>>, Acc) ->
    {Header, [$" | Acc]};
parse_name_addr_double_quote(<<C, Header/binary>>, Acc) ->
    parse_name_addr_double_quote(Header, [C | Acc]).

-spec parse_name_addr_single_quote(ne_binary(), iolist()) ->
                                          {binary(), iolist()}.
parse_name_addr_single_quote(<<"'", Header/binary>>, Acc) ->
    {Header, [$' | Acc]};
parse_name_addr_single_quote(<<C, Header/binary>>, Acc) ->
    parse_name_addr_single_quote(Header, [C | Acc]).

-spec name_addr_start(char(), ne_binary()) -> ne_binary().
name_addr_start(Char, Bin) ->
    wh_util:strip_left_binary(Bin, Char).

-spec parse_params(ne_binary(), wh_json:object()) -> wh_json:object().
parse_params(Params, JObj) ->
    case binary:split(Params, <<";">>, ['trim']) of
        [Param] -> parse_param(Param, JObj);
        [Param, Rest] -> parse_params(Rest, parse_param(Param, JObj))
    end.

-spec parse_param(ne_binary(), wh_json:object()) -> wh_json:object().
-spec parse_param(ne_binary(), ne_binary(), wh_json:object()) -> wh_json:object().
parse_param(Param, JObj) ->
    case binary:split(Param, <<"=">>, ['trim']) of
        [Name, Value] ->
            parse_param(wh_util:strip_binary(Name), wh_util:strip_binary(Value), JObj);
        [Extension] ->
            add_extension(wh_util:strip_binary(Extension), JObj)
    end.

parse_param(?PARAM_REASON, Value, JObj) ->
    wh_json:set_value(?PARAM_REASON, parse_reason_param(Value), JObj);
parse_param(?PARAM_COUNTER, Value, JObj) ->
    wh_json:set_value(?PARAM_COUNTER, wh_util:to_integer(Value), JObj);
parse_param(?PARAM_LIMIT, Value, JObj) ->
    wh_json:set_value(?PARAM_LIMIT, wh_util:to_integer(Value), JObj);
parse_param(?PARAM_PRIVACY, Value, JObj) ->
    wh_json:set_value(?PARAM_PRIVACY, parse_privacy_param(Value), JObj);
parse_param(?PARAM_SCREEN, Value, JObj) ->
    wh_json:set_value(?PARAM_SCREEN, parse_screen_param(Value), JObj);
parse_param(Extension, Value, JObj) ->
   add_extension(Extension, maybe_unquote(Value), JObj).

-spec add_extension(binary(), wh_json:object()) ->
                           wh_json:object().
add_extension(<<>>, JObj) -> JObj;
add_extension(Extension, JObj) ->
    Extensions = wh_json:get_value(?PARAM_EXTENSION, JObj, wh_json:new()),
    wh_json:set_value(?PARAM_EXTENSION
                      ,wh_json:set_value(maybe_unquote(Extension)
                                         ,?SOLO_EXTENSION
                                         ,Extensions
                                        )
                      ,JObj
                     ).

-spec add_extension(ne_binary(), ne_binary(), wh_json:object()) ->
                           wh_json:object().
add_extension(Extension, Value, JObj) ->
    Extensions = wh_json:get_value(?PARAM_EXTENSION, JObj, wh_json:new()),
    wh_json:set_value(?PARAM_EXTENSION
                      ,wh_json:set_value(Extension
                                         ,maybe_unquote(Value)
                                         ,Extensions
                                        )
                      ,JObj
                     ).

-define(PARAM_REASON_LITERALS, [<<"unknown">>
                                ,<<"user-busy">>
                                ,<<"no-answer">>
                                ,<<"unavailable">>
                                ,<<"unconditional">>
                                ,<<"time-of-day">>
                                ,<<"do-not-disturb">>
                                ,<<"deflection">>
                                ,<<"follow-me">>
                                ,<<"out-of-service">>
                                ,<<"away">>
                               ]).
parse_reason_param(Reason) ->
    parse_param_value(Reason, ?PARAM_REASON_LITERALS).

-define(PARAM_PRIVACY_LITERALS, [<<"full">>
                                 ,<<"name">>
                                 ,<<"uri">>
                                 ,<<"off">>
                                ]).
parse_privacy_param(Privacy) ->
    parse_param_value(Privacy, ?PARAM_PRIVACY_LITERALS).

-define(PARAM_SCREEN_LITERALS, [<<"yes">>, <<"no">>]).
parse_screen_param(Screen) ->
    parse_param_value(Screen, ?PARAM_SCREEN_LITERALS).

-spec parse_param_value(ne_binary(), ne_binaries()) -> ne_binary().
parse_param_value(<<"\"", _/binary>> = Param, _Literals) ->
    parse_quoted_param(Param);
parse_param_value(<<"'", _/binary>> = Param, _Literals) ->
    parse_quoted_param(Param);
parse_param_value(Param, [Literal | Literals]) ->
    case Param of
        <<Literal, _/binary>> -> Literal;
        _ -> parse_param_value(Param, Literals)
    end;
parse_param_value(Param, []) ->
    parse_token_param(Param).

-spec parse_token_param(ne_binary(), iolist()) -> binary().
parse_token_param(Token) ->
    parse_token_param(Token, []).
parse_token_param(<<$ , _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_token_param(<<$\n, _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_token_param(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_token_param(<<C, Token/binary>>, Acc) ->
    parse_token_param(Token, [C | Acc]).

-spec parse_quoted_param(ne_binary()) -> ne_binary().
parse_quoted_param(<<"\"", Quoted/binary>>) ->
    parse_double_quoted_param(Quoted, []);
parse_quoted_param(<<"'", Quoted/binary>>) ->
    parse_single_quoted_param(Quoted, []).

-spec parse_double_quoted_param(ne_binary(), iolist()) -> ne_binary().
parse_double_quoted_param(<<"\"", _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_double_quoted_param(<<C, Quoted/binary>>, Acc) ->
    parse_double_quoted_param(Quoted, [C | Acc]).

-spec parse_single_quoted_param(ne_binary(), iolist()) -> ne_binary().
parse_single_quoted_param(<<"'", _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_single_quoted_param(<<C, Quoted/binary>>, Acc) ->
    parse_single_quoted_param(Quoted, [C | Acc]).

-spec maybe_unquote(ne_binary()) -> ne_binary().
maybe_unquote(<<"\"", Value/binary>>) ->
    Length = byte_size(Value),
    binary:part(Value, 0, Length-1);
maybe_unquote(Value) -> Value.

-spec to_binary(wh_json:object()) -> ne_binary().
to_binary(JObj) ->
    Address = wh_json:get_value(?PARAM_ADDRESS, JObj),
    maybe_add_params(JObj, Address).

-spec maybe_add_params(wh_json:object(), ne_binary()) -> ne_binary().
maybe_add_params(JObj, Address) ->
    wh_json:foldl(fun maybe_add_param/3, Address, JObj).

-spec maybe_add_param(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
maybe_add_param(?PARAM_REASON, Reason, Acc) ->
    <<Acc/binary, ";", ?PARAM_REASON/binary, "=", (encode_reason_param(wh_util:strip_binary(Reason)))/binary>>;
maybe_add_param(?PARAM_COUNTER, Count, Acc) ->
    <<Acc/binary, ";", ?PARAM_COUNTER/binary, "=", (wh_util:to_binary(Count))/binary>>;
maybe_add_param(?PARAM_LIMIT, Limit, Acc) ->
    <<Acc/binary, ";", ?PARAM_LIMIT/binary, "=", (wh_util:to_binary(Limit))/binary>>;
maybe_add_param(?PARAM_PRIVACY, Priv, Acc) ->
    <<Acc/binary, ";", ?PARAM_PRIVACY/binary, "=", (encode_privacy_param(wh_util:strip_binary(Priv)))/binary>>;
maybe_add_param(?PARAM_SCREEN, Screen, Acc) ->
    <<Acc/binary, ";", ?PARAM_SCREEN/binary, "=", (encode_screen_param(wh_util:strip_binary(Screen)))/binary>>;
maybe_add_param(?PARAM_EXTENSION, Extensions, Acc) ->
    lists:foldl(fun maybe_add_extension/2, Acc, wh_json:to_proplist(Extensions));
maybe_add_param(_, _, Acc) -> Acc.

-spec maybe_add_extension({ne_binary(), ne_binary()}, ne_binary()) -> ne_binary().
maybe_add_extension({Key, ?SOLO_EXTENSION}, Acc) ->
    <<Acc/binary, ";", (wh_util:to_binary(Key))/binary>>;
maybe_add_extension({Key, Value}, Acc) ->
    <<Acc/binary, ";", (wh_util:to_binary(Key))/binary, "=", (wh_util:to_binary(Value))/binary>>;
maybe_add_extension(Key, Acc) ->
    <<Acc/binary, ";", (wh_util:to_binary(Key))/binary>>.

-spec encode_reason_param(ne_binary()) -> ne_binary().
encode_reason_param(Reason) ->
    encode_param(Reason, ?PARAM_REASON_LITERALS).
encode_privacy_param(Privacy) ->
    encode_param(Privacy, ?PARAM_PRIVACY_LITERALS).
encode_screen_param(Screen) ->
    encode_param(Screen, ?PARAM_SCREEN_LITERALS).

-spec encode_param(ne_binary(), ne_binaries()) -> ne_binary().
encode_param(Param, Literals) ->
    case lists:member(Param, Literals) of
        'true' -> Param;
        'false' -> quote_param(Param)
    end.

-spec quote_param(ne_binary()) -> ne_binary().
quote_param(Param) -> <<"\"", Param/binary, "\"">>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_binary_test() ->
    Header = <<"<sip:2134445555@1.2.3.4>;reason=unavailable;counter=3">>,
    JObj = from_binary(Header),

    ?assertEqual(<<"<sip:2134445555@1.2.3.4>">>, address(JObj)),
    ?assertEqual(<<"unavailable">>, reason(JObj)),
    ?assertEqual(3, counter(JObj)),
    ?assertEqual('undefined', privacy(JObj)).

to_from_binary_test() ->
    Header = <<"<sip:2134445555@1.2.3.4>;reason=unavailable;counter=3">>,
    ?assertEqual(Header, to_binary(from_binary(Header))).

parse_name_addr_test() ->
    Header = <<"<sip:2134445555@1.2.3.4>;reason=unavailable">>,
    {Rest, Name} = parse_name_addr_header(Header),
    ?assertEqual(<<"<sip:2134445555@1.2.3.4>">>, Name),
    ?assertEqual(<<"reason=unavailable">>, Rest).

from_binary_endline_test() ->
    Header = <<" <sip:WeSellFlowers@p4.isp.com>\n    ;reason=time-of-day    ;privacy=\"foo\"">>,
    JObj = from_binary(Header),

    ?assertEqual(<<"<sip:WeSellFlowers@p4.isp.com>">>, address(JObj)),
    ?assertEqual(<<"time-of-day">>, reason(JObj)),
    ?assertEqual(0, counter(JObj)),
    ?assertEqual(<<"foo">>, privacy(JObj)).

from_binary_spaced_header_names_test() ->
    Header = <<"<sip:+12345556543@192.168.47.68:5060>;privacy=off; reason=unconditional; counter=1">>,
    JObj = from_binary(Header),

    ?assertEqual(<<"<sip:+12345556543@192.168.47.68:5060>">>, address(JObj)),
    ?assertEqual(<<"off">>, privacy(JObj)),
    ?assertEqual(<<"unconditional">>, reason(JObj)),
    ?assertEqual(1, counter(JObj)).

from_binary_extensions_test() ->
    Header = <<"<sip:+12345556543@192.168.47.68:5060>;privacy=off;reason=unconditional;counter=1;some_extension;other_extension=foo">>,

    JObj = from_binary(Header),
    Extensions = extensions(JObj),

    ?assertEqual('true', is_list(Extensions)),
    ?assertEqual(<<"foo">>, props:get_value(<<"other_extension">>, Extensions)),
    ?assertEqual('true', props:get_value(<<"some_extension">>, Extensions)),
    ?assertEqual(Header, to_binary(JObj)).


-endif.
