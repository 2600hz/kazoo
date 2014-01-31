%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% Diversion SIP header manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_diversion).

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


-include("ecallmgr.hrl").

-opaque diversion() :: wh_json:object().
-export_type([diversion/0]).

-spec reason(diversion()) -> api_binary().
-spec counter(diversion()) -> api_integer().
-spec limit(diversion()) -> api_integer().
-spec privacy(diversion()) -> api_binary().
-spec screen(diversion()) -> api_binary().
-spec extensions(diversion()) -> api_list().
-spec address(diversion()) -> api_binary().

reason(JObj) ->
    wh_json:get_value(?PARAM_REASON, JObj).
counter(JObj) ->
    wh_json:get_value(?PARAM_COUNTER, JObj).
limit(JObj) ->
    wh_json:get_value(?PARAM_LIMIT, JObj).
privacy(JObj) ->
    wh_json:get_value(?PARAM_PRIVACY, JObj).
screen(JObj) ->
    wh_json:get_value(?PARAM_SCREEN, JObj).
extensions(JObj) ->
    wh_json:get_value(?PARAM_EXTENSION, JObj).
address(JObj) ->
    wh_json:get_value(?PARAM_ADDRESS, JObj).

-spec from_binary(ne_binary()) -> wh_json:object().
from_binary(Header) ->
    parse_header(binary:replace(Header, <<"\n">>, <<>>, ['global'])).

-spec parse_header(ne_binary()) -> wh_json:object().
parse_header(Header) ->
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

parse_name_addr_angle(<<">", Header/binary>>, Acc) ->
    {wh_util:strip_binary(Header), [$> | Acc]};
parse_name_addr_angle(<<C, Header/binary>>, Acc) ->
    parse_name_addr_angle(Header, [C | Acc]).

parse_name_addr_double_quote(<<"\"", Header/binary>>, Acc) ->
    {Header, [$" | Acc]};
parse_name_addr_double_quote(<<C, Header/binary>>, Acc) ->
    parse_name_addr_double_quote(Header, [C | Acc]).

parse_name_addr_single_quote(<<"'", Header/binary>>, Acc) ->
    {Header, [$' | Acc]};
parse_name_addr_single_quote(<<C, Header/binary>>, Acc) ->
    parse_name_addr_single_quote(Header, [C | Acc]).

name_addr_start(Char, Bin) ->
    wh_util:strip_left_binary(Bin, Char).

parse_params(Params, JObj) ->
    case binary:split(Params, <<";">>) of
        [Param] -> parse_param(Param, JObj);
        [Param, Rest] -> parse_params(Rest, parse_param(Param, JObj))
    end.

parse_param(Param, JObj) ->
    case binary:split(Param, <<"=">>) of
        [Name, Value] ->
            parse_param(Name, Value, JObj);
        [Extension] ->
            add_extension(Extension, JObj)
    end.

parse_param(?PARAM_REASON, Value, JObj) ->
    wh_json:set_value(?PARAM_REASON, maybe_unquote(Value), JObj);
parse_param(?PARAM_COUNTER, Value, JObj) ->
    wh_json:set_value(?PARAM_COUNTER, wh_util:to_integer(Value), JObj);
parse_param(?PARAM_LIMIT, Value, JObj) ->
    wh_json:set_value(?PARAM_LIMIT, wh_util:to_integer(Value), JObj);
parse_param(?PARAM_PRIVACY, Value, JObj) ->
    wh_json:set_value(?PARAM_PRIVACY, maybe_unquote(Value), JObj);
parse_param(?PARAM_SCREEN, Value, JObj) ->
    wh_json:set_value(?PARAM_SCREEN, maybe_unquote(Value), JObj);
parse_param(Extension, Value, JObj) ->
   add_extension(Extension, maybe_unquote(Value), JObj).

add_extension(Extension, JObj) ->
    wh_json:set_value(?PARAM_EXTENSION
                      ,[maybe_unquote(Extension)
                        | wh_json:get_value(?PARAM_EXTENSION, JObj, [])
                       ]
                      ,JObj
                     ).
add_extension(Extension, Value, JObj) ->
    wh_json:set_value(?PARAM_EXTENSION
                      ,[{Extension, maybe_unquote(Value)}
                        | wh_json:get_value(?PARAM_EXTENSION, JObj, [])
                       ]
                      ,JObj
                     ).

maybe_unquote(<<"\"", Value/binary>>) ->
    Length = byte_size(Value),
    binary:part(Value, 0, Length-1);
maybe_unquote(Value) -> Value.

-spec to_binary(wh_json:object()) -> ne_binary().
to_binary(JObj) ->
    Address = wh_json:get_value(?PARAM_ADDRESS, JObj),
    maybe_add_params(JObj, Address).

maybe_add_params(JObj, Address) ->
    wh_json:foldl(fun maybe_add_param/3, Address, JObj).

maybe_add_param(?PARAM_REASON, Reason, Acc) ->
    <<Acc/binary, ";", ?PARAM_REASON/binary, "=", (wh_util:to_binary(Reason))/binary>>;
maybe_add_param(?PARAM_COUNTER, Count, Acc) ->
    <<Acc/binary, ";", ?PARAM_COUNTER/binary, "=", (wh_util:to_binary(Count))/binary>>;
maybe_add_param(?PARAM_LIMIT, Limit, Acc) ->
    <<Acc/binary, ";", ?PARAM_LIMIT/binary, "=", (wh_util:to_binary(Limit))/binary>>;
maybe_add_param(?PARAM_PRIVACY, Priv, Acc) ->
    <<Acc/binary, ";", ?PARAM_PRIVACY/binary, "=", (wh_util:to_binary(Priv))/binary>>;
maybe_add_param(?PARAM_SCREEN, Screen, Acc) ->
    <<Acc/binary, ";", ?PARAM_SCREEN/binary, "=", (wh_util:to_binary(Screen))/binary>>;
maybe_add_param(?PARAM_EXTENSION, Extensions, Acc) ->
    lists:foldl(fun maybe_add_extension/2, Acc, Extensions);
maybe_add_param(_, _, Acc) -> Acc.

maybe_add_extension({Key, Value}, Acc) ->
    <<Acc/binary, ";", (wh_util:to_binary(Key))/binary, "=", (wh_util:to_binary(Value))/binary>>;
maybe_add_extension(Key, Acc) ->
    <<Acc/binary, ";", (wh_util:to_binary(Key))/binary>>.

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
    Header = <<" <sip:WeSellFlowers@p4.isp.com>\n    ;reason=time-of-day">>,
    JObj = from_binary(Header),

    ?assertEqual(<<"<sip:WeSellFlowers@p4.isp.com>">>, address(JObj)),
    ?assertEqual(<<"time-of-day">>, reason(JObj)),
    ?assertEqual('undefined', counter(JObj)),
    ?assertEqual('undefined', privacy(JObj)).

-endif.
