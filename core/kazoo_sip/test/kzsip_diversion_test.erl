%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%% Diversion SIP header manipulation tests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzsip_diversion_test).

-include_lib("eunit/include/eunit.hrl").

from_binary_test() ->
    Header = <<"<sip:2134445555@1.2.3.4>;reason=unavailable;counter=3">>,
    JObj = kzsip_diversion:from_binary(Header),

    ?assertEqual(<<"<sip:2134445555@1.2.3.4>">>, kzsip_diversion:address(JObj)),
    ?assertEqual(<<"unavailable">>, kzsip_diversion:reason(JObj)),
    ?assertEqual(3, kzsip_diversion:counter(JObj)),
    ?assertEqual('undefined', kzsip_diversion:privacy(JObj)).

to_from_binary_test() ->
    Header = <<"<sip:2134445555@1.2.3.4>;reason=unavailable;counter=3">>,
    ?assertEqual(Header, kzsip_diversion:to_binary(kzsip_diversion:from_binary(Header))).

parse_name_addr_test() ->
    Header = <<"<sip:2134445555@1.2.3.4>;reason=unavailable">>,
    {Rest, Name} = kzsip_diversion:parse_name_addr_header(Header),
    ?assertEqual(<<"<sip:2134445555@1.2.3.4>">>, Name),
    ?assertEqual(<<"reason=unavailable">>, Rest).

from_binary_endline_test() ->
    Header = <<" <sip:WeSellFlowers@p4.isp.com>\n    ;reason=time-of-day    ;privacy=\"foo\"">>,
    JObj = kzsip_diversion:from_binary(Header),

    ?assertEqual(<<"<sip:WeSellFlowers@p4.isp.com>">>, kzsip_diversion:address(JObj)),
    ?assertEqual(<<"time-of-day">>, kzsip_diversion:reason(JObj)),
    ?assertEqual(0, kzsip_diversion:counter(JObj)),
    ?assertEqual(<<"foo">>, kzsip_diversion:privacy(JObj)).

from_binary_spaced_header_names_test() ->
    Header = <<"<sip:+12345556543@192.168.47.68:5060>;privacy=off; reason=unconditional; counter=1">>,
    JObj = kzsip_diversion:from_binary(Header),

    ?assertEqual(<<"<sip:+12345556543@192.168.47.68:5060>">>, kzsip_diversion:address(JObj)),
    ?assertEqual(<<"off">>, kzsip_diversion:privacy(JObj)),
    ?assertEqual(<<"unconditional">>, kzsip_diversion:reason(JObj)),
    ?assertEqual(1, kzsip_diversion:counter(JObj)).

from_binary_extensions_test() ->
    Header = <<"<sip:+12345556543@192.168.47.68:5060>;privacy=off;reason=unconditional;counter=1;some_extension;other_extension=foo">>,

    JObj = kzsip_diversion:from_binary(Header),
    Extensions = kzsip_diversion:extensions(JObj),

    ?assertEqual('true', is_list(Extensions)),
    ?assertEqual(<<"foo">>, props:get_value(<<"other_extension">>, Extensions)),
    ?assertEqual('true', props:get_value(<<"some_extension">>, Extensions)),
    ?assertEqual(Header, kzsip_diversion:to_binary(JObj)).

to_binary_fix_address_test() ->
    Header = <<"sip:0123456789@1.22.133.4;counter=1 ">>,
    NewHeader = kzsip_diversion:to_binary(kzsip_diversion:from_binary(Header)),
    ?assertEqual(<<"<sip:0123456789@1.22.133.4>;counter=1">>, NewHeader).

user_test() ->
    Header = <<"<sip:0123456789@1.22.133.4>;counter=1">>,
    D = kzsip_diversion:from_binary(Header),
    ?assertEqual(<<"0123456789">>, kzsip_diversion:user(D)).

set_user_test() ->
    Header = <<"<sip:0123456789@1.22.133.4>;counter=1">>,
    D = kzsip_diversion:set_user(kzsip_diversion:from_binary(Header), <<"12345556789">>),

    ?assertEqual(<<"12345556789">>, kzsip_diversion:user(D)),
    ?assertEqual(<<"<sip:12345556789@1.22.133.4>">>, kzsip_diversion:address(D)).
