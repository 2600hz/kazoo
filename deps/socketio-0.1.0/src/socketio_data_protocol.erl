-module(socketio_data_protocol).
-export([encode/1,
         decode/1]).
-compile([{no_auto_import, [error/2]}]).
-include_lib("eunit/include/eunit.hrl").

%% The source code was taken and modified from https://github.com/yrashk/socket.io-erlang/blob/master/src/socketio_data_v1.erl

-define(FRAME, 16#fffd).

encode([Message]) ->
    encode(Message);
encode(Messages) when is_list(Messages) ->
    lists:foldl(fun(Message, AccIn) ->
			Packet = encode(Message),
			LenBin = binary:list_to_bin(integer_to_list(binary_utf8_len(Packet))),
			<<AccIn/binary, ?FRAME/utf8, LenBin/binary, ?FRAME/utf8, Packet/binary>>
		end, <<>>, Messages);
encode({message, Id, EndPoint, Message}) ->
    message(Id, EndPoint, Message);
encode({json, Id, EndPoint, Message}) ->
    json(Id, EndPoint, Message);
encode({connect, Endpoint}) ->
    connect(Endpoint);
encode({event, EventName, EventArgs}) ->
    event(<<>>, <<>>, EventName, EventArgs);
encode(heartbeat) ->
    heartbeat();
encode(nop) ->
    nop();
encode(disconnect) ->
    disconnect(<<>>).

connect(<<>>) ->
    <<"1::">>;
connect(Endpoint) ->
    <<"1::", Endpoint/binary>>.

disconnect(<<>>) ->
    <<"0::">>;
disconnect(Endpoint) ->
    <<"0::", Endpoint/binary>>.

heartbeat() ->
    <<"2::">>.

nop() ->
    <<"8::">>.

message(Id, EndPoint, Msg) when is_integer(Id) ->
    IdBin = binary:list_to_bin(integer_to_list(Id)),
    <<"3:", IdBin/binary, ":", EndPoint/binary, ":", Msg/binary>>;
message(Id, EndPoint, Msg) when is_binary(Id) ->
    <<"3:", Id/binary, ":", EndPoint/binary, ":", Msg/binary>>.

json(Id, EndPoint, JObj) ->
    <<"4:", (wh_util:to_binary(Id))/binary
        ,":", EndPoint/binary
        ,":", (wh_json:encode(JObj))/binary>>.

event(Id, EndPoint, EventName, EventArgs) ->
    JObj = wh_json:from_list([{<<"name">>, EventName}, {<<"args">>, EventArgs}]),
    <<"5:", (wh_util:to_binary(Id))/binary
        ,":", (wh_util:to_binary(EndPoint))/binary
        ,":", (wh_json:encode(JObj))/binary>>.


error(EndPoint, Reason) ->
    [<<"7::">>, EndPoint, $:, Reason].
error(EndPoint, Reason, Advice) ->
    [<<"7::">>, EndPoint, $:, Reason, $+, Advice].

binary_utf8_len(Binary) ->
    binary_utf8_len(Binary, 0).
binary_utf8_len(<<>>, Len) ->
    Len;
binary_utf8_len(<<_X/utf8, Binary/binary>>, Len) ->
    binary_utf8_len(Binary, Len+1).

binary_utf8_split(Binary, Len) ->
    binary_utf8_split(Binary, Len, <<>>).
binary_utf8_split(<<Binary/binary>>, 0, AccIn) ->
    {AccIn, Binary};
binary_utf8_split(<<>>, _, AccIn) ->
    {AccIn, <<>>};
binary_utf8_split(<<X/utf8, Binary/binary>>, Len, AccIn) ->
    binary_utf8_split(Binary, Len-1, <<AccIn/binary, X/utf8>>).

decode_frame_len(X) ->
    decode_frame_len(X, "").
decode_frame_len(<<?FRAME/utf8, Rest/binary>>, Acc) ->
    L = lists:reverse(Acc),
    {list_to_integer(L), Rest};
decode_frame_len(<<Num, Rest/binary>>, Acc) when Num-$0 >= 0, Num-$0 =< 9 ->
    decode_frame_len(Rest, [Num|Acc]).

decode_frame(<<>>, Packets) ->
    Packets;
decode_frame(<<?FRAME/utf8, Rest/binary>>, Packets) ->
    {Len, R1} =  decode_frame_len(Rest),
    {Msg, R2} = binary_utf8_split(R1, Len),
    Packet = decode_packet(Msg),
    decode_frame(R2, [Packet|Packets]).

%%% PARSING
decode(<<?FRAME/utf8, Rest/binary>>) ->
    Frames = decode_frame(<<?FRAME/utf8, Rest/binary>>, []),
    lists:reverse(Frames);

decode(Binary) ->
    [decode_packet(Binary)].

decode_packet(<<"0">>) -> disconnect;
decode_packet(<<"0::", EndPoint/binary>>) -> {disconnect, EndPoint};
%% Incomplete, needs to handle queries
decode_packet(<<"1::", EndPoint/binary>>) -> {connect, EndPoint};
decode_packet(<<"2::">>) -> heartbeat;
decode_packet(<<"3:", Rest/binary>>) ->
    {Id, R1} = id(Rest),
    {EndPoint, Data} = endpoint(R1),
    {message, Id, EndPoint, Data};
decode_packet(<<"4:", Rest/binary>>) ->
    {Id, R1} = id(Rest),
    {EndPoint, Data} = endpoint(R1),
    {json, Id, EndPoint, wh_json:decode(Data)};
decode_packet(<<"5:", Rest/binary>>) ->
    {Id, R1} = id(Rest),
    {EndPoint, Data} = endpoint(R1),
    Json = wh_json:decode(Data),
    EventName = wh_json:get_value(<<"name">>, Json),
    [EventArgs] = wh_json:get_value(<<"args">>, Json, [wh_json:new()]),
    {event, Id, EndPoint, EventName, EventArgs};
decode_packet(<<"7::", Rest/binary>>) ->
    {EndPoint, R1} = endpoint(Rest),
    case reason(R1) of
        {Reason, Advice} ->
            {error, EndPoint, Reason, Advice};
        Reason ->
            {error, EndPoint, Reason}
    end.

id(X) -> id(X, "").
id(<<$:, Rest/binary>>, "") ->
    {<<>>, Rest};
id(<<$:, Rest/binary>>, Acc) ->
    L = lists:reverse(Acc),
    {list_to_integer(L), Rest};
id(<<$+,$:, Rest/binary>>, Acc) when Acc =/= "" ->
    {lists:reverse([$+|Acc]), Rest};
id(<<Num, Rest/binary>>, Acc) when Num-$0 >= 0, Num-$0 =< 9 ->
    id(Rest, [Num|Acc]).

endpoint(X) -> endpoint(X, "").
endpoint(<<$:, Rest/binary>>, Acc) -> {binary:list_to_bin(lists:reverse(Acc)), Rest};
endpoint(<<X, Rest/binary>>, Acc) -> endpoint(Rest, [X|Acc]).

reason(X) ->
    case list_to_tuple(binary:split(X, <<"+">>)) of
	{E} -> E;
	T -> T
    end.