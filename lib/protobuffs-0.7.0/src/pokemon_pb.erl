%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(pokemon_pb).
-export([encode_pikachu/1, decode_pikachu/1]).
-export([has_extension/2, extension_size/1, get_extension/2,
         set_extension/3]).
-export([decode_extensions/1]).
-export([encode/1, decode/2]).
-record(pikachu, {abc, def, '$extensions' = dict:new()}).

%% ENCODE
encode(Record) ->
    encode(element(1, Record), Record).

encode_pikachu(Record) when is_record(Record, pikachu) ->
    encode(pikachu, Record).

encode(pikachu, Record) ->
    iolist_to_binary(iolist(pikachu, Record) ++ encode_extensions(Record)).

encode_extensions(#pikachu{'$extensions' = Extends}) ->
    [pack(Key, Optionalness, Data, Type, Accer) ||
        {Key, {Optionalness, Data, Type, Accer}} <- dict:to_list(Extends)];
encode_extensions(_) -> [].

iolist(pikachu, Record) ->
    [pack(1, required, with_default(Record#pikachu.abc, none), string, [])].

with_default(Val, none) -> Val;
with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];

pack(_, repeated, undefined, _, _) -> [];

pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
    
pack(FNum, required, undefined, Type, _) ->
    exit({error, {required_field_is_undefined, FNum, Type}});

pack(_, repeated, [], _, Acc) -> 
    lists:reverse(Acc);

pack(FNum, repeated, [Head|Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type, [pack(FNum, optional, Head, Type, [])|Acc]);

pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);

pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName|_] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);

pack(FNum, _, Data, Type, _) when Type=:=bool;Type=:=int32;Type=:=uint32;
				  Type=:=int64;Type=:=uint64;Type=:=sint32;
				  Type=:=sint64;Type=:=fixed32;Type=:=sfixed32;
				  Type=:=fixed64;Type=:=sfixed64;Type=:=string;
				  Type=:=bytes;Type=:=float;Type=:=double ->
    protobuffs:encode(FNum, Data, Type);

pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type,Data), enum).

enum_to_int(pikachu,value) ->
    1.

int_to_enum(_,Val) ->
    Val.

%% DECODE
decode_pikachu(Bytes) when is_binary(Bytes) ->
    decode(pikachu, Bytes).
    
decode(pikachu, Bytes) when is_binary(Bytes) ->
    Types = [{1, abc, int32, []}, {2, def, double, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pikachu, Decoded).
    
decode(<<>>, _, Acc) -> Acc;
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keysearch(FNum, 1, Types) of
        {value, {FNum, Name, Type, Opts}} ->
            {Value1, Rest1} = 
                case lists:member(is_record, Opts) of
                    true ->
                        {{FNum, V}, R} = protobuffs:decode(Bytes, bytes),
                        RecVal = decode(list_to_atom(string:to_lower(atom_to_list(Type))), V),
                        {RecVal, R};
                    false ->
                        case lists:member(repeated_packed, Opts) of
                            true ->
                            {{FNum, V}, R} = protobuffs:decode_packed(Bytes, Type),
                            {V, R};
                            false ->
                            {{FNum, V}, R} = protobuffs:decode(Bytes, Type),
                            {unpack_value(V, Type), R}
                        end
                end,
            case lists:member(repeated, Opts) of
                true ->
                    case lists:keytake(FNum, 1, Acc) of
                        {value, {FNum, Name, List}, Acc1} ->
                            decode(Rest1, Types, [{FNum, Name, lists:reverse([int_to_enum(Type,Value1)|lists:reverse(List)])}|Acc1]);
                        false ->
                            decode(Rest1, Types, [{FNum, Name, [int_to_enum(Type,Value1)]}|Acc])
                    end;
                false ->
		                decode(Rest1, Types, [{FNum, Name, int_to_enum(Type,Value1)}|Acc])
            end;
        false ->
            case lists:keysearch('$extensions', 2, Acc) of
                {value,{_,_,Dict}} ->
                    {{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
                    Diff = size(Bytes) - size(R),
                    <<V:Diff/binary,_/binary>> = Bytes,
                    NewDict = dict:store(FNum, V, Dict),
                    NewAcc = lists:keyreplace('$extensions', 2, Acc, {false, '$extensions', NewDict}),
                    decode(R, Types, NewAcc);
                _ ->
                    {ok, Skipped} = protobuffs:skip_next_field(Bytes),
                    decode(Skipped, Types, Acc)
            end
    end.
    
unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.
    
to_record(pikachu, DecodedTuples) ->
    Record1 = lists:foldr(
        fun({_FNum, Name, Val}, Record) ->
            set_record_field(record_info(fields, pikachu), Record, Name, Val)
        end, #pikachu{}, DecodedTuples),
    decode_extensions(Record1).

decode_extensions(#pikachu{'$extensions' = Extensions} = Record) ->
    Types = [],
    NewExtensions = decode_extensions(Types, dict:to_list(Extensions), []),
    Record#pikachu{'$extensions' = NewExtensions};
decode_extensions(Record) ->
    Record.

decode_extensions(Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keysearch(Fnum, 1, Types) of
        {value, {Fnum, Name, Type, Opts}} ->
            {Value1, Rest1} = 
                case lists:member(is_record, Opts) of
                    true ->
                        {{FNum, V}, R} = protobuffs:decode(Bytes, bytes),
                        RecVal = decode(list_to_atom(string:to_lower(atom_to_list(Type))), V),
                        {RecVal, R};
                    false ->
                        case lists:member(repeated_packed, Opts) of
                            true ->
                                {{FNum, V}, R} = protobuffs:decode_packed(Bytes, Type),
                                {V, R};
                            false ->
                                {{FNum, V}, R} = protobuffs:decode(Bytes, Type),
                                {unpack_value(V, Type), R}
                        end
                end,
            case lists:member(repeated, Opts) of
                true ->
                    case lists:keytake(FNum, 1, Acc) of
                        {value, {FNum, Name, List}, Acc1} ->
                            decode(Rest1, Types, [{FNum, Name, lists:reverse([int_to_enum(Type,Value1)|lists:reverse(List)])}|Acc1]);
                        false ->
                            decode(Rest1, Types, [{FNum, Name, [int_to_enum(Type,Value1)]}|Acc])
                    end;
                false ->
                    [{Fnum, {optional, int_to_enum(Type,Value1), Type, Opts}} | Acc]
            end;
        false ->
            [{Fnum, Bytes} | Acc]
    end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions', Value) ->
		Decodable = [],
    NewValue = decode_extensions(element(1, Record), Decodable, dict:to_list(Value)),
		Index = list_index('$extensions', Fields),
		erlang:setelement(Index+1,Record,NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index+1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target|_], Index) -> Index;
list_index(Target, [_|Tail], Index) -> list_index(Target, Tail, Index+1);
list_index(_, [], _) -> -1.

extension_size(#pikachu{'$extensions' = Extensions}) ->
    dict:size(Extensions);
extension_size(_) ->
    0.

has_extension(#pikachu{'$extensions' = Extensions}, FieldKey) ->
    dict:is_key(FieldKey, Extensions);
has_extension(_Record, _FieldName) ->
    false.

get_extension(Record, fieldatom) when is_record(Record, pikachu) ->
    get_extension(Record, 1);
get_extension(#pikachu{'$extensions' = Extensions}, Int) when is_integer(Int) ->
    case dict:find(Int, Extensions) of
        {ok, {_Rule, Value, _Type, _Opts}} ->
            {ok, Value};
        {ok, Binary} ->
            {raw, Binary};
         error ->
             undefined
     end;
get_extension(_Record, _FieldName) ->
    undefined.

set_extension(#pikachu{'$extensions' = Extensions} = Record, fieldname, Value) ->
    NewExtends = dict:store(1, {rule, Value, type, []}, Extensions),
    {ok, Record#pikachu{'$extensions' = NewExtends}};
set_extension(Record, _, _) ->
    {error, Record}.
