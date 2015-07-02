%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_chunk).

-export([new/0]).
-export([setters/2]).
-export([call_id/2, call_id/1]).
-export([append_data/2
         ,data/2
         ,data/1
        ]).
-export([timestamp/2, timestamp/1]).
-export([ref_timestamp/1]).
-export([dst_ip/2, dst_ip/1]).
-export([dst_port/2, dst_port/1]).
-export([src_ip/2, src_ip/1]).
-export([src_port/2, src_port/1]).
-export([parser/2, parser/1]).
-export([label/2, label/1]).
-export([to_json/1
         ,from_json/1
        ]).
-export([is_chunk/1]).
-export([sort_by_timestamp/1
         ,reorder_dialog/1
        ]).

-record(ci_chunk, {call_id :: ne_binary()
                  ,data = [] :: ne_binaries()
                  ,timestamp :: number()
                  ,ref_timestamp :: number()
                  ,src_ip :: ne_binary()
                  ,src_port :: pos_integer()
                  ,dst_ip :: ne_binary()
                  ,dst_port :: pos_integer()
                  ,parser :: atom()
                  ,label :: ne_binary()
                 }).
-type chunk() :: #ci_chunk{}.

-export_type([chunk/0]).

-include("../call_inspector.hrl").

-define(SETTER(Field),
        Field(#ci_chunk{}=Chunk, Value) ->
               Chunk#ci_chunk{Field = Value}
       ).

-define(GETTER(Field),
        Field(#ci_chunk{Field = Value}) ->
               Value
       ).


%% API

-spec new() -> chunk().
new() -> #ci_chunk{}.

-spec setters(chunk(), [{fun((chunk(), A) -> chunk()), A}]) -> chunk().
setters(#ci_chunk{}=Chunk, Setters) ->
    Apply = fun ({Fun, Arg}, C) -> Fun(C, Arg) end,
    NewChunk = lists:foldl(Apply, Chunk, Setters),
    NewChunk#ci_chunk{ref_timestamp = ci_parsers_util:timestamp()}.

-spec call_id(chunk(), ne_binary()) -> chunk().
?SETTER(call_id).
-spec call_id(chunk()) -> api_binary().
?GETTER(call_id).

-spec data(chunk(), ne_binaries()) -> chunk().
?SETTER(data).
-spec data(chunk()) -> ne_binaries().
?GETTER(data).
-spec append_data(chunk(), ne_binary()) -> chunk().
append_data(#ci_chunk{data=D}=Chunk, Data) ->
    Chunk#ci_chunk{data=[Data|D]}.

-spec timestamp(chunk(), integer()) -> chunk().
?SETTER(timestamp).
-spec timestamp(chunk()) -> api_integer().
?GETTER(timestamp).

-spec ref_timestamp(chunk()) -> api_number().
?GETTER(ref_timestamp).

-spec src_ip(chunk(), ne_binary()) -> chunk().
src_ip(#ci_chunk{}=Chunk, Val) ->
    Chunk#ci_chunk{src_ip = resolve(Val)}.
-spec src_ip(chunk()) -> api_binary().
?GETTER(src_ip).

-spec src_port(chunk(), pos_integer()) -> chunk().
?SETTER(src_port).
-spec src_port(chunk()) -> api_pos_integer().
?GETTER(src_port).

-spec dst_ip(chunk(), ne_binary()) -> chunk().
dst_ip(#ci_chunk{}=Chunk, Val) ->
    Chunk#ci_chunk{dst_ip = resolve(Val)}.
-spec dst_ip(chunk()) -> api_binary().
?GETTER(dst_ip).

-spec dst_port(chunk(), pos_integer()) -> chunk().
?SETTER(dst_port).
-spec dst_port(chunk()) -> api_pos_integer().
?GETTER(dst_port).

-spec parser(chunk(), atom()) -> chunk().
?SETTER(parser).
-spec parser(chunk()) -> api_atom().
?GETTER(parser).

-spec label(chunk(), ne_binary()) -> chunk().
?SETTER(label).
-spec label(chunk()) -> api_binary().
?GETTER(label).

-spec to_json(chunk()) -> wh_json:object().
to_json(Chunk) ->
    wh_json:from_list(
      [{<<"call-id">>, call_id(Chunk)}
       ,{<<"timestamp">>, timestamp(Chunk)}
       ,{<<"ref_timestamp">>, wh_util:to_binary(ref_timestamp(Chunk))}
       ,{<<"label">>, label(Chunk)}
       ,{<<"raw">>, data(Chunk)}
       ,{<<"src">>, src(Chunk)}
       ,{<<"dst">>, dst(Chunk)}
       ,{<<"parser">>, parser(Chunk)}
      ]
     ).

-spec from_json(wh_json:object()) -> chunk().
from_json(JObj) ->
    {SrcIP, SrcPort} = src(wh_json:get_value(<<"src">>, JObj)),
    {DstIP, DstPort} = dst(wh_json:get_value(<<"dst">>, JObj)),
    #ci_chunk{src_ip = SrcIP
              ,dst_ip = DstIP
              ,src_port = SrcPort
              ,dst_port = DstPort
              ,call_id = wh_json:get_value(<<"call-id">>, JObj)
              ,timestamp = wh_json:get_value(<<"timestamp">>, JObj)
              ,ref_timestamp = wh_util:to_float(wh_json:get_value(<<"ref_timestamp">>, JObj))
              ,label = wh_json:get_value(<<"label">>, JObj)
              ,data = wh_json:get_value(<<"raw">>, JObj)
              ,parser = wh_json:get_value(<<"parser">>, JObj)
             }.

-spec src(chunk() | ne_binary()) -> ne_binary() | {ne_binary(), pos_integer()}.
src(#ci_chunk{src_ip = Ip, src_port = Port}) ->
    <<Ip/binary, ":", (wh_util:to_binary(Port))/binary>>;
src(Bin = <<_/binary>>) ->
    [IP, Port] = binary:split(Bin, <<":">>),
    {IP, wh_util:to_integer(Port)}.

-spec dst(chunk() | ne_binary()) -> ne_binary() | {ne_binary(), pos_integer()}.
dst(#ci_chunk{dst_ip = Ip, dst_port = Port}) ->
    <<Ip/binary, ":", (wh_util:to_binary(Port))/binary>>;
dst(Bin = <<_/binary>>) ->
    [IP, Port] = binary:split(Bin, <<":">>),
    {IP, wh_util:to_integer(Port)}.

-spec is_chunk(_) -> boolean().
is_chunk(#ci_chunk{}) -> 'true';
is_chunk(_) -> 'false'.


-spec sort_by_timestamp([chunk()]) -> [chunk()].
sort_by_timestamp(Chunks) ->
    lists:keysort(#ci_chunk.ref_timestamp, Chunks).


-spec reorder_dialog([chunk()]) -> [chunk()].
reorder_dialog([]) -> [];
reorder_dialog(Chunks) ->
    RefParser = pick_ref_parser(Chunks),
    do_reorder_dialog(RefParser, Chunks).

-spec pick_ref_parser([chunk()]) -> atom().
pick_ref_parser(Chunks) ->
    GroupedByParser = group_by(fun parser/1, Chunks),
    Counted = lists:keymap(fun erlang:length/1, 2, GroupedByParser),
    {RefParser,_Max} = lists:last(lists:keysort(2, Counted)),
    RefParser.

-spec do_reorder_dialog(atom(), [chunk()]) -> [chunk()].
do_reorder_dialog(RefParser, Chunks) ->
    GroupedByCSeq = lists:keysort(1, group_by(fun c_seq/1, Chunks)),
    lists:flatmap( fun ({_CSeq, ByCSeq}) ->
                           {ByRefParser, Others} = sort_split_uniq(RefParser, ByCSeq),
                           {Done, Rest} = first_pass(ByRefParser, Others),
                           {ReallyDone, NewRest} = second_pass(Done, Rest),
                           ReallyDone ++ NewRest
                   end, GroupedByCSeq).

-spec sort_split_uniq(atom(), [chunk()]) -> {[chunk()], [chunk()]}.
sort_split_uniq(RefParser, Chunks) ->
    Grouper = fun (Chunk) -> RefParser =:= parser(Chunk) end,
    {InOrder, Others} = lists:partition(Grouper, sort_by_timestamp(Chunks)),
    Uniq = [Chunk || Chunk <- Others, not is_duplicate(InOrder, Chunk)],
    {InOrder, Uniq}.

-spec first_pass([chunk()], [chunk()]) -> {[chunk()], [chunk()]}.
first_pass(InOrder, ToOrder) -> first_pass([], InOrder, ToOrder, []).
first_pass(Before, [Ordered|InOrder], [Chunk|ToOrder], UnMergeable) ->
    case {    label(Ordered) =:=    label(Chunk)
         ,   dst_ip(Ordered) =:=   src_ip(Chunk) andalso
           dst_port(Ordered) =:= src_port(Chunk)
         ,   src_ip(Ordered) =:=   dst_ip(Chunk) andalso
           src_port(Ordered) =:= dst_port(Chunk)
         }
    of
        {'true', 'true', 'true'} ->
            first_pass([], Before++[Ordered|InOrder], ToOrder, [Chunk|UnMergeable]);
        {'true', 'true', ______} ->
            first_pass([], Before++[Ordered]++[Chunk|InOrder], ToOrder, UnMergeable);
        {'true', ______, 'true'} ->
            first_pass([], Before++[Chunk]++[Ordered|InOrder], ToOrder, UnMergeable);
        {'true', ______, ______} ->
            first_pass([], Before++[Ordered|InOrder], ToOrder, [Chunk|UnMergeable]);
        {'false', _____, ______} ->
            first_pass(Before++[Ordered], InOrder, [Chunk|ToOrder], UnMergeable)
    end;
first_pass(Before, [], [Chunk|ToOrder], UnMergeable) ->
    first_pass([], Before, ToOrder, [Chunk|UnMergeable]);
first_pass(Before, InOrder, [], UnMergeable) ->
    {Before++InOrder, lists:reverse(UnMergeable)}.

-spec find_previous_packet(chunk(), [chunk()]) -> chunk() | 'no_previous'.
find_previous_packet( #ci_chunk{ parser = Parser
                               , ref_timestamp = RefTimestamp
                               }
                    , Chunks ) ->
    RightPackets = [Chunk || Chunk <- Chunks, parser(Chunk) =:= Parser],
    Compare = fun (Chunk) -> ref_timestamp(Chunk) < RefTimestamp end,
    PreviousPackets = lists:takewhile(Compare, RightPackets),
    try lists:last(PreviousPackets)
    catch 'error':'function_clause' -> 'no_previous'
    end.

-spec second_pass([chunk()], [chunk()]) -> {[chunk()], [chunk()]}.
second_pass(InOrder, ToOrder) ->
    second_pass(InOrder, ToOrder, []).
second_pass(InOrder, [Chunk|ToOrder], UnMergeable) ->
    case find_previous_packet(Chunk, InOrder) of
        'no_previous' ->
            second_pass(InOrder, ToOrder, [Chunk|UnMergeable]);
        PreviousPacket ->
            PreviousPacketId = ref_timestamp(PreviousPacket),
            Reordered =
                lists:flatten(
                  [ case ref_timestamp(Item) =:= PreviousPacketId of
                        'true' -> [Item, Chunk];
                        'false' -> Item
                    end
                    || Item <- InOrder
                  ]
                 ),
            second_pass(Reordered, ToOrder, UnMergeable)
    end;
second_pass(InOrder, [], UnMergeable) ->
    {InOrder, lists:reverse(UnMergeable)}.

-spec is_duplicate([chunk()], chunk()) -> boolean().
%% Assumes CSeq and Callid already equal.
is_duplicate([#ci_chunk{ dst_ip = DstIP
                       , src_ip = SrcIP
                       , dst_port = DstPort
                       , src_port = SrcPort
                       , label = Label
                       }|_]
            , #ci_chunk{ dst_ip = DstIP
                       , src_ip = SrcIP
                       , dst_port = DstPort
                       , src_port = SrcPort
                       , label = Label
                       }) ->
    'true';
is_duplicate([], _) ->
    'false';
is_duplicate([_|Chunks], Chunk) ->
    is_duplicate(Chunks, Chunk).

-spec c_seq(chunk()) -> ne_binary().
c_seq(Chunk) ->
    FullCSeq = ci_parsers_util:c_seq(data(Chunk)),
    [Number, _Tag] = binary:split(FullCSeq, <<$\s>>),
    Number.

-spec group_by(fun((V) -> K), [V]) -> [{K, [V]}] when K :: atom().
group_by(Fun, List) ->
    dict:to_list(group_as_dict(Fun, List)).

-spec group_as_dict(fun((V) -> K), [V]) -> dict() when K :: atom().
group_as_dict(Fun, List) ->
    F = fun (Value, Dict) -> dict:append(Fun(Value), Value, Dict) end,
    lists:foldl(F, dict:new(), List).


-spec resolve(ne_binary()) -> ne_binary().
resolve(IP) -> IP.
