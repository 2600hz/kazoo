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
-export([call_id/2
        ,call_id/1]).
-export([append_data/2
        ,data/2
        ,data/1
        ]).
-export([timestamp/2
        ,timestamp/1]).
-export([ref_timestamp/1]).
-export([dst_ip/2
        ,dst_ip/1]).
-export([dst_port/2
        ,dst_port/1]).
-export([src_ip/2
        ,src_ip/1]).
-export([src_port/2
        ,src_port/1]).
-export([parser/2
        ,parser/1]).
-export([label/2
        ,label/1]).
-export([to_json/1
        ,from_json/1]).
-export([is_chunk/1]).
-export([sort_by_timestamp/1
        ,reorder_dialog/1]).

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

-define(setter(Field),
        Field(#ci_chunk{}=Chunk, Value) ->
               Chunk#ci_chunk{Field = Value}
       ).

-define(getter(Field),
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
?setter(call_id).
-spec call_id(chunk()) -> api_binary().
?getter(call_id).

-spec data(chunk(), ne_binaries()) -> chunk().
?setter(data).
-spec data(chunk()) -> ne_binaries().
?getter(data).
-spec append_data(chunk(), ne_binary()) -> chunk().
append_data(#ci_chunk{data=D}=Chunk, Data) ->
    Chunk#ci_chunk{data=[Data|D]}.

-spec timestamp(chunk(), integer()) -> chunk().
?setter(timestamp).
-spec timestamp(chunk()) -> api_integer().
?getter(timestamp).

-spec ref_timestamp(chunk()) -> api_number().
?getter(ref_timestamp).

-spec src_ip(chunk(), ne_binary()) -> chunk().
src_ip(#ci_chunk{}=Chunk, Val) ->
    Chunk#ci_chunk{src_ip = resolve(Val)}.
-spec src_ip(chunk()) -> api_binary().
?getter(src_ip).

-spec src_port(chunk(), pos_integer()) -> chunk().
?setter(src_port).
-spec src_port(chunk()) -> api_pos_integer().
?getter(src_port).

-spec dst_ip(chunk(), ne_binary()) -> chunk().
dst_ip(#ci_chunk{}=Chunk, Val) ->
    Chunk#ci_chunk{dst_ip = resolve(Val)}.
-spec dst_ip(chunk()) -> api_binary().
?getter(dst_ip).

-spec dst_port(chunk(), pos_integer()) -> chunk().
?setter(dst_port).
-spec dst_port(chunk()) -> api_pos_integer().
?getter(dst_port).

-spec parser(chunk(), atom()) -> chunk().
?setter(parser).
-spec parser(chunk()) -> api_atom().
?getter(parser).

-spec label(chunk(), ne_binary()) -> chunk().
?setter(label).
-spec label(chunk()) -> api_binary().
?getter(label).

-spec to_json(chunk()) -> wh_json:object().
to_json(Chunk) ->
    wh_json:from_list(
      [ {<<"src_ip">>, src_ip(Chunk)}
      , {<<"dst_ip">>, dst_ip(Chunk)}
      , {<<"src_port">>, src_port(Chunk)}
      , {<<"dst_port">>, dst_port(Chunk)}
      , {<<"call-id">>, call_id(Chunk)}
      , {<<"timestamp">>, timestamp(Chunk)}
      , {<<"ref_timestamp">>, ref_timestamp(Chunk)}
      , {<<"label">>, label(Chunk)}
      , {<<"raw">>, data(Chunk)}
      , {<<"parser">>, parser(Chunk)}
      ]
     ).

-spec from_json(wh_json:object()) -> chunk().
from_json(JObj) ->
    #ci_chunk{ src_ip = wh_json:get_value(<<"src_ip">>, JObj)
             , dst_ip = wh_json:get_value(<<"dst_ip">>, JObj)
             , src_port = wh_json:get_value(<<"src_port">>, JObj)
             , dst_port = wh_json:get_value(<<"dst_port">>, JObj)
             , call_id = wh_json:get_value(<<"call-id">>, JObj)
             , timestamp = wh_json:get_value(<<"timestamp">>, JObj)
             , ref_timestamp = wh_json:get_value(<<"ref_timestamp">>, JObj)
             , label = wh_json:get_value(<<"label">>, JObj)
             , data = wh_json:get_value(<<"raw">>, JObj)
             , parser = wh_json:get_value(<<"parser">>, JObj)
             }.

-spec is_chunk(_) -> boolean().
is_chunk(#ci_chunk{}) -> 'true';
is_chunk(_) -> 'false'.


-spec sort_by_timestamp([chunk()]) -> [chunk()].
sort_by_timestamp(Chunks) ->
    lists:keysort(#ci_chunk.ref_timestamp, Chunks).


-spec reorder_dialog([chunk()]) -> [chunk()].
reorder_dialog([]) -> [];
reorder_dialog(Chunks) ->
    %% RefParser = '10.26.0.182:9061',
    RefParser = pick_ref_parser(Chunks),
    do_reorder_dialog(RefParser, Chunks).

pick_ref_parser(Chunks) ->
    GroupedByParser = group_by(fun parser/1, Chunks),
    Counted = lists:keymap(fun erlang:length/1, 2, GroupedByParser),
    io:format("Count ~p\n", [Counted]),
    [{RefParser,_Min}|_Bigger] = lists:keysort(2, Counted),
    RefParser.

do_reorder_dialog(RefParser, Chunks) ->
    io:format(user, "RefParser = ~p\n", [RefParser]),
    GroupedByCSeq = group_by(fun c_seq/1, Chunks),
    lists:flatmap( fun ({_CSeq, ByCSeq}) ->
                           io:format(user, "_CSeq = ~p\n", [_CSeq]),
                           GroupedByParser = group_by(fun parser/1, sort_by_timestamp(ByCSeq)),
                           {RefParser,ByParser} = lists:keyfind(RefParser, 1, GroupedByParser),
                           Others0 = [{Parser,Chs} || {Parser,Chs} <- GroupedByParser, Parser =/= RefParser],
                           Others = remove_duplicates(ByParser, Others0),
                           {Done, Rest} = do_merge([], ByParser, Others, []),
                           io:format(user, ">>> Rest = ~p\n", [lists:map(fun to_json/1,Rest)]),
                           {ReallyDone, NewRest} = second_pass([], Done, Rest, []),
                           io:format(user, ">>> NewRest = ~p\n", [lists:map(fun to_json/1,NewRest)]),
                           ReallyDone ++ NewRest
                   end, lists:keysort(1, GroupedByCSeq)).

remove_duplicates(InOrder, GroupedByParser) ->
    lists:flatten([ [ Chunk
                      || Chunk <- ByParser
                             , not is_duplicate(InOrder, Chunk)
                    ]
                    || {_Parser, ByParser} <- GroupedByParser
                  ]).

do_merge(Before, [Ordered|InOrder], [Chunk|ToOrder], UnMergeable) ->
    case {    label(Ordered) =:=    label(Chunk)
         ,   dst_ip(Ordered) =:=   src_ip(Chunk) andalso
           dst_port(Ordered) =:= src_port(Chunk)
         ,   src_ip(Ordered) =:=   dst_ip(Chunk) andalso
           src_port(Ordered) =:= dst_port(Chunk)
         }
    of
        {'true', 'true', 'true'} ->
            io:format(user, ">>> both match\n~p\n~p\n", [Ordered,Chunk]),
            do_merge([], Before++[Ordered|InOrder], ToOrder, [Chunk|UnMergeable]);
        {'true', 'true', ______} ->
            do_merge([], Before++[Ordered]++[Chunk|InOrder], ToOrder, UnMergeable);
        {'true', ______, 'true'} ->
            do_merge([], Before++[Chunk]++[Ordered|InOrder], ToOrder, UnMergeable);
        {'true', ______, ______} ->
            do_merge([], Before++[Ordered|InOrder], ToOrder, [Chunk|UnMergeable]);
        {'false', _____, ______} ->
            do_merge(Before++[Ordered], InOrder, [Chunk|ToOrder], UnMergeable)
    end;
do_merge(Before, [], [Chunk|ToOrder], UnMergeable) ->
    do_merge([], Before, ToOrder, [Chunk|UnMergeable]);
do_merge(Before, InOrder, [], UnMergeable) ->
    {Before++InOrder, UnMergeable}.

second_pass(Before, [Ordered1,Ordered2|InOrder], [Chunk|ToOrder], UnMergeable) ->
    case {    label(Ordered1),    label(Chunk),    label(Ordered2)
         ,    c_seq(Ordered1),    c_seq(Chunk),    c_seq(Ordered2)
         ,   src_ip(Ordered1),   src_ip(Chunk),   dst_ip(Ordered2)
         , src_port(Ordered1), src_port(Chunk), dst_port(Ordered2)
         }
    of
        { <<"INVITE ",_/binary>>, <<"SIP/2.0 100 Attempting to connect your call">>, <<"SIP/2.0 100 Trying">>
        , CSeq, CSeq, CSeq
        , IP, IP, IP
        , Port, Port, Port
        } ->
            second_pass(Before++[Ordered1,Chunk,Ordered2], InOrder, ToOrder, UnMergeable);
        _ ->
            second_pass(Before++[Ordered1], [Ordered2|InOrder], [Chunk|ToOrder], UnMergeable)
    end;
second_pass(Before, [Ordered], [Chunk|ToOrder], UnMergeable) ->
    io:format(user, "here\n", []),
    second_pass([], Before++[Ordered], ToOrder, [Chunk|UnMergeable]);
second_pass(Before, [], [Chunk|ToOrder], UnMergeable) ->
    second_pass([], Before, ToOrder, [Chunk|UnMergeable]);
second_pass(Before, InOrder, [], UnMergeable) ->
    {Before++InOrder, UnMergeable}.


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

c_seq(Chunk) ->
    FullCSeq = ci_parsers_util:c_seq(data(Chunk)),
    [Number, _Tag] = binary:split(FullCSeq, <<$\s>>),
    Number.

group_by(Fun, List) ->
    dict:to_list(group_as_dict(Fun, List)).

group_as_dict(Fun, List) ->
    F = fun (Value, Dict) -> dict:append(Fun(Value), Value, Dict) end,
    lists:foldl(F, dict:new(), List).


resolve(IP) -> IP.
