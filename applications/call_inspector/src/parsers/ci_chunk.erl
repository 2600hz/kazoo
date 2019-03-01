%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_chunk).

-export([new/0]).
-export([setters/2]).
-export([call_id/2, call_id/1]).
-export([append_data/2
        ,data/1, data/2
        ]).
-export([timestamp/2, timestamp/1]).
-export([ref_timestamp/1]).
-export([dst_ip/2, dst_ip/1]).
-export([dst_port/2, dst_port/1]).
-export([src_ip/2, src_ip/1]).
-export([src_port/2, src_port/1]).
-export([parser/2, parser/1]).
-export([label/2, label/1]).
-export([c_seq/2, c_seq/1]).
-export([to_json/1
        ,from_json/1
        ]).
-export([is_chunk/1]).
-export([reorder_dialog/1]).
-export([get_dialog_entities/1]).

-ifdef(TEST).
- export([do_reorder_dialog/2]).
- export([pick_ref_parser/1]).
-endif.

-record(ci_chunk, {call_id :: kz_term:api_ne_binary()
                  ,data = [] :: kz_term:ne_binaries()
                  ,timestamp :: kz_term:api_float()
                  ,ref_timestamp :: kz_term:api_float()
                  ,src_ip :: kz_term:api_ne_binary()
                  ,src_port :: kz_term:api_pos_integer()
                  ,dst_ip :: kz_term:api_ne_binary()
                  ,dst_port :: kz_term:api_pos_integer()
                  ,parser :: kz_term:api_ne_binary()
                  ,label :: kz_term:api_ne_binary()
                  ,c_seq :: kz_term:api_ne_binary()  %% Parsing Kamailio logs: this can be undefined (DON'T parse them)
                  }).
-type chunk() :: #ci_chunk{}.

-export_type([chunk/0]).

-include("call_inspector.hrl").

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

-spec call_id(chunk(), kz_term:ne_binary()) -> chunk().
?SETTER(call_id).
-spec call_id(chunk()) -> kz_term:api_binary().
?GETTER(call_id).

-spec data(chunk(), kz_term:ne_binaries()) -> chunk().
?SETTER(data).
-spec data(chunk()) -> kz_term:ne_binaries().
?GETTER(data).
-spec append_data(chunk(), kz_term:ne_binary()) -> chunk().
append_data(#ci_chunk{data=D}=Chunk, Data) ->
    Chunk#ci_chunk{data=[Data|D]}.

-spec timestamp(chunk(), kz_term:api_float()) -> chunk().
?SETTER(timestamp).
-spec timestamp(chunk()) -> kz_term:api_float().
?GETTER(timestamp).

-spec ref_timestamp(chunk()) -> float().
?GETTER(ref_timestamp).

-spec src_ip(chunk(), kz_term:ne_binary()) -> chunk().
src_ip(#ci_chunk{}=Chunk, Val) ->
    Chunk#ci_chunk{src_ip = resolve(Val)}.
-spec src_ip(chunk()) -> kz_term:api_binary().
?GETTER(src_ip).

-spec src_port(chunk(), pos_integer()) -> chunk().
?SETTER(src_port).
-spec src_port(chunk()) -> kz_term:api_pos_integer().
?GETTER(src_port).

-spec dst_ip(chunk(), kz_term:ne_binary()) -> chunk().
dst_ip(#ci_chunk{}=Chunk, Val) ->
    Chunk#ci_chunk{dst_ip = resolve(Val)}.
-spec dst_ip(chunk()) -> kz_term:api_binary().
?GETTER(dst_ip).

-spec dst_port(chunk(), pos_integer()) -> chunk().
?SETTER(dst_port).
-spec dst_port(chunk()) -> kz_term:api_pos_integer().
?GETTER(dst_port).

-spec parser(chunk(), atom()) -> chunk().
parser(#ci_chunk{}=Chunk, Parser) ->
    Chunk#ci_chunk{parser = kz_term:to_binary(Parser)}.
-spec parser(chunk()) -> kz_term:api_binary().
?GETTER(parser).

-spec label(chunk(), kz_term:ne_binary()) -> chunk().
?SETTER(label).
-spec label(chunk()) -> kz_term:api_binary().
?GETTER(label).

-spec c_seq(chunk(), kz_term:ne_binary()) -> chunk().
?SETTER(c_seq).
-spec c_seq(chunk()) -> kz_term:api_binary().
?GETTER(c_seq).

-spec to_json(chunk()) -> kz_json:object().
to_json(Chunk) ->
    kz_json:from_list(
      [{<<"call-id">>, call_id(Chunk)}
      ,{<<"timestamp">>, timestamp(Chunk)}
      ,{<<"ref_timestamp">>, kz_term:to_binary(ref_timestamp(Chunk))}
      ,{<<"label">>, label(Chunk)}
      ,{<<"raw">>, data(Chunk)}
      ,{<<"src">>, src(Chunk)}
      ,{<<"dst">>, dst(Chunk)}
      ,{<<"parser">>, parser(Chunk)}
      ,{<<"c_seq">>, c_seq(Chunk)}
      ]
     ).

-spec from_json(kz_json:object()) -> chunk().
from_json(JObj) ->
    {SrcIP, SrcPort} = src(kz_json:get_value(<<"src">>, JObj)),
    {DstIP, DstPort} = dst(kz_json:get_value(<<"dst">>, JObj)),
    #ci_chunk{src_ip = SrcIP
             ,dst_ip = DstIP
             ,src_port = SrcPort
             ,dst_port = DstPort
             ,call_id = kz_json:get_value(<<"call-id">>, JObj)
             ,timestamp = kz_json:get_value(<<"timestamp">>, JObj)
             ,ref_timestamp = kz_json:get_float_value(<<"ref_timestamp">>, JObj)
             ,label = kz_json:get_value(<<"label">>, JObj)
             ,data = kz_json:get_value(<<"raw">>, JObj)
             ,parser = kz_json:get_value(<<"parser">>, JObj)
             ,c_seq = kz_json:get_value(<<"c_seq">>, JObj)
             }.

-spec src(chunk() | kz_term:ne_binary()) -> kz_term:ne_binary() | {kz_term:ne_binary(), pos_integer()}.
src(#ci_chunk{src_ip = Ip, src_port = Port}) ->
    <<Ip/binary, ":", (kz_term:to_binary(Port))/binary>>;
src(Bin = <<_/binary>>) ->
    [IP, Port] = binary:split(Bin, <<":">>),
    {IP, kz_term:to_integer(Port)}.

-spec dst(chunk() | kz_term:ne_binary()) -> kz_term:ne_binary() | {kz_term:ne_binary(), pos_integer()}.
dst(#ci_chunk{dst_ip = Ip, dst_port = Port}) ->
    <<Ip/binary, ":", (kz_term:to_binary(Port))/binary>>;
dst(Bin = <<_/binary>>) ->
    [IP, Port] = binary:split(Bin, <<":">>),
    {IP, kz_term:to_integer(Port)}.

-spec is_chunk(any()) -> boolean().
is_chunk(#ci_chunk{}) -> 'true';
is_chunk(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Gives back an ordered list of entities participating in the SIP dialog.
%% `Chunks' needs to be ordered (e.g. using {@link reorder_dialog/1}).
%% @end
%%------------------------------------------------------------------------------
-spec get_dialog_entities([chunk()]) -> kz_term:ne_binaries().
get_dialog_entities(Chunks) ->
    get_dialog_entities(Chunks, []).
get_dialog_entities([], Acc) ->
    lists:reverse(Acc);
get_dialog_entities([Chunk|Chunks], Acc) ->
    Src = src(Chunk),
    Dst = dst(Chunk),
    Acc1 = case lists:member(Src, Acc) of
               'true'  -> Acc;
               'false' -> [Src|Acc]
           end,
    Acc2 = case lists:member(Dst, Acc1) of
               'true'  -> Acc1;
               'false' -> [Dst|Acc1]
           end,
    get_dialog_entities(Chunks, Acc2).

-spec sort_by_timestamp([chunk()]) -> [chunk()].
sort_by_timestamp(Chunks) ->
    lists:keysort(#ci_chunk.ref_timestamp, Chunks).

-spec reorder_dialog([chunk()]) -> [chunk()].
reorder_dialog([]) -> [];
reorder_dialog(Chunks) ->
    RefParser = pick_ref_parser(Chunks),
    lager:debug("reordering '~s' using parser '~s'", [call_id(hd(Chunks)), RefParser]),
    do_reorder_dialog(RefParser, Chunks).

-spec pick_ref_parser([chunk()]) -> kz_term:ne_binary().
pick_ref_parser(Chunks) ->
    GroupedByParser = group_by(fun parser/1, Chunks),
    Counted = lists:keymap(fun erlang:length/1, 2, GroupedByParser),
    {RefParser,_Max} = lists:last(lists:keysort(2, Counted)),
    RefParser.

-spec do_reorder_dialog(kz_term:ne_binary(), [chunk()]) -> [chunk()].
do_reorder_dialog(RefParser, Chunks) ->
    GroupedByCSeq = lists:keysort(1, group_by(fun c_seq_number/1, Chunks)),
    lists:flatmap(fun({_CSeq, ByCSeq}) ->
                          {ByRefParser, Others} = sort_split_uniq(RefParser, ByCSeq),
                          %% _ = [lager:debug("byRefParser ~s", [kz_json:encode(to_json(C))]) || C <- ByRefParser],
                          {Done, Rest} = first_pass(ByRefParser, Others),
                          %% _ = [lager:debug("done ~s", [kz_json:encode(to_json(C))]) || C <- Done],
                          {ReallyDone, NewRest} = second_pass(Done, Rest),
                          %% _ = [lager:debug("reallyDone ~s", [kz_json:encode(to_json(C))]) || C <- ReallyDone],
                          %% _ = [lager:debug("newRest ~s", [kz_json:encode(to_json(C))]) || C <- NewRest],
                          ReallyDone ++ NewRest
                  end
                 ,GroupedByCSeq
                 ).

-spec sort_split_uniq(kz_term:ne_binary(), [chunk()]) -> {[chunk()], [chunk()]}.
sort_split_uniq(RefParser, Chunks) ->
    Grouper = fun (Chunk) -> RefParser =:= parser(Chunk) end,
    {InOrder, Others} = lists:partition(Grouper, sort_by_timestamp(Chunks)),
    Uniq = [Chunk || Chunk <- Others, not is_duplicate(InOrder, Chunk)],
    {InOrder, Uniq}.

-spec first_pass([chunk()], [chunk()]) -> {[chunk()], [chunk()]}.
first_pass(InOrder, ToOrder) -> first_pass([], InOrder, ToOrder, []).
first_pass(Before, [Ordered|InOrder], [Chunk|ToOrder], UnMergeable) ->
    case {  label(Ordered) =:=    label(Chunk)
         , dst_ip(Ordered) =:=   src_ip(Chunk)
            andalso dst_port(Ordered) =:= src_port(Chunk)
         , src_ip(Ordered) =:=   dst_ip(Chunk)
            andalso src_port(Ordered) =:= dst_port(Chunk)
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
find_previous_packet(#ci_chunk{parser = Parser
                              ,ref_timestamp = RefTimestamp
                              }
                    ,Chunks) ->
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
                  [case ref_timestamp(Item) =:= PreviousPacketId of
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
is_duplicate([#ci_chunk{dst_ip = DstIP
                       ,src_ip = SrcIP
                       ,dst_port = DstPort
                       ,src_port = SrcPort
                       ,label = Label
                       }
              |_]
            ,#ci_chunk{dst_ip = DstIP
                      ,src_ip = SrcIP
                      ,dst_port = DstPort
                      ,src_port = SrcPort
                      ,label = Label
                      }) ->
    'true';
is_duplicate([], _) ->
    'false';
is_duplicate([_|Chunks], Chunk) ->
    is_duplicate(Chunks, Chunk).

-spec c_seq_number(chunk()) -> kz_term:ne_binary().
c_seq_number(Chunk) ->
    [Number, _Tag] = binary:split(c_seq(Chunk), <<$\s>>),
    Number.

-spec group_by(fun((V) -> K), [V]) -> [{K, [V]}] when K :: atom().
group_by(Fun, List) ->
    dict:to_list(group_as_dict(Fun, List)).

-spec group_as_dict(fun((V) -> K), [V]) -> dict:dict() when K :: atom().
group_as_dict(Fun, List) ->
    F = fun(Value, Dict) ->
                case Fun(Value) of
                    'undefined' ->
                        %% Skip this Value (removes Chunks not containing a CSeq)
                        Dict;
                    Res ->
                        dict:append(Res, Value, Dict)
                end
        end,
    lists:foldl(F, dict:new(), List).

-spec resolve(kz_term:ne_binary()) -> kz_term:ne_binary().
resolve(IP) -> IP.
