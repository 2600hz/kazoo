%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%% @hidden

-module(wsock_framing).
-include("wsock.hrl").

-export([to_binary/1, from_binary/1, from_binary/2, frame/1, frame/2]).

-define(OP_CODE_CONT, 0).
-define(OP_CODE_TEXT, 1).
-define(OP_CODE_BIN, 2).
-define(OP_CODE_CLOSE, 8).
-define(OP_CODE_PING, 9).
-define(OP_CODE_PONG, 10).

-spec to_binary(Frame::#frame{}) -> binary().
to_binary(Frame) ->
  Bin1 = <<
    (Frame#frame.fin):1,
    (Frame#frame.rsv1):1, (Frame#frame.rsv2):1, (Frame#frame.rsv3):1,
    (Frame#frame.opcode):4,
    (Frame#frame.mask):1,
    (Frame#frame.payload_len):7,
    (Frame#frame.extended_payload_len):(extended_payload_len_bit_width(Frame#frame.extended_payload_len, 16)),
    (Frame#frame.extended_payload_len_cont):(extended_payload_len_bit_width(Frame#frame.extended_payload_len_cont, 64))
  >>,

  Bin2 = case Frame#frame.masking_key of
    undefined ->
      Bin1;
    Key ->
      <<Bin1/binary, Key:32>>
  end,

  <<Bin2/binary, (Frame#frame.payload)/binary>>.

-spec from_binary(Data::binary()) -> list(#frame{}).
from_binary(Data) ->
  lists:reverse(new_from_binary(Data, [])).

-spec from_binary(Data::binary(), FragmentedFrame :: #frame{}) -> list(#frame{}).
from_binary(Data, FragmentedFrame = #frame{ fragmented = true }) ->
  lists:reverse(continue_from_binary(Data, FragmentedFrame, [])).

%===================
% Internal
%===================

new_from_binary(Data, Acc) ->
  new_frame_decoding(Data, Acc).

new_frame_decoding(<<>>, Acc) ->
  Acc;
new_frame_decoding(Data, Acc) ->
  {Frame, Rest} = from_binary(Data, first_byte, #frame{}),
  new_frame_decoding(Rest, [Frame | Acc]).

continue_from_binary(Data, FragmentedFrame, Acc) ->
  ComposedData = <<(FragmentedFrame#frame.raw)/binary, Data/binary>>,
  {Frame, Rest} = from_binary(ComposedData, next_piece_from_binary(ComposedData, FragmentedFrame#frame.next_piece_size, FragmentedFrame#frame.next_piece), FragmentedFrame),
  new_frame_decoding(Rest, [Frame | Acc]).

from_binary(Data, {not_enough_bytes, ExpectedNextPiece, ExpectedSize}, Frame) ->
  {Frame#frame{fragmented = true, raw = Data, next_piece = ExpectedNextPiece, next_piece_size = ExpectedSize}, <<>>};
from_binary(<<Fin:1, Rsv1:1, Rsv2: 1, Rsv3:1, Opcode:4, Rest/binary>>, first_byte, Frame) ->
  NewFrame = Frame#frame{ fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3, opcode = Opcode },
  from_binary(Rest, next_piece_from_binary(Rest, 1, second_byte), NewFrame);
from_binary(<<Mask:1, 127:7, Rest/binary>>, second_byte, Frame) ->
  NewFrame = Frame#frame{ mask = Mask, payload_len = 127 },
  from_binary(Rest, next_piece_from_binary(Rest, 8, extended_payload_length_cont), NewFrame);
from_binary(<<Mask:1, 126:7, Rest/binary>>, second_byte, Frame) ->
  NewFrame = Frame#frame{ mask = Mask, payload_len = 126 },
  from_binary(Rest, next_piece_from_binary(Rest, 2, extended_payload_length), NewFrame);
from_binary(<<Mask:1, PayloadLen:7, Rest/binary>>, second_byte, Frame) ->
  NewFrame = Frame#frame{ mask = Mask, payload_len = PayloadLen },
  from_binary(Rest, payload_or_masking_key(Rest, NewFrame), NewFrame);
from_binary(<<ExtendedPayloadLength:16, Rest/binary>>, extended_payload_length, Frame)->
  NewFrame = Frame#frame{ extended_payload_len = ExtendedPayloadLength },
  from_binary(Rest, payload_or_masking_key(Rest, NewFrame), NewFrame);
from_binary(<<ExtendedPayloadLengthCont:64, Rest/binary>>, extended_payload_length_cont, Frame)->
  NewFrame = Frame#frame{ extended_payload_len_cont = ExtendedPayloadLengthCont },
  from_binary(Rest, payload_or_masking_key(Rest, NewFrame), NewFrame);
from_binary(Data, masking_key, Frame = #frame{ mask = 0 }) ->
  from_binary(Data, next_piece_from_binary(Data, real_payload_length(Frame), payload), Frame);
from_binary(<<MaskKey:32, Rest/binary>>, masking_key, Frame)->
  NewFrame = Frame#frame{ masking_key = MaskKey },
  from_binary(Rest, next_piece_from_binary(Rest, real_payload_length(Frame), payload), NewFrame);

from_binary(Data, payload, Frame = #frame{ mask = 1, masking_key = MaskingKey })->
  {Payload, Rest} = extract_payload(Data, Frame),
  NewFrame = Frame#frame{ payload = mask(Payload, MaskingKey, <<>>)},
  {finish_frame(NewFrame), Rest};

from_binary(Data, payload, Frame)->
  {Payload, Rest} = extract_payload(Data, Frame),
  NewFrame = Frame#frame{ payload = Payload},
  {finish_frame(NewFrame), Rest}.

extract_payload(Data, Frame) ->
  RL = real_payload_length(Frame),
  <<Payload:RL/binary, Rest/binary>> = Data,
  {Payload, Rest}.

finish_frame(Frame) ->
  Frame#frame{ fragmented = false, raw = <<>>, next_piece = undefined, next_piece_size = undefined}.

real_payload_length(Frame = #frame{ payload_len = 126 }) ->
  Frame#frame.extended_payload_len;
real_payload_length(Frame = #frame{ payload_len = 127 }) ->
  Frame#frame.extended_payload_len_cont;
real_payload_length(Frame) ->
  Frame#frame.payload_len.

extended_payload_len_bit_width(PayloadLen, Max) ->
  case PayloadLen of
    0 -> 0;
    _ -> Max
  end.

payload_or_masking_key(Data, #frame{ mask = 1 }) ->
  next_piece_from_binary(Data, 4, masking_key);
payload_or_masking_key(Data, Frame) ->
  next_piece_from_binary(Data, real_payload_length(Frame), payload).

next_piece_from_binary(Data, RequiredSize, PieceDescription) ->
  case assert_required_bytes(Data, RequiredSize) of
    true -> PieceDescription;
    false -> {not_enough_bytes, PieceDescription, RequiredSize}
  end.

assert_required_bytes(Data, RequiredSize) ->
  byte_size(Data) >= RequiredSize.

-spec frame(Data::binary() | string()) -> #frame{}.
frame(Data) when is_binary(Data) ->
  frame(Data, [{opcode, binary}]);

frame(Data) when is_list(Data)->
  frame(list_to_binary(Data), [{opcode, text}]).

-spec frame(Data::string() | binary(), Options::list()) -> #frame{}.
frame(Data, Options) when is_list(Data) ->
  frame(list_to_binary(Data), Options);

%don't like having this function clause just for close frames
frame({CloseCode, Reason}, Options) ->
  BinReason = list_to_binary(Reason),
  Data = <<CloseCode:16, BinReason/binary>>,
  frame(Data, Options);


frame(Data, Options) ->
  Frame = #frame{ payload = Data},
  Frame2 = length(Frame, Data),
  apply_options(Frame2, Options).

-spec apply_options(Frame::#frame{}, Options::list()) -> #frame{}.
apply_options(Frame, [mask | Tail]) ->
  <<MaskKey:32>> = crypto:rand_bytes(4),
  T = Frame#frame{
    mask = 1,
    masking_key = MaskKey,
    payload = mask(Frame#frame.payload, MaskKey, <<>>)
  },
  apply_options(T, Tail);

apply_options(Frame, [fin | Tail]) ->
  T = Frame#frame{fin = 1},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, continuation} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_CONT},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, text} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_TEXT},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, binary} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_BIN},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, close} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_CLOSE},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, ping} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_PING},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, pong} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_PONG},
  apply_options(T, Tail);

apply_options(Frame, []) ->
  Frame.

-spec length(Frame::#frame{}, Data :: binary()) -> #frame{}.
length(Frame, Data) ->
  Len = byte_size(Data),
  if
    Len =< 125 ->
      Frame#frame{
        payload_len = Len,
        extended_payload_len = 0,
        extended_payload_len_cont = 0
      };
    (Len > 125) and (Len =< 65536) ->
      Frame#frame{
        payload_len = 126,
        extended_payload_len = Len,
        extended_payload_len_cont = 0
      };
    Len > 65536 ->
      Frame#frame{
        payload_len = 127,
        extended_payload_len = 0,
        extended_payload_len_cont = Len
      }
  end.


%
% Masking code got at Cowboy source code
%
-spec mask(Data::binary(), MaskKey::integer(), Acc::binary()) -> binary().
mask(<<Data:32, Rest/bits>>, MaskKey, Acc) ->
  T = Data bxor MaskKey,
  mask(Rest, MaskKey, <<Acc/binary, T:32>>);

mask(<<Data:24>>, MaskKey, Acc) ->
  <<MaskKey2:24, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:24>>;

mask(<<Data:16>>, MaskKey, Acc) ->
  <<MaskKey2:16, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:16>>;

mask(<<Data:8>>, MaskKey, Acc) ->
  <<MaskKey2:8, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:8>>;

mask(<<>>, _, Acc) ->
  Acc.
