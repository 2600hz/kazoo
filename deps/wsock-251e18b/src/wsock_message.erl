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

-module(wsock_message).
-include("wsock.hrl").

-export([encode/2, decode/2, decode/3]).
%-export([encode/3]).


%%========================================
%% Constants
%%========================================
-define(FRAGMENT_SIZE, 4096).

%%========================================
%% Types
%%========================================
-type message_type()   :: begin_message | continue_message.

% @type decode_options() = masked.
% @type encode_options() = mask.
% @type encode_types()   = text | binary | close | ping | pong.
% @type frame_type()     = encode_types() | continuation.
% @type payload()        = string() | binary() | {pos_integer(), string()}.
% @type message() = #message{
%    frames  = list(#frame{}),
%    payload = payload(),
%    type    = encode_types() | fragmented
%  }.

%%========================================
%% Interface functions
%%========================================

% @doc Encode Data into WebSockets frames and returns an iolist with those
% frames.
%
% Options can be:
%
% <dl>
%   <dt>`text'</dt>
%   <dd>To encode data as text messages</dd>
%
%   <dt>`binary'</dt>
%   <dd>To encode data as binary messages</dd>
%
%   <dt>`close'</dt>
%   <dd>To encode data as close messages. When including a payload for this type of messages it
%   has to be a tuple with an integer if the first position (closing status) and a string on the
%   second (reason)</dd>
%
%   <dt>`ping'</dt>
%   <dd>To encode data as ping messages</dd>
%
%   <dt>`pong'</dt>
%   <dd>To encode data as pong messages</dd>
%
%   <dt>`mask'</dt>
%   <dd>To mask the data (i.e. when it is send from clients to servers)</dd>
%
% </dl>
%
% One of the `text', `binary', `close', `ping' or `pong' options is required.
%
% If no type (see {@link encode_types()}) is given the error `missing_datatypes' is returned.
-spec encode(
  Data    :: payload(),
  Options :: [encode_types() | encode_options()]
  ) ->
  [binary()] |
  {error, missing_datatype}.
encode(Data, Options) when is_list(Data) ->
  encode(list_to_binary(Data), Options);
encode(Data, Options) ->
  case extract_type(Options) of
    error ->
      {error, missing_datatype};
    {Type, BaseOptions} ->
      lists:reverse(encode(Data, Type, BaseOptions, []))
  end.


% @doc Decode received frames and return a list of messages
%
% Options can be:
%
% <dl>
%   <dt>`masked'</dt>
%   <dd>Data contains masked data. By default, if this option is not present,
%   the library will consider the data as not masked.
%   </dd>
% </dl>
%
% If a received message is fragmented (WebSocket or TCP fragmentation), this 
% message must be given as parameter to {@link decode/3} when new data is ready to be
% decoded.
%
% A fragmented message has the property `Message#message.type' set with the atom `fragmented'.
-spec decode(
  Data    :: binary(),
  Options :: list(decode_options())
  ) ->
  list(message()) |
  {error,
    fragmented_control_message |
    frames_masked |
    frames_unmasked
  }.
decode(Data, Options) ->
  Masked = proplists:get_value(masked, Options, false),
  decode(Data, begin_message, #message{}, Masked).

% @see decode/2
% @doc Decodes received frames and tries to complete a previous fragmented message.
-spec decode(
  Data    :: binary(),
  Message :: message(),
  Options :: list(decode_options())
  ) ->
  [] |
  list(message()) |
  {error,
    fragmented_control_message |
    frames_masked |
    frames_unmasked
  }.
decode(Data, Message, Options) ->
  Masked = proplists:get_value(masked, Options, false),
  decode(Data, continue_message, Message, Masked).

%%========================================
%% Internal
%%========================================
-spec extract_type(
  Options :: list(encode_options() | encode_types())
  ) ->
  error |
  {encode_types(), []} |
  {encode_types(), encode_options()}.
extract_type(Options) ->
  Types = [text, binary, close, ping, pong],
  Type = lists:filter(fun(E) ->
        true == proplists:get_value(E, Options)
    end, Types),

  case Type of
    [] -> error;
    [T] ->
      OptionsWithoutType = proplists:delete(T, Options),
      {T, OptionsWithoutType}
  end.

-spec encode(
  Data        :: binary(),
  Type        :: frame_type(),
  BaseOptions :: list(encode_options()),
  Acc         :: list(binary())
  ) -> list(binary()).
encode(Data, Type, BaseOptions, _Acc) when Type =:= ping ; Type =:= pong ; Type =:= close->
  [frame(Data, [ fin, {opcode, Type} | BaseOptions])];
encode(<<Data:?FRAGMENT_SIZE/binary>>, Type, BaseOptions, Acc) ->
  [frame(Data, [fin, {opcode, Type} | BaseOptions]) | Acc];
encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, BaseOptions, []) ->
  encode(Rest, continuation, BaseOptions, [frame(Data, [{opcode, Type} | BaseOptions]) | []]);
encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, BaseOptions, Acc) ->
  encode(Rest, Type, BaseOptions, [frame(Data, [{opcode, Type} | BaseOptions]) | Acc]);
encode(<<>>, _Type, _Options, Acc) ->
  Acc;
encode(<<Data/binary>>, Type, BaseOptions, Acc) ->
  [frame(Data, [fin, {opcode, Type} | BaseOptions]) | Acc].

-spec frame(
  Data    :: binary(),
  Options :: list()
  ) -> binary().
frame(Data, Options) ->
  Frame = wsock_framing:frame(Data, Options),
  wsock_framing:to_binary(Frame).

-spec decode(
  Data    :: binary(),
  Type    :: message_type(),
  Message :: message(),
  Masked  :: boolean()
  ) ->
    list(message()) |
    {error,
      frames_unmasked |
      frames_masked |
      fragmented_control_message
    }.
decode(Data, begin_message, _Message, Masked) ->
  do_decode(Data, begin_message, [], Masked);
decode(Data, continue_message, Message, Masked) ->
  do_decode(Data, continue_message, [Message | []], Masked).

-spec do_decode(
  Data   :: binary(),
  Type   :: message_type(),
  Acc    :: list(),
  Masked :: boolean()
  ) ->
    list(message()) |
    {error,
      frames_unmasked |
      frames_masked |
      fragmented_control_message
    }.
do_decode(Data, continue_message, [FragmentedMessage | Acc] = Messages, Masked) ->
  [LastFrame | TailFrames] = FragmentedMessage#message.frames,

  case LastFrame#frame.fragmented of
    true ->
      Frames = wsock_framing:from_binary(Data, LastFrame),
      do_decode_frames(Masked, continue_message, Frames, [FragmentedMessage#message{ frames = TailFrames } | Acc]);
    false ->
      Frames = wsock_framing:from_binary(Data),
      do_decode_frames(Masked, continue_message, Frames, Messages)
  end;
do_decode(Data, Type, Acc, Masked) ->
  Frames = wsock_framing:from_binary(Data),
  do_decode_frames(Masked, Type, Frames, Acc).

-spec do_decode_frames(
  Masked :: boolean(),
  Type   :: message_type(),
  Frames :: list(#frame{}),
  Acc    :: list()
  ) ->
    list(message()) |
    {error,
      frames_unmasked |
      frames_masked |
      fragmented_control_message
    }.
do_decode_frames(_Masked = true, Type, Frames, Acc) ->
  do_decode_masked_frames(ensure_all_frames_mask_value(Frames, 1), Type, Frames, Acc);
do_decode_frames(_Masked = false, Type, Frames, Acc) ->
  do_decode_unmasked_frames(ensure_all_frames_mask_value(Frames, 0), Type, Frames, Acc).

-spec do_decode_masked_frames(
  AllFramesMasked :: boolean(),
  Type            :: message_type(),
  Frames          :: list(#frame{}),
  Acc             :: list()
  ) ->
    list(message()) |
    {error,
        frames_unmasked |
        fragmented_control_message
      }.
do_decode_masked_frames(_AllFramesMasked = true, Type, Frames, Acc) ->
  transform_frames_into_messages(Type, Frames, Acc);
do_decode_masked_frames(_AllFramesMasked = false, _, _, _)  ->
  {error, frames_unmasked}.

-spec do_decode_unmasked_frames(
  AllFramesUnmasked :: boolean(),
  Type              :: message_type(),
  Frames            :: list(#frame{}),
  Acc               :: list()
  ) ->
    list(message()) |
    {error,
      frames_masked |
      fragmented_control_message
    }.
do_decode_unmasked_frames(_AllFramesUnmasked = true, Type, Frames, Acc) ->
  transform_frames_into_messages(Type, Frames, Acc);
do_decode_unmasked_frames(_AllFramesUnmasked = false, _, _, _) ->
  {error, frames_masked}.

-spec ensure_all_frames_mask_value(
  Frames :: list(#frame{}),
  Value  :: integer()
  ) -> boolean().
ensure_all_frames_mask_value(Frames, Value) ->
  lists:all(
    fun(#frame{ fragmented = true }) ->
        true;
    (F) ->
        F#frame.mask == Value
    end, Frames).

-spec transform_frames_into_messages(
  Type   :: message_type(),
  Frames :: list(#frame{}),
  Acc    :: list(message())
  ) ->
    list(message()) |
    {error, fragmented_control_message}.
transform_frames_into_messages(Type, Frames, Acc) ->
  case process_frames(Type, Frames, Acc) of
    {error, Reason} ->
      {error, Reason};
    Messages ->
      lists:reverse(Messages)
  end.

-spec process_frames(
  Type     :: message_type(),
  Frames   :: list(#frame{}),
  Messages :: list(message())
  ) ->
    list(message()) |
    {error, fragmented_control_message}.
process_frames(_, [], Acc) ->
  Acc;
process_frames(begin_message, Frames, Acc) ->
  wtf(Frames, begin_message, #message{}, Acc);
process_frames(continue_message, Frames, [FramgmentedMessage | Acc]) ->
  wtf(Frames, continue_message, FramgmentedMessage, Acc).

wtf([Frame | Frames], Type, XMessage, Acc) ->
  case process_frame(Frame, Type, XMessage) of
    {error, Reason} ->
      {error, Reason};
    {fragmented, Message} ->
      process_frames(continue_message, Frames, [Message#message{type = fragmented} | Acc]);
    {completed, Message} ->
      process_frames(begin_message, Frames, [Message | Acc])
  end.

-spec process_frame(
  Frame       :: #frame{},
  MessageType :: message_type(),
  Message     :: message()
  ) ->
    {fragmented | completed, message()} |
    {error, fragmented_control_message}.
process_frame(Frame, MessageType, Message) ->
  process_frame(contextualize_frame(Frame), MessageType, Frame, Message).

-spec process_frame(
  FrameType   :: atom(),
  MessageType :: message_type(),
  Frame       :: #frame{},
  Message     :: message()
  ) ->
    {frame | completed, message()} |
    {error, fragmented_control_message}.
process_frame(control_fragment, _ ,_, _) ->
  {error, fragmented_control_message};
process_frame(open_close, _, Frame, Message) ->
  frame_to_complete_message(Frame, Message);
process_frame(open_continue, _, Frame, Message) ->
  frame_for_fragmented_message(Frame, Message);
process_frame(continue, continue_message, Frame, Message) ->
  frame_for_fragmented_message(Frame, Message);
process_frame(continue_close, continue_message, Frame, Message) ->
  frame_to_complete_message(Frame, Message);
process_frame(fragmented_frame, _, Frame, Message) ->
  frame_for_fragmented_message(Frame, Message).

frame_to_complete_message(Frame, Message) ->
  UpdatedMessage = append_frame_to_message(Frame, Message),
  BuiltMessage   = build_message(UpdatedMessage, lists:reverse(UpdatedMessage#message.frames)),
  {completed, BuiltMessage}.
frame_for_fragmented_message(Frame, Message) ->
  {fragmented, append_frame_to_message(Frame, Message)}.

append_frame_to_message(Frame, Message) ->
  Frames = Message#message.frames,
  Message#message{frames = [Frame | Frames]}.

-spec contextualize_frame(
  Frame :: #frame{})
  ->
    continue_close |
    open_continue |
    continue |
    open_close |
    control_fragment |
    fragmented_frame.
contextualize_frame(#frame{ fragmented = true }) ->
  fragmented_frame;
contextualize_frame(Frame) ->
  case {Frame#frame.fin, Frame#frame.opcode} of
    {1, 0} -> continue_close;
    {0, 0} -> continue;
    {0, Opcode} when Opcode == 8; Opcode == 9; Opcode == 10 -> control_fragment;
    {1, _} -> open_close;
    {0, _} -> open_continue
  end.

-spec build_message(
  Message :: message(),
  Frames  :: list(#frame{})
  ) -> message().
build_message(Message, Frames) ->
  [HeadFrame | _] = Frames,

  case HeadFrame#frame.opcode of
    1 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = text, payload = Payload};
    2 ->
      Payload = build_payload_from_frames(binary, Frames),
      Message#message{type = binary, payload = Payload};
    8 ->
      Payload = build_payload_from_frames(close, Frames),
      Message#message{type = close, payload = Payload};
    9 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = ping, payload = Payload};
    10 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = pong, payload = Payload}
  end.

-spec build_payload_from_frames(
  Type :: close | binary | text,
  Frames :: list(#frame{})
  ) -> payload().
build_payload_from_frames(close, [Frame]) ->
  case Frame#frame.payload of
    <<>> -> {undefined, undefined};
    <<Status:16, Reason/binary>> -> {Status, binary_to_list(Reason)}
  end;
build_payload_from_frames(binary, Frames) ->
  concatenate_payload_from_frames(Frames);
build_payload_from_frames(text, Frames) ->
  Payload = concatenate_payload_from_frames(Frames),
  binary_to_list(Payload).

-spec concatenate_payload_from_frames(
  Frames :: list(#frame{})
  ) -> binary().
concatenate_payload_from_frames(Frames) ->
  concatenate_payload_from_frames(Frames, <<>>).

-spec concatenate_payload_from_frames(
  Frames :: list(#frame{}),
  Acc    :: binary()
  ) -> binary().
concatenate_payload_from_frames([], Acc) ->
  Acc;
concatenate_payload_from_frames([Frame | Rest], Acc) ->
  concatenate_payload_from_frames(Rest, <<Acc/binary, (Frame#frame.payload)/binary>>).
