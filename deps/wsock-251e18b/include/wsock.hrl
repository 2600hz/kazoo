-record(http_message,{
    type :: response | request,
    start_line :: list({atom(), string()}),
    headers :: list({string(), string()})
  }).

-record(handshake, {
    version      :: integer(),
    type :: handle_open | handle_response | open | response,
    message :: #http_message{}
  }).

-type bit() :: 0..1.

-record(frame, {
    fin = 0:: bit(),
    rsv1 = 0 :: bit(),
    rsv2 = 0 :: bit(),
    rsv3 = 0 :: bit(),
    opcode :: byte(),
    mask = 0 :: bit(),
    payload_len :: byte(),
    extended_payload_len :: byte(),
    extended_payload_len_cont :: integer(),
    masking_key :: integer(),
    payload :: binary(),
    fragmented :: boolean(),
    raw :: binary(), % raw data for a fragmented frame
    next_piece :: atom(),
    next_piece_size :: integer()
  }).

-type decode_options() :: masked.
-type encode_options() :: mask.
-type encode_types()   :: text | binary | close | ping | pong.
-type frame_type()     :: encode_types() | continuation.
-type payload()        :: string() | binary() | {pos_integer(), string()}.

-record(message, {
    frames = [] :: list(#frame{}),
    payload     :: payload(),
    type        :: encode_types() | fragmented
  }).

-type message() :: #message{}.
