-ifndef(KZ_MEDIA_HRL).

-define(CHUNKSIZE, 24576).

-record(media_file, {
          stream_url = <<>> :: binary()
                    ,contents = <<>> :: binary()
                    ,content_type = <<>> :: binary()
                    ,media_name = <<>> :: binary()
                    ,chunk_size = ?CHUNKSIZE :: integer()
                    ,shout_response = "" :: iolist()
                    ,shout_header = {0, <<>>} :: {integer(), binary()} | 'undefined'
                    ,continuous = false :: boolean()
                    ,pad_response = true :: boolean()
         }).

-define(KZ_MEDIA_HRL, 'true').
-endif.
