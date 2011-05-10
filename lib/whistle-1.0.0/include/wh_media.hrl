-define(CHUNKSIZE, 24576).

-record(media_file, {
	  stream_url = <<>> :: binary()
	 ,contents = <<>> :: binary()
         ,content_type = <<>> :: binary()
         ,media_name = <<>> :: binary()
	 ,chunk_size = ?CHUNKSIZE :: integer()
         ,shout_response = "" :: iolist()
         ,shout_header = {0, <<>>} :: tuple(integer(), binary()) | undefined
         ,continuous = false :: boolean()
         ,pad_response = true :: boolean()
	 }).
