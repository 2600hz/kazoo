-ifndef(KZ_MEDIA_HRL).

-define(CHUNKSIZE, 24576).

-record(media_file, {stream_url = <<>> :: binary()
                    ,contents = <<>> :: binary()
                    ,content_type = <<>> :: binary()
                    ,media_name = <<>> :: binary()
                    ,chunk_size = ?CHUNKSIZE :: integer()
                    ,shout_response = "" :: iolist()
                    ,shout_header = {0, <<>>} :: {integer(), binary()} | 'undefined'
                    ,continuous = false :: boolean()
                    ,pad_response = true :: boolean()
                    }).

-type media_store_option() :: {'content-type', kz_term:ne_binary()} |
                              {'doc_type', kz_term:ne_binary()} |
                              {'rev', kz_term:ne_binary()} |
                              {'plan_override', map()} |
                              {'storage_id', kz_term:ne_binary()} |
                              {'doc_owner', kz_term:ne_binary()} |
                              {'save_error', boolean()} |
                              {'error_verbosity', 'verbose'}.
-type media_store_options() :: [media_store_option()].

-record(media_store_path, {db :: kz_term:ne_binary()
                          ,id :: kz_term:ne_binary()
                          ,att :: kz_term:ne_binary()
                          ,opt = [] :: media_store_options()
                          }).

-type media_store_path() :: #media_store_path{}.

-define(KZ_MEDIA_HRL, 'true').
-endif.
