-ifndef(CF_DIRECTORY_HRL).

-record(directory_user, {first_name :: kz_term:ne_binary()
                        ,last_name :: kz_term:ne_binary()
                        ,full_name :: kz_term:ne_binary()
                        ,first_last_keys :: kz_term:ne_binary() % DTMF-version of first, last
                        ,last_first_keys :: kz_term:ne_binary() % DTMF-version of last, first
                        ,callflow_id :: kz_term:ne_binary() % what callflow to use on match
                        ,name_audio_id :: kz_term:api_binary() % pre-recorded audio of user's name
                        }).
-type directory_user() :: #directory_user{}.
-type directory_users() :: [directory_user()].

-type search_field() :: 'first' | 'last' | 'both'.

-record(directory, {sort_by = 'last' :: 'first' | 'last'
                   ,search_fields = 'both' :: search_field()
                   ,min_dtmf :: pos_integer()
                   ,max_dtmf :: non_neg_integer()
                   ,confirm_match = 'false' :: boolean()
                   ,digits_collected = <<>> :: binary()
                   ,users = [] :: directory_users()
                   ,curr_users = [] :: directory_users()
                   }).
-type directory() :: #directory{}.

-type dtmf_action() :: 'route' | 'next' | 'start_over' | 'invalid' | 'continue'.

-define(CF_DIRECTORY_HRL, 'true').
-endif.
