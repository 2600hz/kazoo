-ifndef(KZD_RATEDECK_HRL).

-define(MATCH_RATEDECK_DB_ENCODED(RatedeckId)
       ,<<"ratedeck%2F", RatedeckId/binary>>
       ).
-define(MATCH_RATEDECK_DB_encoded(RatedeckId)
       ,<<"ratedeck%2f", RatedeckId/binary>>
       ).
-define(MATCH_RATEDECK_DB_UNENCODED(RatedeckId)
       ,<<"ratedeck/", RatedeckId/binary>>
       ).

-define(ENCODE_RATEDECK_DB(RatedeckId)
       ,<<"ratedeck%2F", (RatedeckId)/binary>>
       ).
-define(encode_RATEDECK_DB(RatedeckId)
       ,<<"ratedeck%2f", (RatedeckId)/binary>>
       ).
-define(UNENCODED_RATEDECK_DB(RatedeckId)
       ,<<"ratedeck/", (RatedeckId)/binary>>
       ).

-define(KZD_RATEDECK_HRL, 'true').
-endif.
