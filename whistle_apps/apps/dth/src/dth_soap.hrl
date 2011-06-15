-include("dth.hrl").

-record('call_record', {
          customer_id :: binary()
          ,batch_id = <<>> :: binary()
          ,originating_number = <<>> :: binary()
          ,destination_number = <<>> :: binary()
          ,start_time = erlang:now() :: tuple()
          ,duration = 0 :: non_neg_integer()
          ,unique_id = <<>> :: binary()
          ,call_type = 0 :: dth_call_type()
         }).
