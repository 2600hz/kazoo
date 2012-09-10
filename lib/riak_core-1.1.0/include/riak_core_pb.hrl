-ifndef(RIAKOBJECT_PB_PB_H).
-define(RIAKOBJECT_PB_PB_H, true).
-record(riakobject_pb, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    val = erlang:error({required, val})
}).
-endif.

