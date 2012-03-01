-record(riakobject_pb, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    val = erlang:error({required, val})
}).

