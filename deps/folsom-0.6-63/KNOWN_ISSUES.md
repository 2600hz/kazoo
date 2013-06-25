### Known Issues

* erlang:system_info(cpu_topology) returns a proplist that has mutiple values with identical keys which causes mochijson2 to only take the last key/value pair.
