-module(gen_cf_action).

-callback handle(kz_json:object(), kapps_call:call()) -> any().
