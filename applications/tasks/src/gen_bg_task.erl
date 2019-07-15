-module(gen_bg_task).

%% @doc Configure the bindings for the databases / times you want to handle
-callback init() -> 'ok'.
