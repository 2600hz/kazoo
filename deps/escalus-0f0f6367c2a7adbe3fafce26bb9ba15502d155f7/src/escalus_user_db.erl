-module(escalus_user_db).

%% For the example implementations see `escalus_users` and `escalus_ejabberd` modules.
%% For another implementation see ESL internal repository at
%% https://gitlab.erlang-solutions.com/simon.zelazny/fake-auth-server.

-callback start(any()) -> any().
-callback stop(any()) -> any().
-callback create_users(escalus:config(), escalus_users:who()) -> escalus:config().
-callback delete_users(escalus:config(), escalus_users:who()) -> escalus:config().
