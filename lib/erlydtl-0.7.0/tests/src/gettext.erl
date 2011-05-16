%% Dummy impl of a gettext connector
-module(gettext).
-export([key2str/2]).

key2str(String, Locale)->
 		case Locale of
                        undefined -> String;
                        "reverse" -> lists:reverse(String);
                        _ -> throw(only_undefined_and_reverse_locale_allowed_on_test)
                end.
