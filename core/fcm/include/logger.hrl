%% logging callbacks

-define(PRINT(Format, Args), io:format(Format, Args)).

-define(DEBUG(Format, Args), lager:log(debug, self(), Format, Args)).

-define(INFO_MSG(Format, Args), lager:log(info, self(), Format, Args)).

-define(WARNING_MSG(Format, Args), lager:log(warning, self(), Format, Args)).

-define(ERROR_MSG(Format, Args), lager:log(error, self(), Format, Args)).

-define(CRITICAL_MSG(Format, Args), lager:log(critical, self(), Format, Args)).
