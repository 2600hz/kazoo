-ifndef(WHT_HRL).

-include_lib("whistle/include/wh_types.hrl").

-type exec_element_return() ::
        {'ok', whapps_call:call()} |
        {'stop', whapps_call:call()} |
        {'request', whapps_call:call(), ne_binary(), 'get' | 'post', wh_proplist()}.
%%      {request, Call, URI, Method, BaseParams}

-type exec_return() ::
        {'stop', whapps_call:call()} |
        {'request', whapps_call:call(), ne_binary(), 'get' | 'post', wh_proplist()}.

-define(WHT_HRL, true).
-endif.
