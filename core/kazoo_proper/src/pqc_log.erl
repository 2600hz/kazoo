%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_log).

-export([log_info/0]).

-spec log_info() -> [{atom(), iolist()}].
log_info() ->
    log_info(get('now')).
log_info('undefined') ->
    [];
log_info(Now) ->
    [{'elapsed', kz_term:to_list(kz_time:elapsed_ms(Now))}].
