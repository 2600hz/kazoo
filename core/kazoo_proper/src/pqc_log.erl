%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_log).

-export([log_info/0]).

-spec log_info() -> [{atom(), iolist()}].
log_info() ->
    log_info(get('start_time')).
log_info('undefined') ->
    [];
log_info(StartTime) ->
    [{'elapsed', kz_term:to_list(kz_time:elapsed_ms(StartTime))}].
