%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_rsa_drv).

-export([load/0, open/0, close/1, gen_rsa/4]).

-define(DRIVER_NAME, ?MODULE_STRING).

-define(DRV_CMD_INFO, 0).
-define(DRV_CMD_RSA,  1).

-spec load() -> 'ok' | {'error', any()}.
load() ->
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member(?DRIVER_NAME, Drivers) of
        true -> ok;
        false ->
            case erl_ddll:load(priv_dir(), ?DRIVER_NAME) of
                ok -> ok;
                {error, Error} ->
                    error_logger:error_msg(
                      ?MODULE_STRING ": Error loading ~p: ~p~n",
                      [?DRIVER_NAME, erl_ddll:format_error(Error)]
                     ),
                    {error, Error}
            end
    end.

-spec priv_dir() -> file:filename_all().
priv_dir() ->
    case code:priv_dir(kazoo_auth) of
        List when is_list(List) -> List;
        {error, bad_name} ->
            filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end.

-spec open() -> port() | {'error', any()}.
open() ->
    try erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary])
    catch error:badarg ->
            case load() of
                ok -> erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary]);
                {error, _Reason} = Error -> Error
            end
    end.

-spec close(port()) -> 'ok'.
close(Port) when is_port(Port) ->
    try erlang:port_close(Port), ok
    catch error:badarg -> ok end.

-spec gen_rsa(port(), integer(), integer(), integer()) -> any().
gen_rsa(Port, Ref, Bits, E)
  when is_port(Port)
       andalso is_integer(Ref)
       andalso is_integer(Bits)
       andalso Bits > 0
       andalso E band 1 =:= 1 ->
    erlang:port_call(Port, ?DRV_CMD_RSA, {Ref, Bits, E}).
