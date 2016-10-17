%% @author root
%% @doc @todo Add description to registrar_crypto.

-module(registrar_crypto).
-include("reg.hrl").

-on_load(load_nif/0).

-define(NIF_LOAD_INFO, 101).

-define(nif_stub, nif_stub_error(?LINE)).

%% ====================================================================
%% API functions
%% ====================================================================
-export([a3a8/2
        ,load_nif/0
        ]).

-spec a3a8(any(), any()) -> no_return().
a3a8(_, _) -> ?nif_stub.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec nif_stub_error(integer()) -> no_return().
nif_stub_error(Line) ->
    erlang:nif_error({'nif_not_loaded', 'module', ?MODULE, 'line', Line}).

-spec load_nif() -> 'ok' | {'error', {atom(), string()}}.
load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {'error', _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->  Path
              end,
    lager:info("path to nif: ~s", [PrivDir]),
    erlang:load_nif(filename:join(PrivDir, "comp128"), ?NIF_LOAD_INFO).
