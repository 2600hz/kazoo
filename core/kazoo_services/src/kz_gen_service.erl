%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc A generic Kazoo service module
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_gen_service).

-callback reconcile(kz_services:services()) ->
    kz_services:services().
