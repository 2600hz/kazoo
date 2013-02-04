%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(fax_file_proxy).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-include("fax.hrl").

-spec init({'tcp' | 'ssl', 'http'}, #http_req{}, proplist()) -> {'ok', #http_req{}, 'undefined'} |
                                                                      {'shutdown', #http_req{}, 'undefined'}.
init({_Any, http}, Req0, _Opts) ->
    put(callid, wh_util:rand_hex_binary(16)),
    {ok, Req0, undefined};
init({_Any, _Prot}, Req0, _Opts) ->
    put(callid, wh_util:rand_hex_binary(16)),
    {shutdown, Req0, undefined}.

-spec handle(#http_req{}, State) -> {'ok', #http_req{}, State}.
handle(Req0, State) ->
    case cowboy_http_req:path_info(Req0) of
        {[JobId], Req1} -> 
            lager:debug("fetching ~s", [JobId]),
            TmpDir = whapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>),
            File = list_to_binary([TmpDir, JobId]),
            {ok, Req2} = case file:read_file(File) of
                             {ok, Content} ->
                                 lager:debug("sending fax contents", []),
                                 TmpDir = whapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>),
                                 Headers = [{'Content-Type', "image/tiff"}],
                                 cowboy_http_req:reply(200, Headers, Content, Req1);
                             {error, _Reason} ->
                                 lager:debug("could not open file '~s': ~p", [File, _Reason]),
                                 cowboy_http_req:reply(404, Req1)
                         end,
            {ok, Req2, State};
        _Else ->
            {ok, Req1} = cowboy_http_req:reply(404, Req0),
            {ok, Req1, State}
    end.

-spec terminate(#http_req{}, term()) -> 'ok'.
terminate(_Req, _State) ->
    ok.
