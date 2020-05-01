%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_file_proxy).

-export([init/2]).
-export([terminate/3]).

-include("fax.hrl").

-spec init(cowboy_req:req(), any()) ->
          {'ok', cowboy_req:req(), 'ok'}.
init(Req0, _Opts) ->
    kz_log:put_callid(kz_binary:rand_hex(16)),
    maybe_send_job_file(Req0, cowboy_req:path_info(Req0)).

maybe_send_job_file(Req0, [JobId]) ->
    lager:debug("fetching fax job ~s", [JobId]),
    TmpDir = kapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>),
    File = list_to_binary([TmpDir, JobId]),
    Req1 = case file:read_file(File) of
               {'ok', Content} ->
                   lager:debug("sending fax contents for ~s", [File]),
                   Headers = #{<<"content-type">> => <<"image/tiff">>},
                   cowboy_req:reply(200, Headers, Content, Req0);
               {'error', _Reason} ->
                   lager:debug("could not open file '~s': ~p", [File, _Reason]),
                   cowboy_req:reply(404, Req0)
           end,
    {'ok', Req1, 'ok'};
maybe_send_job_file(Req0, _Else) ->
    lager:debug("not sending job file contents for ~p", [_Else]),
    {'ok', cowboy_req:reply(404, Req0), 'ok'}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.
