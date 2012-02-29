-module(static_resource).

-export([init/1, content_types_provided/2, resource_exists/2]).
-export([last_modified/2, generate_etag/2, content/2, encodings_provided/2]).

-include_lib("kernel/include/file.hrl").
-include("crossbar.hrl").

-record(context, {
          root = "" :: iolist()
          ,filepath = "" :: string()
          ,fileinfo = #file_info{} :: #file_info{}
          ,data = <<>> :: binary()
         }).

-spec init/1 :: (Opts) -> {'ok', #context{}} when
      Opts :: proplist().
init(Opts) ->
    Path = [code:priv_dir(crossbar), props:get_value(root, Opts)],
    lager:debug("Init filepath: ~s", [Path]),
    {ok, #context{root=Path}}.

content_types_provided(RD, Context) ->
    Path = wrq:disp_path(RD),
    lager:debug("Request Path: ~p", [Path]),
    Mime = webmachine_util:guess_mime(Path),
    lager:debug("Mime: ~s", [Mime]),
    {[{Mime, content}], RD, Context}.

resource_exists(RD, #context{root=Root}=Context) ->
    FP = filename:join([Root, wrq:disp_path(RD)]),
    lager:debug("Disp_path: ~s", [wrq:disp_path(RD)]),
    lager:debug("Requested file: ~s", [FP]),
    case filelib:is_regular(FP) of
        true ->
            lager:debug("File exists"),
            {ok, FileInfo} = file:read_file_info(FP),
            {true, RD, Context#context{filepath=FP, fileinfo=FileInfo}};
        _ ->
            lager:debug("File doesn't exist"),
            {false, RD, Context}
    end.

generate_etag(RD, #context{filepath=FP}=Context) ->
    {ok, Data} = file:read_file(FP),
    ETag = wh_util:to_hex_binary(crypto:md5(Data)),
    lager:debug("Etag: ~s", [ETag]),
    { ETag, RD, Context#context{data=Data} }.

last_modified(RD, #context{fileinfo=FI}=Context) ->
    {_D, _T}=LastMod = FI#file_info.mtime,
    lager:debug("Last modified: ~p ~p", [_D,_T]),
    {LastMod, RD, Context}.

content(RD, #context{filepath=FP, data = <<>>}=Context) ->
    {ok, Data} = file:read_file(FP),
    lager:debug("Content loaded from disk"),
    {Data, RD, Context};
content(RD, #context{data=Data}=Context) ->
    lager:debug("Content loaded"),
    {Data, RD, Context}.

encodings_provided(RD, Context) ->
    { [
       {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}
      ]
      ,RD, Context}.
