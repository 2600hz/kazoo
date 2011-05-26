-module(static_resource).

-export([init/1, content_types_provided/2, resource_exists/2]).
-export([last_modified/2, generate_etag/2, content/2, encodings_provided/2]).

-record(context, {root, filepath, fileinfo}).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

init(Opts) ->
    Path = lists:concat([filename:dirname(filename:dirname(code:which(?MODULE))), props:get_value(root, Opts)]),
    {ok, #context{root=Path}}.

content_types_provided(RD, Context) ->
    Path = wrq:disp_path(RD),
    Mime = webmachine_util:guess_mime(Path),
    {[{Mime, content}], RD, Context}.

resource_exists(RD, Context=#context{root=Root}) ->
    FP = filename:join([Root, wrq:disp_path(RD)]),
    case filelib:is_regular(FP) of
	true ->
            {ok, FileInfo} = file:read_file_info(FP),
	    {true, RD, Context#context{filepath=FP, fileinfo=FileInfo}};
	_ ->
	    {false, RD, Context}
    end.

generate_etag(RD, #context{filepath=FP}=Context) ->
    {ok, Data} = file:read_file(FP),
    { mochihex:to_hex(crypto:md5(Data)), RD, Context }.

last_modified(RD, Context) ->
    {(Context#context.fileinfo)#file_info.mtime, RD, Context}.

content(RD, Context=#context{filepath=FP}) ->
    {ok, Data} = file:read_file(FP),
    {Data, RD, Context}.

encodings_provided(RD, Context) ->
    { [
       {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}
      ]
      ,RD, Context}.
