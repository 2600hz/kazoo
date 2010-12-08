-module(static_resource).

-export([init/1, content_types_provided/2, resource_exists/2, known_content_type/2]).
-export([last_modified/2, generate_etag/2, content/2, encodings_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

-record(context, {
	  root = [] :: string()
	  ,filepath = [] :: string()
	  ,fileinfo = #file_info{}
	  ,mime_type = "" :: string() %% our guess at what the mime type is
	  ,content_types = [] :: list(string()) %% what mime types do we server
	 }).

init(Opts) ->
    Path = filename:dirname(filename:dirname(code:which(?MODULE))),
    {ok, #context{
       root = lists:concat([Path, props:get_value(root, Opts, [])])
       ,content_types = props:get_value(content_types, Opts, [])
      }}.

known_content_type(RD, #context{content_types=CTs}=Context) ->
    Path = wrq:disp_path(RD),
    Mime = webmachine_util:guess_mime(Path),
    {lists:member(Mime, CTs), RD, Context#context{mime_type=Mime}}.

content_types_provided(RD, #context{mime_type=Mime}=Context) ->
    {[{Mime, content}], RD, Context}.

resource_exists(RD, #context{root=Root}=Context) ->
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

content(RD, #context{filepath=FP}=Context) ->
    {ok, Data} = file:read_file(FP),
    {Data, RD, Context}.

encodings_provided(RD, Context) ->
    { [{"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}
      ],
      RD, Context}.
