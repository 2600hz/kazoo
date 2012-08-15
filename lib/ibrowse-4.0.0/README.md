# ibrowse [![Build Status](https://secure.travis-ci.org/johannesh/ibrowse.png)](http://travis-ci.org/johannesh/ibrowse)

ibrowse is a HTTP client written in erlang.

**License:** ibrowse is available under two different licenses.
  LGPL or the BSD license.

**Comments to:** chandrashekhar.mullaparthi@gmail.com

**Current Version:** 4.0.0

**Latest Version:** git://github.com/cmullaparthi/ibrowse.git



## Features

*  [RFC2616](http://www.ietf.org/rfc/rfc2616.txt) compliant (AFAIK)
*  supports GET, POST, OPTIONS, HEAD, PUT, DELETE, TRACE,
   MKCOL, PROPFIND, PROPPATCH, LOCK, UNLOCK, MOVE and COPY
*  Understands HTTP/0.9, HTTP/1.0 and HTTP/1.1
*  Understands chunked encoding
*  Can generate requests using [Chunked Transfer-Encoding](http://en.wikipedia.org/wiki/Chunked_transfer_encoding)
*  Pools of connections to each webserver
*  Pipelining support
*  Download to file
*  Asynchronous requests. Responses are streamed to a process
*  Basic authentication
*  Supports proxy authentication
*  Can talk to secure webservers using SSL
*  *Any other features in the code not listed here :)*



## Usage Examples

Remember to start ibrowse first:

```erlang
5> ibrowse:start().
{ok,<0.94.0>}
```



### Synchronous Requests

A simple `GET` request:

```erlang
6> ibrowse:send_req("http://intranet/messenger/", [], get).
{ok,"200",
    [{"Server","Microsoft-IIS/5.0"},
     {"Content-Location","http://intranet/messenger/index.html"},
     {"Date","Fri, 17 Dec 2004 15:16:19 GMT"},
     {"Content-Type","text/html"},
     {"Accept-Ranges","bytes"},
     {"Last-Modified","Fri, 17 Dec 2004 08:38:21 GMT"},
     {"Etag","\"aa7c9dc313e4c41:d77\""},
     {"Content-Length","953"}],
    "<html>...</html>"}
```


A `GET` using a proxy:

```erlang
7> ibrowse:send_req("http://www.google.com/", [], get, [],
                 [{proxy_user, "XXXXX"},
                  {proxy_password, "XXXXX"},
                  {proxy_host, "proxy"},
                  {proxy_port, 8080}], 1000).
{ok,"302",
    [{"Date","Fri, 17 Dec 2004 15:22:56 GMT"},
     {"Content-Length","217"},
     {"Content-Type","text/html"},
     {"Set-Cookie",
      "PREF=ID=f58155c797f9..."},
     {"Server","GWS/2.1"},
     {"Location",
      "http://www.google.co.uk/cxfer?c=PREF%3D:TM%3D110329..."},
     {"Via","1.1 netapp01 (NetCache NetApp/5.5R2)"}],
    "<HTML>...</HTML>\r\n"}
```


A `GET` response saved to file. A temporary file is created and the
filename returned. The response will only be saved to file if the
status code is in the `200` range. The directory to download to can
be set using the application env var `download_dir` - the default
is the current working directory:

```erlang
8> ibrowse:send_req("http://www.erlang.se/", [], get, [],
                 [{proxy_user, "XXXXX"},
                  {proxy_password, "XXXXX"},
                  {proxy_host, "proxy"},
                  {proxy_port, 8080},
                  {save_response_to_file, true}], 1000).
{error,req_timedout}

9> ibrowse:send_req("http://www.erlang.se/", [], get, [],
                 [{proxy_user, "XXXXX"},
                  {proxy_password, "XXXXX"},
                  {proxy_host, "proxy"},
                  {proxy_port, 8080},
                  {save_response_to_file, true}], 5000).
{ok,"200",
    [{"Transfer-Encoding","chunked"},
     {"Date","Fri, 17 Dec 2004 15:24:36 GMT"},
     {"Content-Type","text/html"},
     {"Server","Apache/1.3.9 (Unix)"},
     {"Via","1.1 netapp01 (NetCache NetApp/5.5R2)"}],
    {file,"/Users/chandru/code/ibrowse/src/ibrowse_tmp_file_1103297041125854"}}
```


Setting the size of the connection pool and pipeline. This sets the
number of maximum connections to the specified server to `10` and the pipeline
size to `1`. Connections are assumed to be already setup.

```erlang
11> ibrowse:set_dest("www.hotmail.com", 80, [{max_sessions, 10},
                                             {max_pipeline_size, 1}]).
ok
```


Example using the `HEAD` method:

```erlang
56> ibrowse:send_req("http://www.erlang.org", [], head).
{ok,"200",
    [{"Date","Mon, 28 Feb 2005 04:40:53 GMT"},
     {"Server","Apache/1.3.9 (Unix)"},
     {"Last-Modified","Thu, 10 Feb 2005 09:31:23 GMT"},
     {"Etag","\"8d71d-1efa-420b29eb\""},
     {"Accept-ranges","bytes"},
     {"Content-Length","7930"},
     {"Content-Type","text/html"}],
    []}
```


Example using the `OPTIONS` method:

```erlang
62> ibrowse:send_req("http://www.sun.com", [], options).
{ok,"200",
    [{"Server","Sun Java System Web Server 6.1"},
     {"Date","Mon, 28 Feb 2005 04:44:39 GMT"},
     {"Content-Length","0"},
     {"P3p",
      "policyref=\"http://www.sun.com/p3p/Sun_P3P_Policy.xml\", CP=\"CAO DSP COR CUR ADMa DEVa TAIa PSAa PSDa CONi TELi OUR  SAMi PUBi IND PHY ONL PUR COM NAV INT DEM CNT STA POL PRE GOV\""},
     {"Set-Cookie",
      "SUN_ID=X.X.X.X:169191109565879; EXPIRES=Wednesday, 31-Dec-2025 23:59:59 GMT; DOMAIN=.sun.com; PATH=/"},
     {"Allow",
      "HEAD, GET, PUT, POST, DELETE, TRACE, OPTIONS, MOVE, INDEX, MKDIR, RMDIR"}],
    []}
```



### Asynchronous Requests

Example of an asynchronous `GET` request:

```erlang
18> ibrowse:send_req("http://www.google.com", [], get, [],
                     [{proxy_user, "XXXXX"},
                      {proxy_password, "XXXXX"},
                      {proxy_host, "proxy"},
                      {proxy_port, 8080},
                      {stream_to, self()}]).
{ibrowse_req_id,{1115,327256,389608}}

19> flush().
Shell got {ibrowse_async_headers,{1115,327256,389608},
           "302",
           [{"Date","Thu, 05 May 2005 21:06:41 GMT"},
            {"Content-Length","217"},
            {"Content-Type","text/html"},
            {"Set-Cookie",
             "PREF=ID=b601f16bfa32f071:CR=1:TM=1115327201:LM=1115327201:S=OX5hSB525AMjUUu7; expires=Sun, 17-Jan-2038 19:14:07 GMT; path=/; domain=.google.com"},
            {"Server","GWS/2.1"},
            {"Location",
             "http://www.google.co.uk/cxfer?c=PREF%3D:TM%3D1115327201:S%3DDS9pDJ4IHcAuZ_AS&prev=/"},
            {"Via",
             "1.1 hatproxy01 (NetCache NetApp/5.6.2)"}]}
Shell got {ibrowse_async_response,{1115,327256,389608},
           "<HTML>...</HTML>\r\n"}
Shell got {ibrowse_async_response_end,{1115,327256,389608}}
ok
```


Another asynchronous `GET` request:

```erlang
24> ibrowse:send_req("http://yaws.hyber.org/simple_ex2.yaws", [], get, [],
                     [{proxy_user, "XXXXX"},
                      {proxy_password, "XXXXX"},
                      {proxy_host, "proxy"},
                      {proxy_port, 8080},
                      {stream_to, self()}]).
{ibrowse_req_id,{1115,327430,512314}}

25> flush().
Shell got {ibrowse_async_headers,{1115,327430,512314},
           "200",
           [{"Date","Thu, 05 May 2005 20:58:08 GMT"},
            {"Content-Length","64"},
            {"Content-Type","text/html;charset="},
            {"Server",
             "Yaws/1.54 Yet Another Web Server"},
            {"Via",
             "1.1 hatproxy01 (NetCache NetApp/5.6.2)"}]}
Shell got {ibrowse_async_response,{1115,327430,512314},
           "<html>...</html>\n"}
Shell got {ibrowse_async_response_end,{1115,327430,512314}}
```


Example of request which fails when using the async option. Here
the `{ibrowse_req_id, ReqId}` is not returned. Instead the error code is
returned.

```erlang
68> ibrowse:send_req("http://www.earlyriser.org", [], get, [], [{stream_to, self()}]).
{error,conn_failed}
```



### Other Examples

Example of request using both Proxy-Authorization and authorization
by the final webserver:

```erlang
17> ibrowse:send_req("http://www.erlang.se/lic_area/protected/patches/erl_756_otp_beam.README",
                     [], get, [],
                     [{proxy_user, "XXXXX"},
                      {proxy_password, "XXXXX"},
                      {proxy_host, "proxy"},
                      {proxy_port, 8080},
                      {basic_auth, {"XXXXX", "XXXXXX"}}]).
{ok,"200",
    [{"Accept-Ranges","bytes"},
     {"Date","Thu, 05 May 2005 21:02:09 GMT"},
     {"Content-Length","2088"},
     {"Content-Type","text/plain"},
     {"Server","Apache/1.3.9 (Unix)"},
     {"Last-Modified","Tue, 03 May 2005 15:08:18 GMT"},
     {"ETag","\"1384c8-828-427793e2\""},
     {"Via","1.1 hatproxy01 (NetCache NetApp/5.6.2)"}],
    "Patch Id:\t\terl_756_otp_beam\n..."}
```


Example of a `TRACE` request. Very interesting! yaws.hyber.org didn't
support this. Nor did www.google.com. But good old BBC supports this:

```erlang
35> 37> ibrowse:send_req("http://www.bbc.co.uk/", [], trace, [],
                         [{proxy_user, "XXXXX"},
                          {proxy_password, "XXXXX"},
                          {proxy_host, "proxy"},
                          {proxy_port, 8080}]).
{ok,"200",
    [{"Transfer-Encoding","chunked"},
     {"Date","Thu, 05 May 2005 21:40:27 GMT"},
     {"Content-Type","message/http"},
     {"Server","Apache/2.0.51 (Unix)"},
     {"Set-Cookie",
      "BBC-UID=7452e72a..."},
     {"Set-Cookie",
      "BBC-UID=7452e72a..."},
     {"Via","1.1 hatproxy01 (NetCache NetApp/5.6.2)"}],
    "TRACE / HTTP/1.1\r\nHost: www.bbc.co.uk\r\nConnection: keep-alive\r\nX-Forwarded-For: 172.24.28.29\r\nVia: 1.1 hatproxy01 (NetCache NetApp/5.6.2)\r\nCookie: BBC-UID=7452e...\r\n\r\n"}
```
