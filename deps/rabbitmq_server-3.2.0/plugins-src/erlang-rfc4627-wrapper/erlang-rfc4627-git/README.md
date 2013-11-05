# RFC4627 (JSON) and JSON-RPC for Erlang

An implementation of JSON and JSON-RPC for Erlang.

See
[rfc4627.erl](http://tonyg.github.com/erlang-rfc4627/doc/rfc4627.html),
the JSON/RFC4627 codec itself, to learn how to encode and decode JSON
objects from Erlang code.

## Providing and calling JSON-RPC services

See
[rfc4627\_jsonrpc.erl](http://tonyg.github.com/erlang-rfc4627/doc/rfc4627_jsonrpc.html),
a JSON-RPC service registry and transport-neutral service method
invocation mechanism, to learn how to expose Erlang processes as
remotely-callable JSON-RPC services, and to learn how to invoke local
JSON-RPC services from Erlang without the overhead of HTTP.

## Exposing JSON-RPC services over HTTP

### Using Inets

See
[rfc4627\_jsonrpc\_inets.erl](http://tonyg.github.com/erlang-rfc4627/doc/rfc4627_jsonrpc_inets.html),
an Inets HTTP transport binding for JSON-RPC, to learn how to
configure the Inets HTTP server to respond to JSON-RPC requests.

### Using Mochiweb

See
[rfc4627\_jsonrpc\_mochiweb.erl](http://tonyg.github.com/erlang-rfc4627/doc/rfc4627_jsonrpc_mochiweb.html)
to learn how to delegate incoming Mochiweb HTTP requests to the
JSON-RPC service dispatcher.

### Using Cowboy

See
[rfc4627\_jsonrpc\_cowboy.erl](http://tonyg.github.com/erlang-rfc4627/doc/rfc4627_jsonrpc_cowboy.html)
to learn how to delegate incoming Cowboy HTTP requests to the
JSON-RPC service dispatcher.

## Running the example test service that comes with the source code

Included with the Erlang RFC4627 source code is a small Inets-based
example that defines a "hello world"-style JSON-RPC service, and calls
it from a Javascript program embedded in a web page.

At your Erlang shell,

 - after compiling the code with `make all test-compile`,
 - when your current working directory contains the `test` directory
   from the distribution, such that
   `./test/server_root/conf/httpd.conf` exists,

type `test_jsonrpc_inets:start_httpd().` (Don't forget the trailing
"."!) This will

 - start the inets httpd on port 5671 (from `./test/server_root/conf/httpd.conf`)
 - allow HTTP access to JSON-RPC services via a url prefix of `/rpc` (again from `httpd.conf`)
 - start the `rfc4627_jsonrpc` service registry
 - register the test service

Visiting <http://localhost:5671/> in your browser should load a page
that uses javascript to invoke the Erlang-implemented JSON-RPC test
service.

## Invoking JSON-RPC procedures from Javascript

See the [relevant
section](http://tonyg.github.com/erlang-rfc4627/doc/overview-summary.html#Invoking_JSON-RPC_procedures_from_Javascript)
of the `edoc` documentation included with the source code.

## References

 - the [JSON RFC](http://www.ietf.org/rfc/rfc4627.txt).

 - the [JSON-RPC
   specification](http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html)
   1.1 working draft ([mirrored
   locally](http://tonyg.github.com/erlang-rfc4627/doc/JSON-RPC-1-1-WD-20060807.html)).

 - Joe Armstrong's
   [message](http://erlang.org/pipermail/erlang-questions/2005-November/017805.html)
   describing the basis of the JSON data type mapping that the
   `rfc4627.erl` module uses.

## Contributors

The codebase and documentation was originally written between 2007 and
2010 by Tony Garnock-Jones while at LShift in London. Since then, it
has been maintained by Tony and enhanced with a few generous
contributions from others:

 - András Veres-Szentkirályi
 - Eugene Volchek
 - Simon MacMullen
 - Andrey Khozov
 - Erik Timan

## Copyright and Licence

Copyrights:

 - Copyright &copy; 2007-2010, 2011, 2012 Tony Garnock-Jones.
 - Portions copyright 2007-2010 LShift Ltd.
 - Portions copyright contributors to the project, listed above.

License (MIT):

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
    BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
    ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
