exml
====

[![Build Status](https://secure.travis-ci.org/esl/exml.png)](http://travis-ci.org/esl/exml)

**exml** is an Erlang library helpful with parsing XML streams
and doing some basic XML structures manipulation.

Building
========

**exml** is a rebar-compatible OTP application, run `make` or
`./rebar compile` in order to build it.

As a requirement, development headers for expat library are
required.

Using
=====

**exml** can parse both XML streams as well as single XML
documents at once.

To parse a whole XML document:

```erlang
{ok, Parser} = exml:parse(<<"<my_xml_doc/>">>).
```

To generate an XML document from Erlang terms:

```erlang
El = #xmlel{name = <<"foo">>,
            attrs = [{<<"attr1">>, <<"bar">>}],
            children = [{xmlcdata, <<"Some Value">>}]},
exml:to_list(El).
```

or (pastable into `erl` shell):

```erlang
El = {xmlel, <<"foo">>,
      [{<<"attr1">>, <<"bar">>}],
      [{xmlcdata, <<"Some Value">>}]}.
exml:to_list(El).
```

Which results in:

```xml
<foo attr1='bar'>Some Value</foo>
```

`exml:to_binary/1` works similarly.

There're also `exml:to_pretty_iolist/1,3` for a quick'n'dirty document
preview (pastable into `erl`):

```erlang
rr("include/exml.hrl").
El = #xmlel{name = <<"outer">>,
            attrs = [{<<"attr1">>, <<"val1">>},
                     {<<"attr2">>, <<"val-two">>}],
            children = [#xmlel{name = <<"inner-childless">>},
                        #xmlel{name = <<"inner-w-children">>,
                               children = [#xmlel{name = <<"a">>}]}]}.
io:format("~s", [exml:to_pretty_iolist(El)]).
```

which prints:

```xml
<outer attr2='val-two' attr1='val1'>
  <inner-childless/>
  <inner-w-children>
    <a/>
  </inner-w-children>
</outer>
```

For an example of using the streaming API see `test/exml_stream_tests.erl`.
