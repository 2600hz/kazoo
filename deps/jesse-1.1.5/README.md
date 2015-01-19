=====
jesse [![Build Status](https://secure.travis-ci.org/klarna/jesse.png)](http://travis-ci.org/klarna/jesse)
=====

jesse (JSon Schema Erlang) is an implementation of a json schema validator
for Erlang.

jesse implements [Draft 03] (http://tools.ietf.org/html/draft-zyp-json-schema-03) of
the specification. It supports almost all core schema definitions except:

* format
* $ref

Quick start
-----------

There are two ways of using jesse:

* to use jesse internal in-memory storage to keep all your schema definitions
  In this case jesse will look up a schema definition in its own storage,
  and then validate given json.
* it is also possible to provide jesse with schema definitions when jesse is called.

Examples
--------

    NOTE: jesse doesn't have any parsing functionality. It currently works with three
          formats: mochijson2, jiffy and jsx, so json needs to be parsed in advance,
          or you can specify a callback which jesse will use to parse json.

          In examples below and in jesse test suite jiffy parser is used.

* Use jesse's internal in-memory storage:

(parse json in advance)

```erlang
1> Schema = jiffy:decode(<<"{\"items\": {\"type\": \"integer\"}}">>).
{[{<<"items">>,{[{<<"type">>,<<"integer">>}]}}]}
2> jesse:add_schema(some_key, Schema).
ok
3> Json1 = jiffy:decode(<<"[1, 2, 3]">>).
[1,2,3]
4> jesse:validate(some_key, Json1).
{ok,[1,2,3]}
5> Json2 = jiffy:decode(<<"[1, \"x\"]">>).
[1,<<"x">>]
6> jesse:validate(some_key, Json2).
{error,[{data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,<<"x">>,
                      [1]}]}
```

The `[1]` in the error is the path in the original value to `<<"x">>` where the
validation failed. See *Validation errors* below for the full error format.

(using a callback)

```erlang
1> jesse:add_schema(some_key,
1>                  <<"{\"uniqueItems\": true}">>,
1>                  [{parser_fun, fun jiffy:decode/1}]).
ok
2> jesse:validate(some_key,
2>                <<"[1, 2]">>,
2>                [{parser_fun, fun jiffy:decode/1}]).
{ok,[1, 2]}
3> jesse:validate(some_key,
3>                <<"[{\"foo\": \"bar\"}, {\"foo\": \"bar\"}] ">>,
3>                [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"uniqueItems">>,true}]},
                      {not_unique,{[{<<"foo">>,<<"bar">>}]}},
                      [{[{<<"foo">>,<<"bar">>}]},{[{<<"foo">>,<<"bar">>}]}],
                      []}]}
```

* Call jesse with schema definition in place (do not use internal storage)

(parse json in advance)

```erlang
1> Schema = jiffy:decode(<<"{\"pattern\": \"^a*$\"}">>).
{[{<<"pattern">>,<<"^a*$">>}]}
2> Json1 = jiffy:decode(<<"\"aaa\"">>).
<<"aaa">>
3> jesse:validate_with_schema(Schema, Json1).
{ok,<<"aaa">>}
4> Json2 = jiffy:decode(<<"\"abc\"">>).
<<"abc">>
5> jesse:validate_with_schema(Schema, Json2).
{error,[{data_invalid,{[{<<"pattern">>,<<"^a*$">>}]},
                      no_match,
                      <<"abc">>,[]}]}
```

(using a callback)

```erlang
1> Schema = <<"{\"patternProperties\": {\"f.*o\": {\"type\": \"integer\"}}}">>.
<<"{\"patternProperties\": {\"f.*o\": {\"type\": \"integer\"}}}">>
2> jesse:validate_with_schema(Schema,
2>                            <<"{\"foo\": 1, \"foooooo\" : 2}">>,
2>                            [{parser_fun, fun jiffy:decode/1}]).
{ok,{[{<<"foo">>,1},{<<"foooooo">>,2}]}}
3> jesse:validate_with_schema(Schema,
3>                            <<"{\"foo\": \"bar\", \"fooooo\": 2}">>,
3>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,<<"bar">>,
                      [<<"foo">>]}]}
```

* Since 0.4.0 it's possible to say jesse to collect errors, and not stop
  immediately when it finds an error in the given json:

```erlang
1> Schema = <<"{\"properties\": {\"a\": {\"type\": \"integer\"}, \"b\": {\"type\": \"string\"}, \"c\": {\"type\": \"boolean\"}}}">>.
<<"{\"properties\": {\"a\": {\"type\": \"integer\"}, \"b\": {\"type\": \"string\"}, \"c\": {\"type\": \"boolean\"}}}">>
2> jesse:validate_with_schema(Schema,
2>                            <<"{\"a\": 1, \"b\": \"b\", \"c\": true}">>,
2>                            [{parser_fun, fun jiffy:decode/1}]).
{ok,{[{<<"a">>,1},{<<"b">>,<<"b">>},{<<"c">>,true}]}}
```

now let's change the value of the field "b" to an integer

```erlang
3> jesse:validate_with_schema(Schema,
3>                            <<"{\"a\": 1, \"b\": 2, \"c\": true}">>,
3>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]}]}
```

works as expected, but let's change the value of the field "c" as well

```erlang
4> jesse:validate_with_schema(Schema,
4>                            <<"{\"a\": 1, \"b\": 2, \"c\": 3}">>,
4>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]}]}
```

still works as expected, jesse stops validating as soon as finds an error.
let's use the 'allowed_errors' option, and set it to 1

```erlang
5> jesse:validate_with_schema(Schema,
5>                            <<"{\"a\": 1, \"b\": 2, \"c\": 3}">>,
5>                            [{parser_fun, fun jiffy:decode/1},
5>                             {allowed_errors, 1}]).
{error,[{data_invalid,{[{<<"type">>,<<"boolean">>}]},
                      wrong_type,3,
                      [<<"c">>]},
        {data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]}]}
```

now we got a list of two errors. let's now change the value of the field "a"
to a boolean

```erlang
6> jesse:validate_with_schema(Schema,
6>                            <<"{\"a\": true, \"b\": 2, \"c\": 3}">>,
6>                            [{parser_fun, fun jiffy:decode/1},
6>                             {allowed_errors, 1}]).
{error,[{data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]},
        {data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,true,
                      [<<"a">>]}]}
```

we stil got only two errors. let's try using 'infinity' as the argument
for the 'allowed_errors' option

```erlang
7> jesse:validate_with_schema(Schema,
7>                            <<"{\"a\": true, \"b\": 2, \"c\": 3}">>,
7>                            [{parser_fun, fun jiffy:decode/1},
7>                             {allowed_errors, infinity}]).
{error,[{data_invalid,{[{<<"type">>,<<"boolean">>}]},
                      wrong_type,3,
                      [<<"c">>]},
        {data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]},
        {data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,true,
                      [<<"a">>]}]}
```

Json Schema versions
--------------------

Currently there're two drafts of Json Schema: draft3 and draft4. Currently jesse
supports only draft3, but the architecture allows to extend jesse to support
any schema formats. To decide which validator to use jesse tries to read $schema
property from the given schema, and checks if it's a supported one, otherwise it
will return an error. If $schema property isn't provided in the given schema,
jesse will use the default validator (currently the validator for draft3).

To specify which validator to use by default (if there's no $schema property in
the given schema), one should use 'default_schema_ver' option when call
`jesse:validate/3` or `jesse:validate_with_schema/3`, the value should be
a binary consisting a schema path,
 i.e. <<"http://json-schema.org/draft-03/schema#">>.

Validation errors
-----------------

The validation functions `jesse:validate/2` and `jesse:validate_with_schema/2,3`
return `{ok, Value}` on success and `{error, ListOfErrors}` on failure. An error
is either `data_invalid` or `schema_invalid`.

A `data_invalid` error is a tuple on the form `{data_invalid, Schema, ErrorType,
Value, Path}` where
* Schema is the part of the schema where validation failed
* ErrorType is the type of error, usually an atom such as `wrong_type`,
  `not_in_range` or `no_match`
* Value is The part of the value where failed validation agains Schema
* Path is a path to where validation failed within the original value. The path
  is a list of property names and zero-based array indices referencing the
  properties and array items within a JSON document; e.g. in the JSON document
  `{"foo": [42, 43, 44]}`, the path `[<<"foo">>, 0]` refers to the value 42. An
  empty list refers to the whole JSON document.

A `schema_invalid` error is a tuple on the form `{schema_invalid, Schema,
ErrorType}` where
* Schema is the part of the schema which is invalid
* ErrorType is an atom such as `missing_id_field` or a tuple such as
  `{wrong_type_dependency, Dependency}`.

Caveats
-------
* pattern and patternProperty attributes:

  jesse uses standard erlang module `re` for regexp matching, therefore there could be
  some incompatible regular expressions in schemas you define.

  From erlang docs: "re's matching algorithms are currently based on the PCRE library,
  but not all of the PCRE library is interfaced"

  But most of common cases should work fine.

Contributing
------------

If you see something missing or incorrect, a pull request is most welcome!
