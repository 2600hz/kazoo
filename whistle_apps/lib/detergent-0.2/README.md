Detergent - An emulsifying Erlang SOAP library
==============================================

Detergent helps make SOAP interactions in Erlang
uhh... cleaner (pun totally intended)!

## History

Most code in Detergent was originally extracted
from the SOAP implementation in Yaws, the idea
being to make this more readily available without
having to install Yaws itself.

There are obviously changes in Detergent that
make it different from the Yaws implementation,
and these will continue. Hopefully these changes
make Detergent easier to use.

## Dependencies

Detergent depends on the excellent Erlsom library
for all XML interactions. Erlsom takes away quite
a bit of the pain of working with XML.

## Building

Detergent uses rebar for building and wraps it i
a Makefile for convenience.

First clone Detergent from GitHub:

    $ git clone git://github.com/devinus/detergent.git

Then change into the newly created directory:

    $ cd detergent

And make:

    $ make

Rebar will first pull in Erlsom as a dependency
from GitHub, attempt to build it, then build
Detergent.

## Testing

To test your Detergent build, start an Erlang
shell and run the detergent:qtest/0 function.

The qtest/0 function attempts to call a web
service at webservicex.net to retrieve the
weather repeat for Boston.

    $ erl -pa deps/erlsom/ebin ebin
    > inets:start().
    > detergent:qtest().

License
=======

Most code is copyright (c) 2006 by Claes Wikstrom
and you can find his license in LICENSE.

Anything else is provided under public domain
which you can learn more about in UNLICENSE.
