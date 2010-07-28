   Messaging that just works

                                    RabbitMQ

     * Home
     * Download
     * Documentation
     * Get Started
     * Services
     * Partners
     * Community
     * Cloud
     * FAQ
     * Search

   --------------------------------------------------------------------------

   This section describes the process for obtaining a copy of the RabbitMQ
   server source code, as well as instructions for building the server from
   source.

  Table of Contents

     * Obtaining the source
     * Required Libraries and Tools
     * Building the server

Obtaining the source

     * Either download a released source code distribution from the download
       page, or
     * Check the code out directly from our mercurial repositories:

 $ hg clone http://hg.rabbitmq.com/rabbitmq-codegen
 $ hg clone http://hg.rabbitmq.com/rabbitmq-server
 $ cd rabbitmq-server
 $ make

   If you choose to check the code out using mercurial, be aware that the
   code-generation module is a dependency of the server library. If you're
   working with a released source code distribution, though, the
   code-generation module is included.

Required Libraries and Tools

   In order to build RabbitMQ, you will need a few tools.

   The server requires a recent version of Python and simplejson.py (an
   implementation of a JSON reader and writer in Python), for generating AMQP
   framing code. simplejson.py is included as a standard json library in the
   Python core since 2.6 release.

   Additionally, for building the server, you will need

     * the Erlang development and runtime tools
       If you are on a Debian-based system then you need the following
       packages installed as a minimum: erlang-dev, erlang-base or
       erlang-base-hipe, erlang-ssl, erlang-os-mon, erlang-mnesia (on some
       older distributions the last three are bundled into erlang-nox). If
       you are building and installing Erlang from source then you must
       ensure that openssl is installed on your system.
     * a recent version of GNU make

Building the server

   Change to the rabbitmq-server directory, and type make.

   Other interesting Makefile targets include

   all
           The default target. Builds the server.

   run
           Builds the server and starts an instance with an interactive
           Erlang shell. This will by default create a Mnesia database in
           /tmp/rabbit-mnesia, but this location can be overridden by setting
           the Makefile variable MNESIA_DIR:

           make run MNESIA_DIR=/some/other/location/for/rabbit-mnesia

   clean
           Removes build products and wipes the Mnesia database directory
           used by the run target. See the above description of MNESIA_DIR.

   srcdist
           Runs the "clean" target, and constructs a source-code distribution
           tarball in ./dist.

   About us RabbitMQ™ is a Trademark of Rabbit Technologies Ltd.
