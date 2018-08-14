# Continuous Integration

Kazoo makes use of [TravisCI](https://travis-ci.org/2600hz/kazoo/) and [CircleCI](https://circleci.com/gh/2600hz/kazoo/).

## TravisCI

Travis is tasked with building Kazoo against various versions of Erlang and running the test suites (core/ and applications/).

## CircleCI

Circle handles doing checks against the code base, documentation, Dialyzer, and building and testing a release using the currently supported Erlang version.
