Mission
=======

This is a project to revive and modernize the [gen_leader library](http://www.cs.chalmers.se/~hanssv/leader_election/doc/gen_leader.html).
Numerous versions of this project exist, developed by disparate groups with different aims. By collecting and integrating these we hope
to provide a new standard-library-quality module for the Erlang runtime that provides leader-election functionality without many of the
difficulties traditionally associated with such.

Which Version Should I Use?
===========================

Use the version in combined_version.

What Exactly Does It Do?
========================

Leader election behavior.
-------------------------

This application implements a leader election behavior modeled after
gen_server. This behavior intends to make it reasonably
straightforward to implement a fully distributed server with
master-slave semantics.

Current Participants
====================

+ Andrew Thompson (andrew@hijacked.us)
+ Dave Fayram (dfayram@gmail.com)
+ Hans Svensson (hanssv@chalmers.se)
+ Ulf Wiger (ulf@wiger.net)