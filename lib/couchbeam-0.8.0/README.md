#couchbeam 0.7.1

**2009-2011 (c) Benoît Chesneau <benoitc@e-engura.org>**

**couchbeam** is a simple erlang CouchDB framework. couchbeam provides you a full featured and easy client to access and manage multiple couchdb Nodes.

Couchbeam is under MIT. see NOTICE file for more details.

Full documentation of the project is on this [url](http://benoitc.github.com/couchbeam).



##requirements

* Erlang/OTP R13/R14 or newer (compiler to build, kernel,stdlib,ssl,crypto to run)
* GNU Make (might actually build with some other make as well)

##installation

To build the application simply run 'make'. This should build .beam, .app
files in ebin/ folder.

To run tests run 'make test'.
To generate doc, run 'make doc'.
