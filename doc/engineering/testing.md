# Unit- and Property-based testing in Kazoo

## Unit Testing

Kazoo uses EUnit for the majority of unit testing. Running `make eunit` from the root or within an Erlang application directory will run the tests of the project or app respectively. Modules will be cover-compiled as well, generating a cover report that can be viewed in a browser.

!!! note
    If you are using Emacs (and you should be) you can serve the cover directory by making sure you have the `simple-httpd` package installed, then run `httpd-start` followed by `httpd-serve-directory` to chose the cover directory of choice.

## Property Testing

Kazoo uses PropEr for doing property-based testing.
