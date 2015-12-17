# Starting NkSIP

There are three main ways to start NkSIP:
* Embedded in your own Erlang application
* As a stand alone application
* Starting it in a development environment

### Embedding NkSIP

If you want to embed NkSIP in your own Erlang application, and you are using _rebar_, all you have to do is include it in the list of dependencies in your `rebar.config` file:

```erlang
{deps, [
  {nksip, ".*", {git, "git://github.com/kalta/nksip", {tag, "v0.4.0"}}}
]}.
```
 
Then you will have to setup in your erlang environment any [configuration parameter](../reference/configuration.md) you want to change from NkSIP's defaults (usually in your `app.config` file), for `nksip` but also for `lagger` Erlang applications. You can then start NkSIP starting all dependencies (see [nksip.sipapp.src](../../src/nksip.app.src)) and finally start `nksip` Erlang application.


### Start NkSIP as a stand-alone application

NkSIP does not include any standard Erlang release building system. In the near future, NkSIP will be refactored as a module under the upcoming _NkCore_ project, so it makes no sense including one.

If you want to start it stand alone now, you can use your own method based on the _development environment_ method.


### Start NkSIP in a development environment

You can start NkSIP stand alone in the following way. First you will need to download and compile it:
```
> git clone https://github.com/kalta/nksip
> cd nksip
> git checkout v0.4.0 -q
> make
```

Of course, select the _tag_ version you want (do not type the `git checkout` line to use `master`, which should be the latest development version).

Then you can start a Erlang shell that automatically starts NkSIP and its dependencies:
```
> make shell
```

NkSIP will use the environment from file `app.config` and the Erlang virtual machine parameters from file `vm.args`, both on the `priv` directory. Update any [configuration parameter](../reference/configuration.md) you want to change from NkSIP's defaults in the `app.config` file before starting.

