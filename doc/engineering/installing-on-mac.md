# Installing on Mac (for development)

## Prerequisite
These steps are required mostly to build Erlang using Kerl and later build Kazoo. Kerl is recommended although brew can manage multiple Erlang versions.

If macOS version is older than 10.14, Mojave, please make sure you have run `xcode-select --install` to install various build tools that are available(such as zlib).

Some additional dependencies are required to get things up and running on Mac. The easiest way to
install these dependencies is via `brew` so [install brew](https://brew.sh/) if you have not already.

Make sure these modules are already installed by brew:

```
python@3 fop libxslt openssl ncurses expat imagemagick ghostscript htmldoc icu4c
```

`$ brew list` should provide a list of modules already installed.

Brew install any missing modules. This is an example to install the complete list.

```
brew install python@3 fop libxslt openssl ncurses expat imagemagick ghostscript htmldoc icu4c
```

Since macOS 10.14 Mojave does not come with a JDK, or for any needs to upgrade the current Java version to JDK 11 or 12. Openjdk is recommended due to its GNU GPLv2 license. Instructions can be found at [here](https://stackoverflow.com/questions/52524112/how-do-i-install-java-on-mac-osx-allowing-version-switching)

And you can skip installing these (they are included via the Apple command line tools or the brew packages):

* build-essential
* zlib1g-dev
* git-core
* libexpat1-dev

## Additional Dependencies

Some applications are very old or configured differently in Mac OS, so use brew
to install these to bring them up to date:
Note: Make sure to follow the directions on symlinking executables or adding executables
to your path(brew will tell you about it after install completes)

* gnu-sed
* coreutils
* make

You can install all of the above with:

```
brew install gnu-sed coreutils make
```

Next install libreoffice, which can be done via `brew cask install libreoffice`.

## Tooling Dependencies

Several build commands require the gnu version of utilities. For simplicity you can install
the `coreutils` bundle from brew to get the gnu version of utility applications:

```
brew install coreutils
```

Again, make sure to follow the instructions to add them to your path with their default names.

The version of Emacs that comes with MacOS is very out of date, and you will need it for running
the auto formatting command. Install a newer version via brew:

```
brew cask install emacs
```

To build the docs you will need to upgrade bash to version 5. You can do this through:

```
brew install bash
```

And then removing (Perhaps just rename it) your existing bash from `/usr/local/bin/bash` and symlinking the brew bash into its place:

```
ln -s /usr/local/Cellar/bash/5.0.7/bin/bash /usr/local/bin/bash
```

You may also want to do this for `/bin/bash` for consistency but it is not required.

## Fix executable paths

`gnu-sed`, `coreutils`, etc need their executables added to your path. When you install
them via brew they give you the instructions on what to add.

You will also want to add a symlink for libreoffice somewhere in your path because the default
executable name is `soffice` but kazoo expects it to be `libreoffice`. Something like:

```
ln -s /usr/local/bin/soffice /usr/local/bin/libreoffice
```

## Kerl
We recommend Kerl to manage Erlang versions. Kerl is an Erlang version management tool as rvm for Ruby, nvm for node.js. Please familiarize [these steps](https://github.com/kerl/kerl)

Please make sure `~/.kerlrc` contains this, because macOS has not been keeping up the latest openssl version. We would like to refer to the brew installed version.

```
KERL_CONFIGURE_OPTIONS="--with-ssl=/usr/local/opt/openssl"
```

## Brew installed Erlang
Brew directly installs Erlang version. It's also tested but offers less version options as supposed to Kerl.

You have to use Brew link and unlink to switch between version.

## Build Kazoo
The generic build [instruction](https://github.com/2600hz/kazoo/blob/master/doc/installation.md#longer-version) works on macOS.
