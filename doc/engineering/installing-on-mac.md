# Installing on Mac (for development)

Make sure you have run `xcode-select --install` to install various build tools that are available(such as zlib.

Some additional dependencies are required to get things up and running on Mac. The easiest way to
install these dependencies is via `brew` so [install brew](https://brew.sh/) if you have not already.

Run the brew install command below to install dependencies:

```brew install python@2 libxslt openssl ncurses expat imagemagick ghostscript htmldoc```

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
* md5sha1sum
* make

You can install all of the above with:
```brew install gnu-sed md5sha1sum make```

Next install libreoffice, which can be done via `brew cask install libreoffice`.

## Tooling Dependencies

Several build commands require the gnu version of utilities. For simplicity you can install
the `coreutils` bundle from brew to get the gnu version of utility applications:
```brew install coreutils```
Again, make sure to follow the instructions to add them to your path with their default names.

The version of emacs that comes with MacOS is very out of date, and you will need it for running
the auto formatting command. Install a newer version via brew:
```brew cask install emacs```

To build the docs you will need to upgrade bash to version 5. You can do this through:
```brew install bash``` and then removing (Perhaps just rename it) your existing bash 
from `/usr/local/bin/bash` and symlinking the brew bash into its place:
```ln -s /usr/local/Cellar/bash/5.0.7/bin/bash /usr/local/bin/bash```
You may also want to do this for `/bin/bash` for consistency but it is not required.

## Fix executable paths

gnu-sed, md5 etc need their executables added to your path. When you install 
them via brew they give you the instructions on what to add.

You will also want to add a symlink for libreoffice somewhere in your path because the default 
executable name is `soffice` but kazoo expects it to be `libreoffice`. Something like:

```ln -s /usr/local/bin/soffice /usr/local/bin/libreoffice```
