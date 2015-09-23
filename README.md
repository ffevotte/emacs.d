# My Emacs settings [![Build-Status](https://travis-ci.org/ffevotte/emacs.d.svg)](https://travis-ci.org/ffevotte/emacs.d) [![License](https://img.shields.io/badge/license-GPL_v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

This is an ever-changing set of Emacs settings, mostly targetting the following uses:
- development (in Python, C++, and obviously a bit of elisp);
- document authoring (mostly in LaTeX).

## Design principles

### Objectives

This configuration was designed with several objectives in mind:

- **Maintainability**: it relies on John Wiegley's excellent [`use-package`](https://github.com/jwiegley/use-package) system to group package-dependent settings in logical units. Third party packages are automatically installed via the `:ensure` directive.

- **Performance**: another benefit of `use-package` is speed. Most packages benefit from deferred loading, so that the initial startup time is maintained under acceptable limits even with a relatively large number of dependencies.

- **Fault-tolerance**: since Emacs itself will be used to debug its init-file, it should be as tolerant to failures as possible. Part of the fault tolerance is gained at no cost once again by `use-package` (since errors in `:config` blocks are gracefully demoted to warnings). As for errors in the remaining parts of the configuration, they are tackled by a custom `progn-safe` macro. A `make check` command allows testing and debugging the entire configuration at once by ensuring all packages are correctly loaded (debugging with deferred loading can be a somewhat difficult problem).

- **Self-containment and independance**: this setup is designed so that it can be put anywhere and be used from there, without interferring with the regular emacs configuration installed in `~/.emacs.d`. I use this so that the emacs23-based configuration imposed by the system on my work machine can safely co-exist with a locally-installed recent emacs version.

### Files hierarchy

Files are more or less grouped in the following way:

- Sources:
    - `init.el`: top-level file, containing mostly everything;
    - `elisp/setup-*.el`: if the configuration of a package takes more than a few lines, it is put in a
      dedicated file;
    - `elisp/host-*.el`: host-specific settings, loaded at the beginning of startup.

- Variable files: all files used for local persistency, which should not be versioned and shared among machines (recent file list, desktops, ...)
    - `var/*`: 

- Shared files: non-changing files which are needed for operation. Some are versioned in the repository, others are downloaded from public sources during initial setup.
    - `share/info`: specific info files. In particular, the entire Python documentation is included here.
    - `share/fonts`: I use the [Symbola](http://users.teilar.gr/~g1951d/) font for unicode symbols, and version it here since it seems to be (hopefully temporarily?) unavailable.

## Installation and usage

You can of course instal this in your `~/.emacs.d` directory, but you don't need to: this setup can be used from anywhere without interfering with another regular configuration.

```sh
$ git clone https://github.com/ffevotte/emacs.d /path/to/emacs.d
$ cd /path/to/emacs.d
$ make
```

If you installed everything to standard locations, you're done. Otherwise, you need to run emacs like this:

```sh
$ /path/to/emacs.d/bin/em
```

The correct environment to run the `em` command, as well as better integration between your shell and emacs (particularly for shells running within a `term` buffer in emacs), can be obtained by sourcing the provided bash configuration snippet from your `~/.bashrc` file:

```sh
source /path/to/emacs.d/bashrc
```
