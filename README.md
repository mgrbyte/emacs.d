# Another Opinionated Emacs Development Environment

A collection of [emacs-lisp] code tailored for [Python], [Clojure] and
[lisp] web development.

One of the primary motivations for this package is to provide a consistent,
useful development environment.

It is also opinionated, providing mode hooks for a host of extensions.

Each of these can of course be overridden/configured as you prefer.

[use-package] is used to configure the default set of packages that will be installed.

See the [use-package] statements in ``.emacs.d/init.el``.

This [Emacs] setup assumes Emacs 24 or higher.

[Cask]  is used for package management and installation (via [pallet-mode]).

Please see the documentation in [docs] for installation, custom
configuration and more.

Read the [hacking guide] for a guide on developing [emacs.d].
   
## Contributing

### Git Hub
If you think of a feature you'd like to add, or have found a bug,
please raise an issue on [github].

[Cask]: https://github.com/cask/cask
[Emacs]: https://www.gnu.org/software/emacs/
[hacking guide]: HACKING.md
[Python]: https://www.python.org
[Contribution guidelines]: blobs/master/CONTRIBUTING.rst
[docs]: docs
[emacs-lisp]: https://en.wikipedia.org/wiki/Emacs_Lisp
[emacs.d]: https://github.com/mgrbyte/emacs.d
[github]: https://github.com
[lisp]: https://en.wikipedia.org/wiki/Lisp_%28programming_language%29
[python-mode]: https://github.com/fgallina/python.el
[use-package]: https://github.com/jwiegley/use-package
[pallet-mode]: https://github.com/rdallasgray/pallet
[Clojure]: https://www.clojure.org
