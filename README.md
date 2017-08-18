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
Any reporting of issues, and fixes are most welcome.
If you'd like to contribute new features,
please check with the other users of this package.

Please see the [hacking] document for note on  development.

If you think of a feature you'd like to add, or have found a bug,
please [raise an issue].

## Documentation
 
Always run:

  ``M-x chekdoc`

on any `.el` files before commiting them.


[Cask]: https://github.com/cask/cask
[Clojure]: https://www.clojure.org
[Contribution guidelines]: blobs/master/CONTRIBUTING.rst
[Emacs]: https://www.gnu.org/software/emacs/
[Python]: https://www.python.org
[docs]: docs
[emacs-lisp]: https://en.wikipedia.org/wiki/Emacs_Lisp
[emacs.d]: https://github.com/mgrbyte/emacs.d
[raise an issue]: https://github.com/mgrbyte/emacs.d/issues/new
[hacking guide]: HACKING.md
[jedi]: https://github.com/tkf/emacs-jedi
[lisp]: https://en.wikipedia.org/wiki/Lisp_%28programming_language%29
[pallet-mode]: https://github.com/rdallasgray/pallet
[python-mode]: https://github.com/fgallina/python.el
[use-package]: https://github.com/jwiegley/use-package
