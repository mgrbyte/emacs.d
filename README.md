# mgrbyte's Emacs Development Environment

A collection of [emacs-lisp] code tailored for [Python], [Clojure] and [lisp] development.

This is my personal development environment, now managed via [nix].

One of the primary motivations for this package is to provide a consistent, useful development environment.

[use-package] is used to configure the default set of packages that will be installed.

This [Emacs] setup assumes Emacs 24 or higher.

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

  `M-x chekdoc`

on any `.el` files before commiting them.
[nix]: https://github.com/mgrbyte/nix-config.git
[Clojure]: https://www.clojure.org
[Emacs]: https://www.gnu.org/software/emacs/
[Python]: https://www.python.org
[docs]: docs
[emacs-lisp]: https://en.wikipedia.org/wiki/Emacs_Lisp
[emacs.d]: https://github.com/mgrbyte/emacs.d.git
[raise an issue]: https://github.com/mgrbyte/emacs.d/issues/new
[hacking guide]: HACKING.md
[lisp]: https://en.wikipedia.org/wiki/Lisp_%28programming_language%29
[python-mode]: https://github.com/fgallina/python.el
[use-package]: https://github.com/jwiegley/use-package
