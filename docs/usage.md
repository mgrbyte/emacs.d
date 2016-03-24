# Usage

## mgrbyte-mode
I use a custom minor-mode which defines a set of
``emacs`` functions and key bindings that we've found useful over
time.

By default, this minor mode is activated globally (as indicated in
your mode line).

You can turn this off by adding the following in file pointed to by
the ``custom-file`` variable:

```lisp
   (mgrbyte-mode 0)
```

As with all other modes, Key-bindings and functions provided by the
mode are documented in the ``info`` window when you invoke ``C-h m``
or ``M-x describe-mode`` and navigate to ``Mgrbyte``.
 
## python-mode
This package use the built-in [python] mode provided in emacs24.

It has been configure to add hooks which load the [pungi] package.
Additionally, the ``mgrbyte-mode`` function `py-insert-debug` is
enabled, which inserts the ``pdb.set_trace`` command on the current
line indicated by position of the cursor.

## Custom settings and functions

The default ``custom-file`` is:

   ~/.emacs-customize.el

If you use the ``customize`` interface in [Emacs], then any saved
settings generated will also be appended to this file.

Your own settings and functions should be added to
``~/.emacs-custom.el``, this file will be created on your behalf (if
it doesn't already exist) upon installation. Move any personal
preferences, settings, and/or utility functions you've previously used
into this file.

If you require variables to differ depending on the project you're
working on, consider using [directory local variables].

## Package management
New packages can be added to Emacs by using the package manager:

 ``M-x list-packages``

The [pallet] package automatically takes care of keeping the
[Cask dependencies file] up-to-date with packages you may install or
delete with ``list-packages``.

## Coding modes
All linting (wavey-red-lines) is handled by the flycheck package, and
related language specific plugins for checking syntax and correctness
of code.

By default, this package uses the following modes per language:

  Python: [python], [pungi], [jedi], [pyvenv], [sphinx-doc]

  Clojure: [clojure-mode], [cider], [flycheck-pos-tip], [kibit-helper]


See the package documentation for each of the above for a synopsis on
the all the key-bindings and utilities available. 

This is most easily accessed with ```C-h m``` or ```M-x describe-mode```.

[Emacs]: http://www.gnu.org/software/emacs
[buildout]: http://www.buildout.org/en/latest/
[cider]: https://github.com/clojure-emacs/ac-cider
[clojure-mode]: http://github.com/clojure-emacs/clojure-mode
[directory local variables]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
[flycheck-pos-tip]: https://github.com/flycheck/flycheck-pos-tip
[flycheck]: http://flycheck.readthedocs.org/en/latest/
[jedi]: http://jedi.jedidjah.ch/en/latest/
[kibit-helper]: http://www.github.com/brunchboy/kibit-helper
[pallet]: https://github.com/rdallasgray/pallet
[pungi]: https://github.com/mgrbyte/pungi.git
[python]: https://github.com/fgallina/python.el
[pyvenv]: https://github.com/jorgenschaefer/pyvenv
[sphinx-doc]: https://github.com/naiquevin/sphinx-doc.el




