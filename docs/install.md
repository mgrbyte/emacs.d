# Install
Ensure that ``emacs``, [cask][1] are both
available as an executable on your shell's $PATH.

### Cask
If you've not used ``cask`` before, the key is to ensure that you have
a ``.cask`` directory in your $HOME directory, and that your shell
profile adds the ``cask`` binary to $PATH.

Use of this package assumes you know how to configure emacs and cask
appropriately for your platform.


### Migration from an existing configuration
Backing up your existing configuration:

```bash
   test -d ~/.emacs.d && mv ~/emacs.d{,.bak}
   test -f ~/.emacs && mv ~/.emacs{,.bak}
```

## Prerequisites
This package uses ``init.el`` for ``emacs`` initialisation (as opposed
to .emacs which is more commonly used.

flycheck:

    Used for syntax checking in most modes.

## Installation
If you want to run a stable version, please checkout a release tag

See https://github.com/mgrbyte/emacs.d/releases

For the commands below we'll use the ``master`` branch.

```bash

  git clone https://github.com/mgrbyte/emacs.d ~/.emacs.d
  cask install
```

## Updating packages
When installing new packages with the emacs command ``list-packages``,
this will will update the ``cask`` configuration file
``.emacs.d/Cask``.

The emacs package ``pallet`` does this seamlessly
in the background.  Should you want to synchronise the packages
configured by cask in a running emacs without restarting, you can just
invoke:

   ``M-x pallet-update``

It is suggested to fork this package and maintain it using git should
you want to use packages not provided by default.  Alternatively, if
you think given package is really useful, please send a pull request
and we'll consider adding it to the default configuration.


[1]: https://github.com/cask/cask

