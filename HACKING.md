#  Hacking
The follow may help those not too familiar with working with Emacs lisp.

## Debugging

The following will show other useful debug settings::
  apropos RET debug-on

The main setting that helps is:

.. code-block:: cl
 
   (set debug-on-error t)

Although emacs will always prompt you to run with --debug-init
on encountering an error.

## Writing emacs lisp
Please ensure that you've set

.. code-block:: cl

  (auto-insert-mode  t)

Ensure ``flycheck`` mode is on before editing, and
correct any/all errors.

If possible, fix warnings too (less noise is better).

Watch out for usage of ```setq``` and ```setq-default```, with respect
to buffer-local variables.

Use hooks judiciously, especially those involved with saving.

## Releases
Please use semantic versioning with ```git tag -a```.






