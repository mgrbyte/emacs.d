SHELL := /bin/sh
EMACS ?= $(shell which emacs)

all:
	@cask install -v
	${EMACS} \
		--batch -nw -Q \
		--load package \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
