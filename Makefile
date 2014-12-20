MODE_NAME=org2jekyll
VERSION=$$(grep "^;; Version: " $(MODE_NAME).el | cut -f3 -d' ')
PACKAGE_FOLDER=$(MODE_NAME)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
EMACS=emacs

pr:
	hub pull-request -b ardumont:master

.PHONY: clean

deps:
	cask

build:
	cask build


clean-dist:
	rm -rf dist/


clean: clean-dist
	rm -rf *.tar
	cask clean-elc

install:
	cask install

test: clean
	cask exec $(EMACS) --batch \
			-l ert \
			-l ./org2jekyll.el \
			-l ./org2jekyll-tests.el \
			-f ert-run-tests-batch-and-exit

pkg-file:
	cask pkg-file

pkg-el: pkg-file
	cask package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	cask info

release:
	./release.sh $(VERSION)

emacs-install-clean: package
	~/bin/emacs/emacs-install-clean.sh ./$(ARCHIVE)
