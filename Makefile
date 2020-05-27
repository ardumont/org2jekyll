PACKAGE = org2jekyll
VERSION = $$(grep "^;; Version: " $(PACKAGE).el | cut -f3 -d' ')
ARCHIVE = $(PACKAGE)-$(VERSION).tar
EMACS ?= emacs
CASK ?= cask
BLOG ?= testing-blog

pr:
	hub pull-request -b ardumont:master

.PHONY: clean

deps:
	${CASK}

build:
	${CASK} build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	${CASK} clean-elc

install:
	${CASK} install

test: clean
	${CASK} exec ert-runner

pkg-el:
	${CASK} package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	${CASK} info

release:
	./release.sh $(VERSION) $(PACKAGE)

version:
	@echo -e "application $(PACKAGE): $(VERSION)\npackage: $(ARCHIVE)"

update:
	cd $(BLOG) ; bundle update; bundle lock; bundix

run-dev:
	cd $(BLOG); bundle exec jekyll serve --watch --trace

clean-dev:
	cd $(BLOG); bundle exec jekyll clean

run-emacs:
	cd testing-blog && $(EMACS) -Q --load=../org2jekyll.el --load=./testing-blog-config.el
