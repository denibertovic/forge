.PHONY: build repl test release help

.DEFAULT_GOAL = help

VERSION ?= $(shell grep "^version:" forge.cabal | cut -d " " -f9)

## Build project
build:
	@stack build

## Cut new release
release:
	@git tag ${VERSION} && git push --tags

## Run ghcid
ghcid:
	@ghcid \
		--command "stack ghci forge --ghci-options=-fno-code"

## Have ghcid run the test suite on successful recompile
ghcid-test:
	@ghcid \
		--command "stack ghci forge:lib forge:test:hspec --ghci-options=-fobject-code" \
		--test "main"

## Print current version
version:
	@echo ${VERSION}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-0-9_]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

