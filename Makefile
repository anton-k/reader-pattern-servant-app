.PHONY: build test run

all: test-strictly

format:
	cabal-fmt -i --indent 2 reader-proto.cabal
	ormolu --mode inplace -ce $$(find . -name "*.hs" -not -path "./.git/*" -not -path "dist" -not -path "dist-newstyle" -not -path "./*.stack-work/*")

build-fast: format
	stack build --fast --test --no-run-tests --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -fno-warn-typed-holes -fdefer-typed-holes"

build-strictly: format
	stack build --test --no-run-tests --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -Werror=incomplete-patterns -Wincomplete-patterns -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints"

test-fast: format
	stack build --fast --test --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -fno-warn-typed-holes -fdefer-typed-holes"

test-strictly: format
	stack build --test --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -Werror=incomplete-patterns -Wincomplete-patterns -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints"

build: build-fast

test: test-fast

clean:
	stack clean

run: build-strictly
	stack run

# call:
#
# make message='{"message": "waiting for the summer", "tags": ["random"] }' post-save
debug-save:
	curl http://localhost:7070/api/v1/save  -d '$(message)' -v -H "Content-Type: application/json"

debug-save-1: message={"message": "waiting for the summer", "tags": ["random"] }
debug-save-1: debug-save

# call:
# > make id=0 get-by-id
debug-get-by-id:
	curl http://localhost:7070/api/v1/get/message/$(id) -v

# call:
# > make tag=info get-by-tag
debug-get-by-tag:
	curl http://localhost:7070/api/v1/get/tag/$(tag) -v

debug-toggle-log:
	curl http://localhost:7070/api/v1/toggle-logs -d '{}' -v -H "Content-Type: application/json"
