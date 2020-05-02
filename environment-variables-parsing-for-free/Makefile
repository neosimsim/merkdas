# Watch make -s 'CABAL_BUILD_FLAGS=-v0'
.POSIX:

CABAL_BUILD_FLAGS=

all:
	$(MAKE) build
	$(MAKE) check

check: phony
	#cabal check # this package shall not be published
	cabal-fmt environment-variables-parsing-for-free.cabal | diff environment-variables-parsing-for-free.cabal -
	find . -type f | grep -Ev '^\./dist' | grep -E '\.(hs|lhs)$$' | xargs hlint
	# find could be replaced by git diff --cached --name-only
	for file in $$(find . -type f | grep -Ev '^\./dist' | grep -E '\.(hs|lhs)$$'); \
	do \
		 echo $$file; \
		 cat $$file | hindent | stylish-haskell | diff $$file -; \
	done

check-apply: phony
	cabal-fmt -i environment-variables-parsing-for-free.cabal
	find . -type f | grep -Ev '^\./dist' | grep -E '\.(hs|lhs)$$' | xargs hlint --refactor --refactor-options '-i'
	find . -type f | grep -Ev '^\./dist' | grep -E '\.(hs|lhs)$$' | xargs hindent
	find . -type f | grep -Ev '^\./dist' | grep -E '\.(hs|lhs)$$' | xargs stylish-haskell -i

build: phony
	cabal v2-build -f pedantic $(CABAL_BUILD_FLAGS)

test: phony
	cabal v2-run -f pedantic $(CABAL_BUILD_FLAGS) test -- $(HSPEC_ARGS)

clean: phony
	cabal v2-clean

phony: this_file_should_not_exists

this_file_should_not_exists:

