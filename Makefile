.PHONY: all clean compile grammars lint test

all: clean compile lint test

clean:
	eldev clean all

compile:
	eldev compile --warnings-as-errors

# Install (or reinstall) the tree-sitter grammars the tests need.
grammars:
	eldev exec "(asciidoc-install-grammars t)"

lint:
	eldev lint

test:
	eldev test
