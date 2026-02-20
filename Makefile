.PHONY: all clean compile lint test

all: clean compile lint test

clean:
	eldev clean all

compile:
	eldev compile --warnings-as-errors

lint:
	eldev lint

test:
	eldev test
