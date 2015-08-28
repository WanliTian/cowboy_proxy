.PHONY: all compile get-deps clean

all: compile 

compile:
	./rebar compile
get-deps:
	./rebar get-deps
clean:
	./rebar clean
