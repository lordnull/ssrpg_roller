# Variables
REPO ?= ssrpg_roller
PRIVDIR ?= priv
RELDIR ?= $(REPO)
DEVRELDIR ?= dev
REBAR ?= rebar


# The default target
all: script


# Targets that don't correspond to the name of a file
.PHONY: all help compile script deps update-deps doc
.PHONY: clean-deps clean-rel clean-test clean distclean


.DEFAULT:
	$(error "Unrecognized target '$@'! Try 'make help'.")


# Help!
help:
	@echo "Targets:"
	@echo "    all            comple and escriptize"
	@echo "    help           display this help message"
	@echo
	@echo "    deps           fetch all dependencies"
	@echo "    compile        compile the project"
	@echo "    script         create escript executable"
	@echo "    test           run unit tests"
	@echo "    doc            generate code documentation"
	@echo
	@echo "    clean          clean up after 'compile' and 'test'"
	@echo "    clean-deps     clean up after 'deps'"
	@echo "    clean-test     clean up after 'test'"
	@echo "    distclean      clean up everything possible"


# Building
deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

compile: deps
	$(REBAR) compile

script: compile
	$(REBAR) escriptize

doc:
	$(REBAR) doc skip_deps=true


# Testing
eunit test: clean-test compile
	$(REBAR) eunit skip_deps=true


# Cleanup
clean-deps: clean
	$(REBAR) delete-deps
	-rm -rf deps

TEST_LOG_FILE := eunit.log
clean-test:
	-rm -f $(TEST_LOG_FILE)

clean: clean-test
	$(REBAR) clean

distclean: clean clean-deps clean-devcert clean-rel clean-devrel
