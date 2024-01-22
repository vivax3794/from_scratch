COMPILER_TESTS = compiler_tests
RUST_COMPILER_PROJECT = rust_compiler

RUST_COMPILER = ${RUST_COMPILER_PROJECT}/target/debug/${RUST_COMPILER_PROJECT}
COMPILER = ${RUST_COMPILER}

COMPILER_TESTS_SOURCES = ${wildcard ${COMPILER_TESTS}/*.viv}
COMPILER_TESTS_BINARIES = ${COMPILER_TESTS_SOURCES:${COMPILER_TESTS}/%.viv=bin/%}

build: ${RUST_COMPILER}
test: test_compiler

clean:
	rm -rf ${RUST_COMPILER_PROJECT}/target
	rm -rf bin/

${RUST_COMPILER}: FORCE
	cd ${RUST_COMPILER_PROJECT}; mold -run cargo build

bin:
	mkdir bin

bin/%: ${COMPILER_TESTS}/%.viv ${COMPILER} bin
	${COMPILER} build $< $@

test_compiler: ${COMPILER_TESTS_BINARIES}
	for binary in $^; do \
		echo $$binary; \
		./$$binary; \
		if [ $$? -ne 0 ]; then \
			echo "Error running $$binary"; \
			exit 1; \
		fi; \
	done

FORCE:;
