COMPILER_TESTS = compiler_tests
RUST_COMPILER_PROJECT = rust_compiler

RUST_COMPILER = ${RUST_COMPILER_PROJECT}/target/debug/${RUST_COMPILER_PROJECT}
COMPILER = ${RUST_COMPILER}

COMPILER_TESTS_SOURCES = ${wildcard ${COMPILER_TESTS}/*.viv}
COMPILER_TESTS_BINARIES = ${COMPILER_TESTS_SOURCES:${COMPILER_TESTS}/%.viv=bin/%}

build: ${RUST_COMPILER}
test: test_compiler

clean:
	cd ${RUST_COMPILER_PROJECT}; cargo clean
	rm -rf bin/

${RUST_COMPILER}: ${RUST_COMPILER_PROJECT}/src/* 
	cd ${RUST_COMPILER_PROJECT}; cargo build

bin/%: ${COMPILER_TESTS}/%.viv ${COMPILER}
	mkdir -p bin
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
