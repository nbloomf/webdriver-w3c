docs:
	pandoc app/Main.lhs -f markdown+lhs -t markdown -o doc/Tutorial.md

test:
	./dev/run-tests.sh

.PHONY: test
