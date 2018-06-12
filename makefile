docs:
	pandoc app/Main.lhs -f markdown+lhs -t markdown -o doc/Tutorial.md
	pandoc app/TastyDemo.lhs -f markdown+lhs -t markdown -o doc/TastyDemo.md

test:
	./dev/run-tests.sh

demo:
	./dev/run-demo.sh

stress:
	./dev/run-parallel-stress-test.sh

.PHONY: docs test demo stress
