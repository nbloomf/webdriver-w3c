docs:
	pandoc app/Main.lhs -f markdown+lhs -t markdown -o doc/Tutorial.md
