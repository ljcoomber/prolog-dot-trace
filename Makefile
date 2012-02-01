.PHONY = examples

examples: examples/factorial.png examples/hanoi.png

examples/%.png: FORCE
	swipl -q -t "generate." -s examples/$(*F).pl
	dot -Tpng examples/$(*F).dot > $@

FORCE: