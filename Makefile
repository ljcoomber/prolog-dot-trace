.PHONY: examples, test

examples: examples/factorial.png examples/hanoi.png

examples/%.png:
	swipl -q -t "generate." -s examples/$(*F).pl
	dot -Tpng examples/$(*F).dot > $@

test:
	swipl -g "run_tests,halt." -s dot_trace.plt