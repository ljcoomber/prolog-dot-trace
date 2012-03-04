.PHONY: examples, test

PROLOG=swipl

examples: examples/factorial.png examples/hanoi.png

examples/%.png:
	$(PROLOG) -q -t "generate." -s examples/$(*F).pl
	dot -Tpng examples/$(*F).dot > $@

test:
	$(PROLOG) -g "run_tests,halt." -s dot_trace.plt