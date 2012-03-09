.PHONY: clean, examples, exec-tests, test

PROLOG=swipl

test: tmp/p_simple.png tmp/p_backtrack.png

clean:
	rm -r tmp/*

examples: examples/factorial.png examples/hanoi.png

examples/%.png:
	$(PROLOG) -q -t "generate." -s examples/$(*F).pl
	dot -Tpng examples/$(*F).dot > $@

exec-tests:
	$(PROLOG) -g "run_tests,halt." -s dot_trace.plt

tmp/%.png: exec-tests
	dot -Tpng tmp/$(*F).dot > $@
