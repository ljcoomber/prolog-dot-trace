.PHONY: clean, examples, exec-tests, test

PROLOG=swipl

test: tmp/p_simple.png tmp/p_backtrack.png tmp/p_cut.png

clean:
	rm -rf examples/*dot
	rm -rf examples/*png
	rm -rf tmp/*

examples: examples/factorial.png examples/hanoi.png

examples/%.png:
	$(PROLOG) -q -t "generate." -s examples/$(*F).pl
	dot -Tpng examples/$(*F).dot > $@

exec-tests: tmp
	$(PROLOG) -g "run_tests,halt." -s dot_trace.plt

tmp:
	mkdir tmp

tmp/%.png: exec-tests tmp
	dot -Tpng tmp/$(*F).dot > $@
