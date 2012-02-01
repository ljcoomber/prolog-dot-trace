.PHONY = examples

examples: examples/factorial.png

examples/%.png: FORCE
	swipl -q -t "generate_$(*F)." -s examples.pl
	dot -Tpng examples/$(*F).dot > $@

FORCE: