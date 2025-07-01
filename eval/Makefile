all: generated/coverage.tex generated/specs.tex generated/stats.tex generated/timings.pdf generated/timeouts.tsv generated/slow.tsv generated/terms.tsv

generated/%.tex generated/%.tsv: process.py overrides.args data/instructions.cloc
	python3 $< @overrides.args $* > $@

generated/timings.pdf: process.py overrides.args
	python3 $< @overrides.args timings $@

%.cloc: %.rs
	cloc --by-file --json $< > $@

clean:
	$(RM) generated/*
