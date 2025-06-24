all: generated/coverage.tex generated/specs.tex generated/stats.tex generated/timings.pdf generated/timeouts.tsv generated/slow.tsv generated/terms.tsv

generated/%.tex generated/%.tsv: process.py data/instructions.cloc
	python3 $< $* > $@

generated/timings.pdf: process.py
	python3 $< timings $@

%.cloc: %.rs
	cloc --by-file --json $< > $@

clean:
	$(RM) generated/*
