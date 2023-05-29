.PHONY: all clean clean-all preview onchange bench

PAPER = main
INPUTS = $(PAPER).lhs ring.tex $(PAPER).bib
OUTPUTS = $(PAPER).pdf

TEXSRC = $(patsubst %.lhs, %.tex, $(INPUTS))

all: $(OUTPUTS)

bench: $(PAPER).bench.csv
prof:  $(PAPER).prof.elf.eventlog

$(PAPER).pdf: $(PAPER).tex ring.tex $(PAPER).bib
	latexmk -pdf $<

%.tex: %.lhs
	ghc -fno-code $^ # just typecheck
	lhs2TeX $^ > $@

%.bench.csv: %.bench.elf
	# turn off CPU scaling
	./benchprep.sh
	./$< --csv $@ --output $*.bench.html --verbosity 2

%.prof.elf.eventlog: %.prof.elf
	./$< --output $@.html -m glob '*/*2048' --verbosity 2 +RTS -ls


# compile threaded, optimized, but w/o let floating
build = ghc -O -fno-full-laziness -threaded -rtsopts -with-rtsopts=-N4

%.bench.elf: %.noprint.lhs
	$(build) $< -o $@

%.prof.elf: %.noprint.lhs
	$(build) -eventlog $< -o $@


%.noprint.lhs: %.lhs noprint.py
	python noprint.py < $< > $@
	# verify no printlines remain
	if grep --color=always -i '\<\(putstr\|putstrln\|print\)\>' $@; then false; fi

clean: $(TEXSRC)
	latexmk -c

clean-all: $(TEXSRC)
	latexmk -C
	rm -fv main.{bbl,ptb,tex}
	rm -fv main.{,noprint.}{hi,o}
	rm -fv main{,.elf}

preview: $(TEXSRC)
	latexmk -pvc

onchange:
	git ls-files | entr -c make
