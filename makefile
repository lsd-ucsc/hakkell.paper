.PHONY: all clean clean-all preview onchange bench

PAPER = main
INPUTS = $(PAPER).lhs ring.tex $(PAPER).bib
OUTPUTS = $(PAPER).pdf

TEXSRC = $(patsubst %.lhs, %.tex, $(INPUTS))

all: $(OUTPUTS)

bench: $(PAPER).bench.csv
prof:  $(PAPER).prof.elf.eventlog

$(PAPER).pdf: $(PAPER).tex ring.tex $(PAPER).bib
	latexmk -shell-escape -pdf $<

%.tex: %.lhs
	ghc -fno-code $^ # just typecheck
	lhs2TeX $^ > $@

%.bench.csv: %.bench.elf
	# turn off CPU scaling
	./benchprep.sh
	./$< --csv $@ --output $*.bench.html --verbosity 2

%.prof.elf.eventlog: %.prof.elf
	./$< --output $@.html -m glob '*/*2048' --verbosity 2 +RTS -ls


# compile for benchmark
CAPABILITIES ?= 4
build = ghc -O -threaded -fno-full-laziness -rtsopts -with-rtsopts=-N$(CAPABILITIES)

%.bench.elf: %.noprint.lhs
	$(build) $< -o $@

%.prof.elf: %.noprint.lhs
	$(build) -eventlog $< -o $@


%.noprint.lhs: %.lhs noprint.py
	python noprint.py < $< > $@
	# verify no printlines remain
	if grep --color=always -i '\<\(putstr\|putstrln\|print\)\>' $@; then false; fi

clean:
	latexmk -c
	rm -fv *.{o,hi}

clean-all: clean
	latexmk -C
	rm -fv main.{bbl,ptb,tex,xcp}
	rm -fv main.noprint.*
	rm -fv main.bench.*
	rm -fv main.prof.*
	rm -fv comment.cut
	-diff -y <(git ls-files | sort) <(ls -a | sort)

preview: $(TEXSRC)
	latexmk -pvc

onchange:
	git ls-files | entr -c make

artifact:
	zip exceptional-actors.zip \
		main.lhs main.bib ring.tex \
		default.nix shell.nix \
		makefile noprint.py benchprep.sh \
		hakkell-paper.cabal cabal.project.freeze \
		bench-mem/total-allocated.svg \
		bench-time/machine_macbookpro11,5-mean.svg \
		bench-time/machine_c3.8xlarge-mean.svg \
		bench-time/machine_c6a.48xlarge-mean.svg \
		bench-time/group_channels-mean.svg \
		README.md LICENSE \
