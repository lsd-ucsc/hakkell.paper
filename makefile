.PHONY: all clean clean-all preview onchange

PAPER = main
INPUTS = $(PAPER).lhs $(PAPER).bib
OUTPUTS = $(PAPER).pdf

TEXSRC = $(patsubst %.lhs, %.tex, $(INPUTS))

all: $(OUTPUTS)

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	ghc -fno-code $^ # just typecheck
	lhs2TeX $^ > $@

clean: $(TEXSRC)
	latexmk -c

clean-all: $(TEXSRC)
	latexmk -C
	rm -fv main.{bbl,ptb,tex,hi,o}

preview: $(TEXSRC)
	latexmk -pvc

onchange:
	git ls-files | entr -c make
