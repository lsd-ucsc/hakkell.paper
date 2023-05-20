.PHONY: all clean cleaner preview onchange

PAPER = main
INPUTS = $(PAPER).tex $(PAPER).bib
OUTPUTS = $(INPUTS:%.tex=%.pdf)

all: $(OUTPUTS)

%.pdf: %.tex
	latexmk -pdf $<

clean:
	latexmk -c

cleaner:
	latexmk -C

preview:
	latexmk -pvc

onchange:
	git ls-files | entr -c make
