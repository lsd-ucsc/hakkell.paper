.PHONY: all clean clean-all preview onchange

PAPER = main
INPUTS = $(PAPER).lhs $(PAPER).bib
OUTPUTS = $(PAPER).pdf

TEXSRC = $(patsubst %.lhs, %.tex, $(INPUTS))

all: $(OUTPUTS)

%.pdf: %.tex %.bench.raw
	latexmk -pdf $<

%.tex: %.lhs
	ghc -fno-code $^ # just typecheck
	lhs2TeX $^ > $@

%.bench.raw: %.elf
	./$< --raw $@ --output $%.bench.html

%.elf: %.noprint.lhs
	ghc -O -threaded -rtsopts -with-rtsopts=-N $< -o $@

%.noprint.lhs: %.lhs noprint.py
	python noprint.py < $< > $@
	# verify no printlines remain
	if grep --color=always -i '\<putstr\|putstrln\|print\>' $@; then false; fi

clean: $(TEXSRC)
	latexmk -c

clean-all: $(TEXSRC)
	latexmk -C
	#rm -fv main.{bbl,ptb,tex}
	#rm -fv main.{,noprint.}{hi,o}

preview: $(TEXSRC)
	latexmk -pvc

onchange:
	git ls-files | entr -c make
