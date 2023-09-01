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

%.hs: %.lhs
	unlit -i $< -o $@

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
		.gitignore \

# Rebuild until the trace-figures let us fit in 12 pages:
#
# while test 12 -lt "$(pdfinfo main.pdf |grep Pages |cut -d: -f2)"; do touch main.lhs; make; done

sources: $(PAPER).pdf
	mv -v   tex-sources/acmart.cls .
	rm -vrf tex-sources/ tex-sources.zip
	mkdir   tex-sources/
	mv -v   acmart.cls tex-sources/

	cp -vr  svg-inkscape/* tex-sources/
	cp -v   $(TEXSRC) tex-sources/
	cp -v   main.bbl tex-sources/

	# fix the latex code and paths for importing the SVGs
	sed -e 's,\\includesvg\[width=\([^]]*\)]{\([^}]*\)},\\def\\svgwidth{\1}\n\\input{\2},' \
		-e 's,\(bench-time\|bench-mem\)/,,' \
		-e 's,\.svg,_svg-tex.pdf_tex,' \
		-i tex-sources/main.tex

	cd tex-sources/; \
		zip -r ../tex-sources.zip *; \
