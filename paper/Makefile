TEX = main.tex
PDF = main.pdf
# Files \input from main.tex. Without these, editing a figure leaves `make` a no-op.
DEPS = fig-tour.tex fig-hardware.tex xxx.sty main.bib fig-eval-motivating.pdf fig-eval-ptemp.pdf

.PHONY: all clean watch

all: $(PDF)

$(PDF): $(TEX) $(DEPS)
	tectonic -X compile --keep-logs --keep-intermediates $(TEX)

watch:
	tectonic -X watch

clean:
	rm -f main.pdf main.log main.aux main.out main.toc main.bbl main.blg
