TEX = main.tex
PDF = main.pdf

.PHONY: all clean watch

all: $(PDF)

$(PDF): $(TEX)
	tectonic -X compile --keep-logs --keep-intermediates $(TEX)

watch:
	tectonic -X watch

clean:
	rm -f main.pdf main.log main.aux main.out main.toc main.bbl main.blg
