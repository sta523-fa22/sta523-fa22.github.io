SLIDE_QMD_FILES := $(wildcard static/slides/*.qmd)
SLIDE_HTML_FILES  := $(subst qmd,html,$(SLIDE_QMD_FILES))
SLIDE_PDF_FILES  := $(subst qmd,pdf,$(SLIDE_QMD_FILES))

.PHONY: clean push build all pdf

build: $(SLIDE_HTML_FILES) $(SLIDE_PDF_FILES)
	hugo
	rm -rf docs/slides/prev

all: pdf build

html: $(SLIDE_HTML_FILES)
	echo $(SLIDE_HTML_FILES)

pdf: $(SLIDE_PDF_FILES)
	echo $(SLIDE_PDF_FILES)

open: build
	open docs/index.html

clean:
	rm -rf docs/
	rm -f static/slides/*.html
	rm -f static/slides/*.pdf

static/slides/%.html: static/slides/%.qmd
	quarto render $<
	
static/slides/%.pdf: static/slides/%.html
	Rscript -e "renderthis::to_pdf('$<')"

push: build
	git pull
	git add .
	git commit -m "Make update"
	git push