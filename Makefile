SLIDE_RMD_FILES := $(wildcard static/slides/*.Rmd)
SLIDE_HTML_FILES  := $(subst Rmd,html,$(SLIDE_RMD_FILES))
SLIDE_PDF_FILES  := $(subst Rmd,pdf,$(SLIDE_RMD_FILES))

.PHONY: clean push build all pdf

build: $(SLIDE_HTML_FILES)
	hugo

all: html pdf build

html: $(SLIDE_HTML_FILES)

pdf: $(SLIDE_PDF_FILES)

open: build
	open docs/index.html

clean:
	rm -rf docs/
	rm -f static/slides/*.html

static/slides/%.html: static/slides/%.Rmd
	@Rscript -e "rmarkdown::render('$<')"
	
static/slides/%.pdf: static/slides/%.html
	wkhtmltopdf --page-width 120 --page-height 213 -B 0 -L 0 -R 0 -T 0 -O "Landscape" $< $@
	ls -la static/slides/
	#@cd static/slides/; \
		#Rscript -e "xaringan::decktape('$<', '$@')"
		#Rscript -e "xaringanBuilder::build_pdf('$(<F)')"
		#Rscript -e "pagedown::chrome_print('$(<F)')"
		
		# tries for a 16:9 aspect ratio
		