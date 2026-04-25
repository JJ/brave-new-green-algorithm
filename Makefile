RNW_FILES  := $(wildcard *.Rnw)
RMD_FILES  := $(wildcard *.Rmd)
PDF_FILES  := $(RNW_FILES:.Rnw=.pdf)
HTML_FILES := $(RMD_FILES:.Rmd=.html)

.PHONY: all pdfs html deps

all: pdfs html

pdfs: $(PDF_FILES)

html: $(HTML_FILES)

# Knit .Rnw → .tex
%.tex: %.Rnw
	Rscript -e "knitr::knit('$<')"

# Compile .tex → .pdf (bibtex failure is non-fatal)
%.pdf: %.tex
	pdflatex $<
	-bibtex $(<:.tex=)
	pdflatex $<
	pdflatex $<

# Render .Rmd → .html
%.html: %.Rmd
	Rscript -e "rmarkdown::render('$<')"

# Install system and R package dependencies (Ubuntu)
deps:
	sudo apt-get install -y pandoc libuv1-dev
	Rscript -e "install.packages(c('rmarkdown','ggplot2','dplyr','knitr','kableExtra','reshape2','ggridges','marginaleffects','equatiomatic'))"
