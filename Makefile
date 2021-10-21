# If you are new to Makefiles: https://makefiletutorial.com

PAPER := output/paper.pdf

PRESENTATION := output/presentation.pdf

TARGETS :=  $(PAPER) $(PRESENTATION)

WORLDBANK_DATA := data/pulled/worldbank.rda

GENERATED_DATA := data/generated/sample.rda

RESULTS := output/results.rda

RSCRIPT := Rscript --encoding=UTF-8

.phony: all clean very-clean dist-clean

all: $(TARGETS)

clean:
	rm -f $(TARGETS)
	rm -f $(RESULTS)
	rm -f $(GENERATED_DATA)

dist-clean: clean
	rm -f $(WORLDBANK_DATA)

$(WORLDBANK_DATA): code/R/pull_data.R
	$(RSCRIPT) code/R/pull_data.R

$(GENERATED_DATA): $(WORLDBANK_DATA) code/R/prepare_data.R
	$(RSCRIPT) code/R/prepare_data.R

$(RESULTS):	$(GENERATED_DATA) code/R/do_analysis.R
	$(RSCRIPT) code/R/do_analysis.R

$(PAPER): doc/paper.Rmd doc/references.bib $(RESULTS)
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/paper.Rmd")'
	mv doc/paper.pdf output
	rm -f doc/paper.ttt doc/paper.fff

$(PRESENTATION): doc/presentation.rmd $(RESULTS)
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/presentation.Rmd")'
	mv doc/presentation.pdf output
