# If you are new to Makefiles: https://makefiletutorial.com

POWER := output/power_analysis.html

TARGETS :=  $(POWER)

GENERATED_DATA := data/generated/power_runs.rds data/power/differential_effects.rds

RSCRIPT := Rscript --encoding=UTF-8

.phony: all clean dist-clean

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

dist-clean: clean
	rm -f $(GENERATED_DATA)

$(POWER): doc/power_analysis.Rmd code/R/linear_hypothesis_testing_fixest.R
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/power_analysis.Rmd")'
	mv doc/power_analysis.html output/
