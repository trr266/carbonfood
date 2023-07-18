# If you are new to Makefiles: https://makefiletutorial.com

# Commands

RSCRIPT := Rscript --encoding=UTF-8 


# Data locations

DISH_PLAN := data/experiment/dish_plan.csv

TMENT_DATA := data/experiment/tment_data.rds

DISH_CHOICES := data/experiment/dish_choices.rds


# Targets for pre-registered simulated power analysis

POWER_SIM_DATA := data/generated/power_runs.rds \
	data/generated/power_differential_effects.rds

POWER_SIM_RESULTS := output/power_sim_output.zip

POWER_SIM := output/power_analysis.html


# Targets for experimental analysis

EXP_DATA := $(DISH_PLAN) $(TMENT_DATA) $(DISH_CHOICES) 

EXP_SAMPLES := data/generated/exp_sample_dish.rds \
	data/generated/exp_sample_taction.rds \
	output/exp_samples_desc_output.zip

EXP_DEBRIEFING_FIGURES := output/debriefing_res_exp.svg \
	output/debriefing_res_exp.pdf
	
EXP_PREREG_OUTPUT := output/exp_prereg_output_dish.zip \
	output/exp_prereg_output_taction.zip
	 
EXP_ADD_OUTPUT := output/exp_additional_output_dish.zip \
	output/exp_additional_output_taction.zip
	
EXP_RESULTS := $(EXP_DEBRIEFING_FIGURES) $(EXP_PREREG_OUTPUT) $(EXP_ADD_OUTPUT)


# Targets for RDF analysis

RDF_OUTCOMES := output/rdf_code_doc.pdf output/rdf_flowchart.pdf \
	data/generated/rdf_outcomes.rds

RDF_SPEC_CURVES := output/rdf_spec_curves.zip

RDF_RESULTS := $(RDF_OUTCOMES) $(RDF_SPEC_CURVES)


# Static targets

STATIC_POWER_SIM_DATA := static/power_runs.rds \
	static/power_differential_effects.rds

STATIC_DEBRIEFING_FIGURE := static/debriefing_res_exp.svg

STATIC_RDF_OUTCOMES := static/rdf_code_doc.pdf static/rdf_flowchart.pdf \
	static/rdf_outcomes.rds
  
STATIC_TARGETS :=  $(STATIC_POWER_SIM_DATA) $(STATIC_DEBRIEFING_FIGURE) \
  $(STATIC_RDF_OUTCOMES)

  
# All targets combined

TARGETS :=  $(STATIC_TARGETS) $(POWER_SIM) $(EXP_RESULTS) $(RDF_RESULTS)


# Phony targets

.phony: all power findings rdf clean dist-clean

all: $(TARGETS)

static: $(STATIC_TARGETS)

power: $(POWER_SIM)

findings: $(EXP_RESULTS)

rdf: $(RDF_RESULTS)

clean:
	rm -f $(POWER_SIM) $(POWER_SIM_OUTPUT) $(EXP_SAMPLES) $(EXP_RESULTS) \
		$(RDF_SPEC_CURVES) $(POWER_SIM_DATA) $(RDF_OUTCOMES)

dist-clean: clean
	rm -f $(STATIC_TARGETS)


# Recipes for targets

$(STATIC_POWER_SIM_DATA): doc/power_analysis.Rmd \
	code/R/linear_hypothesis_testing_fixest.R
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/power_analysis.Rmd")'
	mv data/generated/power_runs.rds static/
	mv data/generated/power_differential_effects.rds static/
	mv doc/power_analysis.html output

$(POWER_SIM): doc/power_analysis.Rmd code/R/linear_hypothesis_testing_fixest.R \
	$(STATIC_POWER_SIM_DATA)
	cp -f static/power_runs.rds data/generated
	cp -f static/power_differential_effects.rds data/generated
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/power_analysis.Rmd")'
	mv doc/power_analysis.html output

$(EXP_SAMPLES): code/R/prepare_exp_samples.R $(EXP_DATA)
	$(RSCRIPT) code/R/prepare_exp_samples.R

$(STATIC_DEBRIEFING_FIGURE): code/R/exp_debriefing_figures.R $(EXP_SAMPLES)
	$(RSCRIPT) code/R/exp_debriefing_figures.R
	cp output/debriefing_res_exp.svg static/

$(EXP_DEBRIEFING_FIGURES): code/R/exp_debriefing_figures.R $(EXP_SAMPLES)
	$(RSCRIPT) code/R/exp_debriefing_figures.R

$(EXP_PREREG_OUTPUT) $(EXP_PREREG_RESULTS): \
	code/R/exp_preregistered_analyses.R code/R/exp_utils.R $(EXP_SAMPLES)
	$(RSCRIPT) code/R/exp_preregistered_analyses.R
	
$(EXP_ADD_OUTPUT) $(EXP_ADD_RESULTS): \
	code/R/exp_additional_analyses.R code/R/exp_utils.R $(EXP_SAMPLES)
	$(RSCRIPT) code/r/exp_additional_analyses.R

$(STATIC_RDF_OUTCOMES): code/r/rdf_exhaust_design.R \
	code/R/rdf_design.R code/R/exp_utils.R $(EXP_DATA)
	$(RSCRIPT) code/r/rdf_exhaust_design.R
	
$(RDF_OUTCOMES): $(STATIC_RDF_OUTCOMES)
	cp -f static/rdf_code_doc.pdf output/
	cp -f static/rdf_flowchart.pdf output/
	cp -f static/rdf_outcomes.rds data/generated/
	
$(RDF_SPEC_CURVES):	code/r/rdf_create_spec_curves.R $(RDF_OUTCOMES)
	$(RSCRIPT) code/r/rdf_create_spec_curves.R