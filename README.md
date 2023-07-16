## Carbon Footprint Information and Consumer Choice

This is the repository for the study:

> "How Does Carbon Footprint Information Affect Consumer Choice? A Field Experiment", co-authored by Bianca Beyer (Aalto University), Rico Chaskel, Simone Euler, Joachim  Gassen (all Humboldt University Berlin), Ann-Kristin Großkopf, and Thorsten Sellhorn  (both LMU Munich).

This project has been developed within project ["Real Effects of Transparency" (B04)](https://www.accounting-for-transparency.de/projects/real-effects-of-transparency) of the [TRR 266 "Accounting for Transparency"](https://www.accounting-for-transparency.de). 

![Carbon footprint effects of Beyer et al. (forthcoming)](https://github.com/trr266/carbonfood/blob/main/static/debriefing_res_exp.svg)

We have presented our experimental design at the 2021 Registered Report Conference of the Journal of Accounting Research, and then, after incorporating the feedback from the conference and the review process, run a large-scale field experiment to assess how attributes of carbon footprint information affect consumer choice. Partnering with the [Studierendenenwerk München Oberbayern](https://www.studierendenwerk-muenchen-oberbayern.de/en/), we manipulated the understandability, mental accounting framing, and color-coding of carbon footprint information on food labels and measured differences in consumers’ food choices. As displayed in the figure above, treated consumers chose meat dishes significantly less often and reduced their food related carbon footprint by up to 9.2%, depending on the treatment. The effects were strongest for carbon footprint information expressed in monetary units (‘environmental costs’) and color-coded in the familiar traffic-light scheme. A post-experimental survey has shown that these effects were found although respondents, on average, self-reported low concern for the environmental footprint of their meal choices.

This repository contains the following components:

- Code for the fully parameterized simulation-based power analysis that we used in our preregistration report (`doc/power_analysis.Rmd`, the original power simulation can be assessed  [here](https://trr266.de/carbonfood/power_analysis.html)). 
- Code for comparing the fixed effect models used in the analysis to various discrete choice models (`code/R/disc_choice_sim.R`, used during the review process)
- All relevant data collected during the experiment and by the post-experimental survey (`data/experiment`).
- Code to generate our experimental samples (`code/R/prepare_exp_samples.R`)
- Code to produce all tables and figures included in the [debriefing material](https://www.accounting-for-transparency.de/can-carbon-footprint-information-influence-consumer-choice/), the paper, and in the online appendix (`exp_debriefing_figures.R`, `exp_preregistered_analysis.R`,  and `exp_additional_analyses.R` in `code/R/`).
- Code to generate our [online dashboard](https://trr266.wiwi.hu-berlin.de/shiny/carbonfood/) that allows interested readers to assess the robustness of our findings by exploring various design choices. After running `make rdf` (see below), you should be able to start the dashboard locally by executing `code/R/rdf_display.R`.

Feel free to reach out to the authors for questions and/or feedback.


### How do I reproduce the analysis?

If you have RStudio and make/Rtools installed, the process should be relatively straightforward.

1. Download, clone or fork this repository to your local computing environment.
2. Before building everything you most likely need to install additional packages. See the code below for installing the packages.
3. Run 'make all' either via the console or by identifying the 'Build All' button in the 'Build' tab (normally in the upper right quadrant of the RStudio screen). 
4. Eventually, you will be greeted with several files in the `data/generated` and `output` directories containing our generated data and analysis. Congratulations! 

If you do not see a 'Build' tab this is most likely because you do not have 'make' installed on your system. 

- For Windows: Install Rtools: https://cran.r-project.org/bin/windows/Rtools/.
- For MacOS: You need to install the Mac OS developer tools. Open a terminal and run `xcode-select --install`. Follow the instructions.
- On Linux: We have never seen a Unix environment without 'make'. 

**Please note:** Because of the power simulation and the researcher degree of freedom analysis, reproducing all elements takes several hours. You can instead only reproduce subsets of the analysis by running `make power`, `make findings`, or `make rdf`. 

```
# Code to install packages to your system
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("tidyverse")
install_package_if_missing("lubridate")
install_package_if_missing("fixest")
install_package_if_missing("mlogit")
install_package_if_missing("truncnorm")
install_package_if_missing("knitr")
install_package_if_missing("kableExtra")
install_package_if_missing("modelsummary")
install_package_if_missing("ggridges")
install_package_if_missing("ggbeeswarm")
install_package_if_missing("ggforce")
install_package_if_missing("emojifont")
install_package_if_missing("car")
install_package_if_missing("marginaleffects")
devtools::install_github("joachim-gassen/rdfanalysis")

# In addition, if you have no working LaTeX environment, consider
# installing the neat tinytex LateX distribution. It is lightweight and
# you can install it from within R! See https://yihui.org/tinytex/
# To install it, run from the R console:

install_package_if_missing('tinytex')
tinytex::install_tinytex()

# If you run into problems knitting markdown files into PDF this might be 
# because your RScript environment has troubles finding your pandoc 
# installation. In this case make sure that you set the environment variable
# RSTUDIO_PANDOC to whatever it points to from within RStudio (run
# Sys.getenv("RSTUDIO_PANDOC")' from within the R console to find out)
```


### Disclaimer

This project has been funded by the [Deutsche Forschungsgemeinschaft (DFG, German Research Foundation)](https://www.dfg.de/): [Project-ID 403041268 – TRR 266 Accounting for Transparency](https://www.accounting-for-transparency.de/).
The repository was built based on the ['trer' template for reproducible empirical research](https://github.com/trr266/trer).
