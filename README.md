## Carbon Footprint Information and Consumer Choice

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8188497.svg)](https://doi.org/10.5281/zenodo.8188497)

This is the repository for the study:

> Bianca Beyer, Rico Chaskel, Simone Euler, Joachim Gassen, Ann-Kristin Großkopf, and Thorsten Sellhorn (2024): How Does Carbon Footprint Information Affect Consumer Choice? A Field Experiment, Journal of Accounting Research 62(1): 101-136, https://doi.org/10.1111/1475-679X.12505.

This project has been developed within project ["Real Effects of Transparency" (B04)](https://www.accounting-for-transparency.de/projects/real-effects-of-transparency) of the [TRR 266 "Accounting for Transparency"](https://www.accounting-for-transparency.de). 

<p align="center">
<img src="https://raw.githubusercontent.com/trr266/carbonfood/main/static/debriefing_res_exp.svg" alt="Carbon footprint effects of Beyer et al. (JAR, forthcoming)">
</p>

Our study reports the results of a field experiment investigating how attributes of carbon footprint information affect consumer choice in a large dining facility. We have presented our experimental design at the 2021 Registered Report Conference of the Journal of Accounting Research, and then, after incorporating the feedback from the conference and the review process, run a large-scale field experiment to assess our pre-registered research questions. Partnering with the [Studierendenenwerk München Oberbayern](https://www.studierendenwerk-muenchen-oberbayern.de/en/), we manipulated the understandability, mental accounting framing, and color-coding of carbon footprint information on food labels and measured differences in consumers’ food choices. As displayed in the figure above, treated consumers chose meat dishes significantly less often and reduced their food related carbon footprint by up to 9.2%, depending on the treatment. The effects were strongest for carbon footprint information expressed in monetary units (‘environmental costs’) and color-coded in the familiar traffic-light scheme. A post-experimental survey has shown that these effects were found although respondents, on average, self-reported low concern for the environmental footprint of their meal choices.

This repository contains the following components:

- Code for the fully parameterized simulation-based power analysis that we used in our preregistration report (`doc/power_analysis.Rmd`, the original power simulation can be assessed  [here](https://trr266.de/carbonfood/power_analysis.html)). 
- Code for comparing the fixed effect models used in the analysis to various discrete choice models (`code/R/disc_choice_sim.R`, used during the review process)
- All relevant data collected during the experiment (`data/experiment`) and by the post-experimental survey (`data/survey`).
- Code to generate our experimental samples (`code/R/prepare_exp_samples.R`)
- Code to produce all tables and figures included in the [debriefing material](https://www.accounting-for-transparency.de/can-carbon-footprint-information-influence-consumer-choice/), the paper, and in the online appendix (`exp_debriefing_figures.R`, `exp_preregistered_analysis.R`,  `exp_additional_analyses.R`, and `survey_results.R` in `code/R/`).
- Code to generate our researcher degree of freedom analysis (`rdf_design.R`, `rdf_exhaust_design.R`, and `ref_create_spec_curves.R` in `code/R/`)
- Code to generate our [online dashboard](https://trr266.de/carbonfood/) that allows interested readers to assess the robustness of our findings by exploring various design choices via the above mentioned researcher degree of freedom analysis (`code/R/rdf_display.R`).

Feel free to reach out to the authors for questions and/or feedback.


### How do I reproduce the analysis?

This video tutorial shows how to implement option 1 from below and reproduce our analysis in less than half an hour. 

<p>&nbsp;</p>
<p align="center">
<a href="https://www.youtube.com/watch?v=PM7K1Z5P2XY">
<img src="https://img.youtube.com/vi/PM7K1Z5P2XY/mqdefault.jpg" alt="Replicate Beyer et al. (JAR, 2023) using GitHub Codepspaces">
</a>
</p>
<p>&nbsp;</p>

**Setting up the code environment - Option 1: Use GitHub codespaces** 

If you have a GitHub account, we suggest that you start with this option, which uses [GitHub codespaces](https://github.com/features/codespaces). To reproduce our analysis, simply go to the [repository page on GitHub](https://github.com/trr266/carbonfood) and click on the "Code" button in the upper right half of the screen. On the Codespaces tab, it will provide you with the option to create a codespace on the main branch of the repo. Do that, wait patiently for a few minutes while the container is built on the GitHub cloud and then you will be taken to a browser instance of Visual Studio Code that runs within that container. Continue with **Recreating the analysis** below. Please Note: While GitHub codespaces currently provides 60 hours of free monthly access to a two core cloud instance, exceeding this limit will trigger costs. So, if you are in for a deeper dive into our code, we suggest to pick one of the other options.


**Setting up the code environment - Option 2: Use Visual Studio Code and a Docker development container** 

This requires you to have both [Docker](https://www.docker.com) and [Visual Studio Code](https://code.visualstudio.com) installed on your system and allows you to spin up our development container locally. To set up the code environment, start Visual Studio Code, enter the command palette (Ctrl+Shift+P) and run "Dev Containers: Clone Repository in Container Volume", providing the URL of this repository. It will take a while but you should eventually be placed in a local Docker container that contains the code environment and this repository. Continue with **Recreating the analysis** below.


**Setting up the code environment - Option 3: Use your own local code environment** 

This likely will involve some fiddling and requires you to have R, RStudio, and RTools installed (Mac Users: There is no RTools for Mac. Instead make sure that you have XCode installed. Open a terminal and run `xcode-select --install`. Follow the instructions. Linux users: You should be fine as our code only requires 'make' to be available). Download, clone or fork this repository to your local computing environment. Before building everything you most likely need to install additional packages. See the R code below for installing the packages. Please note that, by installing current package versions, you might run into compatibility issues. If you face these problems, you can either try to mimic our code environment (R version 4.2.3 and dependencies as of 2023-06-16) or use one of the two options above.

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
install_package_if_missing("rmarkdown")
install_package_if_missing("knitr")
install_package_if_missing("kableExtra")
install_package_if_missing("modelsummary")
install_package_if_missing("ggridges")
install_package_if_missing("ggbeeswarm")
install_package_if_missing("ggforce")
install_package_if_missing("emojifont")
install_package_if_missing("car")
install_package_if_missing("marginaleffects")
install_package_if_missing("devtools")
install_package_if_missing("rsvg")
install_package_if_missing("RPostgres")
install_package_if_missing("binom")

devtools::install_github("rich-iannone/DiagrammeRsvg")
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

**Recreating the analysis**

Run `make all` either via the terminal or, if you are using RStudio, by identifying the 'Build All' button in the 'Build' tab (normally in the upper right quadrant of the RStudio screen). It will start the build process and source several R code files, generating our samples in the `data/generated` folder and (partly zipped) analysis output in the `output` folder. Please note: Rerunning the power simulation and the researcher degree of freedom analysis would take several hours. To cut down on this, `make all` uses static versions of these data files (stored in `static`). If you want to reproduce these files, you need to run `make dist-clean`, followed by `make all`. You can also just reproduce subsets of the analysis by running `make power`, `make findings`, or `make rdf`.


**Running the researcher degree of freedom dashboard locally**

Execute `Rscript code/R/rdf_display.R` from your local terminal (not from the R console). If you are running this from within a container Visual Studio Code will ask you for permission to open a port and show the output in your browser. It is safe to say "yes".


### Disclaimer

This project has been funded by the [Deutsche Forschungsgemeinschaft (DFG, German Research Foundation)](https://www.dfg.de/): [Project-ID 403041268 – TRR 266 Accounting for Transparency](https://www.accounting-for-transparency.de/).
The repository was built based on the ['trer' template for reproducible empirical research](https://github.com/trr266/trer).
