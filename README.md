## Carbon Footprint Information and Consumer Choice

This is the repository of a project on the impact of carbon footprint information on consumer choice, co-authored by Bianca Beyer, Rico Chaskel, Simone Euler, Joachim Gassen, Ann-Kristin Großkopf and Thorsten Sellhorn. It is being developed within project B04 ("Real Effects of Transparency") of TRR 266 "Accounting for Transparency".

We a are planing to run a large-scale field experiment to assess how attributes of carbon footprint information affect consumer choice. Partnering with a large canteen operator, we manipulate the understandability, mental accounting framing, and salience of carbon footprint food labels in a natural setting. We then measure differences in subjects' food choices with regards to (1) the type of dish selected (high- or low-carbon dishes), (2) the total amount of food chosen, and (3) the carbon footprint of the food chosen. A fully parameterized simulation-based power analysis indicates that the expected features of our setting provide sufficient statistical power to find the predicted effects, if true, at conventional significance levels, making ultimate experimental results interpretable regardless of the outcome.

Currently, this repository contains this power analysis along with some auxilirary data. You can take a [look at it here](https://raw.githubusercontent.com/trr266/carbonfoot/power_analysis.html). If you want to work with the R code itself, it resides in the `doc` directory. You can knit either manually or by running `make all` (assuming that you have a working make environment installed - see below for details).

Feel to reach out to the authors for questions and/or feedback.


### How do I reproduce the analysis?

Assuming that you have RStudio and make/Rtools installed, this should be relatively straightforward.

1. Download, clone or fork the repository to your local computing environment.
2. Before building everything you most likely need to install additional packages. See the code below for installing the packages.
4. Run 'make all' either via the console or by identifying the 'Build All' button in the 'Build' tab (normally in the upper right quadrant of the RStudio screen). 
5. Eventually, you will be greeted with the file `power_analysis.html` in the main directory. Congratulations! 

If you do not see 'Build' tab this is most likely because you do not have 'make' installed on your system. 
  - For Windows: Install Rtools: https://cran.r-project.org/bin/windows/Rtools/
  - For MacOS: You need to install the Mac OS developer tools. Open a terminal and run `xcode-select --install` Follow the instructions
  - On Linux: We have never seen a Unix environment without 'make'. 

```
# Code to install packages to your system
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("tidyverse")
install_package_if_missing("fixest")
install_package_if_missing("truncnorm")
install_package_if_missing("knitr")
install_package_if_missing("kableExtra")
install_package_if_missing("modelsummary")
install_package_if_missing("ggridges")
install_package_if_missing("emojifont")
install_package_if_missing("car")

# In addition, if you have no working LaTeX environment, consider
# installing the neat tinytex LateX distribution. It is lightweight and
# you can install it from wihtin R! See https://yihui.org/tinytex/
# To install it, run from the R console:

install_package_if_missing('tinytex')
tinytex::install_tinytex()

# That's all!
```


### Disclaimer

This repository was built based on the ['trer' template for reproducible emprical research](https://github.com/trr266/trer).
