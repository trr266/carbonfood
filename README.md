## The TRR 266 Template for Reproducible Empirical Research 

This repository provides an infrastructure for open science oriented empirical projects. It is based on the [TREAT repository](https://github.com/trr266/treat) but uses World Bank data instead of WRDS data so that it can be used by everybody interested in reproducible empirical research. Currently, this is all R based but it is not meant to stay that way. You can help by contributing Python and/or Stata code that mimics the R analysis steps via pull requests.


### Where do I start?

For those of you new to R, we have "produced" a [series of short videos](https://www.youtube.com/playlist?list=PL-9XqvJlFJ-5NDUXubrbvF3aEQPeoAki3) that guide you through the process of setting up your computing environment and using the original TREAT repository. Also, there is a [blog post](https://joachim-gassen.github.io/2021/03/get-a-treat/) that details these steps in a written form. The steps of using this repository are esentially identical so this information does still apply.

If you are new to scientific computing, we suggest that you also pick up a reference from the list below and browse through it. The [Gentzkow and Shapiro (2014) paper](https://web.stanford.edu/~gentzkow/research/CodeAndData.pdf) is a particularly easy and also useful read. 

Then browse around the repository and familiarize yourself with its folders. You will quickly see that there are three folders that have files in them:

- `code`: This directory holds program scripts that are being called to download World Bank data, prepare the data, run the analysis and create the output files (a paper and a presentation, both PDF files).

- `data`: A directory where data is stored. You will see that it again contains sub-directories and a README file that explains their purpose.

- `doc`: Here you will find two RMarkdown files containing text and program instructions that will become our paper and presentation, by rendering them through the R markdown process and LaTeX.

You also see an `output` directory but it is empty. Why? Because you will create the output locally on your computer, if you want.


### How do I create the output?

Assuming that you have RStudio and make/Rtools installed, this should be relatively straightforward.

1. Download, clone or fork the repository to your local computing environment.
2. Before building everything you most likely need to install additional packages. This repository follows the established principle not to install any packages automatically. This is your computing environment. You decide what you want to install. See the code below for installing the packages.
4. Run 'make all' either via the console or by identifying the 'Build All' button in the 'Build' tab (normally in the upper right quadrant of the RStudio screen). 
5. Eventually, you will be greeted with the two files in the output directory: "paper.pdf" and "presentation.pdf". Congratulations! You have successfully used an open science resource and reproduced our "analysis". Now modify it and make it your own project!

If you do not see 'Build' tab this is most likely because you do not have 'make' installed on your system. 
  - For Windows: Install Rtools: https://cran.r-project.org/bin/windows/Rtools/
  - For MacOS: You need to install the Mac OS developer tools. Open a terminal and run `xcode-select --install` Follow the instructions
  - On Linux: I have never seen a Unix environment without 'make'. 

```
# Code to install packages to your system
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("tidyverse")
install_package_if_missing("wbstats")
install_package_if_missing("lubridate")
install_package_if_missing("ExPanDaR")
install_package_if_missing("knitr")
install_package_if_missing("kableExtra")
install_package_if_missing("rmarkdown")

# In addition, if you have no working LaTeX environment, consider
# installing the neat tinytex LateX distribution. It is lightweight and
# you can install it from wihtin R! See https://yihui.org/tinytex/
# To install it, run from the R console:

install_package_if_missing('tinytex')
tinytex::install_tinytex()

# That's all!
```

### OK. That was fun. Bot how should I use the repo now?

The basic idea is to clone the repository whenever you start a new project. If you are using GitHub, the simplest way to do this is to click on "Use this Template" above the file list. Then delete everything that you don't like and/or need. Over time, as you develop your own preferences, you can fork this repository and adjust it so that it becomes your very own template targeted to your very own preferences.


### Why do you do abc in a certain way? I like to do things differently!

Scientific workflows are a matter of preference and taste. What we present here is based on our experiences on what works well but this by no means implies that there are no other and better ways to do things. So, feel free to disagree and to build your own template. Or, even better: Convince us about your approach by submitting a pull request!


### But there are other templates. Why yet another one?

Of course there are and they a great. The reason why we decided to whip up our own is we wanted to have a template that is somewhat centered on workflows that are typical in the econ domain. Here you go.


### Licensing

This repository is licensed to you under the MIT license, essentially meaning that you can do whatever you want with it as long as you give credit to us when you use substantial portions of it. What 'substantial' means is not trivial for a template. Here is our understanding. If you 'only' use the workflow, the structure and let's say parts of the Makefile and/or the README sections that describe these aspects, we do not consider this as 'substantial' and you do not need to credit us. If, however, you decide to reuse a significant part of the example code, for example the code pulling World Bank data, maybe giving credit could be appropriate.

In any case, we would love to see you spreading the word by adding a statement like 

```
This repository was built based on the ['trer' template for reproducible emprical research](https://github.com/trr266/trer).
```

to your README file. But this is not a legal requirement but a favor that we ask 😉.


### References

These are some very helpful texts discussing collaborative workflows for scientific computing:

- Christensen, Freese and Miguel (2019): Transparent and Reproducible Social Science Research, Chapter 11: https://www.ucpress.edu/book/9780520296954/transparent-and-reproducible-social-science-research
- Gentzkow and Shapiro (2014): Code and data for the social sciences:
a practitioner’s guide, https://web.stanford.edu/~gentzkow/research/CodeAndData.pdf
- Wilson, Bryan, Cranston, Kitzes, Nederbragt and Teal (2017): Good enough practices in scientific computing, PLOS Computational Biology 13(6): 1-20, https://doi.org/10.1371/journal.pcbi.1005510


