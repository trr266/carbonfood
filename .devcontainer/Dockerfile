FROM rocker/r-ver:4.2

RUN apt clean && apt update 
RUN apt-get -y install cmake git pandoc libxml2-dev libcurl4-openssl-dev && apt-get clean
RUN apt-get -y install libssl-dev libgit2-dev wget libfontconfig1-dev && apt-get clean
RUN apt-get -y install libharfbuzz-dev libfribidi-dev librsvg2-dev && apt-get clean
RUN apt-get -y install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev && apt-get clean
RUN apt-get -y install texlive && apt-get clean
RUN apt-get -y install texlive-latex-extra && apt-get clean
RUN apt-get -y install texlive-xetex && apt-get clean
RUN apt-get -y install texlive-science && apt-get clean

RUN install2.r --error --skipinstalled tidyverse 
RUN install2.r --error --skipinstalled lubridate 
RUN install2.r --error --skipinstalled DBI 
RUN install2.r --error --skipinstalled fixest 
RUN install2.r --error --skipinstalled mlogit  
RUN install2.r --error --skipinstalled truncnorm 
RUN install2.r --error --skipinstalled knitr
RUN install2.r --error --skipinstalled kableExtra
RUN install2.r --error --skipinstalled rmarkdown 
RUN install2.r --error --skipinstalled modelsummary 
RUN install2.r --error --skipinstalled ggridges 
RUN install2.r --error --skipinstalled ggbeeswarm 
RUN install2.r --error --skipinstalled ggforce 
RUN install2.r --error --skipinstalled emojifont
RUN install2.r --error --skipinstalled car
RUN install2.r --error --skipinstalled marginaleffects
RUN install2.r --error --skipinstalled devtools
RUN install2.r --error --skipinstalled rsvg
RUN install2.r --error --skipinstalled Rpostgres
RUN rm -rf /tmp/downloaded_packages

RUN R -e "devtools::install_github('joachim-gassen/rdfanalysis', ref = '23294355')"
RUN R -e "devtools::install_github('rich-iannone/DiagrammeRsvg', ref = '5057edac')"
RUN apt-get -y install texlive-fonts-extra && apt-get clean

CMD /bin/sh

