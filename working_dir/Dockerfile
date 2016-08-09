## Build: docker build -t simmer-check simmer/working_dir
##        R CMD build simmer
## Usage: docker run --rm -ti -v $(pwd):/mnt simmer-check Rdevel CMD check --as-cran /mnt/simmer_x.x.x.tar.gz

## Start with the base image
FROM rocker/r-devel-san:latest
MAINTAINER Iñaki Úcar <i.ucar86@gmail.com>

## Set a useful default locale
ENV LANG=en_US.utf-8

## Install dependencies
RUN apt-get install -y \
  libssl-dev
RUN Rscriptdevel -e 'install.packages(c("MASS", "Rcpp", "BH", "R6", "magrittr", "dplyr", "tidyr", "ggplot2", "scales", "testthat", "knitr", "rmarkdown", "covr"))'
