## Build: docker build -t leak-check -f simmer/working_dir/leak-check.Dockerfile .
##        R CMD build simmer
## Usage: docker run --rm -ti -v $(pwd):/mnt leak-check R CMD check --use-valgrind /mnt/simmer_x.x.x.tar.gz

## Start with the base image
FROM rocker/r-devel-san:latest
MAINTAINER Iñaki Úcar <i.ucar86@gmail.com>

## Set a useful default locale
ENV LANG=en_US.utf-8

## Install dependencies
RUN apt-get install -y \
  libssl-dev
RUN Rscript -e 'install.packages(c("MASS", "Rcpp", "BH", "R6", "magrittr", "dplyr", "tidyr", "ggplot2", "scales", "testthat", "knitr", "rmarkdown", "covr"))'
