## Build: docker build -t r-san -f simmer/working_dir/r-san.Dockerfile .
##        R CMD build simmer
## Usage: docker run --rm -ti -v $(pwd):/mnt r-san R CMD check --use-valgrind /mnt/simmer_x.x.x.tar.gz
## Usage: docker run --rm -ti -v $(pwd):/mnt r-san RD CMD check --as-cran /mnt/simmer_x.x.x.tar.gz

## Start with the base image
FROM rocker/r-devel-san:latest
MAINTAINER Iñaki Úcar <i.ucar86@gmail.com>

## Remain current
RUN apt-get update -qq \
	&& apt-get dist-upgrade -y

## Install dependencies
RUN apt-get install -y \
  libssl-dev
RUN RDscript -e 'install.packages(c("devtools", "MASS", "Rcpp", "BH", "R6", "magrittr", "dplyr", "tidyr", "ggplot2", "scales", "testthat", "knitr", "rmarkdown", "covr"))'

ENV LANG=en_US.utf-8 \
    HOME=/home/docker
WORKDIR /home/docker
USER docker
