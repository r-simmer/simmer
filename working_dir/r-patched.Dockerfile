## Build: docker build -t r-patched -f simmer/working_dir/r-patched.Dockerfile .
##        R CMD build simmer
## Usage: docker run --rm -ti -v $(pwd):/mnt r-patched RP CMD check --as-cran /mnt/simmer_x.x.x.tar.gz

## Start with the base image
FROM r-base:latest
MAINTAINER Iñaki Úcar <i.ucar86@gmail.com>

## Remain current
RUN apt-get update -qq \
	&& apt-get dist-upgrade -y

## From the Build-Depends of the Debian R package, plus subversion
RUN apt-get update -qq \
	&& apt-get install -t unstable -y --no-install-recommends \
		bash-completion \
		bison \
		clang-3.8 \
		libc++-dev \
		libc++abi-dev \
		debhelper \
		default-jdk \
		g++ \
		gcc \
		gdb \
		gfortran \
		groff-base \
		libblas-dev \
		libbz2-dev \
		libcairo2-dev/unstable \
		libcurl4-openssl-dev \
		libjpeg-dev \
		liblapack-dev \
		liblzma-dev \
		libncurses5-dev \
		libpango1.0-dev \
		libpcre3-dev \
		libpng-dev \
		libreadline-dev \
    libssl-dev \
		libtiff5-dev \
		libssl-dev \
		libx11-dev \
		libxml2-dev \
		libxt-dev \
		mpack \
		subversion \
		tcl8.6-dev \
		texinfo \
		texlive-base \
		texlive-extra-utils \
		texlive-fonts-extra \
		texlive-fonts-recommended \
		texlive-generic-recommended \
		texlive-latex-base \
		texlive-latex-extra \
		texlive-latex-recommended \
		tk8.6-dev \
		x11proto-core-dev \
		xauth \
		xdg-utils \
		xfonts-base \
		xvfb \
		zlib1g-dev

## Check out R-patched
RUN cd /tmp \
  && svn co https://svn.r-project.org/R/branches/R-3-3-branch R-patched

## Build and install
RUN cd /tmp/R-patched \
	&& R_PAPERSIZE=letter \
		R_BATCHSAVE="--no-save --no-restore" \
		R_BROWSER=xdg-open \
		PAGER=/usr/bin/pager \
		PERL=/usr/bin/perl \
		R_UNZIPCMD=/usr/bin/unzip \
		R_ZIPCMD=/usr/bin/zip \
		R_PRINTCMD=/usr/bin/lpr \
		LIBnn=lib \
		AWK=/usr/bin/awk \
		CC="clang-3.8" \
	  CXX="clang++-3.8 -stdlib=libc++" \
		CFLAGS=$(R CMD config CFLAGS) \
		CXXFLAGS=$(R CMD config CXXFLAGS) \
		MAIN_LD="clang++-3.8 -stdlib=libc++" \
		FC="gfortran" \
	  F77="gfortran" \
	  ./configure --enable-R-shlib \
                --without-blas \
                --without-lapack \
                --with-readline \
                --without-recommended-packages \
                --program-suffix=dev \
	&& make \
	&& make install \
	&& rm -rf /tmp/R-patched

## Set Renviron to get libs from base R install
RUN echo "R_LIBS=\${R_LIBS-'/usr/local/lib/R/site-library:/usr/local/lib/R/library:/usr/lib/R/library'}" >> /usr/local/lib/R/etc/Renviron

## Set default CRAN repo
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site

RUN cd /usr/local/bin \
  && mv R Rpatched \
  && mv Rscript Rscriptpatched \
  && ln -s Rpatched RP \
  && ln -s Rscriptpatched RPscript

RUN cd /usr/local/lib/R/etc \
  && echo "CXXFLAGS += -I/usr/include/libcxxabi" >> Makevars.site

RUN RPscript -e 'install.packages(c("devtools", "MASS", "Rcpp", "BH", "R6", "magrittr", "dplyr", "tidyr", "ggplot2", "scales", "testthat", "knitr", "rmarkdown", "covr"))'

ENV LANG=en_US.utf-8 \
    HOME=/home/docker
WORKDIR /home/docker
USER docker
