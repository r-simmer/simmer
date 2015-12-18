## Build: docker build -t simmer-check simmer/working_dir
##	  R CMD build simmer
## Usage: docker run --rm -ti -v $(pwd):/mnt simmer-check check.r /mnt/simmer_x.x.x.tar.gz

## start with the Docker 'base R' Debian-based image
FROM r-base:latest

## Remain current
RUN apt-get update -qq \
	&& apt-get dist-upgrade -y

## From the Build-Depends of the Debian R package, plus subversion, and clang-3.5
## 
## Also add   git autotools-dev automake  so that we can build littler from source
##
RUN apt-get update -qq \
	&& apt-get install -t unstable -y --no-install-recommends \
		automake \
		autotools-dev \
		bash-completion \
		bison \
		clang-3.5 \
		debhelper \
		default-jdk \
		g++ \
		gcc \
		gfortran \
		git \
		groff-base \
		libblas-dev \
		libbz2-dev \
		libcairo2-dev \
		libcurl4-openssl-dev \
		libjpeg-dev \
		liblapack-dev \
		liblzma-dev \
		libncurses5-dev \
		libpango1.0-dev \
		libpcre3-dev \
		libpng-dev \
		libreadline-dev \
		libssh2-1-dev \
		libssl-dev \
		libtiff5-dev \
		libx11-dev \
		libxml2-dev \
		libxt-dev \
		mpack \
		qpdf \
		subversion \
		tcl8.5-dev \
		texinfo \
		texlive-base \
		texlive-extra-utils \
		texlive-fonts-extra \
		texlive-fonts-recommended \
		texlive-generic-recommended \
		texlive-latex-base \
		texlive-latex-extra \
		texlive-latex-recommended \
		tk8.5-dev \
		valgrind \
		x11proto-core-dev \
		xauth \
		xdg-utils \
		xfonts-base \
		xvfb \
		zlib1g-dev 

## Check out R-devel
RUN cd /tmp \
	&& svn co http://svn.r-project.org/R/trunk R-devel 

## Build and install according extending the standard 'recipe' I emailed/posted years ago
RUN cd /tmp/R-devel \
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
	   CFLAGS="-pipe -std=gnu99 -Wall -pedantic -g" \
	   CXXFLAGS="-pipe -Wall -pedantic -g" \
	   FFLAGS="-pipe -Wall -pedantic -g" \
	   FCFLAGS="-pipe -Wall -pedantic -g" \
	   CC="clang-3.5 -fsanitize=undefined -fno-sanitize=float-divide-by-zero,vptr,function -fno-sanitize-recover" \
	   CXX="clang++-3.5 -fsanitize=undefined -fno-sanitize=float-divide-by-zero,vptr,function -fno-sanitize-recover" \
	   CXX1X="clang++-3.5 -fsanitize=undefined -fno-sanitize=float-divide-by-zero,vptr,function -fno-sanitize-recover" \
	   FC="gfortran" \
	   F77="gfortran" \
	   ./configure --enable-R-shlib \
	       --without-blas \
	       --without-lapack \
	       --with-readline \
	       --without-recommended-packages \
	       --program-suffix=dev \
	       --disable-openmp \
	&& make \
	&& make install \
	&& make clean

## Set Renviron to get libs from base R install
RUN echo "R_LIBS=\${R_LIBS-'/usr/local/lib/R/site-library:/usr/local/lib/R/library:/usr/lib/R/library'}" >> /usr/local/lib/R/etc/Renviron

## Set default CRAN repo
RUN echo 'options("repos"="http://cran.rstudio.com")' >> /usr/local/lib/R/etc/Rprofile.site

## Check out littler 0.2.3
RUN cd /tmp \
	&& git clone --branch 0.2.3 https://github.com/eddelbuettel/littler.git

RUN cd /tmp/littler \
	&& CC="clang-3.5 -fsanitize=undefined -fno-sanitize=float-divide-by-zero,vptr,function -fno-sanitize-recover" PATH="/usr/local/lib/R/bin/:$PATH" ./bootstrap \
	&& ./configure --prefix=/usr \
	&& make \
	&& make install \
	&& cp -vax examples/*.r /usr/local/bin

RUN cd /usr/local/bin \
	&& mv R Rdevel \
	&& mv Rscript Rscriptdevel \
	&& ln -s Rdevel RD \
	&& ln -s Rscriptdevel RDscript

## Install dependencies
RUN /usr/local/bin/install.r MASS Rcpp BH R6 magrittr dplyr tidyr ggplot2 scales testthat knitr rmarkdown covr

