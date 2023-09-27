FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y \
    libicu-dev \
    libglpk-dev \
    libxml2-dev \
    libcairo2-dev \
    libgit2-dev \
    default-libmysqlclient-dev \
    libpq-dev \
    libsasl2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libxtst6 \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libtiff5-dev \
    libjpeg-dev \
    libxt-dev \
    make \
    unixodbc-dev \
    wget \
    zlib1g-dev \
    pandoc

RUN R -e "install.packages('remotes')"

RUN R -e "remotes::install_github('rstudio/renv@0.16.0')"

WORKDIR /project

RUN mkdir -p renv

RUN mkdir -p pipeline_output

RUN mkdir -p shared_folder

COPY renv.lock renv.lock

COPY .Rprofile .Rprofile

COPY renv/activate.R renv/activate.R

COPY renv/settings.json renv/settings.json

COPY _targets.R _targets.R

COPY run.R run.R

COPY run.sh run.sh

COPY main.Rmd main.Rmd

COPY SM3.Rmd SM3.Rmd

COPY SM4.Rmd SM4.Rmd

COPY SM5.Rmd SM5.Rmd

RUN R -e "renv::restore()"

RUN R -e "targets::tar_make()"

CMD mv pipeline_output/* shared_folder/