FROM pydemull/r_4.3.2:latest

RUN apt-get update \
  && apt-get -y install build-essential \
  && apt-get install -y wget \
  && rm -rf /var/lib/apt/lists/* \
  && wget https://github.com/Kitware/CMake/releases/download/v3.24.1/cmake-3.24.1-Linux-x86_64.sh \
      -q -O /tmp/cmake-install.sh \
      && chmod u+x /tmp/cmake-install.sh \
      && mkdir /opt/cmake-3.24.1 \
      && /tmp/cmake-install.sh --skip-license --prefix=/opt/cmake-3.24.1 \
      && rm /tmp/cmake-install.sh \
      && ln -s /opt/cmake-3.24.1/bin/* /usr/local/bin

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

COPY SM6.Rmd SM6.Rmd

COPY SM7.Rmd SM7.Rmd

COPY SM8.Rmd SM8.Rmd

RUN R -e "renv::restore()"

RUN R -e "targets::tar_make()"

CMD mv pipeline_output/* shared_folder/
