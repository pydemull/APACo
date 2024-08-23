FROM pydemull/r_4.3.2:latest

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
