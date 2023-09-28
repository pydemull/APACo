
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APACo

<!-- badges: start -->

[![R-CMD-check](https://github.com/pydemull/APACo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pydemull/APACo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview of the repository

The aim of this repository is to allow the understanding, the
reproduction, and finally the evaluation of the analytical pipeline
implemented to get the results of the APA&Co project
([ClinicalTrials.gov ID:
NCT04732923](https://clinicaltrials.gov/study/NCT04732923?term=APA%26Co&rank=4)).
The APA&Co project was conducted to observe the change in physical
condition, physical activity, and motivation for physical activity in
coronary artery disease patients at 1 year after having followed a
cardiac rehabilitation program. This research project was managed by the
hospital center of Cholet (France).

The repository has four branches, `master`, `gh-pages`, `docker`, and
`pipeline`:

- The `master` branch hosts the content of the {APACo} package. The
  content of the package includes datasets that have been built from the
  data obtained through the measurements implemented in the APA&Co
  project. These datasets actually are censored versions of the original
  datasets to limit the access to only the materials required to
  understand the results to be reported in scientific publications. The
  package also includes the code (the functions) that has been developed
  to analyse the data of the project. The code is based on R programming
  language. Of note, the functions of the package have not been written
  so that they can be easily reused in other projects. That being said,
  one of the major function of the package, called `analyse_change` and
  that provides both figures and tables describing the differences
  between two marginal distributions of paired data, has been developped
  so that it may be reused when working with similar data.

- The `gh-pages` branch hosts the files on which stands the [{APACo}
  package website](https://pydemull.github.io/APACo/). This website
  contains a [Reference
  tab](https://pydemull.github.io/APACo/reference/index.html) where the
  list of the datasets and functions included in the package is shown.
  Each item of the list briefly presents the content of the datasets or
  the arguments of the considered function. If you want to know the
  computations that the {APACo} package functions implement, you shoud
  go to the `R/` folder of the `master` branch where each file contains
  the code of a given function.

- The `docker` branch contains the code to build a Docker image and to
  push that image to a Docker Hub repository. A Docker image is a kind
  of virtual environment, that can be configured as needed, and that is
  immutable. In the present case, the Docker image includes R software
  (v4.3.1) and a version of the `{renv}` package. The interest of this
  Docker image is to provide an environment in which we can be sure that
  the analytical pipeline of the project will work and will provide
  exactly the same results, forever.

- The `pipeline` branch contains the code that runs the analytical
  pipeline of the APA&Co project. This pipeline, that is based on the
  `{targets}` package, is an organized sequence of actions that use the
  datasets and the functions of the `{APA&Co}` package to generate the
  results embedded in .html files. The main interest of the pipeline is
  to automatically (so non-interactively) drive the analyses. This is an
  important step to ensure the reproducibility of the analyses and the
  results. The branch also contains R code that uses the `{renv}`
  package to allow the restoration of all the package dependencies
  relating to the analytical pipeline, which is another step required to
  ensure the reproducibility of the analytical pipeline and thus the
  results. Of note, this branch also hosts a GitHub Actions workflow
  that builds a Docker image from the one hosted on the Docker Hub
  repository as described above. This new image contains the results of
  the APA&Co project that are stored as .html files in a Docker volume
  after running the installation of the package dependencies of the
  analytical pipeline and then running the analytical pipeline itself.
  After that, the workflow consists in building a Docker container from
  which the .html files are moved to the present GitHub repository. You
  can access these .html files by consulting the last
  [dockerized-pipeline workflow
  run](https://github.com/pydemull/APACo/actions/workflows/run-dockerized-pipeline.yml).
  When you are on the web page of the workflow and click on the link of
  a workflow (the last one for example), you are conducted to a new web
  page where an `Artifacts` section contains the created files embedded
  in a zipped folder. You can finally download that folder on your
  machine to consult the results.

## How the content of this repository can be used to allow the reproduction of the analyses of the APA&Co project

We suggest two approaches to reproduce the analyses of the APA&Co
project : running the analytical pipeline on a PC and its current system
settings; or running the analytical pipeline on the web with GitHub
Actions and Docker. These two approaches are some advantages and limits
depending on you are interested by the understanding or the
reproducibility of the analyses.

### Running the analytical pipeline on a PC and its current system settings

This approach may be interesting if you want to explore, after running
the analytical pipeline, the content of the created objects and the
content of the functions in the same place, that is, in the
environnement where you are running R software. However, the main
downside of this approach is that reproducibility is not fully
garanteed. Indeed, the success of an analytical pipeline run is
dependent on various factors that cannot be controlled by the content of
the present repository, in particular, the current version of R you are
using, and the configuration of the system that lives on your machine.
This is why, in order to reproduce the analytical pipeline exactly as it
was performed to get the results of the project, the second approach is
better, and actually, not so far to be the best. Anyway, if you want to
reproduce the analytical pipeline on your machine in R, please follow
the steps below.

- Step 1: [Install R (version \>= 4.1.0)](https://cran.rstudio.com/),
  [RStudio](https://posit.co/download/rstudio-desktop/), and the [latest
  version of Rtools (if your are a Windows
  user)](https://cran.r-project.org/bin/windows/Rtools/) on your
  machine.
- Step 2: Go to the [`pipeline` branch of the APACo
  repository](https://github.com/pydemull/APACo/tree/pipeline) and click
  on the green button called `Code`. Then click on `Download ZIP`.
- Step 3: Unzip the zipped folder on your machine, open it, and
  double-click on the `APACo.Rproj` file to open the project in RStudio.
- Step 4: Restore the package dependencies of the project with `{renv}`
  using the following command line in the Console:

``` r
renv::restore()
```

This may takes some time so that all the required packages are
downloaded from the web and then installed in the project directory.

- Step 5: Run the analytical pipeline with `{targets}` using the
  following command line in the Console:

``` r
targets::tar_make()
```

This last step will produce all the objects relating to the analytical
process. It also will generate the materials (a .html report with the
main results, .tiff figures, and .html file-based supplementary
materials) that are deemed to be used for a scientific publication. Some
materials will take time to be built because they include several high
resolution figures.

The list of the objects created during the analytical process is shown
in the Console after each ‘target’ expression. Once the analytical
pipeline ended, you will can read any created object running
`targets::tar_read(OBJECT_NAME)` in the Console. You also will can load
any object in the global environment running
`targets::tar_load(OBJECT_NAME)` in the Console. You can load all the
created objects in one go running `targets::tar_load_everything()` in
the Console. All the figures and .html files generated during the run of
the analytical pipeline will appear in the `pipeline_out/` folder placed
at the root of the project. To have more information about what are the
computations actually performed behind the scene, you can open the
`_targets.R` file placed at the root of the project. This file
essentially includes a list of `targets::tar_target()` functions, with
the name of the object created as first argument, and the operations
performed to get that object as second argument. A quick way of looking
at the code could be to copy the name of a function of interest and
paste it in the Console. Another way, which will provide you a more
confortable view of the code, is to go to the [Github repository of the
{APACo} package](https://github.com/pydemull/APACo). The files placed in
the `R/` folder contain the code of each of the functions developped for
the project.

### Running the analytical pipeline on the web with GitHub Actions and Docker

Running an analytical pipeline using a Docker image is a robust approach
to make it reproducible, forever. This approach has already been a
little described above during the `pipeline` branch description.
Actually, this approach could be implemented on a personal machine where
Docker software would be installed but as this may not be easy to
implement by people that are not familiar with this, we do not describe
the steps to do this. Instead, it is possible to rerun the analytical
pipeline on the web in the APACo repository. To do this, go to the [web
page relating to the run-dockerized-pipeline
worflow](https://github.com/pydemull/APACo/actions/workflows/run-dockerized-pipeline.yml).
Then, click, on the last worklow if there are several workflows that
have been run. On the rigth of the screen, you will have a button
`Re-run all jobs`. If you confirm this action, the workflow will run
another time. Then, if you click on the `build` button, you will have a
view of all the installation and analytical steps performed by the
workflow. The downside of this approach is that you will not can
interact with the content to explore the intermediate objects created to
finally get the results. However, if you have sufficient knowledge about
R programming language, you can infere the content of these objects from
the code shown in the `master` branch of the APACo repository.

## How to be sure of the {APACo} package version that is used when running the analytical pipeline?

The answer to this question is essential if you want to know exactly
what pieces of code have been used to get the final results of the
project. To have the answer, you have to look at the `renv.lock` file
placed in the `pipeline` branch. This file contains all the package
dependencies needed to run the analytical pipeline. Moreover, for each
package, beyond the version that could be shown, you must look at the
`RemoteSha` field of the package section in the `renv.lock` file to get
the the commit hash. This commit hash is a number that allows to
identify a particular commit and then the state of the package when the
commit was performed. Once you get that hash, you can add it to the
following incomplete URL: <https://github.com/pydemull/APACo/tree/>. You
will thus get the final URL where is hosted the package as it was when
the commit was performed. For example, at the moment of writing theses
lines, the hash of the commit corresponding to the version of the
{APACo} package used by the analytical pipeline is
`10c80e32a70ff523a02df61219a14ae2e8ef4274`. Thus, the exact code that is
used in the analytical pipeline corresponding to this commit can be
viewed in the `R/` folder at the following adress:
<https://github.com/pydemull/APACo/tree/10c80e32a70ff523a02df61219a14ae2e8ef4274>

## Licenses

### Code

The code of the `{APACo}` package is provided under GNU General Public
License Version 3.0 (please see LICENSE.md file).

### Datasets

<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />The
datasets included in the `{APACo}` package are provided under
<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">License
Creative Commons Attribution NonCommercial-NoDerivatives 4.0
International (CC BY-NC-ND 4.0)</a>.

## Acknowledgements

The Dockerfiles and GitHub Actions workflows that allow an exact
reproduction of the analytical pipeline of the APA&Co project are
versions updated from [Bruno Rodrigues](https://github.com/b-rodrigues)’
work. More precisely:

- The files from the `docker` branch of the APACo repository are based
  on the following repository: <https://github.com/b-rodrigues/ga_demo>.
- The files from the `pipeline` branch of the APACo repository are based
  on the following repository:
  <https://github.com/b-rodrigues/ga_demo_rap>.
