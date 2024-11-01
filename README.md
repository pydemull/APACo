
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

The repository has four branches (`master`, `gh-pages`, `docker`, and
`pipeline`):

- The `master` branch hosts the content of the `{APACo}` package. The
  content of the package includes datasets that have been built from the
  data obtained through the measurements implemented in the APA&Co
  project. These datasets actually are censored versions of the original
  datasets to limit the access to only the materials required to
  understand the results of the project. The package also includes the
  code (the functions) that has been developed to analyse the data of
  the project. The code is based on R programming language. Of note, the
  functions of the package have not been written with the aim to make
  them easily reusable in other projects. That being said, one of the
  major functions of the package, called `analyse_change` and that
  provides both figures and tables describing the differences between
  two marginal distributions of paired data, has been developped so that
  it may be reused when working with similar data.

- The `gh-pages` branch hosts the files on which stands the [`{APACo}`
  package website](https://pydemull.github.io/APACo/). This website
  contains a [Reference
  tab](https://pydemull.github.io/APACo/reference/index.html) where the
  list of the datasets and functions included in the package is shown.
  Each item of the list briefly presents the content of the datasets or
  the arguments of the considered function. If you want to know the
  computations that the `{APACo}` package functions implement, you shoud
  go to the `R/` folder of the `master` branch where each file contains
  the code of a given function.

- The `docker` branch contains the code to build a
  [Docker](https://www.docker.com/) image and to push that image to a
  Docker Hub repository. A Docker image is a kind of virtual
  environment, that can be configured as needed, and that is immutable.
  In the present case, the Docker image includes R software (v4.3.2) and
  a version of the `{renv}` package. The interest of this Docker image
  is to provide an environment in which we can be sure that the
  analytical pipeline of the project will work and will provide exactly
  the same results, for ever.

- The `pipeline` branch contains the code that runs the analytical
  pipeline of the APA&Co project. This pipeline, that is based on the
  `{targets}` package, is an organized sequence of actions that use the
  datasets and the functions of the `{APACo}` package to generate the
  results embedded in .tiff figures and .html files. The main interest
  of the pipeline is to automatically (so non-interactively) drive the
  analyses. This is an important step to ensure the reproducibility of
  the analyses and the results. The branch also contains R code that
  uses the `{renv}` package to allow the restoration of all the package
  dependencies relating to the analytical pipeline, which is another
  step required to ensure the reproducibility of the analytical pipeline
  and thus the results. Of note, this branch also hosts a GitHub Actions
  workflow that builds a Docker image from the one hosted in the Docker
  Hub repository (as described above) with the run of the installation
  of the package dependencies of the analytical pipeline, and then the
  run of the analytical pipeline that generates the results of the
  APA&Co project. Then, the workflow opens a Docker container that
  allows to move the results embedded in .tiff/.html files to the
  present GitHub repository. You may access these files by consulting
  the last [dockerized-pipeline workflow
  run](https://github.com/pydemull/APACo/actions/workflows/run-dockerized-pipeline.yml).
  When you are on the web page of the workflow and click on the link of
  a workflow run (the last one for example, at the top of the list), you
  are conducted to a new web page where an `Artifacts` section contains
  the created files embedded in a zipped folder. You will can finally
  download that folder on your machine to consult the results if the
  worklow has been run since less than 90 days.

## How the content of this repository can be used to allow the reproduction of the analyses of the APA&Co project?

We suggest two approaches to reproduce the analyses of the APA&Co
project: running the analytical pipeline on a PC with RStudio; or
running the analytical pipeline on a PC with Docker. These two
approaches have some advantages and limits depending on you are
interested in the understanding or the reproducibility of the analyses.

### Running the analytical pipeline on a PC with RStudio

This approach may be interesting if you want to explore, after running
the analytical pipeline, the content of the created objects and the
content of the functions in the same place, that is, for the present
case, in an RStudio environment. However, the main downside of this
approach is that reproducibility is not fully garanteed. Indeed, the
success of an analytical pipeline run is dependent on various factors
that cannot be controlled by the content of the present repository, in
particular, the current version of R you are using, and the
configuration of the system that lives on your machine. This is why, in
order to reproduce the analytical pipeline exactly as it was performed
to get the results of the project, the Docker-based approach is better,
and actually, not so far to be the best. Anyway, if you want to
reproduce the analytical pipeline on your machine using an RStudio
environment, please follow the steps below.

- Step 1: [Install R](https://cran.rstudio.com/) (recommended versions:
  \>= 4.1.0 and \<= 4.3.3),
  [RStudio](https://posit.co/download/rstudio-desktop/), and the
  corresponding version of
  [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (if your are
  a Windows user) on your machine. To retrieve previous releases of R,
  you can go [here](https://cran.r-project.org/bin/windows/base/old/)
  for Windows, and [here](https://cran.r-project.org/bin/macosx/) for
  Mac.
- Step 2: Go to the
  [`pipeline`](https://github.com/pydemull/APACo/tree/pipeline) branch
  of the APACo repository and click on the green button called `Code`.
  Then click on `Download ZIP`.
- Step 3: Unzip the zipped folder on your machine, open it, and
  double-click on the `APACo.Rproj` file to open the project in RStudio.
- Step 4: Restore the package dependencies of the project with `{renv}`
  using in the Console the command line shown below and then following
  the instructions proposed in the Console.

``` r
renv::restore()
```

This may take several minutes so that all the required packages are
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
the Console. All the .tiff figures and .html files generated during the
run of the analytical pipeline will appear in the `pipeline_out/` folder
created at the root of the project folder during the processing phase.
To have more information about what are the computations actually
performed behind the scene, you can open the `_targets.R` file placed at
the root of the project. This file essentially includes a list of
`targets::tar_target()` functions, with the name of the object created
as first argument, and the operations performed to get that object as
second argument. A quick way of looking at the code could be to copy the
name of a function of interest and paste it in the Console after writing
`APACo::`. For example, for the `analyse_change()` function of the
`{APACo}` package, you could run in the Console the following command:
`APACo::analyse_change`. Another way, which will provide you a more
confortable view of the code, is to go to the [Github
repository](https://github.com/pydemull/APACo) of the `{APACo}` package.
The files placed in the `R/` folder contain the code of each of the
functions developped for the project.

### Running the analytical pipeline on a PC with Docker

Using a Docker image is a robust approach to reproducibly run an
analytical pipeline. Indeed, a Docker image is a kind of virtual
environment where all the settings can be defined as wanted so that all
the analyses can be performed as when developping the analytical
pipeline. Please follow the steps below to use this approach with your
machine:

- Step 1: Install [Docker Desktop](https://www.docker.com/get-started/)
  on your machine.
- Step 2: Open Docker Desktop on your machine. If you are a Windows
  user, you could have a warning message indicating that Docker Desktop
  requires a newer WSL kernel version. As suggested in the message, run
  the command line `wsl --update` in the Command Prompt app (you know,
  this software that shows you only a window with a black background; do
  not be afraid of it). When the installation is done, try again to open
  Docker Desktop.
- Step 3: Go to the
  [`pipeline`](https://github.com/pydemull/APACo/tree/pipeline) branch
  of the APACo repository and click on the green button called `Code`.
  Then click on `Download ZIP`.
- Step 4: Unzip the zipped folder on your machine and open it.
- Step 5: Open, in the unzipped folder, the Command Prompt (Terminal on
  Mac) app. To do this, if you are a Windows user, when you are inside
  the unzipped folder (at its root), you can simply write `cmd` in the
  address bar displayed at the top of your screen and press `Enter`.
  After that, you should see in the Command Prompt window a line
  beginning by something like that:

``` r
C:\Users\YOUR_USER_NAME\Downloads\APACo-pipeline>
```

In the example above (appropriate for Windows users),
`C:\Users\YOUR_USER_NAME\` stands for the current user session, and
`Downloads\APACo-pipeline` stands for the remaining part of the path to
the unzipped folder named `APACo-pipeline`. You may have several folder
names to use in the path between `YOUR_USER_NAME` and `APACo-pipeline`
depending on where you have placed the unzipped folder on your machine.

If you are a Mac user and thus want to open the Terminal app in the
unzipped folder, you may try the following recommendation: ‘*The easiest
way to open Terminal in your current folder on Mac is to use Finder to
navigate to the location and right-click and open Terminal from the
context menu. You can also use the Finder menu or drag and drop folders
on top of the Terminal application in your dock.*’ (source
[here](https://macosx-faq.com/open-terminal-current-folder/#:~:text=The%20easiest%20way%20to%20open,Terminal%20application%20in%20your%20dock)).

If you want to manually indicate to the Command Prompt (or Terminal) app
that you want to work in the unzipped folder, you can open Command
Prompt (or Terminal), and run the following command line by replacing
`YOUR_USER_NAME` by your actual user name and `Downloads/APACo-pipeline`
by the path to the unzipped folder (below, `C:/Users/` stands for an
example for Windows users):

``` r
cd C:/Users/YOUR_USER_NAME/Downloads/APACo-pipeline
```

- Step 6: Ensure you have an internet connexion, and build the Docker
  image with the installation of the required package dependencies and
  the run of the analytical pipeline that generates the results, this by
  running the following command line in the Command Prompt (Terminal)
  app (do not forget the point at the end of the line):

``` r
docker build -t apaco_image .
```

The whole process may take several minutes as it consists in downloading
the base Docker image (with R v4.3.2 and a version of the `{renv}`
package), building a new Docker image on the top of it by downloading
and installing the R package dependencies, and finally running the
analytical pipeline in R.

- Step 7: Create a Docker container to move the files embedding the
  results from a Docker volume to your machine, and more precisely to
  the folder named `shared_folder` that is inside the unzipped folder
  named `APACo-pipeline`. To do this, run the following command line in
  the Command Prompt (or Terminal) app:

``` r
docker run --rm --name apaco_container -v C:/Users/YOUR_USER_NAME/Downloads/APACo-pipeline/shared_folder:/project/shared_folder:rw apaco_image
```

In the example above (appropriate for Windows users),
`C:/Users/YOUR_USER_NAME/Downloads/APACo-pipeline/shared_folder` is the
absolute path to the folder named `shared_folder` at the root of the
unzipped folder named `APACo-pipeline`. As previoulsy described, you
should replace `C:/Users/YOUR_USER_NAME/Downloads/` by the path that
fits your needs. This is the only part of the code you should modify
(assuming the unzipped folder is still named `APACo-pipeline`).

- Step 8: Finally, go to the `shared_folder` folder at the root of the
  unzipped folder (i.e., inside the `APACo-pipeline` folder). The
  results embedded in .tiff figures and .html files should be there. You
  can now close the Command Prompt (or Terminal) app.

While the presented approach is in principle robust to reproduce the
analytical pipeline, the downside of this approach is that you will not
can easily interact with R to explore the content of the functions and
of the intermediate objects created to get the results (it is possible
using additional command lines). However, if you have sufficient
knowledge about R programming language, you can infere the content of
the objects from the code shown in the `master` branch of the APACo
repository.

## How to be sure of the `{APACo}` package version that is used when running the analytical pipeline?

The answer to this question is essential if you want to know exactly
what pieces of code have been used to get the final results of the
project. To have the answer, you have to look at the `renv.lock` file
placed in the `pipeline` branch. This file contains all the package
dependencies needed to run the analytical pipeline. Moreover, for each
package, beyond the version that could be shown, you must look at the
`RemoteSha` field of the package section in the `renv.lock` file to get
the commit hash. This commit hash is a number that allows to identify a
particular commit and then the state of the package when the commit was
performed. Once you get that hash, you can add it to the following
incomplete URL: <https://github.com/pydemull/APACo/tree/>. You will thus
get the final URL where is hosted the package as it was when the commit
was performed. For example, at the moment of writing theses lines, the
hash of the commit corresponding to the version of the `{APACo}` package
used by the analytical pipeline is
`f088468b46b94d80ace755c89ba2c6ce7effafa5`. Thus, the exact code that is
used in the analytical pipeline corresponding to this commit can be
viewed in the `R/` folder at the following adress:
<https://github.com/pydemull/APACo/tree/f088468b46b94d80ace755c89ba2c6ce7effafa5>.

## Is the Docker-based approach presented here so robust to allow the reproduction of the analytical pipeline?

Actually, there may be a vulnerability. Indeed, the process implemented
in the Docker-based approach starts from a Docker image that has an R
version compatible with the analytical pipeline and a working version of
the `{renv}` package only. This means that the initial Docker image has
not yet the package dependencies of the analytical pipeline installed.
Therefore, the package dependencies have to be installed during the
workflow. Unfortunately, among the package dependencies, there are two
non-CRAN packages to be installed: `{rogme}` and `{APACo}`. This is
important because if the two repositories that host these packages
disappear from the web, it may not be possible to install them during
the workflow, and the run of the analytical pipeline will fail. As the
deletion of these two repositories is very unlikely, we did not perform
extra work to build a Docker image that would contain all the required
materials but that would have an interest only for persons who have the
time and the skills to directly work with this image.

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
